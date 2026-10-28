# Battery Model - Daily Average Energy & Financial Benefits
# Calculates self-sufficiency, stored solar, export, and arbitrage headroom
#
# Dependency: sourced by energy_savings_model.R via source(battery_model.R),
# after model_defaults.csv is loaded — the signature defaults read dflt().
# Fields consumed by energy_savings_model.R::evaluate_solar_battery():
#   export_kwh                    — daytime export; annualised and valued at the feed-in tariff
#   arbitrage_headroom_kwh        — stored solar exported in the evening; annualised and valued
#                                   at tiered 4-8pm wholesale prices for Amber, the FiT otherwise
#   battery_self_consumption_kwh  — stored solar discharged to house load; valued at the
#                                   household's appliance grid price
#   battery_ev_charge_kwh         — stored solar discharged to the EV; valued at the EV's
#                                   home-charging price
#
# Stored-solar dispatch order (all three outputs partition the same usable
# stored kWh, so they can be summed without double-counting):
#   1. House load not already met by daytime solar  → battery_self_consumption_kwh
#   2. Any surplus beyond that is routed by value, via `surplus_ev_first`:
#        TRUE  → battery_ev_charge_kwh first (capped at the EV's unmet
#                home-charging load), then arbitrage_headroom_kwh
#        FALSE → arbitrage_headroom_kwh first, then battery_ev_charge_kwh
#      The caller decides by comparing the EV's avoided import price against the
#      evening export price — see evaluate_solar_battery(). There is no
#      caller-facing on/off switch: sending energy to the lower-value use is
#      never the right answer.
#
# arbitrage_headroom_kwh is therefore "stored solar exported in the evening",
# whatever the tariff — evaluate_solar_battery() prices it at tiered wholesale
# for Amber and at the flat feed-in tariff otherwise. Nothing is dropped: the
# residual left once the EV is full is always exported.

battery_model <- function(
    daily_solar_generation_kwh,       # annual-average daily solar (kWh)
    daily_consumption_kwh,
    self_sufficiency_pct,
    battery_size_kwh,
    battery_round_trip_efficiency,
    household_safeguard_pct,
    useable_capacity_pct      = dflt("battery_useable_capacity_pct"),         # fraction of rated capacity that is useable
    annual_degradation_pct    = dflt("battery_degradation_1yr"),              # annual capacity loss (year-1 rate; callers pass the 15-yr rate)
    years                     = 1,                                            # calculation horizon (1 or 15) — drives time-averaged capacity
    min_grid_consumption_pct  = dflt("battery_min_grid_consumption_pct"),     # caps the REPORTED self-sufficiency only; no $ effect
    solar_seasonal_weights    = c(spring = dflt("solar_seasonal_weight_spring"),  # relative solar generation by season
                                  summer = dflt("solar_seasonal_weight_summer"),
                                  autumn = dflt("solar_seasonal_weight_autumn"),
                                  winter = dflt("solar_seasonal_weight_winter")),
    # --- Battery-eligible load split ---
    # Some loads use solar during the day but should NOT be served by the home
    # battery in the evening (e.g. EVs, which draw off-peak grid overnight for
    # any kWh not solar-charged). The arbitrage_headroom calc reserves battery
    # capacity to cover daily_battery_eligible_load_kwh only — non-eligible
    # loads still appear in daily_consumption_kwh / self_sufficiency_pct so
    # that solar self-consumption and export are calculated correctly.
    # Defaults preserve the original "all load is battery-eligible" behaviour.
    daily_battery_eligible_load_kwh         = NULL,
    daily_battery_eligible_solar_target_kwh = NULL,
    # --- Surplus stored-solar dispatch ---
    # Which of the two surplus uses gets first claim. TRUE (default) sends
    # stored solar to the EV before exporting, which is what the prices say:
    # charging the car avoids a 24-33c/kWh import, while evening wholesale
    # export earns 2-14c/kWh. Set FALSE only if export prices exceed the EV's
    # avoided cost. Whichever runs second takes what's left; nothing is dropped.
    surplus_ev_first                        = TRUE,
    # Daily kWh of home EV charging still to be met after daytime solar.
    # 0 (default) means no EV, so the surplus routes straight to export.
    daily_ev_unmet_load_kwh                 = 0
) {

  # Default to "everything is battery-eligible"
  if (is.null(daily_battery_eligible_load_kwh)) {
    daily_battery_eligible_load_kwh <- daily_consumption_kwh
  }
  if (is.null(daily_battery_eligible_solar_target_kwh)) {
    daily_battery_eligible_solar_target_kwh <- daily_consumption_kwh * self_sufficiency_pct
  }

  # Effective battery capacity: time-averaged over the analysis horizon so a
  # 15-year run uses average capacity (~86% of nameplate at 2.2%/yr), not the
  # end-of-life value (~72%). Linear approximation between year-0 capacity (1)
  # and year-N capacity ((1 - d)^N); within ~1% of the exact exponential
  # integral for the rates used here.
  avg_capacity_factor   <- (1 + (1 - annual_degradation_pct) ^ years) / 2
  effective_battery_kwh <- battery_size_kwh *
    useable_capacity_pct *
    avg_capacity_factor

  max_self_sufficiency <- 1 - min_grid_consumption_pct

  # Normalise weights → multipliers that average to 1, preserving the annual average solar input
  solar_multipliers <- solar_seasonal_weights / mean(solar_seasonal_weights)
  season_names <- names(solar_multipliers)

  # Calculate metrics for each season (0.25 years each)
  season_results <- lapply(season_names, function(season) {

    solar <- daily_solar_generation_kwh * solar_multipliers[[season]]

    # Total solar self-consumed by the home meter (battery-eligible + non-eligible
    # loads). Capped by daily solar generation in low-solar (winter) seasons.
    load_met_by_solar <- pmin(daily_consumption_kwh * self_sufficiency_pct, solar)

    # Battery-eligible (e.g. appliance) share of the solar self-consumption.
    # Eligible loads get priority over non-eligible loads (e.g. EVs) when solar
    # is scarce: appliances absorb up to their target before the EV does.
    battery_eligible_solar_consumed <- pmin(
      daily_battery_eligible_solar_target_kwh,
      load_met_by_solar
    )

    # Daily battery-eligible load not yet covered by solar — what the battery
    # is reserved to cover in the evening (drives arbitrage_headroom below).
    load_unmet_by_solar <- pmax(
      daily_battery_eligible_load_kwh - battery_eligible_solar_consumed,
      0
    )

    # Solar stored in battery (capped at effective battery capacity)
    stored_solar <- pmin(
      (solar - load_met_by_solar) * battery_round_trip_efficiency,
      effective_battery_kwh
    )

    # Exported solar
    export <- solar -
      ((stored_solar / battery_round_trip_efficiency) +
       load_met_by_solar)

    # Self-sufficiency (capped at 1 - min_grid_consumption_pct)
    self_sufficiency <- pmin(
      (stored_solar + load_met_by_solar) / daily_consumption_kwh,
      max_self_sufficiency
    )

    # --- Stored-solar dispatch ---------------------------------------------
    # Usable stored solar after the household reserve (safeguard) is held back.
    # This is the pool that gets partitioned three ways below.
    usable_stored <- stored_solar * (1 - household_safeguard_pct)

    # 1. House load first: evening discharge covering battery-eligible load the
    #    daytime solar didn't reach. Can't exceed either the pool or the load.
    battery_self_consumption <- pmin(usable_stored, load_unmet_by_solar)

    # 2. Whatever is left after the house is satisfied.
    surplus_stored <- pmax(usable_stored - load_unmet_by_solar, 0)

    # 3. Surplus routing, higher-value use first. The EV can only absorb its
    #    unmet home-charging load; the evening export window is unbounded here
    #    (the inverter tier cap is applied when the export is valued, in
    #    evaluate_solar_battery). Whatever the first claim can't take falls
    #    through to the second, so the surplus is fully accounted for.
    ev_capacity <- pmax(daily_ev_unmet_load_kwh, 0)
    if (surplus_ev_first) {
      battery_ev_charge  <- pmin(surplus_stored, ev_capacity)
      arbitrage_headroom <- surplus_stored - battery_ev_charge
    } else {
      arbitrage_headroom <- surplus_stored
      battery_ev_charge  <- 0
    }

    data.frame(
      season                        = season,
      solar_multiplier              = solar_multipliers[[season]],
      daily_solar_generation_kwh    = solar,
      daily_consumption_kwh         = daily_consumption_kwh,
      self_consumption_pct          = (daily_consumption_kwh * self_sufficiency) / solar,
      battery_size_kwh              = battery_size_kwh,
      useable_capacity_pct          = useable_capacity_pct,
      annual_degradation_pct        = annual_degradation_pct,
      years                         = years,
      effective_battery_kwh         = effective_battery_kwh,
      battery_round_trip_efficiency = battery_round_trip_efficiency,
      household_safeguard_pct       = household_safeguard_pct,
      min_grid_consumption_pct      = min_grid_consumption_pct,
      load_unmet_by_solar_kwh       = load_unmet_by_solar,
      stored_solar_kwh              = stored_solar,
      export_kwh                    = export,
      self_sufficiency_pct          = self_sufficiency,
      surplus_ev_first              = surplus_ev_first,
      # The three dispatch outputs partition usable_stored exactly:
      #   battery_self_consumption + battery_ev_charge + arbitrage_headroom
      #     == stored_solar * (1 - household_safeguard_pct)
      battery_self_consumption_kwh  = battery_self_consumption,
      arbitrage_headroom_kwh        = arbitrage_headroom,
      battery_ev_charge_kwh         = battery_ev_charge
    )
  })

  do.call(rbind, season_results)
}

# --- Example usage ---

# 1-year scenario (4% first-year degradation) — returns 4 rows, one per season
result_1yr <- battery_model(
  daily_solar_generation_kwh = 30,
  daily_consumption_kwh      = 18,
  self_sufficiency_pct       = 0.5,
  battery_size_kwh           = 10,
  battery_round_trip_efficiency = 0.90,
  household_safeguard_pct    = 0.20,
  useable_capacity_pct       = 0.9,
  annual_degradation_pct     = 0.04,
  years                      = 1
)

# 15-year scenario (2.2% average annual degradation) — returns 4 rows, one per season
result_15yr <- battery_model(
  daily_solar_generation_kwh = 20,
  daily_consumption_kwh      = 18,
  self_sufficiency_pct       = 0.5,
  battery_size_kwh           = 10,
  battery_round_trip_efficiency = 0.90,
  household_safeguard_pct    = 0.20,
  useable_capacity_pct       = 0.9,
  annual_degradation_pct     = 0.022,
  years                      = 15
)

print(result_1yr[, c("season", "daily_solar_generation_kwh", "self_sufficiency_pct", "arbitrage_headroom_kwh")])
print(result_15yr[, c("season", "daily_solar_generation_kwh", "self_sufficiency_pct", "arbitrage_headroom_kwh")])
