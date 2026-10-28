# =============================================================================
# Energy Savings Model 2026
# Function-level cost comparison: gas/fossil vs electric appliances
# =============================================================================
#
# Sourcing this file defines the model functions and loads every input CSV from
# data_dir. It runs no examples — see energy_savings_comparisons.R for those.
# Change history lives in MODEL_CHANGE_HISTORY.md, not in comments here.

library(tidyverse)

# ---------------------------------------------------------------------------
# 0. File paths
# ---------------------------------------------------------------------------
# Keep these two assignments at the start of a line: external harnesses
# (e.g. electric-saul-comparison) patch them with a ^data_dir / ^model_dir regex.
# options(esm.data_dir = ..., esm.model_dir = ...) before sourcing points the
# model at another copy of the inputs and scripts (e.g. the GitHub repo's
# run_model.R); otherwise it uses the shared-drive folders below.

data_dir <- getOption("esm.data_dir", "/Users/calumharvey-scholes/Library/CloudStorage/GoogleDrive-calum@rewiringaustralia.org/Shared drives/Rewiring Australia Shared Drive/Research/Data/Clean data - for internal use/Energy savings report data/Model input data")

model_dir <- getOption("esm.model_dir", "/Users/calumharvey-scholes/Library/CloudStorage/GoogleDrive-calum@rewiringaustralia.org/Shared drives/Rewiring Australia Shared Drive/Research/Projects/Energy savings 2026 Model")

# ---------------------------------------------------------------------------
# 0a. Model defaults (model_defaults.csv)
# ---------------------------------------------------------------------------
# Every default value and modelling constant that isn't a market price lives in
# model_defaults.csv (parameter, value, type, group, applies_to, description).
# Function signatures read them through dflt(), which R evaluates lazily — at
# call time — so editing MODEL_DEFAULTS after sourcing takes effect on the next
# call. `type` is one of numeric / character / logical / character_vector
# (semicolon-separated) / number_or_keyword (a number, or a keyword such as
# state_average that the model resolves at run time).

.parse_default <- function(value, type) {
  switch(type,
    numeric          = as.numeric(value),
    # A number, or a keyword the model resolves at run time (e.g. state_average).
    number_or_keyword = { v <- suppressWarnings(as.numeric(value)); if (is.na(v)) value else v },
    character        = if (identical(value, "NA")) NA_character_ else value,
    logical          = as.logical(value),
    character_vector = trimws(strsplit(value, ";", fixed = TRUE)[[1]]),
    stop("model_defaults.csv: unknown type '", type, "'")
  )
}

# options(esm.model_defaults_file = "...") before sourcing points the model at a
# different defaults file (e.g. a scenario or test variant) without editing the
# canonical one.
model_defaults_file  <- getOption("esm.model_defaults_file",
                                  file.path(data_dir, "model_defaults.csv"))
model_defaults_table <- read_csv(model_defaults_file,
                                 col_types = cols(.default = col_character()),
                                 na = character())
MODEL_DEFAULTS <- setNames(
  Map(.parse_default, model_defaults_table$value, model_defaults_table$type),
  model_defaults_table$parameter
)

dflt <- function(name) {
  if (!name %in% names(MODEL_DEFAULTS)) {
    stop("model_defaults.csv has no row for '", name, "'", call. = FALSE)
  }
  MODEL_DEFAULTS[[name]]
}

# ---------------------------------------------------------------------------
# 0b. Modelling constants (values from model_defaults.csv)
# ---------------------------------------------------------------------------

# Apartments use less energy than detached houses (ABS/AEMO). Applied to all
# appliance energy; vehicle energy is unaffected.
APARTMENT_ENERGY_FACTOR <- dflt("apartment_energy_factor")

# Default whole-home PV + battery size by dwelling. A detached house has the
# roof and load for a large system; an apartment's share of a strata roof
# supports a much smaller array (costed on microinverters — see
# MICROINVERTER_MAX_SOLAR_KWP). Every function taking solar_kwp /
# battery_size_kwh defaults them to NULL and resolves them here; pass explicit
# values to override.
DEFAULT_SYSTEM_SIZE <- list(
  house     = list(solar_kwp = dflt("house_solar_kwp"),
                   battery_size_kwh = dflt("house_battery_kwh")),
  apartment = list(solar_kwp = dflt("apartment_solar_kwp"),
                   battery_size_kwh = dflt("apartment_battery_kwh"))
)

default_system_size <- function(dwelling_type, solar_kwp = NULL,
                                  battery_size_kwh = NULL) {
  d <- DEFAULT_SYSTEM_SIZE[[if (identical(dwelling_type, "apartment")) "apartment" else "house"]]
  list(solar_kwp        = if (is.null(solar_kwp))        d$solar_kwp        else solar_kwp,
       battery_size_kwh = if (is.null(battery_size_kwh)) d$battery_size_kwh else battery_size_kwh)
}

# Battery degradation for a horizon: the first-year rate for 1-year runs, the
# long-run average for 15-year runs.
default_degradation <- function(n_years) {
  if (n_years >= 15) dflt("battery_degradation_15yr") else dflt("battery_degradation_1yr")
}

# ---------------------------------------------------------------------------
# Solar scenarios and the per-load fraction table (solar_fraction_table.csv)
# ---------------------------------------------------------------------------
# The solar scenario is set at the household level — every appliance in the
# household uses the same one:
#
# "grid_only"        — no solar (0% for all appliances)
# "solar"            — typical rooftop solar, no load-shifting optimisation
# "solar_optimised"  — solar + active load-shifting (timers on hot water, etc.)
#
# solar_fraction_table.csv holds one row per load (category), optionally
# overridden for a specific appliance_type (blank = category default). Columns:
#
# `grid_only` / `solar` / `solar_optimised`
#              — share of each load that falls within the solar generation
#                window, i.e. can be met directly from on-site solar.
#                "Other Cooking" and "Other Electronics" are low because
#                refrigeration runs 24/7; only daytime washing / dishwasher /
#                microwave use lands in the solar window. Pool pumps are
#                timer-driven and easily scheduled into the solar window.
#
# `sso_grid_only` / `sso_solar` / `sso_solar_optimised`
#              — share of the load that can realistically be drawn inside the
#                3-hour 11am-2pm Solar Sharer free window. Only consulted by the
#                solar_sharer tariff (see TARIFFS below). This is the same
#                question the solar columns ask but for a 3-hour block rather
#                than the ~8-hour solar day, so the values sit below
#                solar_optimised for everything except vehicles — an EV parked
#                at home can absorb a 3-hour block far more readily than it can
#                spread charging across the whole solar window.
#
#                One column per solar scenario, because the three scenarios
#                encode increasing load-shifting sophistication and the free
#                window depends on exactly that. NOTE the semantics differ from
#                the solar columns: grid_only is NOT zero here. A household with
#                no panels can still use the free window — it is their only
#                source of cheap energy — it just does so less deliberately,
#                without timers, smart EVSE or the habits that come with having
#                optimised around solar.
#
#                Without these limits the daily free cap never binds: 24 kWh is
#                ~90%+ of an all-electric household's daily load, so "free up to
#                the cap" would collapse to "free", cutting modelled bills by
#                75-80%.
#
#                SENSITIVITY: the Vehicles row dominates — swinging it 0 -> 0.8
#                moves annual savings by ~$1,050/yr in grid_only and ~$520 in
#                solar_optimised, roughly 3x the next-largest load (space
#                heating) and 6x water heating. Tune that row first.
#
# `min_retail` — floor on the share of the load that must be bought from the
#                grid at the tariff's standard rate, whatever solar, battery
#                and free-window capacity are notionally available. Covers
#                unavoidable round-the-clock draw (refrigeration, standby,
#                overnight heating/cooling) that no amount of storage reliably
#                carries on every day of the year. Bounds free window + solar +
#                battery together (see split_load()).
SOLAR_SCENARIOS <- c("grid_only", "solar", "solar_optimised")

SOLAR_FRACTION_TABLE <- read_csv(
  file.path(data_dir, "solar_fraction_table.csv"),
  col_types = cols(category = col_character(), appliance_type = col_character(),
                   .default = col_double())
)

# Look up one column of SOLAR_FRACTION_TABLE for a category, preferring an
# appliance-type-specific row over the category default. Base-R subsetting:
# this runs thousands of times per comparison table.
.solar_table_lookup <- function(category, column, appliance_type = NULL) {
  cats  <- SOLAR_FRACTION_TABLE$category
  types <- SOLAR_FRACTION_TABLE$appliance_type
  if (!is.null(appliance_type)) {
    i <- which(cats == category & !is.na(types) & types == appliance_type)
    if (length(i) > 0) return(SOLAR_FRACTION_TABLE[[column]][i[1]])
  }
  i <- which(cats == category & is.na(types))
  if (length(i) == 0) return(0)
  SOLAR_FRACTION_TABLE[[column]][i[1]]
}

get_solar_fraction <- function(category, solar_scenario = "grid_only",
                                appliance_type = NULL) {
  if (!solar_scenario %in% SOLAR_SCENARIOS) {
    stop("solar_scenario must be one of: ", paste(SOLAR_SCENARIOS, collapse = ", "))
  }
  .solar_table_lookup(category, solar_scenario, appliance_type)
}

# Share of this load that can be drawn inside the Solar Sharer free window,
# for the given solar scenario (the scenario selects the column — it encodes how
# sophisticated the household's load shifting is; see the table header).
get_free_window_fraction <- function(category, solar_scenario = "solar_optimised",
                                      appliance_type = NULL) {
  if (!solar_scenario %in% SOLAR_SCENARIOS) {
    stop("solar_scenario must be one of: ", paste(SOLAR_SCENARIOS, collapse = ", "))
  }
  .solar_table_lookup(category, paste0("sso_", solar_scenario), appliance_type)
}

# Share of a load actually served free, once the min_retail floor is honoured.
# Free-window energy is dispatched BEFORE solar (see the dispatch-order note on
# get_household_free_window), so this depends only on the load itself — not on
# how much solar is available. That's what keeps the free-window and solar
# calculations non-circular.
free_window_share <- function(category, solar_scenario = "solar_optimised",
                                appliance_type = NULL, free_window_scale = 1) {
  share <- min(get_free_window_fraction(category, solar_scenario, appliance_type),
               1 - get_min_retail_fraction(category, appliance_type))
  max(share, 0) * free_window_scale
}

# Floor on the share of this load bought at the tariff's standard grid rate.
get_min_retail_fraction <- function(category, appliance_type = NULL) {
  .solar_table_lookup(category, "min_retail", appliance_type)
}

# ---------------------------------------------------------------------------
# split_load(): the one place a load is divided between supply sources
# ---------------------------------------------------------------------------
# Dispatch order for any electric load is free window > solar > (battery) >
# paid grid. The min_retail floor bounds free window + solar together, so
# on-site supply never covers more than (1 - min_retail) of the load. The
# battery is held to the same floor at household level (see
# evaluate_solar_battery's battery_eligible_load).
#
# Every caller that splits a load — evaluate_option(), .household_context(),
# solar_battery_rows() and evaluate_household()'s baseload row — goes through
# here, so the order and the floor can't drift apart between them.
#
# Args:
#   load_kwh        — daily kWh of the load (already net of public fast charging)
#   solar_frac      — share of the load demanding direct solar (already × any
#                     generation cap, i.e. solar_scale_factor)
#   free_frac       — share eligible for the free window (already × the free-
#                     window cap); 0 when the tariff has no free window
#   min_retail_frac — floor share always bought at the standard rate
#
# Returns a list of daily kWh: min_retail, free, solar, paid (= load - free -
# solar, i.e. what the battery or the paid grid must cover).
split_load <- function(load_kwh, solar_frac = 0, free_frac = 0, min_retail_frac = 0) {
  if (length(load_kwh) != 1 || is.na(load_kwh) || load_kwh <= 0) {
    return(list(min_retail = 0, free = 0, solar = 0, paid = 0))
  }
  min_retail <- load_kwh * min_retail_frac
  serveable  <- max(load_kwh - min_retail, 0)
  free       <- min(load_kwh * free_frac, serveable)
  solar      <- min(load_kwh * solar_frac, max(serveable - free, 0))
  list(min_retail = min_retail, free = free, solar = solar,
       paid = max(load_kwh - solar - free, 0))
}

# split_load() for a SOLAR_FRACTION_TABLE category, resolving the three
# fractions from the table, the tariff spec and the household-level caps.
#   spec        — get_tariff_spec() result (NULL = no free window)
#   solar_scale — household generation cap (get_household_self_sufficiency)
#   free_scale  — household free-window cap (allocate_free_window)
split_category_load <- function(load_kwh, category, solar_scenario, spec = NULL,
                                  solar_scale = 1, free_scale = 1,
                                  appliance_type = NULL) {
  free_frac <- if (!is.null(spec) && isTRUE(spec$free_window))
    free_window_share(category, solar_scenario, appliance_type, free_scale) else 0
  split_load(load_kwh,
             solar_frac      = get_solar_fraction(category, solar_scenario, appliance_type) * solar_scale,
             free_frac       = free_frac,
             min_retail_frac = get_min_retail_fraction(category, appliance_type))
}

# ---------------------------------------------------------------------------
# round_output(): rounding for presentation only
# ---------------------------------------------------------------------------
# Model functions return full precision so rounding never compounds through a
# calculation. Round only when a table is printed, written to CSV or charted:
#
#   evaluate_household(...)$summary |> round_output() |> write_csv(...)
#
# Precision is chosen from the column name:
#   *_pct / *_share / *_fraction / *scale* / *factor*  → 3 dp
#   $ per kWh prices (dol_kwh, price_kwh, cost_per_kwh) → 4 dp
#   emissions (tCO2e)                                    → 2 dp
#   energy (any other column containing "kwh")          → 2 dp
#   inputs that describe the household / system         → untouched
#   everything else (dollars)                            → 0 dp
ROUND_OUTPUT_KEEP <- c("n_occupants", "n_vehicles", "solar_kwp", "battery_size_kwh",
                       "free_battery_kwh", "inverter_kw", "years", "n_units",
                       "loan_rate", "loan_term", "ev_tariff_share")

round_output <- function(df, keep = ROUND_OUTPUT_KEEP) {
  digits_for <- function(col) {
    if (col %in% keep)                                         return(NA)
    if (grepl("_pct$|_share$|_fraction$|scale|factor|pct_", col)) return(3)
    if (grepl("dol_kwh|price_kwh|cost_per_kwh|fit_price", col))  return(4)
    if (grepl("tco2e", col, ignore.case = TRUE))               return(2)
    if (grepl("kwh", col, ignore.case = TRUE))                 return(2)
    0
  }
  for (col in names(df)) {
    if (!is.numeric(df[[col]])) next
    d <- digits_for(col)
    if (!is.na(d)) df[[col]] <- round(df[[col]], d)
  }
  df
}

# ---------------------------------------------------------------------------
# Realistic EV charge mix
# ---------------------------------------------------------------------------
# Australian EVs typically charge at home most of the time but rely on public
# DC fast chargers for road trips, top-ups, and households without off-street
# parking. We model this as a fixed fraction of total vehicle energy that is
# obtained at fast chargers rather than from the home meter:
#
#   - Fast-charged kWh are priced at the "ev_fast_charge" fuel row (national
#     flat rate from Econnex 2025; no daily/standing charge).
#   - Fast-charged kWh do NOT receive any solar self-consumption credit.
#   - Fast-charged kWh do NOT contribute to the home electricity supply
#     charge denominator (they don't pass through the home meter).
#   - The remaining (1 - FAST_CHARGE_FRACTION) is home charging, which is
#     priced according to the household's `tariff` (see TARIFFS below) and IS
#     eligible for solar self-consumption per the household solar scenario.
#
# FAST_CHARGE_DOL_KWH is only a fallback for a fuel_prices CSV with no
# ev_fast_charge rows.
FAST_CHARGE_FRACTION <- dflt("fast_charge_fraction")
FAST_CHARGE_DOL_KWH  <- dflt("fast_charge_dol_kwh_fallback")

# Source battery model — defines battery_model(); its example runs are
# suppressed. Sourced after the defaults because its signature reads dflt().
invisible(capture.output(
  source(file.path(model_dir, "battery_model.R"))
))

# ---------------------------------------------------------------------------
# 1. Load data
# ---------------------------------------------------------------------------

energy_use <- read_csv(file.path(data_dir, "average_energy_use_by_appliance_and_state.csv"))
fuel_prices <- read_csv(file.path(data_dir, "fuel_prices_by_state_simple.csv")) |>
  # Normalise the national state code: older versions of fuel_prices_by_state.R
  # wrote "Australia" for the population-weighted national row, but every other
  # CSV in this model uses "AUS". Convert on load so lookups stay consistent.
  mutate(state = ifelse(state == "Australia", "AUS", state))
solar_lcoe <- read_csv(file.path(data_dir, "solar_lcoe_by_state.csv"), show_col_types = FALSE)
product_prices <- read_csv(file.path(data_dir, "Energy Savings 2026 - input data - FINAL product prices.csv"))
scaling_factors <- read_csv(file.path(data_dir, "energy_consumption_scaling_factors.csv"))
emissions_factors <- read_csv(file.path(data_dir, "emissions_factors_by_state.csv"))
vehicle_costs <- read_csv(file.path(data_dir, "vehicle_costs_2026.csv"))
km_per_day <- read_csv(file.path(data_dir, "average_km_per_day_by_state.csv"))
pop_weights <- read_csv(file.path(data_dir, "state_population_weights.csv"))
vehicle_efficiency <- read_csv(file.path(data_dir, "vehicle_average_efficiency_by_class.csv"))
appliance_subsidies <- read_csv(file.path(data_dir, "Appliance_subsidies.csv"), show_col_types = FALSE)
vehicle_maintenance <- read_csv(file.path(data_dir, "Vehicle_maintenance.csv"), show_col_types = FALSE)

# Number of heating units a typical home in each state owns (e.g. ducted +
# split-system combos, multiple plug-in heaters). Used to scale heating capex
# only — energy use is already a whole-of-household figure in
# average_energy_use_by_appliance_and_state.csv, so no per-unit scaling there.
heater_counts <- read_csv(file.path(data_dir, "Heater_numbers_by_state.csv"),
                          show_col_types = FALSE) |>
  rename(state = State, n_heaters = `Number of heaters`)

# WA postcode → distribution-network mapping. Used to decide which WA battery
# subsidy (Synergy vs Horizon) applies. Optional — model still loads if the
# CSV is missing (network lookup just returns NA, no WA battery subsidy fires).
# Expected columns: postcode (int), network ("Synergy" or "Horizon").
wa_postcode_network <- tryCatch(
  read_csv(file.path(data_dir, "WA_postcode_network.csv"), show_col_types = FALSE),
  error = function(e) NULL
)

# Per-state monthly evening peak prices (4pm-8pm), built by
# evening_peak_export_prices.R. Used to value Amber evening exports.
evening_peak_prices_monthly <- read_csv(
  file.path(data_dir, "evening_peak_prices_monthly.csv"), show_col_types = FALSE
)

# Per-state household averages (vehicles per household, heaters). Used by
# household(n_vehicles = "state_average").
state_average_values <- read_csv(
  file.path(data_dir, "state_average_values.csv"), show_col_types = FALSE
)

# ---------------------------------------------------------------------------
# 1a. Lookup indexes
# ---------------------------------------------------------------------------
# The getters below run thousands of times per comparison table, so they look
# rows up by a pre-built key rather than filter()-ing a data frame each call.
# Keys are "a|b" strings; values are row numbers in the source table.

.row_index <- function(...) {
  keys <- paste(..., sep = "|")
  idx  <- seq_along(keys)
  # First occurrence wins, matching the [1] the getters take on a filter().
  idx  <- idx[!duplicated(keys)]
  setNames(idx, keys[idx])
}
.FUEL_PRICE_ROW <- .row_index(fuel_prices$state, fuel_prices$fuel)
.ENERGY_USE_ROW <- .row_index(energy_use$Category, energy_use$`Appliance Type`)
.EMISSIONS_ROW  <- .row_index(emissions_factors$state)

# ---------------------------------------------------------------------------
# 2. Clean product prices — remove commas from "Total installed" and parse
# ---------------------------------------------------------------------------

product_prices <- product_prices |>
  mutate(`Total installed` = as.numeric(gsub(",", "", `Total installed`)))

# ---------------------------------------------------------------------------
# 3. Helper: occupancy scaling factor
# ---------------------------------------------------------------------------

.SCALING_FACTORS_SORTED <- scaling_factors |> arrange(Occupants)

get_scaling_factor <- function(n_occupants) {
  # Lookup from scaling_factors table; interpolate if between values
  sf <- .SCALING_FACTORS_SORTED

  if (n_occupants %in% sf$Occupants) {
    return(sf$`Energy Consumption Scaling Factor`[sf$Occupants == n_occupants])
  }

  # Linear interpolation between nearest points
  below <- sf |> filter(Occupants <= n_occupants) |> slice_tail(n = 1)
  above <- sf |> filter(Occupants >= n_occupants) |> slice_head(n = 1)

  if (nrow(below) == 0) return(above$`Energy Consumption Scaling Factor`)
  if (nrow(above) == 0) return(below$`Energy Consumption Scaling Factor`)

  frac <- (n_occupants - below$Occupants) / (above$Occupants - below$Occupants)
  below$`Energy Consumption Scaling Factor` + frac * (above$`Energy Consumption Scaling Factor` - below$`Energy Consumption Scaling Factor`)
}

# ---------------------------------------------------------------------------
# 4. Helper: annual loan payment for a given principal
# ---------------------------------------------------------------------------

annual_loan_payment <- function(principal, rate = dflt("loan_rate"),
                                term_years = dflt("loan_term")) {
  if (principal <= 0) return(0)
  monthly_rate <- rate / 12
  n_payments <- term_years * 12
  monthly_payment <- principal * monthly_rate / (1 - (1 + monthly_rate)^(-n_payments))
  monthly_payment * 12
}

# ---------------------------------------------------------------------------
# 5. Helper: get fuel price for a state and fuel type
# ---------------------------------------------------------------------------

get_fuel_price <- function(state, fuel, period = "1year") {
  i <- .FUEL_PRICE_ROW[paste(state, fuel, sep = "|")]
  if (is.na(i)) return(list(dol_kwh = NA_real_, daily_charge = NA_real_))
  row <- fuel_prices[i, ]

  if (period == "1year") {
    list(dol_kwh = row$current_dol_kwh, daily_charge = ifelse(is.na(row$daily_charge_dol_day), 0, row$daily_charge_dol_day))
  } else {
    list(dol_kwh = row$forecast_15yr_avg_dol_kwh, daily_charge = ifelse(is.na(row$forecast_15yr_avg_daily_dol), 0, row$forecast_15yr_avg_daily_dol))
  }
}

# Fast charge price lookup. Tries the fuel_prices CSV first ("ev_fast_charge"
# rows); falls back to the FAST_CHARGE_DOL_KWH constant if the CSV hasn't
# been regenerated to include the new fuel yet.
get_fast_charge_price <- function(state = dflt("state"), period = "1year") {
  p <- get_fuel_price(state, "ev_fast_charge", period)
  if (is.na(p$dol_kwh)) p$dol_kwh <- FAST_CHARGE_DOL_KWH
  p$daily_charge <- 0
  p
}

# ---------------------------------------------------------------------------
# 5b. Electricity tariffs — the single tariff switch
# ---------------------------------------------------------------------------
# Every price and behaviour that depends on the household's retail plan is
# resolved in exactly one place — get_tariff_spec() — from a single `tariff`
# argument, and threaded down from evaluate_household() to evaluate_option()
# and evaluate_solar_battery(). One argument, rather than separate import / EV /
# export settings, means the model can't be put into a combination no real
# retail plan offers.
#
#   "flat"         Single rate for every kWh, at any hour. The standard retail
#                  "electricity" row. No time signal, so nothing to shift.
#
#   "tou"          Time-of-use. Grid imports priced at the "electricity_off_peak"
#                  row, on the modelling convention that a household on TOU has
#                  shifted its discretionary load off peak.
#
#   "amber"        As "tou" for the 20 hours outside the evening peak. What
#                  distinguishes it is the 4-8pm window: exports are settled at
#                  wholesale prices from evening_peak_prices_monthly.csv rather
#                  than the flat feed-in tariff, tiered by inverter capacity
#                  through the four peak hours. Amber's 4-8pm *import* exposure
#                  is not modelled — a battery household is discharging in that
#                  window, not importing.
#
#   "solar_sharer" The 3-free plan. Three legs in the fuel_prices CSV:
#                  electricity_sso_free (11am-2pm, $0/kWh),
#                  electricity_sso_peak and electricity_sso_off_peak, plus its
#                  own elevated daily supply charge (NSW $2.13/day vs $1.39 on
#                  the flat rate — the standing cost of the free window).
#                  Imports outside the window are priced at sso_off_peak (the
#                  sso_peak leg is not used). Free-window energy is capped at
#                  SOLAR_SHARER_CAP_KWH_DAY across the whole household, and each
#                  load can only push its `sso_*` share of demand into the window
#                  (see SOLAR_FRACTION_TABLE). Only offered in some jurisdictions —
#                  see SOLAR_SHARER_STATES.
TARIFFS <- c("flat", "tou", "amber", "solar_sharer")

# Retail plan assumed for the fossil-appliance baseline scenarios (Gas,
# Gas + solar, LPG), regardless of what `tariff` the electrified household is
# put on.
#
# Why: the "tou" convention prices EVERY import at the off-peak rate, on the
# assumption the household has shifted its discretionary load off peak. That
# assumption holds for an electrified home — hot water, the EV and the battery
# are all genuinely schedulable. It does not hold for a gas home, whose
# remaining electric load is baseload (lighting, fridge, electronics) plus
# evening cooling: almost none of it is shiftable, so 100% off-peak billing is
# not achievable and a real gas household would rationally stay on a flat rate.
#
# Leaving the gas baseline on TOU flattered it by ~$325/yr at AUS 15-year
# prices — enough to make an electrified solar + battery household look WORSE
# on TOU than on flat, purely because cheap imports helped the baseline more
# than the home that barely imports.
#
# Set to NA in model_defaults.csv to price the fossil scenarios at the
# household's own tariff.
FOSSIL_BASELINE_TARIFF <- dflt("fossil_baseline_tariff")

# Retail plan the "Electric (grid_only)" scenario in evaluate_household() is
# billed on, whatever tariff the household requested.
#
# Why: the off-peak-for-everything convention behind "tou" (and the off-peak
# import rate under amber / solar_sharer) assumes the household can move its
# whole load out of the peak. Without a battery, a grid-only electric home
# can't: baseload, cooking and evening heating/cooling still land in the peak.
# Billing it entirely at off-peak understated its costs and made solar +
# battery look worth ~$0/yr over grid-only (VIC, TOU, loan-financed).
#
# Applies to evaluate_household() only; the per-appliance compare_*() helpers
# still bill grid_only at the requested tariff. Set to NA in model_defaults.csv
# to bill grid-only on the household's own tariff.
GRID_ONLY_TARIFF <- dflt("grid_only_tariff")

# Household-wide ceiling on free-window energy, kWh/day (24 kWh = 3 hours at
# an 8 kW draw). Shared across every load — appliances and the EV compete for
# the same budget (see allocate_free_window()).
SOLAR_SHARER_CAP_KWH_DAY <- dflt("solar_sharer_cap_kwh_day")

# Jurisdictions where Solar Sharer is offered. Derived from the fuel_prices CSV
# rather than hardcoded, so adding electricity_sso_* rows for a new state (e.g.
# VIC, which has none yet) makes it eligible with no code change.
SOLAR_SHARER_STATES <- fuel_prices |>
  filter(.data$fuel == "electricity_sso_free") |>
  pull(.data$state) |>
  unique()

# Resolve a tariff name into every price and behaviour that depends on it.
# This is the only place tariff -> pricing is decided.
#
# Returns a list with:
#   tariff             — the resolved tariff (may differ from the request; see
#                        the Solar Sharer fallback below)
#   import_dol_kwh     — $/kWh for appliance grid imports outside any free window
#   ev_dol_kwh         — $/kWh for EV home charging outside any free window
#   daily_charge       — the plan's daily supply charge ($/day). Solar Sharer
#                        carries its own, materially higher than the flat rate;
#                        every other tariff uses the standard "electricity" row.
#   free_window        — TRUE if the plan has a zero-priced window
#   free_cap_kwh_day   — household free-window ceiling (0 when free_window is FALSE)
#   export_evening     — "wholesale_peak" (Amber) or "fit" (everything else):
#                        how surplus battery energy exported in the evening is
#                        valued.
#
# Requesting solar_sharer in a state that doesn't offer it warns and falls back
# to "tou" — that household's best available alternative — rather than erroring,
# so by-state loops stay rectangular. The returned $tariff reports what was
# actually used, so callers can label output honestly.
get_tariff_spec <- function(tariff = dflt("tariff"), state = dflt("state"),
                            period = "1year") {
  if (!tariff %in% TARIFFS) {
    stop("tariff must be one of: ", paste(TARIFFS, collapse = ", "))
  }

  if (tariff == "solar_sharer" && !state %in% SOLAR_SHARER_STATES) {
    warning("Solar Sharer is not offered in ", state,
            " (available: ", paste(SOLAR_SHARER_STATES, collapse = ", "),
            "). Falling back to tou.", call. = FALSE)
    tariff <- "tou"
  }

  flat_row <- get_fuel_price(state, "electricity", period)

  if (tariff == "solar_sharer") {
    off_peak <- get_fuel_price(state, "electricity_sso_off_peak", period)
    free_row <- get_fuel_price(state, "electricity_sso_free", period)
    return(list(
      tariff           = tariff,
      import_dol_kwh   = off_peak$dol_kwh,
      ev_dol_kwh       = off_peak$dol_kwh,
      daily_charge     = free_row$daily_charge,
      free_window      = TRUE,
      free_cap_kwh_day = SOLAR_SHARER_CAP_KWH_DAY,
      export_evening   = "fit"
    ))
  }

  import_price <- if (tariff == "flat") {
    flat_row$dol_kwh
  } else {
    get_fuel_price(state, "electricity_off_peak", period)$dol_kwh
  }

  list(
    tariff           = tariff,
    import_dol_kwh   = import_price,
    ev_dol_kwh       = import_price,
    daily_charge     = flat_row$daily_charge,
    free_window      = FALSE,
    free_cap_kwh_day = 0,
    export_evening   = if (tariff == "amber") "wholesale_peak" else "fit"
  )
}

# Maps SOLAR_FRACTION_TABLE category names to the element names
# compute_household_loads() uses in its appliance_loads list. Vehicles are
# handled separately (they're not in appliance_loads).
FREE_WINDOW_CATEGORIES <- list(
  "Space Heating"     = "heating",
  "Space Cooling"     = "cooling",
  "Water Heating"     = "hotwater",
  "Cooktop"           = "cooking",
  "Other Cooking"     = "other_cooking",
  "Other Electronics" = "other_elec"
)

# Allocate the household's daily free-window budget across its loads.
#
# Each load can only draw its `sso_*` share of demand inside the 11am-2pm
# window. The free window is dispatched first — free window > solar > battery >
# paid grid (see split_load()) — and the `min_retail` floor holds back the
# share of each load that must be bought at the standard rate regardless
# (unavoidable 24/7 draw).
#
# When the sum of eligible demand exceeds free_cap_kwh_day the allocation is
# scaled back pro-rata, exactly as get_household_self_sufficiency() caps solar
# demand at generation. Returns the scaling factor so evaluate_option() can
# apply it per appliance without needing to know about the other loads.
#
# Args:
#   free_eligible_kwh — named numeric of per-load free-window-eligible kWh/day
#                       (already net of solar, battery and the min_retail floor)
#   free_cap_kwh_day  — household ceiling from the tariff spec
#
# Returns:
#   free_scale_factor — multiplier in [0, 1] applied to every load's eligible kWh
#   allocated_kwh     — total free kWh/day after the cap
#   demand_kwh        — total eligible kWh/day before the cap
allocate_free_window <- function(free_eligible_kwh, free_cap_kwh_day) {
  demand <- sum(unlist(free_eligible_kwh), na.rm = TRUE)
  if (free_cap_kwh_day <= 0 || demand <= 0) {
    return(list(free_scale_factor = 0, allocated_kwh = 0, demand_kwh = demand))
  }
  scale <- if (demand > free_cap_kwh_day) free_cap_kwh_day / demand else 1
  list(
    free_scale_factor = scale,
    allocated_kwh     = demand * scale,
    demand_kwh        = demand
  )
}

# Household free-window breakdown for one scenario. Called by
# evaluate_household() (to price appliance rows), get_household_self_sufficiency()
# (so solar only claims what the free window left) and evaluate_solar_battery()
# (to shrink the battery-eligible load). All three must agree: energy the free
# window already supplied at $0 is not energy solar needs to cover or the battery
# can be credited with displacing. Without netting it out the battery credit
# exceeds what the appliance rows were charged, inventing household income.
#
# Dispatch order is free window > solar > battery > paid grid for in-window load.
# Free-window grid costs $0 and has NO opportunity cost, whereas a solar kWh
# spent on load is a kWh that can't charge the battery (displacing paid grid at
# 24-33c) or be exported. So serving in-window load from the free window first
# and letting solar flow to storage is strictly the higher-value order. It also
# means free-window demand doesn't depend on solar, which keeps this calculation
# independent of get_household_self_sufficiency() rather than circular with it.
#
# Returns:
#   free_scale_factor  — pro-rata cap applied when demand exceeds the daily cap
#   free_by_load       — named daily free kWh per appliance_loads element
#   appliance_free_kwh — daily free-window kWh serving battery-eligible load
#   vehicle_free_kwh   — daily free-window kWh serving the EV
#   total_free_kwh     — sum of the two
# Daily kWh of each load that must be bought from the grid at the standard rate
# no matter what on-site supply exists — the min_retail floor. Applied LAST, so
# free window + solar + battery together can never cover more than
# (1 - min_retail) of a load. This is what stops storage from appearing to
# eliminate unavoidable round-the-clock draw (refrigeration, standby, overnight
# heating) that in reality outlasts the battery on some days.
#
# Unlike the free window this is tariff-independent: the floor is a physical
# statement about the load, not about the retail plan.
get_household_min_retail <- function(appliance_loads, vehicle_load_kwh) {
  by_load <- vapply(names(FREE_WINDOW_CATEGORIES), function(cat_name) {
    load_kwh <- appliance_loads[[FREE_WINDOW_CATEGORIES[[cat_name]]]]
    if (is.null(load_kwh) || load_kwh <= 0) return(0)
    load_kwh * get_min_retail_fraction(cat_name)
  }, numeric(1))
  names(by_load) <- unlist(FREE_WINDOW_CATEGORIES[names(by_load)])

  vehicle_kwh <- if (is.null(vehicle_load_kwh) || vehicle_load_kwh <= 0) 0 else
    vehicle_load_kwh * get_min_retail_fraction("Vehicles")

  list(
    min_retail_by_load       = by_load,
    appliance_min_retail_kwh = sum(by_load),
    vehicle_min_retail_kwh   = vehicle_kwh
  )
}

get_household_free_window <- function(appliance_loads, vehicle_load_kwh,
                                        solar_scenario = "solar_optimised", spec) {
  zero_by_load <- setNames(rep(0, length(FREE_WINDOW_CATEGORIES)),
                           unlist(FREE_WINDOW_CATEGORIES))
  none <- list(free_scale_factor = 0, free_by_load = zero_by_load,
               appliance_free_kwh = 0, vehicle_free_kwh = 0, total_free_kwh = 0)
  if (is.null(spec) || !isTRUE(spec$free_window)) return(none)

  # solar_scenario only selects which sso_ column to read — it does not depend on
  # any computed solar quantity, so this stays independent of
  # get_household_self_sufficiency() rather than circular with it.
  eligible <- function(load_kwh, category) {
    if (is.null(load_kwh) || load_kwh <= 0) return(0)
    load_kwh * free_window_share(category, solar_scenario)
  }

  appliance_eligible <- vapply(names(FREE_WINDOW_CATEGORIES), function(cat_name) {
    eligible(appliance_loads[[FREE_WINDOW_CATEGORIES[[cat_name]]]], cat_name)
  }, numeric(1))
  vehicle_eligible <- eligible(vehicle_load_kwh, "Vehicles")

  alloc <- allocate_free_window(
    c(as.list(appliance_eligible), list(Vehicles = vehicle_eligible)),
    spec$free_cap_kwh_day
  )
  scale <- alloc$free_scale_factor

  free_by_load <- setNames(appliance_eligible * scale,
                           unlist(FREE_WINDOW_CATEGORIES[names(appliance_eligible)]))

  list(
    free_scale_factor  = scale,
    free_by_load       = free_by_load,
    appliance_free_kwh = sum(appliance_eligible) * scale,
    vehicle_free_kwh   = vehicle_eligible * scale,
    total_free_kwh     = (sum(appliance_eligible) + vehicle_eligible) * scale
  )
}

# ---------------------------------------------------------------------------
# Solar pricing helpers — LCOE and feed-in tariff
# ---------------------------------------------------------------------------
# Two prices apply to solar kWh on the household side:
#   - LCOE  ($/kWh): levelised cost of generation from solar_lcoe_by_state.csv,
#                   computed in solar_lcoe_by_state.R, at the interest-rate row
#                   set by solar_lcoe_interest_rate (typical green-loan rate).
#   - FiT   ($/kWh): retail feed-in tariff, looked up via the "solar_export"
#                   fuel rows in fuel_prices_by_state_simple.csv. Held flat
#                   in nominal terms (no CPI), so the "1year" and "15year"
#                   columns are equal by construction.
#
# Standalone evaluate_option() calls (solar_marginal_cost = TRUE) price each
# self-consumed solar kWh at LCOE, so a single-appliance comparison carries the
# panels' cost. There is no forgone-FiT term: FiT applies only to kWh actually
# exported. evaluate_household() and the compare_*() contexts price
# self-consumed kWh at $0 and add the PV capex once instead.
#
# Scenario-level solar valuation (evaluate_solar_battery): FiT only — exports
# are valued at the per-state FiT, and capex is tracked separately.

SOLAR_LCOE_INTEREST_RATE <- dflt("solar_lcoe_interest_rate")

# Map the model's state code (uses "AUS" for national) to the LCOE CSV label
# ("AUS (pop-weighted)"). All other state codes match.
.lcoe_state_key <- function(state) {
  if (state == "AUS") "AUS (pop-weighted)" else state
}

get_solar_lcoe <- function(state = dflt("state"),
                             interest_rate = SOLAR_LCOE_INTEREST_RATE) {
  key <- .lcoe_state_key(state)
  row <- solar_lcoe |>
    filter(.data$state == !!key, .data$interest_rate == !!interest_rate)
  if (nrow(row) == 0) {
    have <- unique(solar_lcoe$interest_rate[solar_lcoe$state == key])
    stop("solar_lcoe_by_state.csv has no ", interest_rate, " row for ", state,
         " (available: ", paste(have, collapse = ", "), "). Set solar_lcoe_interest_rate ",
         "in model_defaults.csv to one of these, or add the rate to solar_lcoe_by_state.R.",
         call. = FALSE)
  }
  as.numeric(row$cost_per_kwh[1])
}

# Per-state physical/cost properties of a rooftop solar system. Returns
#   cost_per_kw       — installed $ per kW (drives capex from system size)
#   capacity_factor   — average fraction of nameplate output (drives daily kWh)
#   inverter_cost     — replacement inverter $ (one mid-life swap)
# These columns are identical across interest-rate rows for a given state, so
# the state's first row is used — the system's physical and cost inputs must not
# depend on the LCOE financing assumption. AUS uses the pop-weighted national row.
get_solar_system_info <- function(state = dflt("state")) {
  key <- .lcoe_state_key(state)
  row <- solar_lcoe |> filter(.data$state == !!key)
  if (nrow(row) == 0) {
    warning("No solar system info for ", state, "; returning NAs")
    return(list(cost_per_kw     = NA_real_,
                capacity_factor = NA_real_,
                inverter_cost   = NA_real_))
  }
  list(
    cost_per_kw     = as.numeric(row$cost_per_kw[1]),
    capacity_factor = as.numeric(row$capacity_factor[1]),
    inverter_cost   = as.numeric(row$inverter_cost[1])
  )
}

# Generic product_prices lookup: returns the numeric Total installed cost
# for a given Technology / Region row. Used to keep evaluate_household()'s
# appliance capex defaults in sync with the FINAL product prices sheet.
get_product_price <- function(technology, region = "AUS") {
  r <- product_prices |> filter(Technology == technology, Region == region)
  if (nrow(r) == 0) stop("Missing product_prices row for: ", technology, " (", region, ")")
  as.numeric(r$`Total installed`[1])
}

# Battery cost lookup from product_prices CSV (AUS-level only — the CSV has
# no per-state battery rows).
#
# Note: the CSV labels home-battery cost "per kW" but the values are per kWh
# of storage capacity (the residential convention; a 10 kWh battery at $620
# = $6,200 subsidised, which matches market pricing).
#
# Returns a list with:
#   subsidised_per_kwh   — $/kWh when bought alongside a PV system
#   unsubsidised_per_kwh — $/kWh when added to an existing setup (no new PV)
#   installation         — fixed installation cost ($), independent of size
get_battery_cost_info <- function() {
  row_value <- function(tech) {
    r <- product_prices |> filter(Technology == tech, Region == "AUS")
    if (nrow(r) == 0) stop("Missing product_prices row for: ", tech)
    as.numeric(r$`Total installed`[1])
  }
  list(
    subsidised_per_kwh   = row_value("Subsidised home battery cost per kW"),
    unsubsidised_per_kwh = row_value("Unsubsidised home battery cost per kW"),
    installation         = row_value("Battery installation")
  )
}

# Per-state feed-in tariff. Looked up via "solar_export" rows; if the CSV
# hasn't been regenerated yet (no solar_export row), fall back to a
# conservative national flat rate.
DEFAULT_FIT_DOL_KWH <- dflt("default_fit_dol_kwh")

get_fit <- function(state = dflt("state"), period = "1year") {
  p <- get_fuel_price(state, "solar_export", period)
  if (is.na(p$dol_kwh)) return(DEFAULT_FIT_DOL_KWH)
  p$dol_kwh
}

# ---------------------------------------------------------------------------
# 6. Helper: get energy use (kWh/day) for an appliance type in a state
# ---------------------------------------------------------------------------

get_energy_kwh_day <- function(category, appliance_type, state_col = dflt("state")) {
  i <- .ENERGY_USE_ROW[paste(category, appliance_type, sep = "|")]
  if (is.na(i)) return(NA_real_)
  as.numeric(energy_use[[state_col]][i])
}

# Total kWh/day across every appliance type in one or more categories — used
# for the always-electric "Other Cooking" / "Other Electronics" baseload.
get_category_total_kwh_day <- function(category, state_col = dflt("state")) {
  sum(as.numeric(energy_use[[state_col]][energy_use$Category %in% category]),
      na.rm = TRUE)
}

# Grid electricity emissions factor (kgCO2e/kWh) for a state; 0 if missing.
get_grid_ef <- function(state) {
  i <- .EMISSIONS_ROW[state]
  v <- if (is.na(i)) NA_real_ else emissions_factors$grid_electricity_kgCO2e_per_kWh[i]
  if (is.na(v)) 0 else v
}

# Per-state heater count for capex scaling. Loaded from
# Heater_numbers_by_state.csv. Falls back to AUS if the state is missing.
# Note this is *only* for capex — energy figures in
# average_energy_use_by_appliance_and_state.csv are already whole-of-household.
get_heater_count <- function(state = dflt("state")) {
  row <- heater_counts |> filter(state == !!state)
  if (nrow(row) == 0) {
    row <- heater_counts |> filter(state == "AUS")
  }
  as.integer(row$n_heaters[[1]])
}

# ---------------------------------------------------------------------------
# 7. Map appliance types to fuel type
# ---------------------------------------------------------------------------

appliance_fuel_map <- tribble(
  ~category,        ~appliance_type,       ~fuel,
  "Space Heating",  "Electric heat pump",  "electricity",
  "Space Heating",  "Electric resistance", "electricity",
  "Space Heating",  "Natural gas",         "gas",
  "Space Heating",  "LPG",                "lpg",
  "Space Heating",  "Wood",               "wood",
  "Space Cooling",  "Heat pump",          "electricity",
  "Water Heating",  "Electric heat pump",  "electricity",
  "Water Heating",  "Electric resistance", "electricity",
  "Water Heating",  "Natural gas",         "gas",
  "Water Heating",  "LPG",               "lpg",
  "Cooktop",        "Electric induction",  "electricity",
  "Cooktop",        "Electric resistance", "electricity",
  "Cooktop",        "Natural gas",         "gas",
  "Cooktop",        "LPG",                "lpg",
  "Vehicles",       "Petrol",             "petrol",
  "Vehicles",       "Diesel",             "diesel",
  "Vehicles",       "Electric",           "electricity"
)

get_fuel_for_appliance <- function(category, appliance_type) {
  row <- appliance_fuel_map |>
    filter(.data$category == !!category, .data$appliance_type == !!appliance_type)
  if (nrow(row) == 0) {
    # Fall back to matching on appliance_type only
    row <- appliance_fuel_map |> filter(.data$appliance_type == !!appliance_type)
  }
  if (nrow(row) == 0) stop(paste0("Unknown appliance type: ", category, " / ", appliance_type))
  row$fuel[1]
}

# ---------------------------------------------------------------------------
# 7b. Helper: get vehicle capex from vehicle_costs_2026.csv
# ---------------------------------------------------------------------------

get_vehicle_capex <- function(drivetrain, type_group = "SUV", new_used = dflt("new_used")) {
  # Map our appliance types to CSV drivetrain labels
  dt <- case_when(
    drivetrain %in% c("Petrol", "Diesel") ~ "ICE",
    drivetrain == "Electric"              ~ "EV",
    TRUE                                  ~ drivetrain
  )

  row <- vehicle_costs |>
    filter(Drivetrain == dt, Type_group == type_group, `New/Used` == new_used)

  if (nrow(row) == 0) {
    warning(paste0("No vehicle cost found for ", dt, " / ", type_group, " / ", new_used))
    return(0)
  }
  row$`Weighted Avg Base Price`[1]
}

# ---------------------------------------------------------------------------
# 7c. Helper: get vehicle energy (kWh/day) from efficiency × km/day
#     efficiency (Wh/km) comes from vehicle_average_efficiency_by_class.csv
#     km/day comes from average_km_per_day_by_state.csv
# ---------------------------------------------------------------------------

# km_tier selects which column of average_km_per_day_by_state.csv to use:
#   "low"    — lower-bound driving estimate (km_per_day_low)
#   "middle" — central estimate (km_per_day_middle)         [default]
#   "high"   — upper-bound driving estimate (km_per_day_high)
# Threaded through evaluate_option(), evaluate_solar_battery(),
# compare_vehicles() and evaluate_household() so the same tier flows
# consistently from single-appliance to whole-household runs.
KM_TIERS <- c("low", "middle", "high")

get_vehicle_energy_kwh_day <- function(drivetrain, vehicle_class = dflt("appliance_vehicle_class"),
                                        state = dflt("state"),
                                        km_tier = dflt("km_tier")) {
  if (!km_tier %in% KM_TIERS) {
    stop("km_tier must be one of: ", paste(KM_TIERS, collapse = ", "))
  }

  # Map vehicle_class to efficiency CSV labels
  eff_class <- case_when(
    tolower(vehicle_class) %in% c("suv")                          ~ "SUV",
    tolower(vehicle_class) %in% c("hatchback/small car", "hatchback", "hatchback/micro car", "hatch") ~ "hatchback/micro car",
    tolower(vehicle_class) %in% c("sedan")                        ~ "sedan",
    TRUE                                                           ~ tolower(vehicle_class)
  )

  row <- vehicle_efficiency |> filter(vehicle_class == eff_class)
  if (nrow(row) == 0) {
    warning(paste0("No vehicle efficiency found for class '", vehicle_class, "'"))
    return(NA_real_)
  }

  # Get Wh/km based on drivetrain
  wh_per_km <- if (drivetrain == "Electric") {
    row$avg_energy_consumption_wh_per_km[1]
  } else {
    row$ice_avg_energy_consumption_wh_per_km[1]
  }

  # Get km/day for this state from the tiered CSV (low / middle / high)
  km_row <- km_per_day |> filter(.data$state == !!state)
  if (nrow(km_row) == 0) {
    warning(paste0("No km/day data for state '", state, "'"))
    return(NA_real_)
  }
  km_col <- paste0("km_per_day_", km_tier)

  wh_per_km * km_row[[km_col]][1] / 1000
}

# ---------------------------------------------------------------------------
# 7d. Helper: state appliance subsidies (Appliance_subsidies.csv)
# ---------------------------------------------------------------------------
# Resolve a WA postcode to its distribution network ("Synergy" or "Horizon").
# Returns NA if the CSV isn't present, the postcode isn't found, or postcode
# is missing — in which case no Synergy/Horizon eligibility is added and the
# WA-specific battery subsidies don't fire.
get_wa_network <- function(postcode) {
  if (is.null(wa_postcode_network) || length(postcode) != 1 || is.na(postcode)) return(NA_character_)
  row <- wa_postcode_network |> filter(.data$postcode == !!postcode)
  if (nrow(row) == 0) return(NA_character_)
  as.character(row$network[1])
}

# Build the set of eligibility tags this household qualifies for. "All" is
# always in the set; the more specific tags are added based on the household
# properties passed in. Used to match rows in appliance_subsidies whose
# Eligibility column may carry a comma-separated list of required tags (e.g.
# "VPP, Synergy") — every required tag must be in the household's tag set
# for the row to apply. WA postcode is auto-resolved to a network tag.
.build_subsidy_eligibilities <- function(state          = NA_character_,
                                          dwelling_type = dflt("dwelling_type"),
                                          is_landlord   = FALSE,
                                          battery_vpp   = FALSE,
                                          postcode      = NA_integer_) {
  elig <- "All"
  if (identical(dwelling_type, "apartment")) elig <- c(elig, "Apartment")
  if (isTRUE(is_landlord))                   elig <- c(elig, "Landlord")
  if (isTRUE(battery_vpp))                   elig <- c(elig, "VPP")
  if (identical(state, "WA")) {
    network <- get_wa_network(postcode)
    if (length(network) == 1 && !is.na(network) && nzchar(network)) elig <- c(elig, network)
  }
  unique(elig)
}

# Best-applicable state subsidy ($) for a given appliance in a given state.
# Returns 0 when no row matches. For battery rows priced per kWh, the value
# is `Subsidy value` × battery_size_kwh. When multiple rows are applicable
# (e.g. "All" and "Apartment" for solar) the most generous is returned —
# more specific eligibilities ship with higher values, so this also picks
# the "right" one rather than stacking. Pass battery_size_kwh = 0 when
# evaluating non-battery appliances. For WA, postcode resolves to the
# Synergy / Horizon network tag automatically via wa_postcode_network.
get_appliance_subsidy <- function(appliance, state,
                                    dwelling_type    = dflt("dwelling_type"),
                                    is_landlord      = FALSE,
                                    battery_vpp      = FALSE,
                                    postcode         = NA_integer_,
                                    battery_size_kwh = 0) {
  rows <- appliance_subsidies |>
    filter(.data$State == !!state, .data$Appliance == !!appliance)
  if (nrow(rows) == 0) return(0)

  elig <- .build_subsidy_eligibilities(state, dwelling_type, is_landlord,
                                          battery_vpp, postcode)
  keep <- vapply(rows$Eligibility, function(e) {
    req <- trimws(unlist(strsplit(e, ",")))
    all(req %in% elig)
  }, logical(1))
  rows <- rows[keep, , drop = FALSE]
  if (nrow(rows) == 0) return(0)

  vals <- ifelse(rows$`Paid per` == "kWh",
                  rows$`Subsidy value` * battery_size_kwh,
                  rows$`Subsidy value`)
  max(vals)
}

# ---------------------------------------------------------------------------
# 7e. Helper: vehicle annual maintenance ($/year) per vehicle
# ---------------------------------------------------------------------------
# Reads Vehicle_maintenance.csv. Combined service + tyres + consumables; does
# not include registration, insurance or fuel/electricity. Class normalises
# to the labels used elsewhere in the model; drivetrain maps to EV / ICE.
get_vehicle_maintenance <- function(vehicle_class = dflt("appliance_vehicle_class"),
                                       drivetrain    = "Electric") {
  model_label <- case_when(
    tolower(vehicle_class) == "suv"                                                              ~ "SUV",
    tolower(vehicle_class) %in% c("hatchback/small car", "hatchback", "hatchback/micro car", "hatch") ~ "Hatchback",
    tolower(vehicle_class) == "sedan"                                                            ~ "Sedan",
    TRUE                                                                                          ~ vehicle_class
  )
  fuel_label <- if (drivetrain == "Electric") "EV" else "ICE"
  row <- vehicle_maintenance |>
    filter(.data$Model == model_label, .data$Fuel == fuel_label)
  if (nrow(row) == 0) return(0)
  as.numeric(row$Annual[1])
}

# ---------------------------------------------------------------------------
# 7f. Helper: build a vehicle fleet from (vehicle_class, n_vehicles)
# ---------------------------------------------------------------------------
# Mirrors the visualiser's vehicleEntries(). Returns a data frame of one row
# per fleet entry with columns:
#   class  — vehicle class label (passed straight through to per-class lookups)
#   weight — how many vehicles of that class are in the fleet
#
# vehicle_class describes the fleet's COMPOSITION and n_vehicles its SIZE. Every
# class given is kept, and the count is spread evenly across them:
#     weight = n_vehicles / length(vehicle_class)
# So c("SUV","hatch") with n_vehicles = 2 is one of each, and the same pair with
# the 1.8 household average is 0.9 each — the mix is preserved and only the size
# changes. Scalar vehicle_class reduces to weight = n_vehicles. Energy,
# maintenance and capex are all linear in weight.
#
# Honouring the count matters: an SUV's EV costs far more than its petrol
# equivalent while a hatchback's costs about the same, so fleet composition and
# size dominate the capex side.
#
# n_vehicles <= 0 (or NA), or an empty vehicle_class, returns an empty fleet —
# callers treat that as "no vehicles". A car-free household stays car-free
# regardless of what vehicle_class holds, so n_vehicles = 0 with a leftover class
# vector can't silently reintroduce a fleet.
make_fleet <- function(vehicle_class = dflt("appliance_vehicle_class"), n_vehicles = dflt("appliance_n_vehicles")) {
  if (length(vehicle_class) == 0) {
    return(data.frame(class = character(0), weight = numeric(0),
                       stringsAsFactors = FALSE))
  }
  if (length(n_vehicles) != 1 || is.na(n_vehicles) || n_vehicles <= 0) {
    return(data.frame(class = character(0), weight = numeric(0),
                       stringsAsFactors = FALSE))
  }
  data.frame(class  = as.character(vehicle_class),
             weight = as.numeric(n_vehicles) / length(vehicle_class),
             stringsAsFactors = FALSE)
}

# Map a vehicle_class label to the Type_group used in vehicle_costs_2026.csv.
vehicle_class_to_cost_type <- function(vehicle_class) {
  case_when(
    tolower(vehicle_class) == "suv"                                                              ~ "SUV",
    tolower(vehicle_class) %in% c("hatchback/small car", "hatchback", "hatchback/micro car", "hatch") ~ "Hatchback/small car",
    tolower(vehicle_class) == "sedan"                                                            ~ "Sedan",
    TRUE                                                                                          ~ vehicle_class
  )
}

# BYD model representing a new EV of the given cost_type_group; NA outside
# the three supported classes.
byd_model_for_cost_type <- function(cost_type_group) {
  case_when(
    cost_type_group == "SUV"                  ~ "BYD Sealion",
    cost_type_group == "Hatchback/small car"  ~ "BYD Dolphin",
    cost_type_group == "Sedan"                ~ "BYD Seal",
    TRUE                                       ~ NA_character_
  )
}

# Sum of get_vehicle_energy_kwh_day across the fleet.
fleet_vehicle_energy_kwh_day <- function(drivetrain, fleet, state = dflt("state"),
                                          km_tier = dflt("km_tier")) {
  if (nrow(fleet) == 0) return(0)
  sum(vapply(seq_len(nrow(fleet)), function(i) {
    v <- get_vehicle_energy_kwh_day(drivetrain, fleet$class[i], state, km_tier)
    if (is.na(v)) 0 else v * fleet$weight[i]
  }, numeric(1)))
}

# Sum of get_vehicle_maintenance across the fleet.
fleet_vehicle_maintenance <- function(fleet, drivetrain = "Electric") {
  if (nrow(fleet) == 0) return(0)
  sum(vapply(seq_len(nrow(fleet)), function(i) {
    get_vehicle_maintenance(fleet$class[i], drivetrain) * fleet$weight[i]
  }, numeric(1)))
}

# Sum of get_vehicle_capex across the fleet. For new EVs the per-class BYD
# model is used automatically; other classes / used / ICE flow new_used
# straight through.
fleet_vehicle_capex <- function(drivetrain, fleet, new_used = dflt("new_used")) {
  if (nrow(fleet) == 0) return(0)
  sum(vapply(seq_len(nrow(fleet)), function(i) {
    cost_type <- vehicle_class_to_cost_type(fleet$class[i])
    lookup    <- new_used
    if (drivetrain == "Electric" && new_used == "New") {
      byd <- byd_model_for_cost_type(cost_type)
      if (!is.na(byd)) lookup <- byd
    }
    get_vehicle_capex(drivetrain, cost_type, lookup) * fleet$weight[i]
  }, numeric(1)))
}

# ---------------------------------------------------------------------------
# 8. Supply charge helpers
#    Both gas and electricity supply charges are split proportionally across
#    functions based on each function's energy consumption share for that fuel.
# ---------------------------------------------------------------------------

# Compute all-electric household consumption (kWh/day) by function.
# Used to determine proportional electricity supply charge shares.
# Uses energy consumption values directly from the energy use data
# (efficiencies are already baked into those figures).
#
# Memoised: evaluate_option() calls this for every row's supply-charge share,
# always with the same handful of households, so results are cached per
# argument set for the session.
.all_electric_cache <- new.env(parent = emptyenv())

get_all_electric_kwh_day <- function(state = dflt("state"), n_occupants = dflt("n_occupants"),
                                       n_vehicles = dflt("appliance_n_vehicles"), km_tier = dflt("km_tier"),
                                       vehicle_class = dflt("appliance_vehicle_class")) {
  key <- paste(state, sprintf("%.17g", n_occupants), sprintf("%.17g", n_vehicles),
               km_tier, paste(vehicle_class, collapse = ";"), sep = "|")
  hit <- .all_electric_cache[[key]]
  if (!is.null(hit)) return(hit)
  .all_electric_cache[[key]] <- .compute_all_electric_kwh_day(
    state, n_occupants, n_vehicles, km_tier, vehicle_class)
}

.compute_all_electric_kwh_day <- function(state, n_occupants, n_vehicles,
                                            km_tier, vehicle_class) {
  occ_scale <- get_scaling_factor(n_occupants)

  heating  <- get_energy_kwh_day("Space Heating", "Electric heat pump", state) * occ_scale
  cooling  <- get_energy_kwh_day("Space Cooling", "Heat pump", state) * occ_scale
  hotwater <- get_energy_kwh_day("Water Heating", "Electric heat pump", state) * occ_scale
  cooking  <- get_energy_kwh_day("Cooktop", "Electric induction", state) * occ_scale

  heating  <- ifelse(is.na(heating), 0, heating)
  cooling  <- ifelse(is.na(cooling), 0, cooling)
  hotwater <- ifelse(is.na(hotwater), 0, hotwater)
  cooking  <- ifelse(is.na(cooking), 0, cooking)

  # Other appliances (always electric)
  other <- get_category_total_kwh_day(c("Other Cooking", "Other Electronics"), state) * occ_scale

  # Vehicles (summed across the fleet; not occupancy-scaled) — uses
  # efficiency × km/day at the selected km_tier. Only the home-charged share
  # (1 - FAST_CHARGE_FRACTION) passes through the home meter and is eligible
  # for the home electricity supply-charge share. vehicle_class may be a
  # scalar (with n_vehicles as multiplier, supports fractional 1.8 and 0) or
  # a vector (one car per element, mixed classes allowed) — see make_fleet().
  fleet <- make_fleet(vehicle_class, n_vehicles)
  vehicles <- fleet_vehicle_energy_kwh_day("Electric", fleet, state, km_tier) *
                (1 - FAST_CHARGE_FRACTION)

  list(
    `Space Heating` = heating,
    `Space Cooling` = cooling,
    `Water Heating` = hotwater,
    Cooktop = cooking,
    Other = other,
    Vehicles = vehicles
  )
}

# Get proportional share of electricity supply charge for a given function.
# Space Heating and Space Cooling are independent buckets — a reverse-cycle
# heat pump produces both heating and cooling kWh, but they're billed via
# separate rows so the per-appliance Space Heating share is a fair
# heating-vs-gas-heater comparison.
get_elec_supply_share <- function(category, state = dflt("state"), n_occupants = dflt("n_occupants"),
                                   include_vehicles = FALSE, n_vehicles = dflt("appliance_n_vehicles"),
                                   km_tier = dflt("km_tier"), vehicle_class = dflt("appliance_vehicle_class")) {
  elec <- get_all_electric_kwh_day(state, n_occupants, n_vehicles, km_tier, vehicle_class)

  this_use <- elec[[category]]
  if (length(this_use) != 1 || is.na(this_use)) return(0)

  total <- elec[["Space Heating"]] + elec[["Space Cooling"]] +
           elec[["Water Heating"]] + elec[["Cooktop"]] + elec[["Other"]]
  if (include_vehicles) total <- total + elec[["Vehicles"]]

  if (total == 0) return(0)
  this_use / total
}

# Household solar/load breakdown — provides the inputs evaluate_solar_battery
# threads into battery_model. Computes the appliance/EV solar self-consumption
# bottom-up from per-load demand and (if provided) caps it at what the PV
# system actually generates that day.
#
# Args:
#   available_solar_kwh — daily PV generation (kWh). If NULL (default), the
#                          per-load demand for solar is reported uncapped —
#                          this matches the pre-existing behaviour and is the
#                          right thing for callers who don't know the system
#                          size (e.g., single-appliance comparisons). If a
#                          number is supplied, demand exceeding it is clipped
#                          proportionally across appliance + vehicle loads.
#   vehicle_class       — vehicle class for the EV energy lookup (defaults to
#                          SUV; pass through from the calling household).
#
# Returns a list with:
#   total_pct           — household-level self-sufficiency = (capped) total_solar
#                          / total_load (passed to battery_model as
#                          self_sufficiency_pct)
#   appliance_load_kwh  — daily kWh of battery-eligible load (heating/cooling/
#                          hotwater/cooking/other; what the home battery serves
#                          overnight)
#   appliance_solar_kwh — daily kWh of appliance load met directly by solar,
#                          AFTER the generation cap
#   vehicle_load_kwh    — daily kWh of home-charged EV load (uses solar during
#                          the day and draws off-peak grid overnight, NOT the
#                          home battery — see evaluate_option's tariff)
#   vehicle_solar_kwh   — daily kWh of EV load met directly by solar, AFTER cap
#   solar_demand_kwh    — daily kWh demanded for self-consumption BEFORE cap
#                          (sum of per-load demand × SOLAR_FRACTION_TABLE)
#   solar_scale_factor  — applied multiplier on the demanded solar (1.0 = no
#                          cap; < 1.0 = generation-limited; 0 = no PV)
#
# Driving battery_model.R from this breakdown (rather than free parameters)
# keeps the appliance-cost side and the solar/battery side consistent: the
# kWh attributed to self-consumption on the appliance side are the same kWh
# that reduce exportable surplus on the battery side.
# Canonical household appliance + vehicle daily kWh, as seen by both the
# per-appliance cost side (via evaluate_option) and the solar/battery side
# (via get_household_self_sufficiency). Centralises the scaling so the
# occupancy and dwelling factors are applied in exactly one place — keeps
# the two sides consistent and means a future per-household adjustment only
# needs to be added once.
#
# Returns:
#   appliance_loads  — named list of daily kWh per always-electric appliance,
#                      already scaled by occupants × dwelling_scale.
#   vehicle_load_kwh — daily home-charged EV kWh (already × n_vehicles ×
#                      (1 - FAST_CHARGE_FRACTION); dwelling-independent).
compute_household_loads <- function(state         = dflt("state"),
                                      n_occupants   = dflt("n_occupants"),
                                      dwelling_type = dflt("dwelling_type"),
                                      n_vehicles    = dflt("appliance_n_vehicles"),
                                      vehicle_class = dflt("appliance_vehicle_class"),
                                      km_tier       = dflt("km_tier"),
                                      # Per-category appliance types — control which loads count
                                      # as electric for solar self-consumption / battery sizing.
                                      # Non-electric appliance types (Natural gas, LPG, Petrol,
                                      # Diesel, Wood) contribute zero electric load. Space
                                      # Cooling is always electric and has no toggle.
                                      heating_type  = "Electric heat pump",
                                      hotwater_type = "Electric heat pump",
                                      cooking_type  = "Electric induction",
                                      vehicle_type  = "Electric",
                                      hh = NULL) {
  if (is_household(state)) hh <- state
  if (!is.null(hh)) .use_household(hh, environment(), sys.function(), match.call())
  occ_scale      <- get_scaling_factor(n_occupants)
  dwelling_scale <- if (dwelling_type == "apartment") APARTMENT_ENERGY_FACTOR else 1
  household_scale <- occ_scale * dwelling_scale

  load_kwh <- function(category, appliance_type) {
    v <- get_energy_kwh_day(category, appliance_type, state)
    ifelse(is.na(v), 0, v) * household_scale
  }

  is_electric <- function(category, appliance_type) {
    get_fuel_for_appliance(category, appliance_type) == "electricity"
  }

  heating  <- if (is_electric("Space Heating", heating_type))
                load_kwh("Space Heating", heating_type)  else 0
  cooling  <- load_kwh("Space Cooling", "Heat pump")
  hotwater <- if (is_electric("Water Heating", hotwater_type))
                load_kwh("Water Heating", hotwater_type) else 0
  cooking  <- if (is_electric("Cooktop", cooking_type))
                load_kwh("Cooktop", cooking_type)        else 0

  other_cooking <- get_category_total_kwh_day("Other Cooking",     state) * household_scale
  other_elec    <- get_category_total_kwh_day("Other Electronics", state) * household_scale

  # Vehicles: home-charged share only (fast-charge kWh don't pass through the
  # home meter so they can't be solar-self-consumed). Not affected by
  # dwelling_type — an apartment EV uses the same kWh/km as a house EV.
  # Summed across the fleet (see make_fleet()); supports scalar or vector
  # vehicle_class. Zero when the scenario runs a fossil vehicle.
  vehicle_load  <- if (vehicle_type == "Electric") {
    fleet <- make_fleet(vehicle_class, n_vehicles)
    fleet_vehicle_energy_kwh_day("Electric", fleet, state, km_tier) *
      (1 - FAST_CHARGE_FRACTION)
  } else 0

  list(
    appliance_loads = list(
      heating       = heating,
      cooling       = cooling,
      hotwater      = hotwater,
      cooking       = cooking,
      other_cooking = other_cooking,
      other_elec    = other_elec
    ),
    vehicle_load_kwh = vehicle_load
  )
}

# Allocates the household's daily kWh across solar self-consumption (per
# appliance via SOLAR_FRACTION_TABLE), capped at what the PV system actually
# generates that day. Returns the breakdown evaluate_solar_battery passes
# into battery_model.
#
# Args (all pre-computed by the caller — no CSV reads here):
#   appliance_loads      — named list from compute_household_loads(): heating,
#                          cooling, hotwater, cooking, other_cooking, other_elec
#                          (daily kWh, already scaled for occupants + dwelling).
#   vehicle_load_kwh     — daily home-charged EV kWh (already × n_vehicles ×
#                          (1 - FAST_CHARGE_FRACTION)).
#   solar_scenario       — "grid_only" / "solar" / "solar_optimised".
#   available_solar_kwh  — daily PV generation, kWh. NULL leaves self-
#                          consumption uncapped (for callers that don't know
#                          the system size).
get_household_self_sufficiency <- function(appliance_loads,
                                             vehicle_load_kwh,
                                             solar_scenario      = "grid_only",
                                             available_solar_kwh = NULL,
                                             # Daily free-window kWh already
                                             # serving each load, from
                                             # get_household_free_window(). Solar
                                             # can only claim what the free window
                                             # left, since the free window is
                                             # dispatched first. NULL/0 (default)
                                             # = no free window, so solar demand is
                                             # unaffected.
                                             free_by_load     = NULL,
                                             vehicle_free_kwh = 0,
                                             # Per-load min_retail floor from
                                             # get_household_min_retail(). Solar
                                             # can never cover this share.
                                             min_retail_by_load     = NULL,
                                             vehicle_min_retail_kwh = 0) {
  appliance_load <- appliance_loads$heating + appliance_loads$cooling +
                    appliance_loads$hotwater + appliance_loads$cooking +
                    appliance_loads$other_cooking + appliance_loads$other_elec
  vehicle_load   <- vehicle_load_kwh
  total_load     <- appliance_load + vehicle_load

  if (solar_scenario == "grid_only" || total_load == 0) {
    return(list(
      total_pct           = 0,
      appliance_load_kwh  = appliance_load,
      appliance_solar_kwh = 0,
      vehicle_load_kwh    = vehicle_load,
      vehicle_solar_kwh   = 0,
      solar_demand_kwh    = 0,
      solar_scale_factor  = if (solar_scenario == "grid_only") 0 else 1
    ))
  }

  # Per-load solar self-consumption *demand* (before the generation cap), at
  # each load's SOLAR_FRACTION_TABLE share. Pool Equipment is intentionally
  # excluded from the load aggregation, so it doesn't appear here either.
  #
  # Each load's demand is capped at what the free window hasn't already served
  # (free window dispatches first — see get_household_free_window). Under any
  # tariff without a free window free_kwh is 0 and this is a no-op.
  lookup <- function(tbl, load_name) {
    if (is.null(tbl)) return(0)
    v <- tbl[[load_name]]
    if (length(v) != 1 || is.na(v)) 0 else v
  }
  # Solar is bounded below by the min_retail floor as well as by what the free
  # window already served: free + solar <= load * (1 - min_retail).
  solar_demand_for <- function(load_name, category) {
    load_kwh <- appliance_loads[[load_name]]
    if (is.null(load_kwh) || load_kwh <= 0) return(0)
    serveable <- max(load_kwh - lookup(min_retail_by_load, load_name), 0)
    min(load_kwh * get_solar_fraction(category, solar_scenario),
        max(serveable - lookup(free_by_load, load_name), 0))
  }

  appliance_solar_demand <-
    solar_demand_for("heating",       "Space Heating")     +
    solar_demand_for("cooling",       "Space Cooling")     +
    solar_demand_for("hotwater",      "Water Heating")     +
    solar_demand_for("cooking",       "Cooktop")           +
    solar_demand_for("other_cooking", "Other Cooking")     +
    solar_demand_for("other_elec",    "Other Electronics")
  vehicle_solar_demand <- min(
    vehicle_load * get_solar_fraction("Vehicles", solar_scenario),
    max(vehicle_load - vehicle_min_retail_kwh - vehicle_free_kwh, 0)
  )
  total_solar_demand   <- appliance_solar_demand + vehicle_solar_demand

  # Bottom-up cap: actual self-consumption can't exceed what the PV generates
  # on the day. If the caller didn't give the system size, leave it uncapped
  # (scale = 1).
  scale <- 1
  if (!is.null(available_solar_kwh)) {
    if (available_solar_kwh <= 0) {
      scale <- 0
    } else if (total_solar_demand > available_solar_kwh) {
      scale <- available_solar_kwh / total_solar_demand
    }
  }

  appliance_solar <- appliance_solar_demand * scale
  vehicle_solar   <- vehicle_solar_demand   * scale
  total_solar     <- appliance_solar + vehicle_solar

  list(
    total_pct           = total_solar / total_load,
    appliance_load_kwh  = appliance_load,
    appliance_solar_kwh = appliance_solar,
    vehicle_load_kwh    = vehicle_load,
    vehicle_solar_kwh   = vehicle_solar,
    solar_demand_kwh    = total_solar_demand,
    solar_scale_factor  = scale
  )
}

# Get proportional share of gas supply charge for a given function
get_gas_supply_share <- function(category, state = dflt("state"), n_occupants = dflt("n_occupants")) {
  occ_scale <- get_scaling_factor(n_occupants)

  gas_heating  <- get_energy_kwh_day("Space Heating", "Natural gas", state) * occ_scale
  gas_hotwater <- get_energy_kwh_day("Water Heating", "Natural gas", state) * occ_scale
  gas_cooktop  <- get_energy_kwh_day("Cooktop", "Natural gas", state) * occ_scale

  gas_heating  <- ifelse(is.na(gas_heating), 0, gas_heating)
  gas_hotwater <- ifelse(is.na(gas_hotwater), 0, gas_hotwater)
  gas_cooktop  <- ifelse(is.na(gas_cooktop), 0, gas_cooktop)

  total_gas <- gas_heating + gas_hotwater + gas_cooktop
  if (total_gas == 0) return(0)

  this_use <- case_when(
    category == "Space Heating" ~ gas_heating,
    category == "Water Heating" ~ gas_hotwater,
    category == "Cooktop"       ~ gas_cooktop,
    TRUE                        ~ 0
  )
  this_use / total_gas
}

# ---------------------------------------------------------------------------
# 9. Core function: evaluate a SINGLE appliance option
#    Returns one row with all cost, energy and emissions data.
#    Multiple options are compared by row-binding results.
# ---------------------------------------------------------------------------

evaluate_option <- function(
    category,              # "Space Heating", "Water Heating", "Cooktop", "Vehicles"
    appliance_type,        # e.g. "Natural gas", "Electric heat pump", "LPG"
    label = NULL,          # display name
    capex = 0,             # upfront cost per unit ($)
    n_units = 1,           # number of units (affects capex only)
    state = dflt("state"),
    n_occupants = dflt("n_occupants"),
    period = dflt("household_period"),      # "1year" or "15year"
    use_loan = dflt("use_loan"),
    loan_rate = dflt("loan_rate"),
    loan_term = dflt("loan_term"),
    n_vehicles = dflt("appliance_n_vehicles"),        # for vehicle comparisons
    vehicle_class = dflt("appliance_vehicle_class"), # vehicle class for efficiency lookup
    include_vehicles = FALSE,  # include vehicles in elec supply share denominator
    dwelling_type = dflt("dwelling_type"),   # "house" or "apartment" (apartments use 21% less energy)
    solar_scenario = dflt("option_solar_scenario"), # household-level: "grid_only", "solar", "solar_optimised"
    fast_charge_fraction = FAST_CHARGE_FRACTION, # EV-only: share of total kWh charged at DC fast chargers
    km_tier = dflt("km_tier"),        # "low", "middle", "high" — column of average_km_per_day_by_state.csv
    solar_scale_factor = 1,    # bottom-up cap on solar self-consumption. 1 = no cap (use
                                # SOLAR_FRACTION_TABLE as-is). < 1 scales down per-appliance
                                # solar self-consumption when PV generation is insufficient
                                # to meet aggregate demand. Set by evaluate_household() from
                                # get_household_self_sufficiency()$solar_scale_factor.
    tariff = dflt("tariff"),             # The household's retail electricity plan — one of TARIFFS
                                # ("flat", "tou", "amber", "solar_sharer"). Resolved via
                                # get_tariff_spec(), which is the single source of truth for
                                # every price that depends on the plan: appliance and EV
                                # import rates, the daily supply charge, whether a free
                                # window exists, and how evening battery exports are valued.
    free_window_scale = 1,      # Cap on free-window self-supply, analogous to
                                # solar_scale_factor. 1 = this load can draw its full
                                # `sso_free` share of demand inside the 11am-2pm window.
                                # < 1 scales that back when aggregate household demand for
                                # the window exceeds the plan's daily cap. Set by
                                # evaluate_household() from allocate_free_window(); ignored
                                # unless the tariff has a free window.
    ev_tariff_share = dflt("ev_tariff_share"),        # Cap on the share of TOTAL EV energy that may be billed at
                                # the tariff's EV rate. 1 (default) = uncapped, i.e. every
                                # home-charged kWh gets that rate. Below 1, EV energy splits:
                                #   fast_charge_fraction        → public DC fast-charge price
                                #   solar_frac of the remainder → free (self-consumed)
                                #   free-window share           → free (Solar Sharer only)
                                #   up to ev_tariff_share × total → the tariff's EV rate
                                #   anything still left          → the tariff's import rate
                                # Models a household that can only shift part of its charging
                                # onto the cheap window. Note the cap is a share of total EV
                                # demand, so raising solar self-consumption eats into the
                                # expensive residual first.
    solar_marginal_cost = TRUE, # Charge LCOE per self-consumed solar kWh.
                                # TRUE (default — for standalone calls):
                                #   solar_cost_per_kwh = LCOE so the panels' capex is
                                #   amortised into appliance opex. No "forgone FiT"
                                #   component (the FiT flow is independent of
                                #   self-consumption and only applies to actual exports).
                                # FALSE (used by evaluate_household and the compare_*()
                                #   solar contexts): self-consumed kWh
                                #   are free at the appliance level — the PV system's
                                #   capex is added once at the household level via
                                #   evaluate_solar_battery's solar_capex.
    include_supply_charge = TRUE, # Allocate a share of the daily fuel supply
                                # charge to this appliance row.
                                # TRUE (default — standalone / compare_* calls):
                                #   the appliance is charged a proportional
                                #   slice of the daily electricity / gas / LPG
                                #   supply charge based on its kWh share of an
                                #   all-electric or all-fuel household. This
                                #   makes per-appliance opex comparable across
                                #   options on the same axes.
                                # FALSE (used by evaluate_household): supply
                                #   charges are zero on the row. The household
                                #   total is computed once at the household
                                #   level (1 × daily_charge × 365 per connected
                                #   fuel) so a gas-only home is not under-
                                #   counted and an EV doesn't over-count.
    is_landlord = FALSE,        # Eligibility flags for state appliance subsidies
    battery_vpp = FALSE,        # (Appliance_subsidies.csv). dwelling_type already
    postcode    = NA_integer_,  # carries the Apartment tag. battery_vpp / postcode
                                # only affect battery subsidies and are unused at
                                # the appliance level — they're carried so the
                                # signature can be threaded uniformly from
                                # evaluate_household. postcode resolves to the
                                # WA distribution network (Synergy / Horizon)
                                # via wa_postcode_network.
    hh = NULL   # household() settings object (see section 13)
) {
  if (is_household(state)) hh <- state
  if (!is.null(hh)) .use_household(hh, environment(), sys.function(), match.call())

  if (is.null(label)) label <- appliance_type

  occ_scale <- get_scaling_factor(n_occupants)
  dwelling_scale <- if (dwelling_type == "apartment") APARTMENT_ENERGY_FACTOR else 1
  n_years <- ifelse(period == "1year", 1, 15)

  # --- Fuel type ---
  fuel <- get_fuel_for_appliance(category, appliance_type)

  # Resolve the tariff up front — the free-window split below needs it, as do
  # the import prices and the supply charge further down. Electricity only;
  # gas/LPG/petrol rows have no retail electricity plan.
  spec <- if (fuel == "electricity") get_tariff_spec(tariff, state, period) else NULL

  # --- Energy consumption (kWh/day) ---
  # Space Heating is heating-only — even with a reverse-cycle heat pump (which
  # is the same physical hardware as the AC), cooling kWh are billed via a
  # separate Space Cooling row. Keeps per-appliance comparisons fair
  # (heating-vs-heating) and matches the visualiser's Chart 2.
  if (category == "Vehicles") {
    # Dwelling type does not affect vehicle energy use. Fleet sum supports
    # scalar vehicle_class (× n_vehicles) and vector vehicle_class (mixed
    # classes, one car per element) — see make_fleet().
    fleet  <- make_fleet(vehicle_class, n_vehicles)
    energy <- fleet_vehicle_energy_kwh_day(appliance_type, fleet, state, km_tier)
  } else {
    energy <- get_energy_kwh_day(category, appliance_type, state) * occ_scale * dwelling_scale
  }

  total_energy <- energy

  # --- Realistic EV charge mix ---
  # For EVs, a fixed share of kWh is obtained at DC fast chargers (away from
  # the home meter, no solar or free-window contribution, separate $/kWh
  # price). The rest is home charging. Non-EV options have fast_charge_kwh = 0.
  fast_charge_kwh <- 0
  if (category == "Vehicles" && fuel == "electricity") {
    fast_charge_kwh <- total_energy * fast_charge_fraction
  }
  home_energy   <- total_energy - fast_charge_kwh

  # --- Free window / solar / grid split (electricity only) ---
  # split_load() applies the dispatch order (free window > solar > battery >
  # paid grid) and the min_retail floor. Self-consumed solar and free-window
  # kWh are priced at $0; supply charges are NOT affected — they depend on
  # connection, not source. Non-electric fuels have no split.
  #
  # solar_scale_factor (≤ 1) caps the solar share when aggregate household
  # demand for solar exceeds what the PV system generates that day, and
  # free_window_scale does the same for the Solar Sharer daily cap. Calls from
  # evaluate_household() pass household-level values; single-appliance callers
  # leave both at 1 (i.e. assume enough PV / free-window budget). The battery
  # is applied later, at household level (evaluate_solar_battery).
  split <- if (fuel == "electricity") {
    split_category_load(home_energy, category, solar_scenario, spec,
                        solar_scale = solar_scale_factor,
                        free_scale  = free_window_scale,
                        appliance_type = if (category == "Vehicles") NULL else appliance_type)
  } else {
    split_load(home_energy)
  }
  free_kwh      <- split$free
  solar_energy  <- split$solar
  home_grid_kwh <- home_energy - solar_energy
  grid_energy   <- home_grid_kwh + fast_charge_kwh   # all non-solar kWh
  paid_home_grid_kwh <- split$paid

  # --- Fuel price + energy cost ---
  # Note: `price` is also used below for solar-cost stripping and as the
  # supply-charge daily rate, so we keep it as the standard fuel lookup.
  # The per-kWh price actually charged on grid imports is `home_grid_price`,
  # which for electricity comes from the tariff spec rather than the flat
  # "electricity" row.
  price <- get_fuel_price(state, fuel, period)
  home_grid_price <- price$dol_kwh
  if (fuel == "electricity") {
    home_grid_price <- if (category == "Vehicles") spec$ev_dol_kwh else spec$import_dol_kwh
  }


  if (category == "Vehicles" && fuel == "electricity") {
    fc_price <- get_fast_charge_price(state, period)
    # ev_tariff_share caps how much of the car's TOTAL demand can be billed at
    # the tariff's EV rate. Fast-charge, solar and free-window kWh are already
    # carved out above, so the cap bites only on paid_home_grid_kwh; whatever
    # exceeds it falls back to the tariff's appliance import rate. At the
    # default share of 1 the cap always exceeds the paid kWh, so ev_other_kwh
    # is 0 and this collapses to single-rate behaviour.
    ev_rate_cap_kwh <- max(ev_tariff_share, 0) * total_energy
    ev_rate_kwh     <- min(paid_home_grid_kwh, ev_rate_cap_kwh)
    ev_other_kwh    <- paid_home_grid_kwh - ev_rate_kwh
    annual_energy_cost <- ev_rate_kwh     * 365 * home_grid_price +
                          ev_other_kwh    * 365 * spec$import_dol_kwh +
                          fast_charge_kwh * 365 * fc_price$dol_kwh
  } else {
    paid_grid_kwh      <- paid_home_grid_kwh + fast_charge_kwh
    annual_energy_cost <- paid_grid_kwh * 365 * home_grid_price
  }

  # --- Solar pricing for the appliance (electricity-only) ---
  # Behaviour depends on solar_marginal_cost:
  #   TRUE  (default — standalone calls): self-consumed solar kWh
  #         are charged at LCOE so that the panels' capex is amortised into
  #         appliance opex. A single-appliance comparison doesn't have
  #         anywhere else to put the PV system's cost, so this is how it
  #         shows up. No "forgone FiT" charge — the FiT flow is independent
  #         and only applies to kWh that are actually exported.
  #   FALSE (evaluate_household and the compare_*() solar contexts):
  #         self-consumed solar is free at the appliance level. The PV
  #         system's capex is added once at the household level via
  #         evaluate_solar_battery's solar_capex, so charging LCOE per kWh here
  #         would double-count the panels.
  # Either way the value of self-consumption to the household is the avoided
  # grid import cost (at the tariff's import rate) — that emerges from the
  # grid-energy pricing above, not from this term.
  solar_cost_per_kwh <- 0
  annual_solar_cost  <- 0
  if (solar_marginal_cost && fuel == "electricity" && solar_energy > 0) {
    solar_lcoe_dol_kwh <- get_solar_lcoe(state)
    if (is.na(solar_lcoe_dol_kwh)) solar_lcoe_dol_kwh <- 0
    solar_cost_per_kwh <- solar_lcoe_dol_kwh
    annual_solar_cost  <- solar_energy * 365 * solar_cost_per_kwh
  }
  annual_energy_cost <- annual_energy_cost + annual_solar_cost

  # --- Supply charges (proportional by consumption) ---
  # When include_supply_charge = FALSE (set by evaluate_household), the per-
  # appliance share allocation is skipped and the household-level supply is
  # applied once per connected fuel by the caller instead.
  elec_supply <- 0
  gas_supply <- 0

  if (include_supply_charge) {
    # The daily supply charge comes from the tariff spec, not the flat
    # "electricity" row: Solar Sharer carries its own materially higher standing
    # charge, which is the standing cost of the free window and has to be billed
    # alongside it or the plan looks free.
    if (fuel == "electricity" && category != "Vehicles") {
      elec_share <- get_elec_supply_share(category, state, n_occupants, include_vehicles, n_vehicles, km_tier, vehicle_class)
      elec_supply <- spec$daily_charge * 365 * elec_share
    }
    if (fuel == "electricity" && category == "Vehicles") {
      elec_share <- get_elec_supply_share(category, state, n_occupants, include_vehicles = TRUE, n_vehicles = n_vehicles, km_tier = km_tier, vehicle_class = vehicle_class)
      elec_supply <- spec$daily_charge * 365 * elec_share
    }

    if (fuel %in% c("gas", "lpg") && category != "Vehicles") {
      gas_share <- get_gas_supply_share(category, state, n_occupants)
      gas_supply <- price$daily_charge * 365 * gas_share
    }
  }

  # --- Annual maintenance (vehicles only) ---
  # Per-vehicle service + tyres + consumables; not occupancy-scaled. Excludes
  # fuel/electricity (already in annual_energy_cost) and registration/insurance.
  # Summed across the fleet (mixed classes supported).
  annual_maintenance <- 0
  if (category == "Vehicles") {
    annual_maintenance <- fleet_vehicle_maintenance(fleet, appliance_type)
  }

  annual_supply <- elec_supply + gas_supply
  annual_opex <- annual_energy_cost + annual_supply + annual_maintenance
  total_opex <- annual_opex * n_years

  # --- Capex (with state appliance subsidy) ---
  total_capex <- capex * n_units

  # Heat pump space heating + water heating attract state-level subsidies
  # (e.g. VIC's Energy Upgrades programme). Solar PV and battery subsidies
  # are applied in evaluate_solar_battery() because the capex for those
  # components is tracked there, not here.
  subsidy <- 0
  if (category %in% c("Space Heating", "Water Heating") &&
      appliance_type == "Electric heat pump") {
    subsidy <- get_appliance_subsidy("Heat pump", state,
                                       dwelling_type = dwelling_type,
                                       is_landlord   = is_landlord)
  }
  total_capex <- max(0, total_capex - subsidy)

  # --- Loan ---
  if (use_loan && total_capex > 0) {
    annual_loan <- annual_loan_payment(total_capex, loan_rate, loan_term)
    loan_years <- min(n_years, loan_term)
    total_loan <- annual_loan * loan_years
    total_cost <- total_opex + total_loan
  } else {
    annual_loan <- 0
    total_loan <- 0
    total_cost <- total_opex + total_capex
  }

  # --- Emissions ---
  ef_col <- case_when(
    fuel == "electricity" ~ "grid_electricity_kgCO2e_per_kWh",
    fuel == "gas"         ~ "natural_gas_kgCO2e_per_kWh",
    fuel == "lpg"         ~ "lpg_kgCO2e_per_kWh",
    fuel == "wood"        ~ "wood_kgCO2e_per_kWh",
    fuel == "petrol"      ~ "petrol_kgCO2e_per_kWh",
    fuel == "diesel"      ~ "diesel_kgCO2e_per_kWh"
  )
  ef_i   <- .EMISSIONS_ROW[state]
  ef_val <- if (is.na(ef_i)) NA_real_ else emissions_factors[[ef_col]][ef_i]
  # Missing state or missing factor (e.g. no gas in NT/TAS) counts as 0.
  if (length(ef_val) == 0 || is.na(ef_val)) ef_val <- 0
  # For electricity, only grid-sourced kWh emit (solar self-consumption is
  # treated as zero-emission at the household level). Non-electric fuels have
  # no solar share, so grid_energy == total_energy.
  emitting_energy <- if (fuel == "electricity") grid_energy else total_energy
  annual_emissions <- emitting_energy * 365 * ef_val / 1000  # tonnes CO2e

  tibble(
    category            = category,
    option              = label,
    fuel                = fuel,
    state               = state,
    n_occupants         = n_occupants,
    period              = period,
    use_loan            = use_loan,
    solar_scenario      = solar_scenario,
    tariff              = if (is.null(spec)) NA_character_ else spec$tariff,
    energy_kwh_day      = total_energy,
    solar_kwh_day       = solar_energy,
    home_grid_kwh_day   = home_grid_kwh,
    # Grid kWh bought inside the Solar Sharer free window: zero-cost but still
    # grid-sourced, so they stay inside grid_kwh_day and keep emitting below.
    free_kwh_day        = free_kwh,
    grid_kwh_day        = grid_energy,
    solar_cost_per_kwh  = solar_cost_per_kwh,
    capex               = total_capex,
    subsidy             = subsidy,
    annual_energy_cost  = annual_energy_cost,
    annual_supply_charge = annual_supply,
    annual_maintenance  = annual_maintenance,
    annual_opex         = annual_opex,
    total_cost          = total_cost,
    annual_tCO2e        = annual_emissions,
  )
}

# =============================================================================
# 10. Pre-defined multi-option comparisons per function
# =============================================================================

# Attach baseline running-cost columns to a comparison tibble.
#   baseline_annual_opex            — the chosen baseline row's annual_opex
#   annual_opex_savings_vs_baseline — baseline_annual_opex - annual_opex
# Works for both 1-year and 15-year runs (annual_opex is annualised either way).
# Returns NA savings if the baseline row is absent (e.g. NT has no Gas row).
.add_savings_vs_baseline <- function(df, baseline_option) {
  if (nrow(df) == 0 || !"option" %in% names(df) || !"annual_opex" %in% names(df)) {
    return(df)
  }
  # Baselines are struck WITHIN each system_context: the gas baseline for the
  # solar_battery context must be the gas row as it stands in that context, not
  # whichever context happened to sort first. (Gas rows are identical across
  # contexts, but electric-baseline comparisons are not.)
  if ("system_context" %in% names(df)) {
    return(
      df |>
        group_by(.data$system_context) |>
        mutate(
          baseline_annual_opex = {
            b <- .data$annual_opex[.data$option == baseline_option]
            if (length(b) == 0) NA_real_ else b[1]
          }
        ) |>
        ungroup() |>
        mutate(annual_opex_savings_vs_baseline =
                 .data$baseline_annual_opex - .data$annual_opex)
    )
  }
  baseline_row  <- df |> filter(.data$option == baseline_option)
  baseline_opex <- if (nrow(baseline_row) == 0) NA_real_ else baseline_row$annual_opex[1]
  df |>
    mutate(
      baseline_annual_opex            = baseline_opex,
      annual_opex_savings_vs_baseline = baseline_opex - .data$annual_opex
    )
}

# ---------------------------------------------------------------------------
# Solar / Battery rows — appended to each compare_*() output
# ---------------------------------------------------------------------------
# compare_*() appliance rows are evaluated at solar_scenario = "grid_only": no
# self-consumption is baked into any appliance's own cost. This function is a
# disaggregated view of evaluate_household()'s own solar/battery accounting:
# it computes the SAME whole-household $ figures evaluate_household() already
# derives internally, and surfaces them as two standalone rows —
# category = "solar" / option = "solar", and category = "battery" /
# option = "battery" — carrying the FULL (undivided) solar/battery capex and
# opex, not a per-appliance share. The same two rows are appended, unchanged,
# to every compare_*() output, so e.g. "Heat pump HW" and "solar" can be read
# side by side in compare_water_heating()'s own table.
#
# Household load is assumed all-electric-efficient (heat pump heating & HW,
# induction cooktop, EV) when deriving the solar/battery figures, per the
# model's own convention for scenario electrification flags.
#
# Solar row value = the household's true avoided grid cost from self-consumed
# solar (energy cost at grid_only minus energy cost with solar, at the
# tariff's import rates — see household_energy_cost() below) plus
# annual_export_value + annual_headroom_value (exported / arbitraged surplus).
# Appliance rows here are at grid_only, so this row is where that value lives.
#
# Battery row value = annual_battery_self_consumption_value (evening
# discharge covering appliance load) + annual_battery_ev_value (opportunistic
# surplus-solar-to-EV credit) — read straight off evaluate_solar_battery()'s
# summary, exactly as evaluate_household() does.
#
# Both rows carry the full 10kW solar / 15kWh battery system capex, financed
# the same way evaluate_option() capitalises appliance capex.
EFFICIENT_ELECTRIC_TYPE <- list(
  "Space Heating" = "Electric heat pump",
  "Water Heating" = "Electric heat pump",
  "Cooktop"       = "Electric induction",
  "Vehicles"      = "Electric"
)

# Collapse the duplicated solar / battery rows when several compare_*() outputs
# are bound together. Every compare_*() appends an identical pair of them —
# they describe ONE physical system serving the whole house, not one per
# appliance — so a plain bind_rows() of the four tables would count their
# capex and savings 4x in any cross-category total.
#
# Deliberately targeted rather than a blanket distinct(): grid_only appliance
# rows are identical across scenario
# runs too, and a blanket distinct() would collapse those as well — dropping
# the per-scenario copies that scenario-faceted charts rely on. This touches
# only the solar / battery rows and leaves every appliance row untouched.
dedupe_solar_battery <- function(df) {
  if (nrow(df) == 0 || !"category" %in% names(df)) return(df)
  is_sb <- df$category %in% c("solar", "battery")
  bind_rows(df[!is_sb, , drop = FALSE],
            dplyr::distinct(df[is_sb, , drop = FALSE]))
}

# ---------------------------------------------------------------------------
# Per-appliance system contexts
# ---------------------------------------------------------------------------
# Each appliance option is priced in three contexts, so a single table can
# answer "what does hot water cost me on grid alone / with solar / with solar
# + battery":
#   grid_only     — no solar, no battery
#   solar         — solar self-consumption per `solar_scenario`
#   solar_battery — solar, then the home battery covers what solar didn't
#
# Gas / LPG / petrol options are identical in all three (no electricity), which
# is what makes "gas vs heat pump under solar+battery" a direct subtraction.
#
# ATTRIBUTION WARNING. These rows and the whole-house solar / battery rows
# (category "solar" / "battery", tagged system_context = "system") describe the
# SAME dollars under two different attributions and must never be summed
# together:
#   * whole-home total that reconciles with evaluate_household():
#       grid_only appliance rows  +  the "system" rows
#     (this is the path the 256-variation validation script checks)
#   * per-appliance view: the solar / solar_battery appliance rows ALONE.
# Use whole_home_view() / appliance_view() to take one coherently.
#
# Per the project's costing convention, the solar and solar_battery appliance
# rows carry APPLIANCE capex only — no share of the PV / battery system capex.
# That cost is reported once, undivided, in the "system" rows. So these rows
# answer "what does this appliance cost to run in a solar home", not "what does
# the whole solar home cost".
SYSTEM_CONTEXTS <- c("grid_only", "solar", "solar_battery")

# Household-level quantities the per-appliance contexts need: the solar
# generation cap, the free-window cap, and the battery coverage RATE.
#
# The battery is one whole-house asset, so it is allocated as a single uniform
# coverage rate = battery kWh available / total post-solar remaining kWh across
# every appliance load (heating, cooling, hot water, cooktop, Other Cooking,
# Other Electronics). Capped at 1. A uniform rate is exactly "pro-rata by
# remaining need, capped at 100%, with no load prioritised over another" —
# and because it is a rate rather than a fixed kWh split, it applies to any
# option, including the less efficient electric ones whose remaining need is
# larger than the efficient household the pool was derived from.
# Vehicles get their own rate from the separate battery -> EV credit.
.household_context <- function(hh, solar_scenario) {
  n_years  <- hh$n_years
  spec     <- get_tariff_spec(hh$tariff, hh$state, hh$period)
  sys_info <- get_solar_system_info(hh$state)
  daily_solar_kwh_eff <- sys_info$capacity_factor * 24 * hh$solar_kwp

  loads <- compute_household_loads(
    hh = hh,
    heating_type  = "Electric heat pump", hotwater_type = "Electric heat pump",
    cooking_type  = "Electric induction", vehicle_type = "Electric"
  )
  free <- get_household_free_window(
    appliance_loads = loads$appliance_loads,
    vehicle_load_kwh = loads$vehicle_load_kwh,
    solar_scenario = solar_scenario, spec = spec
  )
  min_ret <- get_household_min_retail(
    appliance_loads = loads$appliance_loads,
    vehicle_load_kwh = loads$vehicle_load_kwh
  )
  breakdown <- get_household_self_sufficiency(
    appliance_loads     = loads$appliance_loads,
    vehicle_load_kwh    = loads$vehicle_load_kwh,
    solar_scenario      = solar_scenario,
    available_solar_kwh = daily_solar_kwh_eff,
    free_by_load        = free$free_by_load,
    vehicle_free_kwh    = free$vehicle_free_kwh,
    min_retail_by_load  = min_ret$min_retail_by_load,
    vehicle_min_retail_kwh = min_ret$vehicle_min_retail_kwh
  )
  scen_scale      <- breakdown$solar_scale_factor
  scen_free_scale <- if (isTRUE(spec$free_window)) free$free_scale_factor else 1

  # Post-solar remaining paid grid kWh/day, per load group.
  paid_for <- function(load_kwh, category) {
    split_category_load(load_kwh, category, solar_scenario, spec,
                        solar_scale = scen_scale, free_scale = scen_free_scale)$paid
  }
  al <- loads$appliance_loads
  total_remaining <-
    paid_for(al$heating,       "Space Heating")     +
    paid_for(al$cooling,       "Space Cooling")     +
    paid_for(al$hotwater,      "Water Heating")     +
    paid_for(al$cooking,       "Cooktop")           +
    paid_for(al$other_cooking, "Other Cooking")     +
    paid_for(al$other_elec,    "Other Electronics")
  ev_remaining <- paid_for(loads$vehicle_load_kwh, "Vehicles")

  sb <- evaluate_solar_battery(
    hh = hh, solar_scenario = solar_scenario,
    heating_type  = "Electric heat pump", hotwater_type = "Electric heat pump",
    cooking_type  = "Electric induction", vehicle_type = "Electric"
  )$summary

  battery_pool_kwh_day <- sb$annual_battery_self_consumption_kwh / 365
  ev_pool_kwh_day      <- sb$annual_battery_ev_kwh / 365

  grid_ef <- get_grid_ef(hh$state)

  list(
    spec = spec, n_years = n_years,
    scen_scale = scen_scale, scen_free_scale = scen_free_scale,
    coverage_rate    = if (total_remaining > 0) min(1, battery_pool_kwh_day / total_remaining) else 0,
    ev_coverage_rate = if (ev_remaining > 0)    min(1, ev_pool_kwh_day / ev_remaining)         else 0,
    grid_ef = grid_ef
  )
}

# Price one appliance option in all three system contexts.
.option_contexts <- function(opt, ctx, solar_scenario, hh) {
  base <- list(category = opt$category, appliance_type = opt$appliance_type,
               label = opt$label, capex = opt$capex,
               n_units = if (is.null(opt$n_units)) 1 else opt$n_units,
               include_vehicles = isTRUE(opt$include_vehicles),
               hh = hh)

  grid_row <- do.call(evaluate_option, c(base, list(solar_scenario = "grid_only")))

  if (solar_scenario == "grid_only") {
    return(grid_row |> mutate(system_context = "grid_only", battery_kwh_day = 0))
  }

  solar_row <- do.call(evaluate_option, c(base, list(
    solar_scenario      = solar_scenario,
    solar_scale_factor  = ctx$scen_scale,
    free_window_scale   = ctx$scen_free_scale,
    solar_marginal_cost = FALSE
  )))

  is_veh <- opt$category == "Vehicles"
  is_elec <- solar_row$fuel[1] == "electricity"
  # Battery serves what solar and the free window left, at this household's
  # uniform coverage rate. Fast-charge kWh are already outside home_grid_kwh_day.
  remaining <- max(solar_row$home_grid_kwh_day[1] - solar_row$free_kwh_day[1], 0)
  rate      <- if (is_veh) ctx$ev_coverage_rate else ctx$coverage_rate
  batt_cov  <- if (is_elec) min(remaining * rate, remaining) else 0
  price     <- if (is_veh) ctx$spec$ev_dol_kwh else ctx$spec$import_dol_kwh
  saving    <- batt_cov * 365 * price

  batt_row <- solar_row |>
    mutate(
      system_context     = "solar_battery",
      battery_kwh_day    = batt_cov,
      grid_kwh_day       = .data$grid_kwh_day - batt_cov,
      home_grid_kwh_day  = .data$home_grid_kwh_day - batt_cov,
      annual_energy_cost = .data$annual_energy_cost - saving,
      annual_opex        = .data$annual_opex - saving,
      total_cost         = .data$total_cost - saving * ctx$n_years,
      # Battery discharge is stored solar, so it is zero-emission by the
      # model's convention (same netting evaluate_household() applies).
      annual_tCO2e       = pmax(.data$annual_tCO2e - batt_cov * 365 * ctx$grid_ef / 1000, 0)
    )

  bind_rows(
    grid_row  |> mutate(system_context = "grid_only", battery_kwh_day = 0),
    solar_row |> mutate(system_context = "solar",     battery_kwh_day = 0),
    batt_row
  )
}

# Take one coherent attribution out of a compare_*() table (see the warning
# above). whole_home_view() is the one that reconciles with evaluate_household().
whole_home_view <- function(df) {
  df |> filter(.data$system_context %in% c("grid_only", "system"))
}
appliance_view <- function(df, context = "solar_battery") {
  stopifnot(context %in% SYSTEM_CONTEXTS)
  df |> filter(.data$system_context == context)
}

solar_battery_rows <- function(state = dflt("state"), n_occupants = dflt("n_occupants"), period = dflt("appliance_period"),
                                 use_loan = dflt("use_loan"), dwelling_type = dflt("dwelling_type"),
                                 tariff = dflt("tariff"),
                                 n_vehicles = dflt("appliance_n_vehicles"), vehicle_class = dflt("appliance_vehicle_class"),
                                 km_tier = dflt("km_tier"),
                                 fast_charge_fraction = FAST_CHARGE_FRACTION,
                                 ev_tariff_share = dflt("ev_tariff_share"),
                                 solar_kwp = NULL, battery_size_kwh = NULL,  # NULL: default_system_size(dwelling_type)
                                 loan_rate = dflt("loan_rate"), loan_term = dflt("loan_term"),
                                 # Defaults deliberately match evaluate_household()'s,
                                 # not evaluate_solar_battery()'s bare defaults, so a
                                 # whole-home total assembled from compare_*() rows
                                 # reconciles against evaluate_household().
                                 household_safeguard_pct = dflt("household_safeguard_pct"),
                                 useable_capacity_pct    = dflt("battery_useable_capacity_pct"),
                                 annual_degradation_pct  = NULL,
                                 battery_round_trip_efficiency = dflt("battery_round_trip_efficiency"),
                                 free_battery_kwh = dflt("free_battery_kwh"),
                                 fit_price_kwh = NULL,
                                 is_landlord = FALSE, battery_vpp = FALSE,
                                 postcode = NA_integer_,
                                 # Which solar scenarios to build rows for. The
                                 # compare_*() helpers pass just the one they need.
                                 solar_scenarios = c("solar", "solar_optimised"),
    hh = NULL   # household() settings object (see section 13)
) {
  if (is_household(state)) hh <- state
  if (!is.null(hh)) .use_household(hh, environment(), sys.function(), match.call())
  sys_size         <- default_system_size(dwelling_type, solar_kwp, battery_size_kwh)
  solar_kwp        <- sys_size$solar_kwp
  battery_size_kwh <- sys_size$battery_size_kwh
  n_years <- ifelse(period == "1year", 1, 15)
  if (is.null(annual_degradation_pct)) {
    annual_degradation_pct <- default_degradation(n_years)
  }
  hh_eff <- .household_from_env(environment())

  capitalise <- function(annual_value, capex) {
    if (use_loan && capex > 0) {
      annual_loan <- annual_loan_payment(capex, loan_rate, loan_term)
      loan_years  <- min(n_years, loan_term)
      annual_value * n_years + annual_loan * loan_years
    } else {
      annual_value * n_years + capex
    }
  }

  bind_rows(lapply(solar_scenarios, function(scen) {
    # Whole-household solar self-consumption, derived through exactly the same
    # helper chain evaluate_household() uses — compute_household_loads() →
    # get_household_free_window() → get_household_min_retail() →
    # get_household_self_sufficiency(). This covers EVERY electric load the
    # household runs (heating, cooling, hot water, cooktop, Other Cooking,
    # Other Electronics, vehicles), and critically applies the bottom-up
    # generation cap: when aggregate demand for solar exceeds what the PV
    # system actually generates, solar_scale_factor (< 1) scales it back.
    # Computing this per-appliance with solar_scale_factor left at its default
    # of 1 would overstate self-consumption on undersized systems.
    spec_sb   <- get_tariff_spec(tariff, state, period)
    sys_info  <- get_solar_system_info(state)
    daily_solar_kwh_eff <- sys_info$capacity_factor * 24 * solar_kwp

    hh_loads <- compute_household_loads(
      hh = hh_eff,
      heating_type  = "Electric heat pump", hotwater_type = "Electric heat pump",
      cooking_type  = "Electric induction", vehicle_type = "Electric"
    )
    hh_free <- get_household_free_window(
      appliance_loads  = hh_loads$appliance_loads,
      vehicle_load_kwh = hh_loads$vehicle_load_kwh,
      solar_scenario   = scen, spec = spec_sb
    )
    hh_min_retail <- get_household_min_retail(
      appliance_loads  = hh_loads$appliance_loads,
      vehicle_load_kwh = hh_loads$vehicle_load_kwh
    )
    hh_breakdown <- get_household_self_sufficiency(
      appliance_loads     = hh_loads$appliance_loads,
      vehicle_load_kwh    = hh_loads$vehicle_load_kwh,
      solar_scenario      = scen,
      available_solar_kwh = daily_solar_kwh_eff,
      free_by_load        = hh_free$free_by_load,
      vehicle_free_kwh    = hh_free$vehicle_free_kwh,
      min_retail_by_load  = hh_min_retail$min_retail_by_load,
      vehicle_min_retail_kwh = hh_min_retail$vehicle_min_retail_kwh
    )

    annual_solar_self_kwh <- (hh_breakdown$appliance_solar_kwh +
                                hh_breakdown$vehicle_solar_kwh) * 365
    scen_scale      <- hh_breakdown$solar_scale_factor
    scen_free_scale <- if (isTRUE(spec_sb$free_window)) hh_free$free_scale_factor else 1

    # --- Value of self-consumption = TRUE avoided grid cost ------------------
    # NOT annual_solar_self_kwh × flat retail. evaluate_household() reports a
    # flat-retail figure in its annual_solar_self_value column, but that column
    # is explicitly reporting-only and never enters its total_cost — the real
    # saving there is implicit in the appliance rows' reduced bills, struck at
    # the TARIFF's import rate (and the EV rate for vehicles), not flat retail.
    # Under "flat" the two coincide; under "tou" / "solar_sharer" the import
    # rate is the off-peak rate and pricing at flat retail overstates the
    # saving by up to 13c/kWh. Here the figure IS load-bearing (it is this
    # row's opex), so it must be the genuine avoided cost: total household
    # energy cost at grid_only minus total household energy cost with solar.
    household_energy_cost <- function(sc, solar_scale, free_scale) {
      appliance_cost <- sum(vapply(names(EFFICIENT_ELECTRIC_TYPE), function(category) {
        is_veh <- category == "Vehicles"
        evaluate_option(category, EFFICIENT_ELECTRIC_TYPE[[category]], "ref",
                         capex = 0, hh = hh_eff, use_loan = FALSE,
                         include_vehicles = is_veh, solar_scenario = sc,
                         solar_scale_factor = solar_scale, free_window_scale = free_scale,
                         solar_marginal_cost = FALSE,
                         include_supply_charge = FALSE)$annual_energy_cost
      }, numeric(1)))

      cooling_cost <- evaluate_option("Space Cooling", "Heat pump", "ref", capex = 0,
                                        hh = hh_eff, use_loan = FALSE, solar_scenario = sc,
                                        solar_scale_factor = solar_scale,
                                        free_window_scale = free_scale,
                                        solar_marginal_cost = FALSE,
                                        include_supply_charge = FALSE)$annual_energy_cost

      # "Other electric loads" baseload — split by split_load(), exactly as
      # evaluate_household()'s baseload row is.
      occ_scale      <- get_scaling_factor(n_occupants)
      dwelling_scale <- if (dwelling_type == "apartment") APARTMENT_ENERGY_FACTOR else 1
      baseload_paid <- sum(vapply(c("Other Cooking", "Other Electronics"), function(category) {
        load_kwh <- get_category_total_kwh_day(category, state) * occ_scale * dwelling_scale
        split_category_load(load_kwh, category, sc, spec_sb,
                            solar_scale = solar_scale, free_scale = free_scale)$paid
      }, numeric(1)))

      appliance_cost + cooling_cost + baseload_paid * 365 * spec_sb$import_dol_kwh
    }

    # grid_only baseline uses scale = 1, matching the defaults the compare_*()
    # appliance rows themselves are evaluated at.
    annual_solar_self_value <- household_energy_cost("grid_only", 1, 1) -
                                 household_energy_cost(scen, scen_scale, scen_free_scale)

    sb <- evaluate_solar_battery(
      hh = hh_eff, solar_scenario = scen,
      heating_type  = "Electric heat pump", hotwater_type = "Electric heat pump",
      cooking_type  = "Electric induction", vehicle_type = "Electric"
    )$summary

    annual_solar_fit         <- sb$annual_export_value + sb$annual_headroom_value
    annual_battery_credit    <- sb$annual_battery_self_consumption_value
    annual_battery_ev_credit <- sb$annual_battery_ev_value
    annual_solar_value       <- annual_solar_self_value + annual_solar_fit
    annual_battery_value     <- annual_battery_credit   + annual_battery_ev_credit

    solar_row <- tibble(
      category = "solar", option = "solar", fuel = "solar", state = state,
      system_context = "system",
      n_occupants = n_occupants, period = period, use_loan = use_loan,
      solar_scenario = scen, tariff = tariff,
      energy_kwh_day = annual_solar_self_kwh / 365, solar_kwh_day = annual_solar_self_kwh / 365,
      home_grid_kwh_day = NA_real_, free_kwh_day = NA_real_, grid_kwh_day = NA_real_,
      solar_cost_per_kwh = NA_real_,
      capex = sb$total_solar_capex, subsidy = sb$solar_subsidy,
      annual_energy_cost = -annual_solar_value,
      annual_supply_charge = 0, annual_maintenance = 0,
      annual_opex = -annual_solar_value,
      total_cost = capitalise(-annual_solar_value, sb$total_solar_capex),
      annual_tCO2e = 0
    )
    battery_row <- tibble(
      category = "battery", option = "battery", fuel = "battery", state = state,
      system_context = "system",
      n_occupants = n_occupants, period = period, use_loan = use_loan,
      solar_scenario = scen, tariff = tariff,
      energy_kwh_day = (sb$annual_battery_self_consumption_kwh + sb$annual_battery_ev_kwh) / 365,
      solar_kwh_day = NA_real_,
      home_grid_kwh_day = NA_real_, free_kwh_day = NA_real_, grid_kwh_day = NA_real_,
      solar_cost_per_kwh = NA_real_,
      capex = sb$battery_capex, subsidy = sb$battery_subsidy,
      annual_energy_cost = -annual_battery_value,
      annual_supply_charge = 0, annual_maintenance = 0,
      annual_opex = -annual_battery_value,
      total_cost = capitalise(-annual_battery_value, sb$battery_capex),
      annual_tCO2e = 0
    )
    bind_rows(solar_row, battery_row)
  }))
}

# Shared builder for the compare_*() family: prices every option in all three
# system contexts, attaches the fossil-baseline savings columns per context, and
# appends the whole-house solar / battery "system" rows.
#
# When solar_scenario == "grid_only" only the grid_only context is produced and
# no system rows are appended — this keeps compare_across_solar(), which
# iterates all of SOLAR_SCENARIOS, working unchanged.
.compare_build <- function(opts, baseline_option, solar_scenario, hh) {
  has_solar <- solar_scenario %in% c("solar", "solar_optimised")

  ctx <- if (has_solar) .household_context(hh, solar_scenario) else NULL

  rows <- bind_rows(lapply(opts, .option_contexts, ctx = ctx,
                             solar_scenario = solar_scenario, hh = hh)) |>
    .add_savings_vs_baseline(baseline_option)

  if (has_solar) {
    rows <- bind_rows(rows, solar_battery_rows(hh = hh, solar_scenarios = solar_scenario))
  }
  rows
}

compare_space_heating <- function(state = dflt("state"), n_occupants = dflt("n_occupants"), period = dflt("appliance_period"),
                                   use_loan = dflt("use_loan"), dwelling_type = dflt("dwelling_type"),
                                   solar_scenario = dflt("compare_solar_scenario"),
                                   tariff = dflt("tariff"),
                                   n_vehicles = dflt("appliance_n_vehicles"), vehicle_class = dflt("appliance_vehicle_class"),
                                   km_tier = dflt("km_tier"),
                                   fast_charge_fraction = FAST_CHARGE_FRACTION,
                                   ev_tariff_share = dflt("ev_tariff_share"),
                                   solar_kwp = NULL, battery_size_kwh = NULL,  # NULL: default_system_size(dwelling_type)
                                   household_safeguard_pct = dflt("household_safeguard_pct"),
                                   annual_degradation_pct = NULL,
                                   loan_rate = dflt("loan_rate"), loan_term = dflt("loan_term"),
    hh = NULL   # household() settings object (see section 13)
) {
  if (is_household(state)) hh <- state
  if (!is.null(hh)) .use_household(hh, environment(), sys.function(), match.call())
  # Number of heating units in a typical home for this state (e.g. multi-zone
  # ducted, multiple split-system heads, or several plug-in heaters). Drives
  # heating capex only — energy is already a whole-of-household figure.
  # Resistive heaters cover smaller zones than ducted / split systems, so we
  # assume households install 2x as many of them per heating zone.
  n_heaters <- get_heater_count(state)
  # Capex from the product prices CSV — same rows evaluate_household() uses.
  opts <- list(
    list(category = "Space Heating", appliance_type = "Natural gas",        label = "Gas heater",       capex = get_product_price("Gas Heaters ~7kW"),     n_units = n_heaters),
    list(category = "Space Heating", appliance_type = "LPG",                label = "LPG heater",       capex = get_product_price("LPG Heaters"),         n_units = n_heaters),
    list(category = "Space Heating", appliance_type = "Electric resistance", label = "Resistive heater", capex = get_product_price("Resistive Heaters"),   n_units = n_heaters * dflt("resistive_heaters_per_heating_unit")),
    list(category = "Space Heating", appliance_type = "Electric heat pump",  label = "Heat pump (AC)",   capex = get_product_price("Reverse cycle AC ~7kW"), n_units = n_heaters)
  )
  .compare_build(opts, "Gas heater", solar_scenario,
                  .household_from_env(environment()))
}

compare_water_heating <- function(state = dflt("state"), n_occupants = dflt("n_occupants"), period = dflt("appliance_period"),
                                    use_loan = dflt("use_loan"), dwelling_type = dflt("dwelling_type"),
                                    solar_scenario = dflt("compare_solar_scenario"),
                                    tariff = dflt("tariff"),
                                    n_vehicles = dflt("appliance_n_vehicles"), vehicle_class = dflt("appliance_vehicle_class"),
                                    km_tier = dflt("km_tier"),
                                    fast_charge_fraction = FAST_CHARGE_FRACTION,
                                    ev_tariff_share = dflt("ev_tariff_share"),
                                    solar_kwp = NULL, battery_size_kwh = NULL,  # NULL: default_system_size(dwelling_type)
                                    household_safeguard_pct = dflt("household_safeguard_pct"),
                                    annual_degradation_pct = NULL,
                                    loan_rate = dflt("loan_rate"), loan_term = dflt("loan_term"),
    hh = NULL   # household() settings object (see section 13)
) {
  if (is_household(state)) hh <- state
  if (!is.null(hh)) .use_household(hh, environment(), sys.function(), match.call())
  # Capex reads the product prices CSV, same rows evaluate_household() uses, so
  # the per-appliance and whole-household views cost identical hardware.
  opts <- list(
    list(category = "Water Heating", appliance_type = "Natural gas",        label = "Gas instant",   capex = get_product_price("Water Heater NGas Instant or LPG Instant")),
    list(category = "Water Heating", appliance_type = "LPG",                label = "LPG instant",   capex = get_product_price("Water Heater NGas Instant or LPG Instant")),
    list(category = "Water Heating", appliance_type = "Electric resistance", label = "Electric tank", capex = get_product_price("Water Heater Electric Storage Med/Large")),
    list(category = "Water Heating", appliance_type = "Electric heat pump",  label = "Heat pump HW",  capex = get_product_price("Water Heater Heat Pumps"))
  )
  .compare_build(opts, "Gas instant", solar_scenario,
                  .household_from_env(environment()))
}

compare_cooktops <- function(state = dflt("state"), n_occupants = dflt("n_occupants"), period = dflt("appliance_period"),
                              use_loan = dflt("use_loan"), dwelling_type = dflt("dwelling_type"),
                              solar_scenario = dflt("compare_solar_scenario"),
                              tariff = dflt("tariff"),
                              n_vehicles = dflt("appliance_n_vehicles"), vehicle_class = dflt("appliance_vehicle_class"),
                              km_tier = dflt("km_tier"),
                              fast_charge_fraction = FAST_CHARGE_FRACTION,
                              ev_tariff_share = dflt("ev_tariff_share"),
                              solar_kwp = NULL, battery_size_kwh = NULL,  # NULL: default_system_size(dwelling_type)
                              household_safeguard_pct = dflt("household_safeguard_pct"),
                              annual_degradation_pct = NULL,
                              loan_rate = dflt("loan_rate"), loan_term = dflt("loan_term"),
    hh = NULL   # household() settings object (see section 13)
) {
  if (is_household(state)) hh <- state
  if (!is.null(hh)) .use_household(hh, environment(), sys.function(), match.call())
  # Capex from the product prices CSV — same rows evaluate_household() uses.
  opts <- list(
    list(category = "Cooktop", appliance_type = "Natural gas",        label = "Gas cooktop",       capex = get_product_price("Gas Cooktops")),
    list(category = "Cooktop", appliance_type = "Electric resistance", label = "Electric cooktop",  capex = get_product_price("Electric Cooktops")),
    list(category = "Cooktop", appliance_type = "Electric induction",  label = "Induction cooktop", capex = get_product_price("Induction Cooktops"))
  )
  .compare_build(opts, "Gas cooktop", solar_scenario,
                  .household_from_env(environment()))
}

compare_vehicles <- function(state = dflt("state"), n_vehicles = dflt("appliance_n_vehicles"), period = dflt("appliance_period"),
                              use_loan = dflt("use_loan"), vehicle_class = dflt("appliance_vehicle_class"),
                              new_used = dflt("new_used"),
                              solar_scenario = dflt("compare_solar_scenario"),
                              fast_charge_fraction = FAST_CHARGE_FRACTION,
                              km_tier = dflt("km_tier"),
                              tariff = dflt("tariff"),
                              ev_tariff_share = dflt("ev_tariff_share"),
                              solar_kwp = NULL, battery_size_kwh = NULL,  # NULL: default_system_size(dwelling_type)
                              n_occupants = dflt("n_occupants"), dwelling_type = dflt("dwelling_type"),
                              household_safeguard_pct = dflt("household_safeguard_pct"),
                              annual_degradation_pct = NULL,
                              loan_rate = dflt("loan_rate"), loan_term = dflt("loan_term"),
    hh = NULL   # household() settings object (see section 13)
) {
  if (is_household(state)) hh <- state
  if (!is.null(hh)) .use_household(hh, environment(), sys.function(), match.call())
  # Fleet-aware: vehicle_class may be a scalar (x n_vehicles) or a vector
  # (one car per element, mixed classes allowed). Capex per drivetrain is
  # summed across the fleet via fleet_vehicle_capex(); for new EVs the
  # per-class BYD model is picked automatically.
  fleet <- make_fleet(vehicle_class, n_vehicles)

  # Label: single-class new-EV fleets keep the BYD model name; mixed-class
  # or used fleets fall back to "EV".
  ev_label <- "EV"
  if (length(vehicle_class) == 1 && new_used == "New") {
    byd <- byd_model_for_cost_type(vehicle_class_to_cost_type(vehicle_class))
    if (!is.na(byd)) ev_label <- byd
  }

  opts <- list(
    list(category = "Vehicles", appliance_type = "Petrol",   label = "Petrol car", capex = fleet_vehicle_capex("Petrol",   fleet, new_used), include_vehicles = TRUE),
    list(category = "Vehicles", appliance_type = "Diesel",   label = "Diesel car", capex = fleet_vehicle_capex("Diesel",   fleet, new_used), include_vehicles = TRUE),
    list(category = "Vehicles", appliance_type = "Electric", label = ev_label,     capex = fleet_vehicle_capex("Electric", fleet, new_used), include_vehicles = TRUE)
  )
  .compare_build(opts, "Petrol car", solar_scenario,
                  .household_from_env(environment()))
}

# ---------------------------------------------------------------------------
# Run any compare_* function across all three solar scenarios and bind
# results into a long-format tibble. Each row already carries solar_scenario
# (set by evaluate_option), so the returned tibble can be faceted or coloured
# by scenario directly from a plotting script.
# ---------------------------------------------------------------------------

compare_across_solar <- function(compare_fn, ...,
                                  scenarios = SOLAR_SCENARIOS) {
  bind_rows(lapply(scenarios, function(scen) compare_fn(..., solar_scenario = scen)))
}

# ---------------------------------------------------------------------------
# Annual savings from the 15-year analysis
# ---------------------------------------------------------------------------
# Annualises each option's 15-year total cost (capex + 15 yrs of opex, or
# loan repayments + opex if `use_loan = TRUE` was used upstream) and computes
# savings vs a fossil baseline within each (category x solar_scenario) group.
# Positive `annual_savings` means cheaper than the baseline.

DEFAULT_SAVINGS_BASELINES <- tribble(
  ~category,        ~baseline_option,
  "Space Heating",  "Gas heater",
  "Water Heating",  "Gas instant",
  "Cooktop",        "Gas cooktop",
  "Vehicles",       "Petrol car"
)

compute_annual_savings <- function(df_15yr, baselines = DEFAULT_SAVINGS_BASELINES) {
  if (!all(c("period", "category", "option", "total_cost") %in% names(df_15yr))) {
    stop("compute_annual_savings expects a tibble from a compare_* / compare_across_solar call")
  }
  if (!all(df_15yr$period == "15year")) {
    stop("compute_annual_savings requires period = '15year' rows only")
  }

  # system_context too: the solar scenarios carry a gas baseline row in both
  # the "solar" and "solar_battery" contexts, so joining on category x
  # solar_scenario alone matched every row to two baselines and doubled it.
  group_keys <- intersect(c("category", "solar_scenario", "system_context"),
                          names(df_15yr))

  df_annual <- df_15yr |> mutate(annualised_cost = total_cost / 15)

  baseline_costs <- df_annual |>
    inner_join(baselines, by = c("category", "option" = "baseline_option")) |>
    select(all_of(group_keys), baseline_annualised_cost = annualised_cost)

  df_annual |>
    left_join(baseline_costs, by = group_keys) |>
    mutate(annual_savings = baseline_annualised_cost - annualised_cost,
           annualised_cost = annualised_cost,
           baseline_annualised_cost = baseline_annualised_cost)
}

# =============================================================================
# 11. Solar + battery value
# =============================================================================

# --- Evening peak export pricing -------------------------------------------
# Headroom kWh are exported through the household inverter during the 4-hour
# evening peak. The inverter capacity sets the maximum kWh exportable per
# hour, which fills the four hourly "buckets" in price order: the first
# inverter_kw of kWh earn peak_hour_1_kwh, the next earn peak_hour_2_kwh, etc.
# Anything beyond 4 × inverter_kw kWh/day still has to leave through the
# inverter at peak_hour_4, so the spill is priced there.
#
# Inverter size scales with PV system: small/mid-size residential (≤ ~6.6 kWp)
# typically uses a 5 kW inverter; ≥ 10 kWp systems use a 10 kW inverter and
# can therefore push 10 kWh into each peak hour. evaluate_solar_battery()
# auto-derives this from solar_kwp at the LARGE_SYSTEM_SOLAR_KWP threshold,
# but it can be overridden via the inverter_kw parameter.
INVERTER_KW              <- dflt("inverter_kw")               # small/mid-system inverter (kW)
LARGE_SYSTEM_INVERTER_KW <- dflt("large_system_inverter_kw")  # inverter used when solar_kwp ≥ threshold
LARGE_SYSTEM_SOLAR_KWP   <- dflt("large_system_solar_kwp")    # threshold for the large inverter

# Small PV systems (≤ 5 kWp, e.g. apartments) use microinverters instead of a
# single string inverter: capex scales with array size rather than being a
# fixed per-state unit price, and only half the units are assumed to fail and
# need replacing by the mid-life replacement year.
MICROINVERTER_MAX_SOLAR_KWP     <- dflt("microinverter_max_solar_kwp")     # systems at or below this kWp use microinverters
MICROINVERTER_COST_PER_KW       <- dflt("microinverter_cost_per_kw")       # $ per kWp of array
MICROINVERTER_REPLACEMENT_SHARE <- dflt("microinverter_replacement_share") # share of units replaced by inverter_replacement_year
PEAK_TIER_KWH            <- INVERTER_KW   # tiered_headroom_value_per_day default

# Australian seasons → months. Names match the battery_model() season rows.
SEASON_MONTHS <- list(
  summer = c(12, 1, 2),
  autumn = c(3, 4, 5),
  winter = c(6, 7, 8),
  spring = c(9, 10, 11)
)

# Load per-state monthly evening peak prices (4pm-8pm) from Model input data/.
# Built by evening_peak_export_prices.R.
# Loaded once with the other inputs (section 1); kept as a function for callers
# outside this file.
load_monthly_peak_prices <- function() {
  evening_peak_prices_monthly
}

# Mean peak_hour_1..4 prices ($/kWh) by season for a given state. Falls back
# to the "National" rows when the state has no monthly data (AUS, NT, ACT).
get_seasonal_peak_prices <- function(monthly_prices, state) {
  rows <- monthly_prices |> filter(.data$state == !!state)
  if (nrow(rows) == 0) rows <- monthly_prices |> filter(.data$state == "National")

  bind_rows(lapply(names(SEASON_MONTHS), function(s) {
    r <- rows |> filter(.data$month %in% SEASON_MONTHS[[s]])
    tibble(
      season          = s,
      peak_hour_1_kwh = mean(r$peak_hour_1_kwh, na.rm = TRUE),
      peak_hour_2_kwh = mean(r$peak_hour_2_kwh, na.rm = TRUE),
      peak_hour_3_kwh = mean(r$peak_hour_3_kwh, na.rm = TRUE),
      peak_hour_4_kwh = mean(r$peak_hour_4_kwh, na.rm = TRUE)
    )
  }))
}

# Daily $-value of battery export headroom under the 5 kW / 4-hour window.
# `prices` is a one-row tibble / list with peak_hour_1_kwh ... peak_hour_4_kwh.
tiered_headroom_value_per_day <- function(daily_headroom_kwh, prices,
                                            tier_kwh = PEAK_TIER_KWH) {
  tier_prices <- c(prices$peak_hour_1_kwh, prices$peak_hour_2_kwh,
                    prices$peak_hour_3_kwh, prices$peak_hour_4_kwh)
  remaining <- daily_headroom_kwh
  value     <- 0
  for (i in seq_along(tier_prices)) {
    take      <- pmin(remaining, tier_kwh)
    value     <- value + take * tier_prices[i]
    remaining <- remaining - take
    if (remaining <= 0) break
  }
  # Spill above 20 kWh/day still leaves through the inverter at hour 4.
  if (remaining > 0) value <- value + remaining * tier_prices[length(tier_prices)]
  value
}

# Evaluate the financial value of solar + battery:
#   - plain daytime export at the feed-in tariff (FiT)
#   - battery discharge to house load and to the EV, credited at the rates
#     evaluate_option() billed those kWh at
#   - surplus stored solar exported in the evening: at tiered 4-8pm wholesale
#     prices under "amber" (FiT as a floor), at the FiT otherwise
#
# Calls battery_model() (sourced from battery_model.R) and annualises the
# seasonal results (each of 4 seasons = 365/4 days).
# If daily_consumption_kwh is omitted, it is derived from the all-electric
# household model for the given state and occupancy.
#
# Solar system sizing (kWp → kWh & $):
#   The primary system input is `solar_kwp` (nameplate kW). Daily generation,
#   PV capex and inverter cost are all derived per state from
#   solar_lcoe_by_state.csv via get_solar_system_info():
#     daily_solar_kwh           = capacity_factor × 24 × solar_kwp
#     solar_capex               = cost_per_kw × solar_kwp   (panels only)
#     inverter_replacement_cost = state-specific inverter_cost from the CSV
#                                 (one unit price — used for both the initial
#                                  install and the mid-life replacement)
#   Any of these can be supplied directly to override (pass a non-NULL value).
#   Set solar_kwp = 0 to model "no PV" (generation and panel capex go to 0;
#   the inverter still applies if a battery is present — see below).
#
# Inverter accounting:
#   One inverter is shared between the PV array and the home battery, so at
#   most one initial unit and one mid-life replacement are counted, gated on
#   (solar_kwp > 0 OR battery_size_kwh > 0). A battery-only setup therefore
#   still incurs the inverter cost; a setup with neither incurs none.
#   Arrays of 0 < solar_kwp ≤ MICROINVERTER_MAX_SOLAR_KWP (5) use
#   microinverters instead: initial cost = MICROINVERTER_COST_PER_KW ($300)
#   × solar_kwp, replacement = MICROINVERTER_REPLACEMENT_SHARE (50%) of that.
#   Passing inverter_replacement_cost explicitly forces the single-inverter
#   costing at that unit price.
#
# Battery sizing (kWh → $):
#   battery_capex = battery_size_kwh × cost_per_kwh + installation
#   battery_subsidised defaults to TRUE when solar_kwp > 0 (Australian battery
#   subsidies typically require concurrent PV installation), FALSE otherwise.
#   Both the per-kWh price and the fixed installation come from product_prices
#   CSV via get_battery_cost_info() (AUS-level — no per-state breakdown).
#   The fixed `installation` charge is only added when solar_kwp == 0 (a
#   standalone battery retrofit). When solar_kwp > 0 the install crew is
#   already on site for the PV, so the marginal battery install cost is 0.
#   Pass battery_capex / battery_subsidised explicitly to override.
#
# Free battery capacity (V2G emulation):
#   `free_battery_kwh` adds storage capacity that is available for solar self-
#   consumption and evening-peak arbitrage at zero capital cost. Intended to
#   emulate vehicle-to-grid (V2G) where the EV's battery is already paid for
#   via the vehicle capex. The free kWh add to the battery_model's working
#   capacity (so arbitrage / self-consumption scale up) but do NOT enter
#   battery_capex, the battery subsidy lookup, or the inverter-presence gate.
evaluate_solar_battery <- function(
    solar_kwp = dflt("standalone_solar_kwp"),     # rooftop system size, kW. 0 disables.
    battery_size_kwh = dflt("standalone_battery_kwh"),
    free_battery_kwh = dflt("free_battery_kwh"),                   # additional storage capacity at zero
                                            # capex (V2G emulation — see header
                                            # note). Adds to battery_size_kwh
                                            # inside battery_model() only.
    state = dflt("state"),
    n_occupants = dflt("n_occupants"),
    n_vehicles = dflt("appliance_n_vehicles"), # household EV count (supports 0, 1.8, etc.),
                                            # spread evenly across vehicle_class
    vehicle_class = dflt("appliance_vehicle_class"),                  # scalar (× n_vehicles) or vector — one entry
                                            # per car for a mixed-class fleet, e.g.
                                            # c("SUV","Hatchback/small car"). See make_fleet().
    daily_consumption_kwh = NULL,           # if NULL, derived from all-electric model
    solar_scenario = dflt("standalone_solar_scenario"), # "grid_only", "solar", "solar_optimised"
    self_sufficiency_pct = NULL,            # if NULL, derived from solar_scenario
    battery_round_trip_efficiency = dflt("battery_round_trip_efficiency"),
    household_safeguard_pct = dflt("household_safeguard_pct"),
    useable_capacity_pct = dflt("battery_useable_capacity_pct"),
    annual_degradation_pct = dflt("battery_degradation_1yr"), # 1-year rate whatever `years` is; callers
                                            # pass default_degradation(n_years) for 15-year runs
    years = 1,
    fit_price_kwh = NULL,                   # per-kWh feed-in tariff for plain export.
                                            # If NULL, looked up per-state from
                                            # fuel_prices CSV ("solar_export" rows).
                                            # Pass an explicit value to override.
    # --- Solar PV system: all derived from solar_kwp + state unless overridden ---
    daily_solar_kwh           = NULL,       # if NULL: capacity_factor × 24 × solar_kwp
    solar_capex               = NULL,       # if NULL: cost_per_kw × solar_kwp
    # --- Battery cost: derived from battery_size_kwh unless overridden ---
    battery_capex             = NULL,       # if NULL: cost_per_kwh × battery_size_kwh
                                            # + installation (0 if battery_size_kwh = 0)
    battery_subsidised        = NULL,       # if NULL: TRUE when solar_kwp > 0
    inverter_replacement_cost = NULL,       # if NULL: per-state inverter_cost
    inverter_replacement_year = dflt("inverter_replacement_year"),         # Year the inverter is replaced. Set to 12
                                            # (not the 15 used by solar_lcoe_by_state.R)
                                            # so that within a 15-year household horizon
                                            # the replacement is always captured —
                                            # year_15 >= 12 includes it, whereas
                                            # year_15 >= 15 would only just qualify.
                                            # The LCOE script's 15-year value reflects
                                            # mid-life on a 30-year operation, a
                                            # different framing.
    inverter_kw               = NULL,       # Household inverter capacity (kW), sets the
                                            # per-hour cap on evening-peak exports. If NULL
                                            # (default), auto-derived from solar_kwp:
                                            # LARGE_SYSTEM_INVERTER_KW (10) when solar_kwp
                                            # >= LARGE_SYSTEM_SOLAR_KWP (10), else
                                            # INVERTER_KW (5). Pass an explicit value to
                                            # override for non-standard installs.
    battery_self_consumption_price = NULL,  # $/kWh price the battery's evening discharge
                                            # offsets on the household appliance bill.
                                            # NULL (default) → resolved from `tariff` via
                                            # get_tariff_spec()$import_dol_kwh, i.e. exactly
                                            # the rate evaluate_option() bills appliance grid
                                            # kWh at, so the credit nets against the charge.
                                            # Pass a value only to override deliberately.
    tariff        = dflt("tariff"),                  # The household's retail plan — one of TARIFFS.
                                            # Sets the appliance and EV rates the battery's
                                            # discharge is credited against, and whether
                                            # evening exports settle at wholesale (amber) or
                                            # at the flat feed-in tariff (everything else).
                                            # Must match the tariff passed to
                                            # evaluate_option so credits and charges cancel.
    ev_tariff_share = dflt("ev_tariff_share"),                    # Must match evaluate_option's ev_tariff_share.
                                            # Below 1 the EV's home charging is billed in two
                                            # blocks (the tariff's EV rate up to the cap, then
                                            # household rate); the battery credit is valued
                                            # against the expensive block first — see the
                                            # battery -> EV valuation below.
    km_tier       = dflt("km_tier"),               # "low" / "middle" / "high" — vehicle km/day tier
    dwelling_type = dflt("dwelling_type"),                # "house" or "apartment". Flows into
                                            # compute_household_loads() so the appliance
                                            # kWh fed into battery_model match the values
                                            # evaluate_option() bills at retail.
    # --- State subsidy eligibilities (Appliance_subsidies.csv) ---
    is_landlord  = FALSE,                   # QLD landlord solar subsidy eligibility
    battery_vpp  = FALSE,                   # NSW / WA battery-via-VPP eligibility
    postcode     = NA_integer_,             # WA battery subsidy: postcode resolves
                                            # to Synergy / Horizon via wa_postcode_network.
    solar_subsidy   = NULL,                 # Override $ value of solar PV subsidy. NULL =
                                            # look up per state from Appliance_subsidies.csv.
    battery_subsidy = NULL,                 # Override $ value of battery subsidy (total,
                                            # already including ×kWh). NULL = look up.
    # --- Scenario electrification flags ---
    # Per-category appliance types describing what's actually electric in the
    # current scenario. Forwarded to compute_household_loads(): non-electric
    # types (Natural gas, LPG, Petrol, etc.) contribute zero electric load,
    # which is what governs self-consumption, battery self-consumption credit,
    # and FiT-eligible export volumes. Defaults are all-electric.
    heating_type  = "Electric heat pump",
    hotwater_type = "Electric heat pump",
    cooking_type  = "Electric induction",
    vehicle_type  = "Electric",
    hh = NULL   # household() settings object (see section 13)
) {
  if (is_household(state)) hh <- state
  if (!is.null(hh)) .use_household(hh, environment(), sys.function(), match.call())

  # Derive system sizing from kWp + state (any caller-supplied values win).
  sys_info <- get_solar_system_info(state)
  if (is.null(daily_solar_kwh)) {
    daily_solar_kwh <- sys_info$capacity_factor * 24 * solar_kwp
  }
  if (is.null(solar_capex)) {
    solar_capex <- sys_info$cost_per_kw * solar_kwp
  }
  # Inverter type: microinverters for small arrays (0 < solar_kwp ≤ 5), else a
  # single string inverter at the per-state unit price. An explicit
  # inverter_replacement_cost forces the single-inverter costing.
  use_microinverters <- is.null(inverter_replacement_cost) &&
                          solar_kwp > 0 && solar_kwp <= MICROINVERTER_MAX_SOLAR_KWP
  if (use_microinverters) {
    inverter_unit_cost        <- MICROINVERTER_COST_PER_KW * solar_kwp
    inverter_replacement_cost <- inverter_unit_cost * MICROINVERTER_REPLACEMENT_SHARE
  } else {
    if (is.null(inverter_replacement_cost)) {
      inverter_replacement_cost <- sys_info$inverter_cost
    }
    inverter_unit_cost <- inverter_replacement_cost
  }

  # Inverter capacity: 10 kW for large (≥ 10 kWp) systems, otherwise 5 kW.
  # Drives the per-hour cap on evening-peak exports in
  # tiered_headroom_value_per_day().
  if (is.null(inverter_kw)) {
    inverter_kw <- if (solar_kwp >= LARGE_SYSTEM_SOLAR_KWP) LARGE_SYSTEM_INVERTER_KW
                   else                                      INVERTER_KW
  }

  # Battery capex: subsidy default tied to whether new PV is being installed.
  if (is.null(battery_subsidised)) {
    battery_subsidised <- solar_kwp > 0
  }
  if (is.null(battery_capex)) {
    if (battery_size_kwh <= 0) {
      battery_capex <- 0
    } else {
      binfo <- get_battery_cost_info()
      cost_per_kwh <- if (battery_subsidised) binfo$subsidised_per_kwh
                      else                    binfo$unsubsidised_per_kwh
      # Installation labour is only charged for standalone battery retrofits.
      # When PV is being installed at the same time (solar_kwp > 0) the crew
      # is already on site, so the marginal battery install cost is 0.
      installation_cost <- if (solar_kwp > 0) 0 else binfo$installation
      battery_capex <- cost_per_kwh * battery_size_kwh + installation_cost
    }
  }

  # --- State subsidies (Appliance_subsidies.csv) ---
  # Flat $ discount on solar PV capex when state + eligibility match (e.g.
  # VIC apartment solar = $2,400). Battery subsidy is $/kWh × battery size
  # (e.g. NSW VPP battery = $50/kWh, WA Horizon = $380/kWh). Both default
  # to looking the rates up; pass solar_subsidy / battery_subsidy explicitly
  # to override.
  if (is.null(solar_subsidy)) {
    solar_subsidy <- if (solar_kwp > 0) {
      get_appliance_subsidy("Solar", state,
                              dwelling_type = dwelling_type,
                              is_landlord   = is_landlord)
    } else 0
  }
  if (is.null(battery_subsidy)) {
    battery_subsidy <- if (battery_size_kwh > 0) {
      get_appliance_subsidy("Battery", state,
                              dwelling_type    = dwelling_type,
                              battery_vpp      = battery_vpp,
                              postcode         = postcode,
                              battery_size_kwh = battery_size_kwh)
    } else 0
  }
  solar_capex   <- max(0, solar_capex   - solar_subsidy)
  battery_capex <- max(0, battery_capex - battery_subsidy)

  # Get the household solar/load breakdown for this state/occupancy/dwelling/
  # n_vehicles. Drives both the self-sufficiency default and the appliance/EV
  # split passed to battery_model (so the home battery is reserved for
  # appliance load only; EV non-solar charging is priced at the tariff's EV
  # rate by evaluate_option per the household's tariff setting).
  # Pass daily_solar_kwh as the cap so self-consumption is bounded by actual
  # generation; that way the kWh attributed to self-consumption equal the
  # kWh that reduce exportable surplus inside battery_model.
  hh_eff <- .household_from_env(environment(),
                                list(period = if (years >= 15) "15year" else "1year"))
  household_loads <- compute_household_loads(
    hh            = hh_eff,
    heating_type  = heating_type,
    hotwater_type = hotwater_type,
    cooking_type  = cooking_type,
    vehicle_type  = vehicle_type
  )
  # Free window first, then solar on what's left (see get_household_free_window).
  spec_early <- get_tariff_spec(tariff, state,
                                if (years >= 15) "15year" else "1year")
  free_window <- get_household_free_window(
    appliance_loads  = household_loads$appliance_loads,
    vehicle_load_kwh = household_loads$vehicle_load_kwh,
    solar_scenario   = solar_scenario,
    spec             = spec_early
  )
  min_retail <- get_household_min_retail(
    appliance_loads  = household_loads$appliance_loads,
    vehicle_load_kwh = household_loads$vehicle_load_kwh
  )
  breakdown <- get_household_self_sufficiency(
    appliance_loads     = household_loads$appliance_loads,
    vehicle_load_kwh    = household_loads$vehicle_load_kwh,
    solar_scenario      = solar_scenario,
    available_solar_kwh = daily_solar_kwh,
    free_by_load        = free_window$free_by_load,
    vehicle_free_kwh    = free_window$vehicle_free_kwh,
    min_retail_by_load  = min_retail$min_retail_by_load,
    vehicle_min_retail_kwh = min_retail$vehicle_min_retail_kwh
  )

  if (is.null(self_sufficiency_pct)) {
    self_sufficiency_pct <- breakdown$total_pct
  }

  # Default FiT from per-state "solar_export" fuel rows if not supplied.
  # FiT is held flat over time so the period selection is informational only.
  if (is.null(fit_price_kwh)) {
    fit_price_kwh <- get_fit(state, if (years >= 15) "15year" else "1year")
  }

  # Derive daily consumption from the breakdown if not supplied. If the caller
  # passes a custom scalar we don't know the appliance/EV split, so fall back
  # to "all load is battery-eligible" by leaving the battery_eligible inputs
  # to default inside battery_model.
  # Free-window energy (Solar Sharer) is already supplied at $0, so it has to
  # come out of the load the battery is credited with covering — otherwise the
  # battery is paid for displacing kWh that were never charged for, and the
  # credit exceeds what evaluate_option() billed. Computed above, before the
  # solar breakdown, because the free window dispatches first.
  if (is.null(daily_consumption_kwh)) {
    daily_consumption_kwh   <- breakdown$appliance_load_kwh + breakdown$vehicle_load_kwh
    # Net of BOTH the free window (already supplied at $0) and the min_retail
    # floor (must be bought from the grid however much storage exists). What's
    # left is the only load the battery can be credited with covering.
    battery_eligible_load   <- max(breakdown$appliance_load_kwh -
                                     free_window$appliance_free_kwh -
                                     min_retail$appliance_min_retail_kwh, 0)
    battery_eligible_solar  <- breakdown$appliance_solar_kwh
  } else {
    battery_eligible_load   <- NULL
    battery_eligible_solar  <- NULL
  }

  # Run battery model — returns 4 rows, one per season.
  # battery_size_kwh + free_battery_kwh is the effective storage capacity
  # available to the model. The free portion (V2G) has no capital cost and
  # behaves identically to the paid battery for arbitrage / self-consumption.
  effective_battery_kwh <- battery_size_kwh + free_battery_kwh
  # EV home-charging load the daytime solar didn't cover — the cap on how much
  # surplus stored solar can be diverted to the car.
  # Zero for fossil-vehicle scenarios and when the caller supplied its own
  # daily_consumption_kwh (we don't know the appliance/EV split in that case).
  # Net of free-window charging and the min_retail floor — the same limits the
  # appliance side applies.
  ev_unmet_load_kwh <- if (is.null(battery_eligible_load)) 0 else {
    max(breakdown$vehicle_load_kwh - breakdown$vehicle_solar_kwh -
          free_window$vehicle_free_kwh - min_retail$vehicle_min_retail_kwh, 0)
  }

  # --- Surplus dispatch order, decided by price not by a flag ---------------
  # Stored solar the house can't use has two possible homes: the car, or the
  # evening export window. Send it wherever it's worth more. Charging the EV
  # avoids an import at the tariff's EV rate (24-33c/kWh); exporting earns
  # either tiered wholesale (amber) or the FiT (2-9c/kWh), so in practice the EV
  # wins — but deriving it from prices means the model can't be told to send
  # energy to the lower-value use.
  spec <- spec_early
  seasonal_peak_prices <- get_seasonal_peak_prices(load_monthly_peak_prices(), state)
  # Representative export price for the ordering decision: the best evening
  # tier under amber, otherwise the flat FiT.
  evening_marginal_price <- if (spec$export_evening == "wholesale_peak") {
    max(seasonal_peak_prices$peak_hour_1_kwh, na.rm = TRUE)
  } else {
    fit_price_kwh
  }
  surplus_ev_first <- ev_unmet_load_kwh > 0 &&
                      spec$ev_dol_kwh >= evening_marginal_price

  battery_seasonal <- battery_model(
    daily_solar_generation_kwh              = daily_solar_kwh,
    daily_consumption_kwh                   = daily_consumption_kwh,
    self_sufficiency_pct                    = self_sufficiency_pct,
    battery_size_kwh                        = effective_battery_kwh,
    battery_round_trip_efficiency           = battery_round_trip_efficiency,
    household_safeguard_pct                 = household_safeguard_pct,
    useable_capacity_pct                    = useable_capacity_pct,
    annual_degradation_pct                  = annual_degradation_pct,
    years                                   = years,
    daily_battery_eligible_load_kwh         = battery_eligible_load,
    daily_battery_eligible_solar_target_kwh = battery_eligible_solar,
    surplus_ev_first                        = surplus_ev_first,
    daily_ev_unmet_load_kwh                 = ev_unmet_load_kwh
  )

  # Annualise: each season covers 365/4 days
  days_per_season <- 365 / 4
  annual_export_kwh   <- sum(battery_seasonal$export_kwh   * days_per_season)
  annual_headroom_kwh <- sum(battery_seasonal$arbitrage_headroom_kwh * days_per_season)

  # Evening export valuation. Under "amber" the household settles at wholesale:
  # per-season, tiered through the 4-hour evening peak window with an
  # inverter-sized cap per hour (5 kW for residential / ≤10 kWp systems, 10 kW
  # for ≥ 10 kWp), using seasonal mean prices from
  # evening_peak_prices_monthly.csv across the months in SEASON_MONTHS.
  # Every other tariff exports at the flat feed-in tariff instead.
  #
  # Even on amber the FiT is a floor: a household wouldn't accept a wholesale
  # tier below the FiT it could otherwise get, and the late peak hours do fall
  # under it (NSW hour 4 ≈ 2.1c vs a 5c FiT).
  seasonal_headroom <- battery_seasonal |>
    select(season, daily_headroom_kwh = arbitrage_headroom_kwh) |>
    left_join(seasonal_peak_prices, by = "season")
  if (spec$export_evening == "wholesale_peak") {
    floored_prices <- seasonal_headroom |>
      mutate(across(starts_with("peak_hour_"), ~ pmax(.x, fit_price_kwh)))
    seasonal_headroom$daily_value <- vapply(seq_len(nrow(floored_prices)), function(i) {
      tiered_headroom_value_per_day(floored_prices$daily_headroom_kwh[i],
                                      floored_prices[i, ],
                                      tier_kwh = inverter_kw)
    }, numeric(1))
  } else {
    seasonal_headroom$daily_value <- seasonal_headroom$daily_headroom_kwh * fit_price_kwh
  }
  seasonal_headroom <- seasonal_headroom |>
    mutate(seasonal_headroom_kwh   = daily_headroom_kwh * days_per_season,
           seasonal_headroom_value = daily_value * days_per_season)

  annual_export_value   <- annual_export_kwh * fit_price_kwh
  annual_headroom_value <- sum(seasonal_headroom$seasonal_headroom_value)

  # --- Battery self-consumption (free appliance kWh) ---
  # Stored solar discharged in the evening to serve appliance load — the first
  # claim on the usable stored pool (see battery_model()'s dispatch order).
  # These kWh are paid for once via solar_capex + battery_capex (capital) and
  # don't appear as grid imports in the appliance bill, so we value them at the
  # household's appliance grid price: the household sees the equivalent of a
  # free grid offset.
  annual_battery_self_consumption_kwh <-
    sum(battery_seasonal$battery_self_consumption_kwh * days_per_season)
  # The credit has to be struck at exactly the rate evaluate_option() charged
  # those same kWh at, or the two don't cancel and the difference silently
  # becomes household income. Both sides read the same tariff spec.
  if (is.null(battery_self_consumption_price)) {
    battery_self_consumption_price <- spec$import_dol_kwh
  }
  annual_battery_self_consumption_value <-
    annual_battery_self_consumption_kwh * battery_self_consumption_price

  # --- Battery → EV charging ---
  # Surplus stored solar diverted to the car rather than exported.
  # Valued at the rate the EV would otherwise have paid for those kWh, so the
  # credit exactly cancels the grid cost the Vehicles row still bills.
  #
  # When ev_tariff_share < 1, evaluate_option bills the EV's home charging in
  # two blocks: everything up to ev_tariff_share × total EV demand at the EV
  # rate, and the remainder at the household rate. A household would obviously
  # use battery discharge against the EXPENSIVE block first, so we deplete that
  # block before falling back to the EV-rate block. At ev_tariff_share = 1 the
  # expensive block is empty and this collapses to a flat EV-rate credit.
  annual_battery_ev_kwh <- sum(battery_seasonal$battery_ev_charge_kwh * days_per_season)

  ev_rate_price <- spec$ev_dol_kwh
  # The household rate the over-cap EV kWh are billed at — the tariff's import
  # rate, which is the same rule evaluate_option applies to ev_other_kwh.
  ev_over_cap_price <- spec$import_dol_kwh

  # Reconstruct evaluate_option's two-block split of the EV's home grid kWh.
  # breakdown$vehicle_load_kwh is already net of fast charging (built with the
  # global FAST_CHARGE_FRACTION in compute_household_loads), so gross back up
  # through the same constant to recover total EV demand — the base the cap
  # is a share of.
  ev_home_grid_kwh_day <- ev_unmet_load_kwh
  ev_total_demand_kwh_day <- if (FAST_CHARGE_FRACTION < 1) {
    breakdown$vehicle_load_kwh / (1 - FAST_CHARGE_FRACTION)
  } else 0
  ev_rate_cap_kwh_day  <- max(ev_tariff_share, 0) * ev_total_demand_kwh_day
  ev_over_cap_kwh_year <- max(ev_home_grid_kwh_day - ev_rate_cap_kwh_day, 0) * 365

  # Deplete the expensive block first, then the EV-rate block.
  ev_from_over_cap <- min(annual_battery_ev_kwh, ev_over_cap_kwh_year)
  ev_from_ev_rate  <- annual_battery_ev_kwh - ev_from_over_cap
  annual_battery_ev_value <- ev_from_over_cap * ev_over_cap_price +
                             ev_from_ev_rate  * ev_rate_price
  # Blended $/kWh the credit was struck at.
  avg_battery_ev_price <- if (annual_battery_ev_kwh > 0)
                            annual_battery_ev_value / annual_battery_ev_kwh
                          else NA_real_

  total_export_value              <- annual_export_value              * years
  total_headroom_value            <- annual_headroom_value            * years
  total_battery_self_consumption  <- annual_battery_self_consumption_value * years
  total_battery_ev_value          <- annual_battery_ev_value          * years

  # --- Solar PV system capex over the horizon ---
  # grid_only has no solar system — skip capex.
  # Otherwise: panel capex, plus the inverter (one initial unit shared between
  # solar and battery, plus one mid-life replacement if the horizon reaches
  # the replacement year). The inverter is gated on either solar OR battery
  # being present, so a battery-only setup still carries the inverter cost.
  has_solar_or_battery <- solar_kwp > 0 || battery_size_kwh > 0
  inverter_initial_cost <- if (has_solar_or_battery) inverter_unit_cost else 0
  inverter_replacement_now <- if (has_solar_or_battery && years >= inverter_replacement_year)
                                inverter_replacement_cost else 0
  if (solar_scenario == "grid_only") {
    total_solar_capex <- 0
  } else {
    total_solar_capex <- solar_capex + inverter_initial_cost + inverter_replacement_now
  }

  net_value <- total_export_value + total_headroom_value +
               total_battery_self_consumption + total_battery_ev_value -
               total_solar_capex - battery_capex

  list(
    summary = tibble(
      state                    = state,
      years                    = years,
      solar_scenario           = solar_scenario,
      self_sufficiency_pct     = self_sufficiency_pct,
      daily_solar_kwh          = daily_solar_kwh,
      daily_consumption_kwh    = daily_consumption_kwh,
      battery_size_kwh         = battery_size_kwh,
      free_battery_kwh         = free_battery_kwh,
      inverter_kw              = inverter_kw,
      inverter_type            = if (use_microinverters) "micro" else "string",
      tariff                           = spec$tariff,
      export_evening                     = spec$export_evening,
      surplus_ev_first                   = surplus_ev_first,
      annual_export_kwh                  = annual_export_kwh,
      annual_headroom_kwh                = annual_headroom_kwh,
      annual_battery_self_consumption_kwh = annual_battery_self_consumption_kwh,
      annual_battery_ev_kwh              = annual_battery_ev_kwh,
      fit_price_kwh                      = fit_price_kwh,
      avg_headroom_price_kwh             = if (annual_headroom_kwh > 0) annual_headroom_value / annual_headroom_kwh else NA_real_,
      annual_export_value                = annual_export_value,
      annual_headroom_value              = annual_headroom_value,
      annual_battery_self_consumption_value = annual_battery_self_consumption_value,
      annual_battery_ev_value            = annual_battery_ev_value,
      avg_battery_ev_price_kwh           = avg_battery_ev_price,
      total_annual_solar_value           = annual_export_value + annual_headroom_value +
                                                 annual_battery_self_consumption_value +
                                                 annual_battery_ev_value,
      solar_capex                        = solar_capex,
      solar_subsidy                      = solar_subsidy,
      inverter_initial                   = if (solar_scenario != "grid_only") inverter_initial_cost else 0,
      inverter_replacement               = if (solar_scenario != "grid_only") inverter_replacement_now else 0,
      total_solar_capex                  = total_solar_capex,
      battery_subsidised                 = battery_subsidised,
      battery_capex                      = battery_capex,
      battery_subsidy                    = battery_subsidy,
      net_value                          = net_value
    ),
    seasonal_headroom = seasonal_headroom,
    battery_seasonal  = battery_seasonal
  )
}

# =============================================================================
# 12. Whole-household summary across fossil + electric scenarios
# =============================================================================

# Evaluates the full household stack across six scenarios:
#   - "Gas"                       : gas heater + gas HW + gas cooktop + fossil car
#   - "Gas + solar"               : as Gas, plus rooftop solar + battery. Only
#                                    cooling and the "Other electric loads"
#                                    baseload benefit from the solar.
#   - "LPG"                       : LPG heater + LPG HW + LPG cooktop +
#                                    fossil car. The cooktop uses the natural-
#                                    gas energy figure (the CSV row is the
#                                    same) and the gas cooktop capex, but is
#                                    priced at LPG rates and contributes to
#                                    the LPG supply-charge pool. Override
#                                    `lpg_cooktop_type` to model an electric
#                                    cooktop (e.g., "Electric induction" for
#                                    a typical NT setup) if preferred.
#   - "Electric (grid_only)"      : heat pump + heat pump HW + induction + EV,
#                                    no on-site solar. Set
#                                    `electric_cooktop_type = "electric"` to
#                                    fit a resistive cooktop instead of
#                                    induction across all three Electric
#                                    scenarios.
#   - "Electric (solar)"          : same appliances, with rooftop solar
#   - "Electric (solar_optimised)": same appliances, with solar + load-shifting
#
# What it combines:
#   - evaluate_option()             — per appliance: capex, opex, kWh, tCO2e
#   - get_household_self_sufficiency() — drives the solar/load breakdown used
#                                       by evaluate_solar_battery (via that
#                                       function — not called directly here)
#   - evaluate_solar_battery()      — for the three solar scenarios only: PV
#                                     and battery capex, export income,
#                                     battery credits
#
# Solar accounting:
#   Self-consumed solar kWh are treated as free at the appliance level — they
#   simply avoid grid imports at the scenario's tariff import rate. The PV system's
#   capex is added once at the household level via solar_capex from
#   evaluate_solar_battery(). Exported kWh earn FiT through that function's
#   annual_export_value. There is no "forgone FiT" charge on self-consumed
#   kWh — the FiT flow only applies to kWh that are actually exported, so the
#   two flows are independent rather than compounded.
#
# Output: a list with three tibbles:
#   $summary       — one row per scenario (6 rows): capex split, opex,
#                    solar income, total cost over the horizon, kWh and tCO2e
#   $appliances    — per-appliance rows for every scenario (6 per scenario:
#                    heating, hot water, cooktop, vehicles, cooling, other), with
#                    the same columns evaluate_option() returns plus a
#                    `scenario` label
#   $solar_battery — evaluate_solar_battery() summary per scenario (zero
#                    for the three grid-only scenarios)

evaluate_household <- function(
    # --- Household ---
    state                = dflt("state"),
    n_occupants          = dflt("n_occupants"),
    dwelling_type        = dflt("dwelling_type"),
    n_vehicles           = dflt("household_n_vehicles"),
    vehicle_class        = dflt("household_vehicle_class"),
    new_used             = dflt("new_used"),
    period               = dflt("household_period"),      # "1year" or "15year"
    use_loan             = dflt("use_loan"),
    loan_rate            = dflt("loan_rate"),
    loan_term            = dflt("loan_term"),
    fast_charge_fraction = FAST_CHARGE_FRACTION,
    km_tier              = dflt("km_tier"),     # "low" / "middle" / "high" — vehicle km/day tier
    tariff               = dflt("tariff"),          # The household's retail electricity plan — one of
                                          # TARIFFS ("flat", "tou", "amber", "solar_sharer").
                                          # The single tariff switch: it sets appliance and EV
                                          # import rates, the daily supply charge, whether a
                                          # free midday window applies, and how evening
                                          # battery exports are valued. Threaded to every
                                          # evaluate_option() call and to
                                          # evaluate_solar_battery() so charges and credits
                                          # are always struck at the same rates.
    ev_tariff_share      = dflt("ev_tariff_share"),            # Share of total EV demand that may be billed at
                                          # the tariff's EV rate; the excess falls back to the
                                          # tariff's appliance import rate. 1 = uncapped
                                          # (default). See evaluate_option.
    # --- Solar / battery system (used in the two solar scenarios) ---
    # solar_kwp drives both generation and capex; daily_solar_kwh, solar_capex
    # and inverter_replacement_cost can be passed explicitly to override the
    # per-state derivation from solar_lcoe_by_state.csv.
    solar_kwp                     = NULL, # nameplate system size, kW. NULL: by dwelling
                                          # (10 house / 2 apartment) via default_system_size()
    battery_size_kwh              = NULL, # NULL: by dwelling (15 house / 5 apartment)
    free_battery_kwh              = dflt("free_battery_kwh"),     # extra storage at zero capex — emulates V2G
                                            # using the EV's battery (already costed in
                                            # via vehicle capex). Adds to working
                                            # capacity in battery_model only; no
                                            # battery capex or subsidy is charged on it.
    daily_solar_kwh               = NULL,  # default: capacity_factor × 24 × solar_kwp
    solar_capex                   = NULL,  # default: cost_per_kw × solar_kwp
    battery_capex                 = NULL,  # default: cost_per_kwh × battery_size_kwh + install
    battery_subsidised            = NULL,  # default: TRUE if solar_kwp > 0
    inverter_replacement_cost     = NULL,  # default: per-state inverter_cost
    inverter_replacement_year     = dflt("inverter_replacement_year"),    # within 15-yr horizon (see evaluate_solar_battery)
    inverter_kw                   = NULL,  # default: 10 kW if solar_kwp >= 10, else 5 kW
    battery_round_trip_efficiency = dflt("battery_round_trip_efficiency"),
                                          # Surplus stored solar (after house load) goes to
                                          # the EV or the evening export window, whichever is
                                          # worth more at this tariff's prices — derived in
                                          # evaluate_solar_battery(), not a caller setting.
                                          # Reported as annual_battery_ev_credit /
                                          # annual_solar_fit in $summary.
    household_safeguard_pct       = dflt("household_safeguard_pct"),
    useable_capacity_pct          = dflt("battery_useable_capacity_pct"),
    annual_degradation_pct        = NULL,  # default: 0.04 for 1yr, 0.022 for 15yr
    fit_price_kwh                 = NULL,  # default: per-state from fuel_prices
    # --- Appliance capex ---
    # Defaults are pulled from product_prices (Energy Savings 2026 - input
    # data - FINAL product prices.csv) so they stay in sync with the sheet.
    # Pass explicit values to override per call.
    # NB: the CSV has a single "NGas Instant or LPG Instant" row for instant
    # water heaters, so gas_hotwater_capex and lpg_hotwater_capex share it.
    gas_heater_capex             = get_product_price("Gas Heaters ~7kW"),
    lpg_heater_capex             = get_product_price("LPG Heaters"),
    heatpump_heater_capex        = get_product_price("Reverse cycle AC ~7kW"),
    gas_hotwater_capex           = get_product_price("Water Heater NGas Instant or LPG Instant"),
    lpg_hotwater_capex           = get_product_price("Water Heater NGas Instant or LPG Instant"),
    heatpump_hotwater_capex      = get_product_price("Water Heater Heat Pumps"),
    gas_cooktop_capex            = get_product_price("Gas Cooktops"),
    induction_cooktop_capex      = get_product_price("Induction Cooktops"),
    resistive_cooktop_capex      = get_product_price("Electric Cooktops"),
    # Standalone split AC for fossil-heated scenarios (Gas / LPG). Electric
    # heat pump scenarios already bundle cooling into the heating row.
    cooling_only_capex           = dflt("cooling_only_capex"),
    # One-off switchboard upgrade added to every all-electric scenario. Set to 0 to disable.
    switchboard_upgrade_capex    = dflt("switchboard_upgrade_capex"),
    # --- Scenario knobs ---
    fossil_vehicle_type          = dflt("fossil_vehicle_type"),            # "Petrol" or "Diesel"
    lpg_cooktop_type             = dflt("lpg_cooktop_type"),               # see header note
    electric_cooktop_type        = dflt("electric_cooktop_type"),         # cooktop fitted in the three Electric
                                                        # scenarios: "induction" (default) or
                                                        # "electric" (resistive element).
                                                        # Full names ("Electric induction" /
                                                        # "Electric resistance") also accepted.
    # --- State appliance subsidy eligibilities (Appliance_subsidies.csv) ---
    # dwelling_type already carries the Apartment tag for solar. The remaining
    # eligibility tags are off by default so a default AUS / house run gets the
    # baseline "All" subsidies only.
    is_landlord                  = FALSE,
    battery_vpp                  = FALSE,
    postcode                     = NA_integer_,
    hh = NULL   # household() settings object (see section 13)
) {
  if (is_household(state)) hh <- state
  if (!is.null(hh)) .use_household(hh, environment(), sys.function(), match.call())

  sys_size         <- default_system_size(dwelling_type, solar_kwp, battery_size_kwh)
  solar_kwp        <- sys_size$solar_kwp
  battery_size_kwh <- sys_size$battery_size_kwh
  n_years <- if (period == "1year") 1 else 15
  if (is.null(annual_degradation_pct)) {
    annual_degradation_pct <- default_degradation(n_years)
  }
  n_vehicles <- resolve_n_vehicles(n_vehicles, state)
  # This run's settings, passed down to every evaluate_option(),
  # compute_household_loads() and evaluate_solar_battery() call below.
  hh_eff <- .household_from_env(environment())

  # Vehicle capex — fleet-aware. vehicle_class may be a scalar (× n_vehicles)
  # or a vector (one car per element, mixed classes allowed); the per-class
  # BYD model is picked automatically inside fleet_vehicle_capex() for new
  # EVs. Single-class new-EV fleets keep the BYD model name as the label;
  # mixed or used fleets fall back to "EV".
  vehicle_fleet <- make_fleet(vehicle_class, n_vehicles)
  ev_label <- "EV"
  if (length(vehicle_class) == 1 && new_used == "New") {
    byd <- byd_model_for_cost_type(vehicle_class_to_cost_type(vehicle_class))
    if (!is.na(byd)) ev_label <- byd
  }
  fossil_capex <- fleet_vehicle_capex(fossil_vehicle_type, vehicle_fleet, new_used)
  ev_capex     <- fleet_vehicle_capex("Electric",          vehicle_fleet, new_used)

  # LPG-scenario cooktop: gas and LPG cooktops are the same hardware (gas
  # cooktop capex), electric cooktops use the induction capex.
  lpg_cooktop_capex <- if (lpg_cooktop_type %in% c("Natural gas", "LPG")) gas_cooktop_capex else induction_cooktop_capex

  # Electric-scenario cooktop. Induction and resistive are both electricity but
  # differ on both sides of the ledger: induction uses less energy (AUS 0.85 vs
  # 0.94 kWh/day in average_energy_use_by_appliance_and_state.csv) and costs
  # more to buy. Only the three Electric scenarios are affected — the Gas and
  # LPG baselines keep their own cooktops.
  elec_cooktop <- switch(
    tolower(electric_cooktop_type),
    "induction"           = ,
    "electric induction"  = list(type  = "Electric induction",
                                 capex = induction_cooktop_capex,
                                 label = "Induction cooktop"),
    "electric"            = ,
    "resistive"           = ,
    "resistance"          = ,
    "electric resistance" = list(type  = "Electric resistance",
                                 capex = resistive_cooktop_capex,
                                 label = "Electric cooktop"),
    stop("electric_cooktop_type must be \"induction\" or \"electric\" (got: ",
         electric_cooktop_type, ")")
  )

  # --- Bottom-up solar self-consumption cap ----------------------------------
  # Per-appliance solar fractions in SOLAR_FRACTION_TABLE describe daytime
  # demand for solar (what fraction of each load wants to be served from the
  # roof). Actual self-consumption can't exceed what the PV system actually
  # generates that day, so we compute a household-level scaling factor per
  # scenario (loads depend on which appliances are electric in that scenario)
  # and pass it through to every evaluate_option() call. Scenarios with no PV
  # get solar_frac = 0, so the factor has no effect there.
  sys_info <- get_solar_system_info(state)
  if (is.null(daily_solar_kwh)) {
    daily_solar_kwh_eff <- sys_info$capacity_factor * 24 * solar_kwp
  } else {
    daily_solar_kwh_eff <- daily_solar_kwh
  }

  # Scenario specs — each entry names the appliance type, capex and unit count
  # for the four categories, plus the household-level solar scenario.
  # Heating unit count is per-state (Heater_numbers_by_state.csv) — scales
  # heating capex only. Cooling, water heating, cooktop and vehicles stay at
  # 1 unit (cooling-only AC for fossil scenarios is handled separately below).
  n_heaters <- get_heater_count(state)
  scenarios <- list(
    list(name = "Gas",                        solar = "grid_only",
         heating  = list(type = "Natural gas",         capex = gas_heater_capex,       units = n_heaters, label = "Gas heater"),
         hotwater = list(type = "Natural gas",         capex = gas_hotwater_capex,     units = 1, label = "Gas instant"),
         cooktop  = list(type = "Natural gas",         capex = gas_cooktop_capex,      units = 1, label = "Gas cooktop"),
         vehicle  = list(type = fossil_vehicle_type,   capex = fossil_capex,           units = 1, label = paste(fossil_vehicle_type, "car"))),
    # Gas + petrol home that has added a rooftop solar + battery system but
    # kept its fossil appliances and fossil vehicle. Only the cooling AC and
    # the "Other electric loads" baseload benefit from solar self-consumption;
    # all gas/petrol energy is unchanged from the "Gas" scenario above.
    list(name = "Gas + solar",                solar = "solar",
         heating  = list(type = "Natural gas",         capex = gas_heater_capex,       units = n_heaters, label = "Gas heater"),
         hotwater = list(type = "Natural gas",         capex = gas_hotwater_capex,     units = 1, label = "Gas instant"),
         cooktop  = list(type = "Natural gas",         capex = gas_cooktop_capex,      units = 1, label = "Gas cooktop"),
         vehicle  = list(type = fossil_vehicle_type,   capex = fossil_capex,           units = 1, label = paste(fossil_vehicle_type, "car"))),
    list(name = "LPG",                        solar = "grid_only",
         heating  = list(type = "LPG",                 capex = lpg_heater_capex,       units = n_heaters, label = "LPG heater"),
         hotwater = list(type = "LPG",                 capex = lpg_hotwater_capex,     units = 1, label = "LPG instant"),
         cooktop  = list(type = lpg_cooktop_type,      capex = lpg_cooktop_capex,      units = 1, label = paste(lpg_cooktop_type, "cooktop")),
         vehicle  = list(type = fossil_vehicle_type,   capex = fossil_capex,           units = 1, label = paste(fossil_vehicle_type, "car"))),
    list(name = "Electric (grid_only)",       solar = "grid_only",
         heating  = list(type = "Electric heat pump",  capex = heatpump_heater_capex,   units = n_heaters, label = "Heat pump (AC)"),
         hotwater = list(type = "Electric heat pump",  capex = heatpump_hotwater_capex, units = 1, label = "Heat pump HW"),
         cooktop  = list(type = elec_cooktop$type,     capex = elec_cooktop$capex,     units = 1, label = elec_cooktop$label),
         vehicle  = list(type = "Electric",            capex = ev_capex,                units = 1, label = ev_label)),
    list(name = "Electric (solar)",           solar = "solar",
         heating  = list(type = "Electric heat pump",  capex = heatpump_heater_capex,   units = n_heaters, label = "Heat pump (AC)"),
         hotwater = list(type = "Electric heat pump",  capex = heatpump_hotwater_capex, units = 1, label = "Heat pump HW"),
         cooktop  = list(type = elec_cooktop$type,     capex = elec_cooktop$capex,     units = 1, label = elec_cooktop$label),
         vehicle  = list(type = "Electric",            capex = ev_capex,                units = 1, label = ev_label)),
    list(name = "Electric (solar_optimised)", solar = "solar_optimised",
         heating  = list(type = "Electric heat pump",  capex = heatpump_heater_capex,   units = n_heaters, label = "Heat pump (AC)"),
         hotwater = list(type = "Electric heat pump",  capex = heatpump_hotwater_capex, units = 1, label = "Heat pump HW"),
         cooktop  = list(type = elec_cooktop$type,     capex = elec_cooktop$capex,     units = 1, label = elec_cooktop$label),
         vehicle  = list(type = "Electric",            capex = ev_capex,                units = 1, label = ev_label))
  )

  appliance_tibbles      <- list()
  solar_battery_tibbles  <- list()
  summary_tibbles        <- list()

  # Resolve the tariff once per scenario. Every appliance row and the
  # solar/battery side of a scenario read from that one spec, so imports,
  # credits and the supply charge can't drift apart. Named tariff_spec to avoid
  # shadowing eval_appliance's `spec` (the appliance spec).
  #
  # Fossil-appliance scenarios are held on FOSSIL_BASELINE_TARIFF rather than
  # the household's own plan — a gas home's residual load isn't shiftable, so
  # it wouldn't rationally be on TOU. See FOSSIL_BASELINE_TARIFF.
  scenario_is_fossil <- function(scen) {
    any(c(scen$heating$type, scen$hotwater$type, scen$cooktop$type) %in%
          c("Natural gas", "LPG"))
  }
  # The grid-only electric scenario is likewise held on GRID_ONLY_TARIFF — with
  # no battery it can't shift its whole load off peak. See GRID_ONLY_TARIFF.
  scenario_tariff <- function(scen) {
    if (scenario_is_fossil(scen) && !is.na(FOSSIL_BASELINE_TARIFF))
      FOSSIL_BASELINE_TARIFF
    else if (scen$solar == "grid_only" && !is.na(GRID_ONLY_TARIFF))
      GRID_ONLY_TARIFF
    else tariff
  }

  # Household-level spec (the electrified household's own plan). Used for
  # anything decided before the scenario loop; inside the loop every read goes
  # through the scenario's own spec.
  tariff_spec <- get_tariff_spec(tariff, state, period)

  # Helper to evaluate one appliance row with the scenario's solar setting.
  # solar_scale is the bottom-up cap derived from get_household_self_sufficiency
  # for this scenario — it scales per-appliance solar self-consumption when PV
  # generation is insufficient to meet aggregate household demand.
  # free_scale is the equivalent cap on free-window self-supply, from
  # allocate_free_window() — only bites under Solar Sharer.
  eval_appliance <- function(category, spec, solar_scenario, solar_scale,
                             free_scale = 1,
                             include_vehicles = FALSE,
                             extra_args = list(),
                             tariff_name = tariff) {
    args <- c(
      list(category = category, appliance_type = spec$type, label = spec$label,
           capex = spec$capex, n_units = spec$units,
           hh = hh_eff, solar_scenario = solar_scenario,
           include_vehicles = include_vehicles,
           solar_scale_factor = solar_scale,
           tariff = tariff_name, free_window_scale = free_scale,
           # Solar PV capex is added once at the household level via
           # evaluate_solar_battery; turn off the per-kWh solar marginal cost here
           # to avoid double-counting the panels.
           solar_marginal_cost = FALSE,
           # Supply charges are computed once at the household level (one full
           # daily_charge × 365 per connected fuel) — not allocated per
           # appliance — to avoid under-counting gas-home elec supply or
           # over-counting electric-home supply when an EV is present.
           include_supply_charge = FALSE),
      extra_args
    )
    do.call(evaluate_option, args)
  }

  # --- "Other electric loads" baseload (precomputed, scenario-invariant pieces) ---
  # Always-electric appliances present in every household configuration
  # (lighting, fridge, electronics, washing, dishwasher, microwave, etc.).
  # Energy is identical across Gas/LPG/Electric scenarios; only the solar
  # coverage varies with solar_scenario.
  occ_scale <- get_scaling_factor(n_occupants)
  dwelling_scale <- if (dwelling_type == "apartment") APARTMENT_ENERGY_FACTOR else 1
  baseload_other_cooking_kwh <- get_category_total_kwh_day("Other Cooking",     state) *
                                  occ_scale * dwelling_scale
  baseload_other_elec_kwh    <- get_category_total_kwh_day("Other Electronics", state) *
                                  occ_scale * dwelling_scale
  baseload_energy_kwh_day <- baseload_other_cooking_kwh + baseload_other_elec_kwh

  # baseload_grid_price is resolved per scenario inside the loop — the fossil
  # scenarios are billed at FOSSIL_BASELINE_TARIFF, not the household's plan.

  baseload_ef <- get_grid_ef(state)

  for (scen in scenarios) {
    # The plan this scenario is billed on. Fossil baselines stay on
    # FOSSIL_BASELINE_TARIFF; the electrified scenarios use the household's own.
    scen_tariff <- scenario_tariff(scen)
    # Reuse the household spec when the scenario is on the household's own
    # plan, so the Solar Sharer fallback warning fires once, not once per row.
    scen_spec   <- if (identical(scen_tariff, tariff)) tariff_spec
                   else get_tariff_spec(scen_tariff, state, period)
    baseload_grid_price <- scen_spec$import_dol_kwh

    # Per-scenario electric load profile: fossil categories (gas heating, gas
    # HW, gas/LPG cooktop, petrol/diesel vehicle) contribute zero electric load
    # and so don't enlarge the solar denominator or absorb battery discharge.
    scen_loads <- compute_household_loads(
      hh            = hh_eff,
      heating_type  = scen$heating$type,
      hotwater_type = scen$hotwater$type,
      cooking_type  = scen$cooktop$type,
      vehicle_type  = scen$vehicle$type
    )
    # --- Free-window budget (Solar Sharer only) ---------------------------
    # Every load competes for one household-wide daily cap, so the allocation
    # has to be made here rather than inside evaluate_option — an appliance
    # can't know what the others have already claimed. Shared with
    # evaluate_solar_battery() via get_household_free_window() so both sides
    # agree on how much load the free window already served.
    #
    # Computed BEFORE the solar breakdown: the free window dispatches first, so
    # solar only claims load the free window hasn't already covered.
    scen_free <- get_household_free_window(
      appliance_loads  = scen_loads$appliance_loads,
      vehicle_load_kwh = scen_loads$vehicle_load_kwh,
      solar_scenario   = scen$solar,
      spec             = scen_spec
    )
    scen_free_scale <- if (scen_spec$free_window) scen_free$free_scale_factor else 1

    scen_min_retail <- get_household_min_retail(
      appliance_loads  = scen_loads$appliance_loads,
      vehicle_load_kwh = scen_loads$vehicle_load_kwh
    )
    scen_breakdown <- get_household_self_sufficiency(
      appliance_loads     = scen_loads$appliance_loads,
      vehicle_load_kwh    = scen_loads$vehicle_load_kwh,
      solar_scenario      = scen$solar,
      available_solar_kwh = daily_solar_kwh_eff,
      free_by_load        = scen_free$free_by_load,
      vehicle_free_kwh    = scen_free$vehicle_free_kwh,
      min_retail_by_load  = scen_min_retail$min_retail_by_load,
      vehicle_min_retail_kwh = scen_min_retail$vehicle_min_retail_kwh
    )
    scen_scale <- scen_breakdown$solar_scale_factor

    # --- Per-appliance evaluation ---
    heating_row  <- eval_appliance("Space Heating", scen$heating,  scen$solar, scen_scale, scen_free_scale, tariff_name = scen_tariff)
    hotwater_row <- eval_appliance("Water Heating", scen$hotwater, scen$solar, scen_scale, scen_free_scale, tariff_name = scen_tariff)
    cooktop_row  <- eval_appliance("Cooktop",       scen$cooktop,  scen$solar, scen_scale, scen_free_scale, tariff_name = scen_tariff)
    vehicle_row  <- eval_appliance(
      "Vehicles", scen$vehicle, scen$solar, scen_scale, scen_free_scale,
      include_vehicles = TRUE,
      tariff_name = scen_tariff
    )

    # Every scenario gets an explicit cooling row — the Space Heating row is
    # heating-only regardless of appliance. Fossil-heated scenarios carry
    # a standalone split-AC capex (`cooling_only_capex`); the
    # all-electric scenarios share the heat pump unit already paid for in
    # heating capex, so cooling capex is $0 there. Cooling kWh are billed
    # at electricity retail (priced inside evaluate_option) in both cases.
    is_all_electric <- scen$heating$type == "Electric heat pump"
    cooling_capex   <- if (is_all_electric) 0 else cooling_only_capex
    cooling_label   <- if (is_all_electric) "Heat pump (cooling)" else "Split AC (cooling only)"
    cooling_row     <- NULL
    if (cooling_capex >= 0) {
      cooling_row <- eval_appliance(
        "Space Cooling",
        list(type  = "Heat pump", capex = cooling_capex,
             units = 1,           label = cooling_label),
        scen$solar, scen_scale, scen_free_scale,
        tariff_name = scen_tariff
      )
    }

    # --- "Other electric loads" baseload row ---
    # Blends the per-category solar fractions for Other Cooking + Other
    # Electronics, capped by the scenario's household-wide solar_scale_factor.
    # Free window first, then solar on the remainder, both inside the min_retail
    # floor — the same order and bound the appliance rows use.
    bl_split <- function(load_kwh, category) {
      split_category_load(load_kwh, category, scen$solar, scen_spec,
                          solar_scale = scen_scale, free_scale = scen_free_scale)
    }
    oc <- bl_split(baseload_other_cooking_kwh, "Other Cooking")
    oe <- bl_split(baseload_other_elec_kwh,    "Other Electronics")
    baseload_free_kwh_day  <- oc$free + oe$free
    baseload_solar_kwh_day <- oc$solar + oe$solar
    baseload_grid_kwh_day  <- baseload_energy_kwh_day - baseload_solar_kwh_day
    baseload_paid_kwh_day  <- max(baseload_grid_kwh_day - baseload_free_kwh_day, 0)
    # Supply charge is rolled up at the household level below — exclude it
    # from the per-row opex so it isn't double-counted.
    baseload_annual_opex   <- baseload_paid_kwh_day * 365 * baseload_grid_price
    baseload_total_cost    <- baseload_annual_opex * n_years
    # Free-window kWh are still grid-sourced, so they still emit.
    baseload_emissions_t   <- baseload_grid_kwh_day * 365 * baseload_ef / 1000

    other_row <- tibble(
      category            = "Other",
      option              = "Other electric loads",
      fuel                = "electricity",
      state               = state,
      n_occupants         = n_occupants,
      period              = period,
      use_loan            = use_loan,
      solar_scenario      = scen$solar,
      tariff              = scen_spec$tariff,
      energy_kwh_day      = baseload_energy_kwh_day,
      solar_kwh_day       = baseload_solar_kwh_day,
      home_grid_kwh_day   = baseload_grid_kwh_day,
      free_kwh_day        = baseload_free_kwh_day,
      grid_kwh_day        = baseload_grid_kwh_day,
      solar_cost_per_kwh  = 0,
      capex               = 0,
      subsidy             = 0,
      annual_maintenance  = 0,
      annual_opex         = baseload_annual_opex,
      total_cost          = baseload_total_cost,
      annual_tCO2e        = baseload_emissions_t
    )

    # Per-appliance opex already excludes any solar self-consumption charge
    # (evaluate_option treats self-consumed kWh as free; the PV system's
    # capex is added once at the household level via solar_capex below, and
    # FiT income is tracked separately on actually-exported kWh).
    scen_rows <- bind_rows(heating_row, hotwater_row, cooktop_row, vehicle_row,
                            cooling_row, other_row) |>
      mutate(scenario = scen$name)
    appliance_tibbles[[scen$name]] <- scen_rows

    # --- Solar PV system: capex + export + arbitrage revenue ---
    # Only the two solar scenarios carry a PV system. For the fossil and
    # grid-only electric scenarios we still emit a row with zeros so the
    # output is rectangular.
    if (scen$solar == "grid_only") {
      # Grid-only / fossil scenarios have no PV system and no battery, so all
      # solar and battery values are zero by construction.
      sb_summary <- tibble(
        scenario                              = scen$name,
        solar_scenario                        = "grid_only",
        self_sufficiency_pct                  = 0,
        daily_solar_kwh                       = 0,
        annual_export_kwh                     = 0,
        annual_headroom_kwh                   = 0,
        annual_battery_self_consumption_kwh   = 0,
        annual_battery_ev_kwh                 = 0,
        annual_export_value                   = 0,
        annual_headroom_value                 = 0,
        annual_battery_self_consumption_value = 0,
        annual_battery_ev_value               = 0,
        avg_battery_ev_price_kwh              = NA_real_,
        total_solar_capex                     = 0,
        solar_subsidy                         = 0,
        battery_capex                         = 0,
        battery_subsidy                       = 0,
        battery_subsidised                    = NA
      )
    } else {
      # The battery's evening discharge offsets appliance grid kWh — value it at
      # the rate those displaced kWh actually cost the household, which is the
      # tariff's import rate and exactly what evaluate_option() billed them at.
      battery_displaces_price <- scen_spec$import_dol_kwh

      sb <- evaluate_solar_battery(
        hh                            = hh_eff,
        heating_type                  = scen$heating$type,
        hotwater_type                 = scen$hotwater$type,
        cooking_type                  = scen$cooktop$type,
        vehicle_type                  = scen$vehicle$type,
        solar_scenario                = scen$solar,
        tariff                        = scen_tariff,
        # System inputs a caller may pass explicitly; NULL = derive as usual.
        daily_solar_kwh               = daily_solar_kwh,
        solar_capex                   = solar_capex,
        battery_capex                 = battery_capex,
        battery_subsidised            = battery_subsidised,
        inverter_replacement_cost     = inverter_replacement_cost,
        inverter_kw                   = inverter_kw,
        battery_self_consumption_price = battery_displaces_price
      )
      sb_summary <- sb$summary |>
        mutate(scenario = scen$name) |>
        select(scenario, solar_scenario, self_sufficiency_pct, daily_solar_kwh,
               annual_export_kwh, annual_headroom_kwh,
               annual_battery_self_consumption_kwh, annual_battery_ev_kwh,
               annual_export_value, annual_headroom_value,
               annual_battery_self_consumption_value, annual_battery_ev_value,
               avg_battery_ev_price_kwh,
               total_solar_capex, solar_subsidy,
               battery_capex, battery_subsidy, battery_subsidised)
    }
    solar_battery_tibbles[[scen$name]] <- sb_summary

    # --- Household-level supply charges ---
    # One full daily_charge × 365 per connected fuel, not allocated per
    # appliance. Every scenario carries the electricity connection fee
    # (lights / fridge / electronics keep the household grid-connected).
    # Gas and LPG charges only apply if the scenario contains at least one
    # appliance of that fuel.
    scen_fuels <- unique(scen_rows$fuel)
    # From the tariff spec, not the flat "electricity" row: Solar Sharer's
    # standing charge is materially higher (NSW $2.13/day vs $1.39) and is the
    # price the household pays for the free window.
    elec_daily_charge <- scen_spec$daily_charge
    gas_daily_charge  <- if ("gas" %in% scen_fuels)
      get_fuel_price(state, "gas", period)$daily_charge else 0
    lpg_daily_charge  <- if ("lpg" %in% scen_fuels)
      get_fuel_price(state, "lpg", period)$daily_charge else 0
    household_annual_supply <- (elec_daily_charge + gas_daily_charge + lpg_daily_charge) * 365

    # --- Switchboard upgrade (all-electric scenarios only) ---
    # One-off cost reflecting the upgrade needed to handle the cumulative
    # heat pump + induction + EV load. Loan-financed alongside the
    # appliances when use_loan = TRUE.
    switchboard_capex <- if (is_all_electric) switchboard_upgrade_capex else 0
    if (use_loan && switchboard_capex > 0) {
      switchboard_total <- annual_loan_payment(switchboard_capex, loan_rate, loan_term) *
                            min(n_years, loan_term)
    } else {
      switchboard_total <- switchboard_capex
    }

    # --- Scenario roll-up ---
    # appliance_opex / appliance_total exclude supply charges (rows were
    # built with include_supply_charge = FALSE). Supply is added once at the
    # household level below.
    appliance_capex          <- sum(scen_rows$capex,        na.rm = TRUE)
    appliance_opex           <- sum(scen_rows$annual_opex,  na.rm = TRUE)
    annual_solar_fit         <- sb_summary$annual_export_value + sb_summary$annual_headroom_value
    annual_battery_credit    <- sb_summary$annual_battery_self_consumption_value
    # Battery → EV discharge (non-zero whenever surplus stored solar exists
    # and the EV has unmet home-charging load to absorb it).
    # The Vehicles row still bills these kWh as grid imports at the EV rate
    # because evaluate_option has no visibility of the battery; crediting them
    # back here at the same rate nets them to zero, matching how
    # annual_battery_credit handles appliance load.
    annual_battery_ev_credit <- sb_summary$annual_battery_ev_value
    # Value of self-consumed solar = (daytime direct + battery-discharged) kWh
    # priced at the household's flat electricity retail rate. Daytime direct
    # consumption is already implicit in appliance_opex (those kWh are $0); this
    # column surfaces the total benefit for reporting. Not added to total_cost.
    elec_retail_price        <- get_fuel_price(state, "electricity", period)$dol_kwh
    annual_solar_self_kwh    <- sum(scen_rows$solar_kwh_day, na.rm = TRUE) * 365
    annual_solar_self_value  <- annual_solar_self_kwh * elec_retail_price
    total_solar_capex        <- sb_summary$total_solar_capex
    scen_battery_capex       <- sb_summary$battery_capex
    appliance_total          <- sum(scen_rows$total_cost,   na.rm = TRUE)
    total_capex              <- appliance_capex + total_solar_capex + scen_battery_capex +
                                  switchboard_capex
    net_annual_opex          <- appliance_opex + household_annual_supply -
                                  annual_solar_fit - annual_battery_credit -
                                  annual_battery_ev_credit

    # Solar PV and battery capex: financed alongside appliances/switchboard
    # when use_loan = TRUE. The inverter replacement (year 12) is bundled into
    # total_solar_capex and financed from year 0 along with the rest of the
    # system — a simplification consistent with how appliance capex is
    # handled. Summary columns keep reporting principal; total_cost reflects
    # loan repayments.
    if (use_loan && total_solar_capex > 0) {
      total_solar_payable <- annual_loan_payment(total_solar_capex, loan_rate, loan_term) *
                              min(n_years, loan_term)
    } else {
      total_solar_payable <- total_solar_capex
    }
    if (use_loan && scen_battery_capex > 0) {
      battery_payable <- annual_loan_payment(scen_battery_capex, loan_rate, loan_term) *
                          min(n_years, loan_term)
    } else {
      battery_payable <- scen_battery_capex
    }

    # appliance_total already includes per-row capex (or loan repayments) and
    # n_years of opex (electric grid kWh at the scenario's tariff, daytime
    # self-consumed solar and free-window kWh at $0). Add the PV side: solar + battery capex once, plus solar
    # income (FiT + arbitrage) and battery self-consumption credit (free
    # appliance kWh from evening battery discharge) subtracted over the horizon.
    # Supply charges are added at the household level (one full charge per
    # connected fuel × n_years), not per appliance.
    # Switchboard capex / loan repayments are added once for electrified
    # scenarios via switchboard_total.
    total_cost <- appliance_total + total_solar_payable + battery_payable +
                  switchboard_total +
                  household_annual_supply * n_years -
                  (annual_solar_fit + annual_battery_credit +
                   annual_battery_ev_credit) * n_years

    # --- Loan costs ---
    # Strip n_years of opex from appliance_total to recover the per-row capex
    # outlay (principal when use_loan = FALSE; principal + interest repayments
    # when use_loan = TRUE). Combined with the household-level payables for
    # solar, battery and the switchboard, this is the total spent on capex
    # over the horizon. Subtracting total_capex (the principal) leaves the
    # interest component — zero when not loan-financed.
    appliance_payable     <- appliance_total - appliance_opex * n_years
    total_loan_repayment  <- appliance_payable + total_solar_payable +
                              battery_payable + switchboard_total
    total_interest        <- if (use_loan) total_loan_repayment - total_capex else 0
    annual_loan_repayment <- if (use_loan) total_loan_repayment / min(n_years, loan_term) else 0

    # Energy balance in the summary:
    #   total_energy = solar + battery + grid_net
    # where grid_net is the gross appliance-attributed grid_kwh_day minus the
    # battery's evening discharge (the kWh that the per-appliance rows think
    # come from grid but are actually served by the household battery — see
    # annual_battery_credit). Per-appliance rows in $appliances stay un-netted
    # so single-appliance comparisons aren't affected.
    # Battery discharge to house load + to the EV are both solar kWh that the
    # per-appliance rows counted as grid imports, so both come out of grid_kwh_day
    # and out of the emissions total below.
    battery_kwh_day_scen <- (sb_summary$annual_battery_self_consumption_kwh +
                                     sb_summary$annual_battery_ev_kwh) / 365
    summary_tibbles[[scen$name]] <- tibble(
      scenario              = scen$name,
      solar_scenario        = scen$solar,
      state                 = state,
      # The tariff actually applied, which is not always the one requested —
      # Solar Sharer falls back to tou outside SOLAR_SHARER_STATES. Report it so
      # by-state tables can label a fallback rather than implying coverage.
      tariff                = scen_spec$tariff,
      n_occupants           = n_occupants,
      n_vehicles            = n_vehicles,
      period                = period,
      use_loan              = use_loan,
      total_energy_kwh_day  = sum(scen_rows$energy_kwh_day, na.rm = TRUE), 
      solar_kwh_day         = sum(scen_rows$solar_kwh_day,  na.rm = TRUE),
      battery_kwh_day       = battery_kwh_day_scen,
      grid_kwh_day          = sum(scen_rows$grid_kwh_day,  na.rm = TRUE) -
                                       battery_kwh_day_scen,
      appliance_capex       = appliance_capex,
      solar_pv_capex        = total_solar_capex,
      battery_capex         = scen_battery_capex,
      total_capex           = total_capex,
      annual_loan_repayment = annual_loan_repayment,
      total_loan_repayment  = total_loan_repayment,
      total_interest        = total_interest,
      appliance_opex        = appliance_opex,
      annual_supply_charge  = household_annual_supply,
      # Supply charge split by fuel. Reported so downstream consumers (the
      # TS<->R regression harness, plots) can attribute the standing charge to
      # the right cost segment instead of re-deriving the split and drifting
      # from the rules applied above.
      annual_supply_elec    = elec_daily_charge * 365,
      annual_supply_gas     = gas_daily_charge  * 365,
      annual_supply_lpg     = lpg_daily_charge  * 365,
      annual_solar_fit      = annual_solar_fit,
      annual_solar_self_value = annual_solar_self_value,
      annual_battery_credit = annual_battery_credit,
      annual_battery_ev_credit = annual_battery_ev_credit,
      net_annual_opex       = net_annual_opex,
      total_cost            = total_cost,
      # Emissions: per-appliance rows count battery-dispatched kWh as grid
      # imports (they're inside grid_energy in evaluate_option). At the
      # household level, evening battery discharge actually comes from stored
      # solar — by the model's "self-consumed solar = zero-emission" convention
      # those kWh should not be charged at grid intensity. Net them out here
      # using the per-state grid EF (baseload_ef), the same way grid_kwh_day
      # above subtracts battery_kwh_day_scen from the energy balance.
      annual_tCO2e          = sum(scen_rows$annual_tCO2e, na.rm = TRUE) -
                                       battery_kwh_day_scen * 365 * baseload_ef / 1000
    )
  }

  # Annualised total-cost savings vs the Gas scenario (mirrors the
  # compute_annual_savings convention: total_cost / horizon_years). NA if the
  # Gas scenario row is missing (shouldn't happen here, but guards against
  # future refactors that drop scenarios).
  household_summary <- bind_rows(summary_tibbles)
  gas_total_cost <- household_summary$total_cost[household_summary$scenario == "Gas"]
  gas_total_cost <- if (length(gas_total_cost) == 0) NA_real_ else gas_total_cost[1]
  household_summary <- household_summary |>
    mutate(
      annual_savings_vs_gas = 
        (gas_total_cost - .data$total_cost) / if_else(.data$period == "15year", 15, 1)
    )

  list(
    summary       = household_summary,
    appliances    = bind_rows(appliance_tibbles),
    solar_battery = bind_rows(solar_battery_tibbles)
  )
}


# =============================================================================
# 13. Household settings object
# =============================================================================
# household() describes one household once — who lives there, where, what they
# drive, their retail plan, finance and solar/battery system — and every public
# function accepts it:
#
#   hh <- household(state = "NSW", n_occupants = 3, tariff = "solar_sharer")
#   evaluate_household(hh)
#   compare_water_heating(hh)
#   evaluate_solar_battery(hh = hh)
#   by_state <- map(c("NSW", "VIC"), ~ evaluate_household(update(hh, state = .x)))
#
# Anything not given comes from model_defaults.csv. household() also validates
# the inputs (a misspelt tariff or unknown state fails here, not three functions
# later) and resolves the derived values once: system size from dwelling type,
# battery degradation from the horizon, and the number of years.
#
# Passing hh: give it as the first argument where that argument is `state`
# (evaluate_household, compare_*, solar_battery_rows), or as `hh = ` anywhere.
# Arguments named explicitly alongside hh override it for that call, e.g.
# compare_water_heating(hh, solar_scenario = "solar").
#
# hh holds properties of the household. Switches that describe how a
# calculation runs (solar_scenario / system context in compare_*(),
# include_supply_charge, solar_marginal_cost) stay as ordinary arguments.
#
# n_vehicles defaults to household_n_vehicles in model_defaults.csv, which is
# "state_average": vehicles per household for the state from
# state_average_values.csv (AUS 1.88). Pass a number to override.

household <- function(
    state                         = dflt("state"),
    n_occupants                   = dflt("n_occupants"),
    dwelling_type                 = dflt("dwelling_type"),
    n_vehicles                    = dflt("household_n_vehicles"),
    vehicle_class                 = dflt("household_vehicle_class"),
    new_used                      = dflt("new_used"),
    km_tier                       = dflt("km_tier"),
    tariff                        = dflt("tariff"),
    ev_tariff_share               = dflt("ev_tariff_share"),
    period                        = dflt("household_period"),
    use_loan                      = dflt("use_loan"),
    loan_rate                     = dflt("loan_rate"),
    loan_term                     = dflt("loan_term"),
    solar_kwp                     = NULL,   # NULL: by dwelling (DEFAULT_SYSTEM_SIZE)
    battery_size_kwh              = NULL,   # NULL: by dwelling
    free_battery_kwh              = dflt("free_battery_kwh"),
    household_safeguard_pct       = dflt("household_safeguard_pct"),
    useable_capacity_pct          = dflt("battery_useable_capacity_pct"),
    battery_round_trip_efficiency = dflt("battery_round_trip_efficiency"),
    annual_degradation_pct        = NULL,   # NULL: by period (default_degradation)
    inverter_replacement_year     = dflt("inverter_replacement_year"),
    fit_price_kwh                 = NULL,   # NULL: per-state feed-in tariff
    fast_charge_fraction          = FAST_CHARGE_FRACTION,
    electric_cooktop_type         = dflt("electric_cooktop_type"),
    fossil_vehicle_type           = dflt("fossil_vehicle_type"),
    lpg_cooktop_type              = dflt("lpg_cooktop_type"),
    cooling_only_capex            = dflt("cooling_only_capex"),
    switchboard_upgrade_capex     = dflt("switchboard_upgrade_capex"),
    is_landlord                   = FALSE,
    battery_vpp                   = FALSE,
    postcode                      = NA_integer_
) {
  # Raw inputs are kept so update() can re-derive (e.g. a new dwelling type
  # re-sizes the system unless solar_kwp was set explicitly).
  inputs <- as.list(environment())

  # Case-insensitive, so "House" / "Apartment" work as they did before
  # household() existed (the model compares against lower-case values).
  if (is.character(dwelling_type)) dwelling_type <- tolower(dwelling_type)

  .check <- function(ok, msg) if (!isTRUE(ok)) stop("household(): ", msg, call. = FALSE)
  .num1  <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x)
  valid_states <- unique(fuel_prices$state)
  .check(length(state) == 1 && state %in% valid_states,
         paste0("state must be one of ", paste(valid_states, collapse = ", "), " (got '", state, "')"))
  n_vehicles <- resolve_n_vehicles(n_vehicles, state)
  .check(.num1(n_occupants) && n_occupants > 0, "n_occupants must be a positive number")
  .check(dwelling_type %in% c("house", "apartment"), "dwelling_type must be 'house' or 'apartment'")
  .check(.num1(n_vehicles) && n_vehicles >= 0, "n_vehicles must be a number >= 0 or 'state_average'")
  .check(length(vehicle_class) >= 1 &&
           all(vehicle_class_to_cost_type(vehicle_class) %in% c("SUV", "Sedan", "Hatchback/small car")),
         "vehicle_class entries must be SUV, sedan or hatch(back)")
  .check(new_used %in% c("New", "Used"), "new_used must be 'New' or 'Used'")
  .check(km_tier %in% KM_TIERS, paste0("km_tier must be one of ", paste(KM_TIERS, collapse = ", ")))
  .check(tariff %in% TARIFFS, paste0("tariff must be one of ", paste(TARIFFS, collapse = ", ")))
  .check(.num1(ev_tariff_share) && ev_tariff_share >= 0, "ev_tariff_share must be >= 0")
  .check(period %in% c("1year", "15year"), "period must be '1year' or '15year'")
  .check(is.logical(use_loan) && length(use_loan) == 1 && !is.na(use_loan), "use_loan must be TRUE or FALSE")
  .check(.num1(loan_rate) && loan_rate >= 0, "loan_rate must be >= 0")
  .check(.num1(loan_term) && loan_term > 0, "loan_term must be > 0")
  .check(is.null(solar_kwp) || (.num1(solar_kwp) && solar_kwp >= 0), "solar_kwp must be >= 0")
  .check(is.null(battery_size_kwh) || (.num1(battery_size_kwh) && battery_size_kwh >= 0),
         "battery_size_kwh must be >= 0")
  .check(fossil_vehicle_type %in% c("Petrol", "Diesel"), "fossil_vehicle_type must be 'Petrol' or 'Diesel'")
  .check(tolower(electric_cooktop_type) %in%
           c("induction", "electric induction", "electric", "resistive", "resistance", "electric resistance"),
         "electric_cooktop_type must be 'induction' or 'electric'")

  n_years  <- if (period == "1year") 1 else 15
  sys_size <- default_system_size(dwelling_type, solar_kwp, battery_size_kwh)

  hh <- list(
    state = state, n_occupants = n_occupants, dwelling_type = dwelling_type,
    n_vehicles = n_vehicles, vehicle_class = vehicle_class, new_used = new_used,
    km_tier = km_tier, tariff = tariff, ev_tariff_share = ev_tariff_share,
    period = period, n_years = n_years,
    use_loan = use_loan, loan_rate = loan_rate, loan_term = loan_term,
    solar_kwp = sys_size$solar_kwp, battery_size_kwh = sys_size$battery_size_kwh,
    free_battery_kwh = free_battery_kwh,
    household_safeguard_pct = household_safeguard_pct,
    useable_capacity_pct = useable_capacity_pct,
    battery_round_trip_efficiency = battery_round_trip_efficiency,
    annual_degradation_pct = if (is.null(annual_degradation_pct))
                               default_degradation(n_years) else annual_degradation_pct,
    inverter_replacement_year = inverter_replacement_year,
    fit_price_kwh = fit_price_kwh,
    fast_charge_fraction = fast_charge_fraction,
    electric_cooktop_type = electric_cooktop_type,
    fossil_vehicle_type = fossil_vehicle_type, lpg_cooktop_type = lpg_cooktop_type,
    cooling_only_capex = cooling_only_capex,
    switchboard_upgrade_capex = switchboard_upgrade_capex,
    is_landlord = is_landlord, battery_vpp = battery_vpp, postcode = postcode
  )
  structure(hh, class = "household", inputs = inputs)
}

is_household <- function(x) inherits(x, "household")

# Copy of hh with some settings changed; derived values are re-resolved.
update.household <- function(object, ...) {
  changes <- list(...)
  bad <- setdiff(names(changes), names(formals(household)))
  if (length(bad)) stop("update(): unknown household setting(s): ", paste(bad, collapse = ", "),
                        call. = FALSE)
  do.call(household, utils::modifyList(attr(object, "inputs"), changes, keep.null = TRUE))
}

print.household <- function(x, ...) {
  fmt <- function(v) if (is.null(v)) "NULL" else paste(v, collapse = ", ")
  cat("<household>\n")
  for (n in names(x)) cat(sprintf("  %-30s %s\n", n, fmt(x[[n]])))
  invisible(x)
}

# n_vehicles may be a number or "state_average" (vehicles per household for the
# state, from state_average_values.csv). A number given as text is accepted.
resolve_n_vehicles <- function(n_vehicles, state) {
  if (identical(n_vehicles, "state_average")) return(get_state_average_vehicles(state))
  if (is.character(n_vehicles)) {
    v <- suppressWarnings(as.numeric(n_vehicles))
    if (length(v) != 1 || is.na(v)) {
      stop("n_vehicles must be a number or 'state_average' (got '", n_vehicles, "')", call. = FALSE)
    }
    return(v)
  }
  n_vehicles
}

get_state_average_vehicles <- function(state) {
  i <- match(state, state_average_values$state)
  if (is.na(i)) stop("No vehicles_per_household for '", state, "' in state_average_values.csv",
                     call. = FALSE)
  state_average_values$vehicles_per_household[i]
}

# Build a household from the arguments a public function ended up with (its
# defaults, any hh it was given, and explicit overrides), so it can pass one
# object down to the helpers it calls instead of a long argument list.
# `overrides` supplies fields the function doesn't hold under the household
# name (e.g. evaluate_solar_battery has `years`, not `period`).
.household_from_env <- function(env, overrides = list()) {
  fields <- intersect(names(formals(household)), ls(env, all.names = TRUE))
  vals   <- mget(fields, envir = env)
  do.call(household, utils::modifyList(vals, overrides, keep.null = TRUE))
}

# Function arguments whose household field has a different name.
.HH_ARG_ALIASES <- c(years = "n_years")

# Called first thing in each public function that accepts hh. Copies every hh
# field the function has an argument for into the function's environment,
# except arguments the caller named explicitly (those win). When hh arrived in
# the `state` slot, state is taken from hh too.
.use_household <- function(hh, env, fn, call) {
  if (!is_household(hh)) stop("hh must be created with household()", call. = FALSE)
  state_carries_hh <- is_household(get("state", envir = env))
  supplied <- setdiff(names(as.list(call))[-1],
                      c("", "hh", if (state_carries_hh) "state"))
  for (arg in setdiff(names(formals(fn)), c(supplied, "hh", "..."))) {
    field <- if (arg %in% names(.HH_ARG_ALIASES)) .HH_ARG_ALIASES[[arg]] else arg
    if (field %in% names(hh)) assign(arg, hh[[field]], envir = env)
  }
  invisible(NULL)
}
