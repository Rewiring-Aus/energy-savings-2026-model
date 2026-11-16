# =============================================================================
# Energy Savings Model 2026 — reproduce the report's modelled figures
# =============================================================================
# Reruns the model results published in Rewiring Australia's Home Energy
# Savings Report 2026:
#   https://rewiringaustralia.org/impact/research/home-energy-savings-report-2026
#
# Run from the repository root:
#
#   Rscript run_model.R
#
# Results print to the console and are written to output/ as CSVs. README.md
# lists which report figure each output corresponds to.
# =============================================================================

suppressMessages(library(tidyverse))
options(readr.show_col_types = FALSE)
suppressMessages(source("energy_savings_model.R"))

out_dir <- "output"
dir.create(out_dir, showWarnings = FALSE)

gas  <- "Gas"
grid <- "Electric (grid_only)"
elec <- "Electric (solar_optimised)"

# Annual cost of one evaluate_household() scenario, split the way the report's
# stacked bars are: upfront cost and loan interest (annualised over the
# horizon), then gas, petrol and electricity bills. Vehicle servicing is in
# the petrol / electricity bars, as in the report.
cost_breakdown <- function(ev, scenario) {
  s <- ev$summary[ev$summary$scenario == scenario, ]
  a <- ev$appliances[ev$appliances$scenario == scenario, ]
  n_years <- as.numeric(sub("year", "", s$period))
  gas_bill    <- sum(a$annual_opex[a$fuel %in% c("gas", "lpg")]) +
                 s$annual_supply_gas + s$annual_supply_lpg
  petrol_bill <- sum(a$annual_opex[a$fuel %in% c("petrol", "diesel")])
  tibble(
    scenario       = scenario,
    upfront        = s$total_capex / n_years,
    loan_interest  = s$total_interest / n_years,
    gas            = gas_bill,
    petrol         = petrol_bill,
    electricity    = s$net_annual_opex - gas_bill - petrol_bill,
    total          = s$total_cost / n_years,
    annual_tCO2e   = s$annual_tCO2e
  )
}

show <- function(title, df) {
  cat("\n", title, "\n", sep = "")
  print(as.data.frame(round_output(df)), row.names = FALSE)
}

# -----------------------------------------------------------------------------
# 1. The average Australian house (Figures 2, 4 and 5)
# -----------------------------------------------------------------------------
# The model defaults: AUS, 2.7 occupants, 1.88 vehicles (SUV + hatchback),
# 10 kW solar + 15 kWh battery, 15-year costs, loan at 6%.

hh    <- household()
house <- evaluate_household(hh)

house_costs <- map_dfr(c(gas, grid, elec), \(s) cost_breakdown(house, s)) |>
  mutate(saving_vs_gas = total[1] - total)
show("Figures 4 and 5. House: annual cost, gas & petrol vs all-electric ($/year)", house_costs)
write_csv(round_output(house_costs), file.path(out_dir, "house_annual_cost.csv"))
write_csv(round_output(house$summary), file.path(out_dir, "house_summary.csv"))

# -----------------------------------------------------------------------------
# 2. Where the savings come from (Figures 2 and 3)
# -----------------------------------------------------------------------------
# bill_saving is the energy-bill basis of Figures 2 and 3; total_saving also
# counts upfront cost, loan interest and vehicle servicing. The second table
# switches tariff before adding the battery, which gives the "plus about $600
# on a time-of-use tariff" figure.

sources <- household_savings_breakdown(hh)
cat("\nFigures 2 and 3. Savings by source\n")
print_savings_breakdown(sources)
cat("\nFigure 3, tariff switched before the battery is added\n")
print_savings_breakdown(household_savings_breakdown(hh, tariff_before_battery = TRUE))
write_csv(round_output(as_tibble(sources)), file.path(out_dir, "house_savings_by_source.csv"))

# -----------------------------------------------------------------------------
# 3. Savings by state (Figure 6)
# -----------------------------------------------------------------------------
# Each state at its average occupancy for separate houses (2021 Census) and
# average vehicles per household. NT has almost no mains gas, so it is
# compared with the LPG home. The report used unrounded occupancy, so these
# results can differ from the published figures by a dollar or two.

state_occupancy <- c(NSW = 2.85, VIC = 2.76, QLD = 2.76, SA = 2.57,
                     WA = 2.69, TAS = 2.44, NT = 3.26, ACT = 2.85)

by_state <- imap_dfr(state_occupancy, function(occupants, state) {
  s <- evaluate_household(update(hh, state = state, n_occupants = occupants))$summary
  baseline <- if (state == "NT") "LPG" else gas
  tibble(state, n_occupants = occupants,
         n_vehicles    = s$n_vehicles[1],
         compared_with = baseline,
         annual_saving = (s$total_cost[s$scenario == baseline] -
                            s$total_cost[s$scenario == elec]) / hh$n_years,
         annual_tCO2e_saved = s$annual_tCO2e[s$scenario == baseline] -
                              s$annual_tCO2e[s$scenario == elec])
})
show("Figure 6. Annual savings from going all-electric, by state ($/year)", by_state)
write_csv(round_output(by_state), file.path(out_dir, "state_savings.csv"))

# -----------------------------------------------------------------------------
# 4. Apartment (Figure 7)
# -----------------------------------------------------------------------------

apartment <- evaluate_household(household(
  dwelling_type = "apartment", n_occupants = 1.8,
  n_vehicles = 1, vehicle_class = "SUV",
  solar_kwp = 2.5, battery_size_kwh = 5
))
apartment_costs <- map_dfr(c(gas, elec), \(s) cost_breakdown(apartment, s)) |>
  mutate(saving_vs_gas = total[1] - total)
show("Figure 7. Apartment: annual cost, gas & petrol vs all-electric ($/year)", apartment_costs)
write_csv(round_output(apartment_costs), file.path(out_dir, "apartment_annual_cost.csv"))

# -----------------------------------------------------------------------------
# 5. Electric vehicles by class (Figures 15 to 17)
# -----------------------------------------------------------------------------
# One new car, average driving, 15 years, loan at 6%. Charging is 85% at home
# on the time-of-use plan and 15% at public fast chargers, with no solar.
# The EV is priced at the class average for new EVs.

ev_classes <- c(Hatchback = "hatch", Sedan = "sedan", SUV = "SUV")
ev_cost_type <- c(hatch = "Hatchback/small car", sedan = "Sedan", SUV = "SUV")
free_solar_share <- 0.5   # Figure 16: share of home charging that is free

vehicles <- imap_dfr(ev_classes, function(cls, label) {
  petrol <- compare_vehicles("AUS", 1, "15year", use_loan = TRUE, vehicle_class = cls,
                             solar_scenario = "grid_only",
                             tariff = "tou") |>
    filter(fuel == "petrol")
  ev <- evaluate_option(category = "Vehicles", appliance_type = "Electric",
                        label = "New EV (class average)",
                        capex = get_vehicle_capex("Electric", ev_cost_type[[cls]], "New"),
                        include_vehicles = TRUE, state = "AUS", n_vehicles = 1,
                        vehicle_class = cls, period = "15year", use_loan = TRUE,
                        solar_scenario = "grid_only", tariff = "tou")
  # Figure 16: half of home charging is free rooftop solar, the rest stays on
  # the time-of-use plan. No battery, and the solar itself is not costed here.
  ev_solar <- evaluate_option(category = "Vehicles", appliance_type = "Electric",
                              label = "EV + free solar charging", capex = ev$capex,
                              include_vehicles = TRUE, state = "AUS", n_vehicles = 1,
                              vehicle_class = cls, period = "15year", use_loan = TRUE,
                              solar_scenario = "solar_optimised",
                              solar_scale_factor = free_solar_share /
                                get_solar_fraction("Vehicles", "solar_optimised"),
                              solar_marginal_cost = FALSE, tariff = "tou")
  tibble(
    class                    = label,
    petrol_price             = petrol$capex,
    ev_price                 = ev$capex,
    petrol_running_cost      = petrol$annual_opex,
    ev_running_cost          = ev$annual_opex,
    running_cost_saving      = petrol$annual_opex - ev$annual_opex,
    running_cost_saving_free_solar = petrol$annual_opex - ev_solar$annual_opex,
    saving_incl_purchase     = (petrol$total_cost - ev$total_cost) / 15
  )
})
show("Figures 15 to 17. New EV vs new petrol car of the same class ($/year)", vehicles)
write_csv(round_output(vehicles), file.path(out_dir, "vehicle_savings.csv"))

cat("\nCSV outputs written to ", out_dir, "/\n", sep = "")
