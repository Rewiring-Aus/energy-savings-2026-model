# =============================================================================
# Energy Savings Model 2026 — example run
# =============================================================================
# Runs the whole-household comparison for the national default household, then
# the same household in every state. Run from the repository root:
#
#   Rscript run_model.R
#
# or open the folder in RStudio, set it as the working directory and source this
# file. Results print to the console and are written to output/ as CSVs.
# See README.md for the household settings you can change.
# =============================================================================

suppressMessages(library(tidyverse))

# Point the model at this repository's scripts and data/ folder (by default it
# looks for them on the Rewiring Australia shared drive).
repo_dir <- normalizePath(".")
options(esm.model_dir = repo_dir,
        esm.data_dir  = file.path(repo_dir, "data"),
        readr.show_col_types = FALSE)
source(file.path(repo_dir, "energy_savings_model.R"))

out_dir <- file.path(repo_dir, "output")
dir.create(out_dir, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. National default household
# -----------------------------------------------------------------------------
# Every setting not given here comes from data/model_defaults.csv. To try a
# different household, change the arguments, e.g.
#   household(state = "VIC", n_occupants = 4, tariff = "flat", n_vehicles = 1)

hh <- household()
print(hh)

national <- evaluate_household(hh)

national_summary <- national$summary |>
  select(scenario, total_capex, net_annual_opex, total_cost,
         annual_savings_vs_gas, annual_tCO2e)

cat("\nNational default household: cost of each scenario over",
    hh$n_years, "years\n")
print(round_output(national_summary), n = Inf, width = Inf)

write_csv(round_output(national$summary),    file.path(out_dir, "national_summary.csv"))
write_csv(round_output(national$appliances), file.path(out_dir, "national_appliances.csv"))

# -----------------------------------------------------------------------------
# 2. State comparison
# -----------------------------------------------------------------------------
# The same household in each state. update() changes only the state; the
# vehicle count follows the state average because n_vehicles defaults to
# "state_average". NT has no mains gas price in the inputs, so its Gas and
# Gas + solar scenarios (and savings vs Gas) are NA; compare against LPG there.

states <- c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "AUS")

by_state <- map_dfr(states, function(s) {
  evaluate_household(update(hh, state = s))$summary
})

state_savings <- by_state |>
  select(state, n_vehicles, scenario, annual_savings_vs_gas) |>
  pivot_wider(names_from = scenario, values_from = annual_savings_vs_gas)

cat("\nAnnual savings vs the Gas scenario, by state ($/yr, averaged over",
    hh$n_years, "years)\n")
print(round_output(state_savings), n = Inf, width = Inf)

write_csv(round_output(by_state), file.path(out_dir, "state_summary.csv"))

cat("\nCSV outputs written to", out_dir, "\n")
