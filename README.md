# Energy Savings Model 2026

Rewiring Australia's household energy savings model. It estimates what an Australian household
spends to run its home and cars on gas, LPG or petrol, compared with running them all on
electricity, with and without rooftop solar and a battery. Costs cover appliances, installation,
energy, supply charges, maintenance and finance over 1 or 15 years. Emissions are also reported.

The reasoning behind each assumption is in [Methodology_v1.md](Methodology_v1.md).

## What's in this repository

| Path | What it is |
|---|---|
| `energy_savings_model.R` | The model. Sourcing it loads the inputs and defines the functions; it runs nothing on its own. |
| `battery_model.R` | Solar and battery dispatch, sourced by the model. |
| `run_model.R` | Example run: the national default household, then the same household in every state. |
| `data/` | Every input the model reads: prices, energy use, tariffs, emissions factors, solar output, vehicle costs and the model defaults. |
| `Methodology_v1.md` | Methodology. |

## Running it

You need R (tested with 4.4) and the tidyverse:

```r
install.packages("tidyverse")
```

Then, from the repository folder:

```bash
Rscript run_model.R
```

You can also open the folder in RStudio, set it as the working directory and source `run_model.R`.
The script prints two tables and writes CSVs to `output/`:

- `national_summary.csv`: one row per scenario for the national default household.
- `national_appliances.csv`: the same broken down by appliance.
- `state_summary.csv`: one row per scenario for each state.

To use the model in your own script, point it at this repository before sourcing it:

```r
library(tidyverse)
options(esm.model_dir = getwd(), esm.data_dir = file.path(getwd(), "data"))
source("energy_savings_model.R")
```

Without those two options the model looks for its files on the Rewiring Australia shared drive.

## The scenarios

`evaluate_household()` costs the same household six ways:

| Scenario | Appliances |
|---|---|
| Gas | Gas space heating, gas hot water, gas cooktop, petrol car(s) |
| Gas + solar | As Gas, plus rooftop solar and a battery. Only cooling and other electric loads use the solar. |
| LPG | LPG heating, hot water and cooktop, petrol car(s) |
| Electric (grid_only) | Heat pump heating and hot water, induction cooktop, electric car(s), no solar |
| Electric (solar) | As above, with rooftop solar and a battery |
| Electric (solar_optimised) | As above, with flexible loads shifted into solar hours (e.g. hot water on a timer) |

Savings are reported against the Gas scenario (`annual_savings_vs_gas`). The fossil scenarios and
Electric (grid_only) are billed on a flat tariff, and the solar scenarios on the household's
`tariff` (see `fossil_baseline_tariff` and `grid_only_tariff` in `data/model_defaults.csv`).

NT has no mains gas price in the inputs, so its Gas and Gas + solar results are blank. Compare
against LPG there.

## Describing a household

A run starts from `household()`. Any setting you leave out takes its value from
`data/model_defaults.csv`. Use `update()` to vary one setting:

```r
hh <- household(state = "VIC", n_occupants = 4, tariff = "flat")
evaluate_household(hh)

evaluate_household(update(hh, dwelling_type = "apartment"))
evaluate_household(update(hh, period = "1year", use_loan = FALSE))
```

`household()` checks its inputs, so a misspelt tariff or state fails immediately with a message
listing the allowed values. `print(hh)` shows every setting.

### Household settings

Defaults shown are the current values in `data/model_defaults.csv`.

**Who and where**

| Setting | Default | Allowed values / meaning |
|---|---|---|
| `state` | `"AUS"` | `NSW`, `VIC`, `QLD`, `SA`, `WA`, `TAS`, `NT`, `ACT`, or `AUS` (population-weighted national average) |
| `n_occupants` | `2.7` | Any positive number. 2.7 is the Census average; energy use scales with occupancy. |
| `dwelling_type` | `"house"` | `house` or `apartment`. Apartments use 21% less energy and get a smaller solar and battery system. |
| `postcode` | `NA` | Optional. Used only for WA state subsidy eligibility. |

**Vehicles**

| Setting | Default | Allowed values / meaning |
|---|---|---|
| `n_vehicles` | `"state_average"` | A number (0 or more), or `"state_average"`, which uses the state's cars per household (AUS 1.88). |
| `vehicle_class` | `c("SUV", "hatch")` | One or more of `SUV`, `sedan`, `hatch`. Vehicles are split evenly across the classes. |
| `new_used` | `"New"` | `New` or `Used` vehicle prices |
| `km_tier` | `"middle"` | `low`, `middle` or `high` daily driving distance (`data/average_km_per_day_by_state.csv`) |
| `fossil_vehicle_type` | `"Petrol"` | `Petrol` or `Diesel`, used in the fossil scenarios |
| `fast_charge_fraction` | `0.15` | Share of EV charging done at public fast chargers |

**Tariff**

| Setting | Default | Allowed values / meaning |
|---|---|---|
| `tariff` | `"tou"` | Retail plan for the electrified household: `flat`, `tou` (time of use), `amber` (wholesale-linked), `solar_sharer` (free midday window) |
| `ev_tariff_share` | `1` | Share of EV charging billed at the EV rate. Currently has no effect: the EV rate equals the import rate on every plan. |

**Time horizon and finance**

| Setting | Default | Allowed values / meaning |
|---|---|---|
| `period` | `"15year"` | `1year` or `15year`. Totals cover the horizon; `annual_savings_vs_gas` is per year. |
| `use_loan` | `TRUE` | `TRUE`: upfront costs are financed by a loan. `FALSE`: paid in cash. |
| `loan_rate` | `0.06` | Annual loan interest rate |
| `loan_term` | `15` | Loan term in years |

**Solar and battery**

| Setting | Default | Allowed values / meaning |
|---|---|---|
| `solar_kwp` | by dwelling | Solar system size in kW. Defaults to 10 for a house and 2 for an apartment. |
| `battery_size_kwh` | by dwelling | Battery size in kWh. Defaults to 15 for a house and 5 for an apartment. |
| `free_battery_kwh` | `0` | Extra storage with no purchase cost, for example to emulate vehicle-to-grid |
| `household_safeguard_pct` | `0` | Share of stored solar held back as a reserve |
| `useable_capacity_pct` | `0.9` | Usable share of rated battery capacity |
| `battery_round_trip_efficiency` | `0.9` | Battery round-trip efficiency |
| `annual_degradation_pct` | by period | Battery degradation per year: 4% for a 1-year horizon, 2.2% averaged over 15 years |
| `inverter_replacement_year` | `12` | Year the inverter is replaced (inside a 15-year horizon) |
| `fit_price_kwh` | by state | Feed-in tariff in $/kWh. Defaults to the state value in `data/fuel_prices_by_state_simple.csv`. |
| `battery_vpp` | `FALSE` | Whether the battery is in a virtual power plant (affects state subsidy eligibility) |

**Appliances and costs**

| Setting | Default | Allowed values / meaning |
|---|---|---|
| `electric_cooktop_type` | `"induction"` | `induction` or `electric` (resistive) |
| `lpg_cooktop_type` | `"LPG"` | Cooktop in the LPG scenario |
| `cooling_only_capex` | `2000` | Cost of a split-system air conditioner for cooling in fossil-heated homes ($) |
| `switchboard_upgrade_capex` | `2500` | Switchboard upgrade charged to every all-electric scenario ($) |
| `is_landlord` | `FALSE` | Whether the household is a landlord (affects state subsidy eligibility) |

## Changing the assumptions

All defaults and modelling constants that aren't market prices live in
`data/model_defaults.csv`. Each row has a description and the functions it applies to. Edit a
value there to change it for every run. Examples include the finance rate, system sizes, battery
efficiency, the solar finance rate and the fast-charging share.

To test a variant without editing the file, copy it and point the model at the copy before
sourcing:

```r
options(esm.model_defaults_file = "my_variant_defaults.csv")
```

Prices (energy, appliances, vehicles) are in their own CSVs in `data/`. The file names describe
their contents.

## Other functions

`evaluate_household()` is the whole-household view. The model also compares one appliance
category at a time. Each function accepts a `household()` as its first argument:

| Function | Compares |
|---|---|
| `compare_space_heating(hh)` | Gas, LPG, resistive and heat pump (reverse-cycle AC) heating |
| `compare_water_heating(hh)` | Gas instant, LPG instant, electric tank and heat pump hot water |
| `compare_cooktops(hh)` | Gas, electric (resistive) and induction cooktops |
| `compare_vehicles(hh)` | Petrol, diesel and electric vehicles |
| `evaluate_solar_battery(hh = hh)` | The solar and battery system on its own |

These have a `solar_scenario` argument (`"grid_only"`, `"solar"`, `"solar_optimised"`). Note that
the appliance-level functions use some different defaults from `evaluate_household()`: for
example a 1-year horizon and one vehicle (see the `appliance_*` rows in `model_defaults.csv`).
Settings you give in `household()` override them.

## Output

Model functions return full-precision numbers. Round only when presenting results, with
`round_output(df)`, which rounds dollars to whole dollars, $/kWh to 4 places and energy and
emissions to 2.

Main columns of `evaluate_household(hh)$summary`:

| Column | Meaning |
|---|---|
| `total_capex` | Upfront cost of appliances, vehicles, solar and battery |
| `net_annual_opex` | Yearly running cost after solar and battery credits |
| `total_cost` | Total cost over the horizon, including loan interest |
| `annual_savings_vs_gas` | Yearly saving compared with the Gas scenario |
| `annual_tCO2e` | Yearly emissions in tonnes of CO2-equivalent |
| `total_energy_kwh_day`, `solar_kwh_day`, `grid_kwh_day` | Daily energy use and where it comes from |
