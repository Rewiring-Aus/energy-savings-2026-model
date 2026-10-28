# Energy Savings 2026 Model — Methodology (v1)

*Draft, updated 28 October 2026 (first drafted 14 October). Supersedes `METHODOLOGY.md` (April 2026). Describes `energy_savings_model.R` and `battery_model.R` as they stand at 28 October 2026; Appendix C lists what changed since the 14 October draft. The change log is in `MODEL_CHANGE_HISTORY.md`. Scripts named here that are not in the GitHub repository (`Rewiring-Aus/energy-savings-2026-model`) are on the Rewiring Australia shared drive.*

---

## 1 Purpose and design principles

The model estimates how much an Australian household saves by replacing fossil-fuelled appliances and vehicles with efficient electric ones, with or without rooftop solar and a home battery. It reports costs over one year (an operating-cost snapshot at 2026 prices) and over 15 years (including the cost of buying the equipment).

These design choices run through every section below.

1. **Compare like with like, at the level of household functions.** The unit of comparison is a household *function*: space heating, space cooling, water heating, cooking or driving. It is not a particular product. Every option for a function is costed on the same basis: the same household, the same prices and the same horizon.
2. **Use measured, average energy use.** Energy use comes from survey and baseline-study data on what Australian households actually consume. It is not built up from rated efficiencies. Appliance efficiency is therefore already reflected in the inputs, and applying COPs or star ratings on top would count it twice (§3.1).
3. **Be conservative about the electric option.** Where an assumption is uncertain, the model leans toward understating savings. Examples: fossil homes stay on a flat tariff (§6.3); a grid-only electric home is not given off-peak pricing it couldn't realistically achieve (§6.3); 15% of EV charging is priced at public fast-charger rates (§3.4); every electrified home pays for a switchboard upgrade (§7.1); and a minimum share of every load is always bought from the grid (§8.4).
4. **Every kWh is accounted for once.** Solar, the Solar Sharer free window, the battery and the grid all compete to supply the same load. The model allocates each kWh to exactly one source, in a fixed order, and values any credit at the same rate that the kWh was billed at. Most of the bugs fixed in 2026 broke this rule (see `MODEL_CHANGE_HISTORY.md`, "Themes").
5. **Keep a single source of truth.** Tariffs are resolved in one function (`get_tariff_spec()`). Household loads are computed in one function (`compute_household_loads()`). Prices come from versioned CSVs, and every other default and modelling constant comes from `model_defaults.csv` (§2.6), not literals in the code. The interactive visualiser is a port of this model and is checked against it by an automated regression test (§12).

---

## 2 Model structure

### 2.1 Two views of the same household

The model answers two related questions through two entry points:

| View | Function(s) | Question answered |
|---|---|---|
| **Whole household** | `evaluate_household()` | What does it cost to run a typical gas home compared with an all-electric home, with and without solar and a battery? |
| **Per appliance** | `compare_space_heating()`, `compare_water_heating()`, `compare_cooktops()`, `compare_vehicles()` | For one function (e.g. hot water), how do the gas, LPG, resistive and heat-pump options compare? |

Both views are built from the same core function, `evaluate_option()`, which costs one appliance option for one household. The validation script `validate_compare_vs_household.R` checks that the two views reconcile (§12).

### 2.2 Household scenarios (whole-household view)

`evaluate_household()` costs six scenarios for the same household:

| Scenario | Heating | Hot water | Cooking | Vehicle(s) | Solar + battery |
|---|---|---|---|---|---|
| **Gas** *(baseline)* | Gas heater(s) | Gas instantaneous | Gas cooktop | Petrol | No |
| **Gas + solar** | Gas heater(s) | Gas instantaneous | Gas cooktop | Petrol | Yes (`solar`) |
| **LPG** | LPG heater(s) | LPG instantaneous | LPG cooktop | Petrol | No |
| **Electric (grid only)** | Reverse-cycle heat pump | Heat-pump HW | Induction | EV | No |
| **Electric (solar)** | Reverse-cycle heat pump | Heat-pump HW | Induction | EV | Yes (`solar`) |
| **Electric (solar optimised)** | Reverse-cycle heat pump | Heat-pump HW | Induction | EV | Yes (`solar_optimised`) |

Every scenario also has a **space cooling** row and an **"other electric loads"** row covering lighting, refrigeration, electronics, washing and other cooking appliances. These loads are electric in every home, so they are included to get the bill and the solar dispatch right, but their energy use is the same across scenarios.

The headline result is **annualised savings against the Gas scenario**:

$$\text{annual savings}_{s} = \frac{\text{total cost}_{\text{Gas}} - \text{total cost}_{s}}{\text{horizon (years)}}$$

### 2.3 Per-appliance comparisons and system contexts

Each `compare_*()` function prices every option for the function in up to three **system contexts**:

| Context | Meaning |
|---|---|
| `grid_only` | No solar, no battery |
| `solar` | A share of the appliance's load is met directly by rooftop solar (§8) |
| `solar_battery` | As `solar`, plus the home battery covers part of the remaining load |

Gas, LPG and petrol options cost the same in all three contexts because they use no electricity. That makes "gas vs heat pump in a solar + battery home" a straightforward subtraction.

The solar PV system and battery serve the whole house, not any one appliance. So the per-appliance rows carry **appliance capex only**. The system's capex and value are reported once, undivided, in two extra rows (`category = "solar"` and `category = "battery"`, `system_context = "system"`).

> [!WARNING]
> The per-appliance `solar` / `solar_battery` rows and the whole-house `system` rows describe the **same dollars under two different attributions**. They must never be added together. Use `whole_home_view()` (grid-only appliance rows + system rows, which reconciles with `evaluate_household()`) or `appliance_view()` (the per-appliance context rows on their own).

### 2.4 Horizons and cost components

| | 1-year | 15-year |
|---|---|---|
| Energy prices | Current 2026 prices | 15-year average forecast prices (§5.2) |
| Capital costs | Included (upfront or as one year of loan repayments) | Included (upfront or as 15 years of loan repayments) |
| Battery degradation | 4% (first-year rate) | 2.2% p.a. average |
| Inverter replacement | Not reached | Included (year 12) |

For each option or scenario the model reports energy use (kWh/day, split into solar, battery, free-window and paid-grid kWh), annual operating cost (energy + supply charges + vehicle maintenance), capex (net of subsidies), loan repayments and interest, total cost over the horizon, and annual emissions (t CO₂e).

### 2.5 Reference household

The model's defaults, and the standard runs in `energy_savings_comparisons.R`, use the following reference household:

| Parameter | Value | Basis |
|---|---|---|
| State | AUS (population-weighted national average) | `state_population_weights.csv` |
| Occupants | 2.7 | 2021 Census average household size |
| Dwelling | Detached house | — |
| Vehicles | The state average (AUS 1.88, from 1.66 in the ACT to 2.05 in WA), split evenly between an SUV and a hatchback | Vehicles per household by state (`state_average_values.csv`) |
| New EV models | BYD Sealion (SUV), BYD Dolphin (hatch), BYD Seal (sedan) | §7.2 |
| Driving distance | 36.4 km/day per vehicle (middle tier) | §3.4 |
| Solar / battery | 10 kW PV + 15 kWh battery (house); 2 kW + 5 kWh (apartment) | `DEFAULT_SYSTEM_SIZE` |
| Tariff | Time-of-use (`tou`) | §6 |
| Horizon | 15 years | §2.4 |
| Finance | Loan at 6% over 15 years | §9 |

> [!NOTE]
> When no vehicle count is given, the model uses the state's average vehicles per household (`household_n_vehicles = state_average` in `model_defaults.csv`), as the visualiser does. A number can be given instead.

### 2.6 Describing a household and setting defaults

A run is described once with `household()`, which records the state, occupants, dwelling, vehicles, tariff, horizon, finance and solar/battery system, and checks each value (an unknown state or misspelt tariff fails straight away). Anything not given is taken from `model_defaults.csv`. Derived values are resolved in one place: system size from the dwelling type (§8.6), battery degradation from the horizon (§8.7) and the number of years. `update(hh, ...)` returns a copy with some settings changed, for example `update(hh, state = "VIC")`. Every public function accepts the household object; the repository README lists every setting.

`model_defaults.csv` holds every default and modelling constant that is not a market price: the reference household, tariff rules (§6.3), finance (§9.2), battery and inverter parameters (§8.6–8.7), the apartment energy factor (§3.3), the fast-charging share (§3.4), switchboard and cooling-only capex (§7.1), and the interest rate for standalone solar LCOE pricing (§8.5). Each row has a description. Changing a value there changes it for every run. The per-load solar, free-window and `min_retail` shares (§8.2) are in their own file, `solar_fraction_table.csv`.

Model functions return full-precision values. Rounding (whole dollars, kWh and t CO₂e to 2 decimal places, $/kWh to 4) is applied only to output tables, so rounded figures never feed later calculations.

---

## 3 Energy use

### 3.1 Appliance energy

Average household energy use by appliance type and state (kWh/day) comes from `average_energy_use_by_appliance_and_state.csv`. This file is derived mainly from the [Australian and New Zealand Residential Baseline Study 2021](https://www.energyrating.gov.au/industry-information/publications/report-2021-residential-baseline-study-australia-and-new-zealand-2000-2040) and from the DISER building-stock survey.

These are **measured delivered-energy** figures. The efficiency gap between, say, a gas heater and a heat pump is already in the data. The model doesn't apply a separate COP or efficiency factor. The implied heat-pump-to-resistance ratios are about 3.9 for space heating (3.7–4.2 by state) and about 3.7 for water heating (3.4–3.9 by state). Applying the COPs in `Product energy performance.csv` on top of these figures would double-count efficiency, so that file is reference only.

| Function | Fossil options | Electric options |
|---|---|---|
| Space heating | Natural gas, LPG, wood | Electric resistance, reverse-cycle heat pump |
| Space cooling | — | Heat pump (split or reverse-cycle AC) |
| Water heating | Natural gas, LPG | Electric resistance (storage), heat pump |
| Cooktop | Natural gas, LPG | Electric resistance, induction |
| Vehicles | Petrol, diesel | Battery electric |

**Heating and cooling are costed as separate rows.** A reverse-cycle heat pump physically provides both, but costing them separately keeps the space-heating comparison fair (heating against heating). Fossil-heated homes are assumed to also own a split-system AC for cooling (§7.1). In the all-electric home, the heat pump bought for heating also does the cooling, so its cooling row has no extra capex.

### 3.2 Occupancy scaling

Energy use doesn't scale linearly with the number of occupants. Appliance energy is multiplied by a scaling factor derived from the [AER residential consumption benchmarks (2020)](https://www.aer.gov.au/industry/registers/resources/guidelines/electricity-and-gas-consumption-benchmarks-residential-customers-2020):

| Occupants | 1 | 2 | **2.7** | 3 | 4 | 5+ |
|---|---|---|---|---|---|---|
| Scaling factor | 0.56 | 0.90 | **1.00** | 1.03 | 1.07 | 1.37 |

Values in between are linearly interpolated. Vehicle energy isn't scaled by occupancy.

### 3.3 Dwelling type

Apartments use about 21% less energy than detached houses (ABS/AEMO). For apartments, all appliance energy is multiplied by 0.79 (`apartment_energy_factor` in `model_defaults.csv`). Vehicle energy is unaffected. Apartments also get a smaller default solar and battery system (§8.6) and may be eligible for apartment-specific solar subsidies (§7.3).

### 3.4 Vehicles

Vehicle energy is calculated as efficiency × distance:

$$E_{\text{vehicle}}\ (\text{kWh/day}) = \frac{\text{efficiency}_{\text{Wh/km}} \times \text{km/day}_{\text{state}}}{1000} \times \text{number of vehicles}$$

**Efficiency** (`vehicle_average_efficiency_by_class.csv`, national):

| Class | EV (Wh/km) | ICE (Wh/km) | ICE (≈ L/100 km petrol) |
|---|---|---|---|
| Hatchback / small car | 161.8 | 831.6 | 8.8 |
| Sedan | 173.9 | 1,036.8 | 10.9 |
| SUV | 190.8 | 1,019.2 | 10.7 |

EV efficiency is the average of manufacturer figures across models in each class. ICE efficiency is built in two steps (`ice_fleet_consumption_calibration.R`):
1. Inflate laboratory test figures by 1.16 to reflect real-world consumption (van Gijlswijk et al.).
2. Scale all petrol passenger vehicles multiplicatively so the fleet mean matches the [ABS Survey of Motor Vehicle Use (2020)](https://www.abs.gov.au/statistics/industry/tourism-and-transport/survey-motor-vehicle-use-australia/) average of 11.1 L/100 km. This keeps the relative spread between classes unchanged.

**Distance** (`average_km_per_day_by_state.csv`) has three tiers (`low` / `middle` / `high`). The default is `middle`: 36.4 km/day per vehicle nationally, from 33.1 (TAS) to 38.0 (VIC).

**Fleet composition.** `vehicle_class` sets the mix and `n_vehicles` sets the size. The count is spread evenly across the classes listed, so 1.88 vehicles over an SUV and a hatchback means 0.94 of each. Energy, maintenance and capex are all linear in this weight.

**EV charging mix.** 15% of EV energy (`fast_charge_fraction`) is assumed to come from public DC fast chargers (road trips, top-ups, households without off-street parking), priced at $0.65/kWh (Econnex 2025). Fast-charged kWh get no solar or free-window benefit and don't count toward the home supply-charge allocation. The other 85% is charged at home and priced on the household's tariff (§6).

---

## 4 Emissions

$$\text{annual emissions (t CO}_2\text{e)} = E_{\text{kWh/day}} \times 365 \times \text{EF}_{\text{kg CO}_2\text{e/kWh}} \div 1000$$

Emissions factors come from `emissions_factors_by_state.csv`, which is based on the National Greenhouse Accounts factors:

| Fuel | kg CO₂e/kWh |
|---|---|
| Grid electricity | State-specific: SA 0.26, TAS 0.23, WA 0.56, NT 0.65, NSW/ACT 0.67, QLD 0.76, VIC 0.87; AUS 0.69 |
| Natural gas | 0.20–0.23 by state (AUS 0.219) |
| LPG | 0.291 |
| Petrol | 0.305 |
| Diesel | 0.316 |
| Wood | 0.004 |

Accounting rules:
- Solar self-consumed directly or **via the battery** counts as zero-emission. Battery discharge is stored solar, so it is netted out of grid kWh in the household totals.
- Solar Sharer free-window kWh are still **grid** electricity, so they emit at the grid factor even though they cost nothing.
- The grid factor is a static, current-year average. The model doesn't project grid decarbonisation over the 15 years, so electric-option emissions are likely **overstated** over the horizon.

---

## 5 Energy prices

### 5.1 Current (2026) prices

`fuel_prices_by_state.R` compiles retail prices into `fuel_prices_by_state_simple.csv`. Each row has a volume price ($/kWh) and, where applicable, a daily supply charge ($/day).

| Fuel row | Source | AUS current | AUS 15-yr avg |
|---|---|---|---|
| `electricity` (flat) | "FINAL 2026 electricity prices by state", averaged across networks | 34.0 c/kWh + $1.33/day | 39.8 c + $1.56/day |
| `electricity_off_peak` | DMO 2026–27 TOU default offers (`TOU_tariffs_DMO2026.csv`) | 26.0 c/kWh | 30.5 c |
| `electricity_sso_*` | Solar Sharer standing offers, AER CDR extract | free 0 c; off-peak 28.2 c; peak 52.1 c; $1.97/day | — ; 33.0 c; 61.0 c; $2.30/day |
| `solar_export` (FiT) | Retailer feed-in tariffs, by state | 3.46 c/kWh | 3.46 c (held flat) |
| `gas` | "FINAL 2026 gas prices by state" (per MJ → per kWh at 3.6 MJ/kWh) | 16.9 c/kWh + $0.72/day | 21.1 c + $0.90/day |
| `lpg` | "FINAL 2026 LPG prices by state" | 25.4 c/kWh + $0.29/day | 31.8 c + $0.37/day |
| `petrol` | AIP annual retail prices, CY2025; 9.5 kWh/L (34.2 MJ/L) | 19.5 c/kWh | 22.0 c |
| `diesel` | AIP annual retail prices, CY2025; 10.7 kWh/L (38.6 MJ/L) | 19.2 c/kWh | 21.7 c |
| `ev_fast_charge` | Econnex 2025 (national) | 65.0 c/kWh | 76.0 c |

Notes:
- The **AUS** row is the population-weighted average of the states (`state_population_weights.csv`).
- AIP doesn't report the **ACT** separately for petrol or diesel, so NSW prices are used.
- The **NT** has no reticulated gas. It has no gas row, its `annual_savings_vs_gas` is NA, and LPG is the relevant fossil comparison there.
- **Solar Sharer** covers only NSW, QLD (SEQ), SA and the ACT (proxied from NSW). There are no standing offers in the VIC or TAS feeds, and WA and the NT are outside the NEM.

### 5.2 15-year price forecasts

The 15-year figure is the average of a geometric price path:

$$P_t = P_0 (1 + g)^{t-1}, \qquad \bar{P}_{15} = \frac{1}{15}\sum_{t=1}^{15} P_t$$

The growth rate $g$ for each fuel is its **real** price inflation, the national since-2000 average of (energy CPI sub-group YoY − all-groups CPI YoY) from ABS CPI data:

| Fuel | CPI sub-group | Real growth p.a. | 15-yr avg ÷ current |
|---|---|---|---|
| Electricity (all bands, fast charging) | Electricity | 2.2% | 1.17 |
| Gas, LPG | Gas and other household fuels | 3.1% | 1.25 |
| Petrol, diesel | Automotive fuel | 1.7% | 1.13 |
| Feed-in tariff | — | 0% (held flat) | 1.00 |

Daily supply charges grow at the same rate as their fuel. The national since-2000 series is used rather than state or capital-city series because it is longer (~100 quarterly observations) and isn't distorted by state-specific rebates.

> [!IMPORTANT]
> **Real vs nominal — needs a decision.** The header of `fuel_prices_by_state.R` and the April draft describe the growth rate as *nominal* (real + all-groups CPI ≈ 5.1% for electricity). The code actually applies only the **real** rate. A `nominal_growth_pct` column is calculated but not used. So 15-year energy prices are effectively in constant 2026 dollars, while the 6% loan rate (§9) is nominal. This mismatch probably *understates* electrification savings, because fuel savings are deflated and repayments aren't. Either the script comment or the method should be changed to make the two consistent before publication.

---

## 6 Electricity tariffs

### 6.1 The single tariff switch

Every price that depends on the household's retail plan is resolved in one place, `get_tariff_spec()`, from a single `tariff` argument:

| Tariff | Appliance and EV imports | Daily supply charge | Free window | Evening battery export valued at |
|---|---|---|---|---|
| `flat` | Flat rate | Flat-rate charge | No | FiT |
| `tou` *(default)* | Off-peak rate | Flat-rate charge | No | FiT |
| `amber` | Off-peak rate | Flat-rate charge | No | Wholesale 4–8 pm price, tiered by inverter size, with the FiT as a floor |
| `solar_sharer` | Solar Sharer off-peak rate | Solar Sharer charge (higher) | 11 am–2 pm at $0, capped at 24 kWh/day | FiT |

`solar_sharer` falls back to `tou` (with a warning) in states that don't offer it. Results report the tariff actually used.

**The TOU convention.** Under `tou`, every grid import is priced at the off-peak rate. The assumption is that an electrified household with a battery, a hot-water timer and a schedulable EV can move its discretionary load out of the peak. This is a simplification: the peak-rate rows in `TOU_tariffs_DMO2026.csv` are not used. Because it is generous, the model limits it to the scenarios where it's realistic (§6.3).

### 6.2 Solar Sharer

Solar Sharer (from 1 July 2026) gives up to 24 kWh/day free between 11 am and 2 pm, in exchange for a higher daily charge (e.g. NSW $2.13/day against $1.39 on a flat plan). The higher charge is always billed with the free window, so the plan never looks free.

A literal "free up to 24 kWh/day" would cover 90%+ of an all-electric household's daily load and cut modelled bills by 75–80%, which isn't realistic. Instead, each load can move only a limited share of its demand into a three-hour window (the `sso_*` columns of `SOLAR_FRACTION_TABLE`, §8.2). That share rises with how deliberately the household shifts load. If the total eligible demand exceeds 24 kWh/day, every load is scaled back pro rata.

The Vehicles row of the free-window table is the most sensitive single assumption. Moving it from 0 to 0.8 shifts annual savings by about $1,050 in a grid-only home.

### 6.3 Which scenarios get which tariff

The requested tariff is applied only where the household could realistically use it:

| Scenario | Tariff applied | Why |
|---|---|---|
| Gas, Gas + solar, LPG | **Always `flat`** (`fossil_baseline_tariff`) | A gas home's remaining electric load is baseload plus evening cooling, which is hardly shiftable, so it would rationally stay on a flat plan. Leaving it on TOU flattered the baseline by about $325/yr. |
| Electric (grid only) | **Always `flat`** (`grid_only_tariff`) | Without a battery, the home can't move its whole load off peak. Billing it all at off-peak made solar + battery look worth about $0/yr. |
| Electric (solar), Electric (solar optimised) | The requested tariff | Solar, the battery and timers make load shifting achievable. |

These overrides apply in `evaluate_household()` and are set in `model_defaults.csv`. The per-appliance `compare_*()` functions bill every row at the requested tariff.

---

## 7 Capital costs

### 7.1 Appliances

Installed prices come from `Energy Savings 2026 - input data - FINAL product prices.csv` (AUS level):

| Item | Installed cost | Units |
|---|---|---|
| Gas heater (~7 kW) | $1,500 | × heaters per home |
| LPG heater | $1,500 | × heaters per home |
| Resistive heater | $220 | × 2 × heaters per home (each covers a smaller zone) |
| Reverse-cycle AC (~7 kW) | $3,200 | × heaters per home |
| Split AC, cooling only (fossil homes) | $2,000 | 1 |
| Gas / LPG instantaneous water heater | $1,900 | 1 |
| Electric storage water heater | $2,600 | 1 |
| Heat-pump water heater | $3,900 | 1 |
| Gas cooktop | $1,200 | 1 |
| Electric (resistive) cooktop | $1,000 | 1 |
| Induction cooktop | $2,200 | 1 |
| Switchboard upgrade (all-electric scenarios) | $2,500 | 1 |

**Heaters per home** (`Heater_numbers_by_state.csv`) is 1 in QLD and the NT; 2 in NSW, SA, WA and AUS; and 3 in VIC, TAS and the ACT. It scales heating **capex only**. Heating energy is already a whole-of-household figure.

The **switchboard upgrade** is charged to every all-electric scenario, even though many homes won't need one. This is a deliberately conservative choice. An EV charger isn't costed separately.

Equipment is assumed to be bought new in every scenario, including the Gas baseline. The model therefore compares the full cost of two equipment sets, which fits a household whose existing appliances are due for replacement. It isn't the marginal cost of an early swap-out.

### 7.2 Vehicles

Vehicle prices come from `vehicle_costs_2026.csv` (weighted-average base prices):

| Class | New ICE | New EV (BYD model used) | Used EV |
|---|---|---|---|
| Hatchback / small car | $30,990 | $29,990 (Dolphin) | $21,182 |
| Sedan | $36,040 | $52,990 (Seal) | $30,000 |
| SUV | $42,204 | $54,990 (Sealion) | $41,562 |

New EVs are priced at the representative BYD model in each class. Fleet capex is the price × the fleet weight (§3.4).

**Annual maintenance** (`Vehicle_maintenance.csv`) covers servicing, tyres and consumables. Registration and insurance are excluded.

| Class | ICE | EV |
|---|---|---|
| Hatchback | $750 | $510 |
| Sedan | $790 | $640 |
| SUV | $790 | $800 |

### 7.3 Subsidies

State subsidies (`Appliance_subsidies.csv`) are deducted from capex. Where several apply, the most generous applicable row is used; they are not stacked.

| State | Item | Value | Eligibility |
|---|---|---|---|
| VIC | Heat pump (space or water heating) | $1,000 | All |
| VIC | Solar PV | $1,400 / $2,400 | All / apartments |
| NSW, ACT | Solar PV | $2,400 | Apartments |
| QLD | Solar PV | $3,500 | Landlords |
| NSW | Battery | $50/kWh | VPP participation |
| WA | Battery | $130/kWh / $380/kWh | VPP + Synergy / VPP + Horizon (from postcode) |

The federal battery discount is built into the "subsidised" battery price (§8.6), not into this table. Federal small-scale technology certificates (STCs) are assumed to be reflected in the installed PV price.

---

## 8 Solar, the free window and batteries

### 8.1 Solar scenarios

Each household is assigned one solar scenario. It sets the share of each load that can be met directly from rooftop solar:

| Scenario | Meaning |
|---|---|
| `grid_only` | No solar |
| `solar` | Rooftop PV, no deliberate load shifting |
| `solar_optimised` | Rooftop PV plus active load shifting (hot-water timers, daytime heat-pump pre-heating, daytime EV charging) |

### 8.2 Per-load fractions (`solar_fraction_table.csv`)

| Load | Solar: `solar` | Solar: `solar_optimised` | Free window: grid only | Free window: solar | Free window: optimised | `min_retail` floor |
|---|---|---|---|---|---|---|
| Water heating (heat pump) | 50% | 90% | 20% | 35% | 50% | 10% |
| Water heating (resistive) | 40% | 70% | 15% | 28% | 50% | 10% |
| Space heating | 15% | 30% | 5% | 10% | 15% | 10% |
| Space cooling | 40% | 65% | 15% | 22% | 30% | 10% |
| Cooktop | 10% | 10% | 5% | 5% | 5% | 10% |
| Other cooking (fridge, dishwasher, etc.) | 25% | 25% | 5% | 8% | 10% | 10% |
| Other electronics | 33% | 33% | 12% | 13% | 15% | 10% |
| Vehicles (home-charged share) | 10% | 25% | 25% | 40% | 55% | 10% |

How to read the table:
- **Solar columns:** the share of each load that falls in the solar generation window (about 8 hours).
- **Free-window columns:** the share that can be drawn in Solar Sharer's 3-hour window. These are lower than the solar shares for everything except vehicles: a parked EV can soak up a 3-hour block more easily than it can spread charging across the whole day. Unlike the solar columns, `grid_only` isn't zero, because a household without panels can still use the free window, just less deliberately.
- **`min_retail`:** the minimum share of each load that is always bought from the grid at the standard rate. This covers round-the-clock draw (refrigeration, standby, overnight heating) that storage can't reliably carry every day of the year.

These fractions are modelling judgements, not measured values. They are the most important assumptions to test with sensitivity runs (§11).

### 8.3 Solar generation and the generation cap

Daily PV generation is calculated as:

$$S = \text{capacity factor}_{\text{state}} \times 24 \times \text{kWp}$$

Capacity factors come from `solar_lcoe_by_state.csv` (the same at every finance rate): AUS 0.183, ranging from TAS 0.144 to the NT 0.241. A 10 kW system in the AUS case generates about 43.9 kWh/day.

If the loads' combined demand for solar (load × solar fraction) exceeds daily generation, every load's solar share is scaled back pro rata (`solar_scale_factor`). This mostly matters for small apartment systems.

### 8.4 Dispatch order

Each electric load is supplied in this fixed order:

1. **Public fast charging** (EVs only): 15% of EV energy, which never reaches the home meter.
2. **Solar Sharer free window** (`solar_sharer` only): $0, no opportunity cost.
3. **Direct solar**: $0 at the point of use.
4. **Home battery** (evening discharge of stored solar).
5. **Paid grid** at the tariff's import rate.

Throughout, the **`min_retail` floor** limits steps 2–4 together to at most (1 − min_retail) = 90% of each load.

The free window goes before solar because a free-window kWh has no opportunity cost. A solar kWh used directly is one that can't charge the battery or be exported. This order also keeps the calculation non-circular, since free-window demand doesn't depend on how much solar there is.

The household-level free-window, solar and battery allocations are each calculated once, in shared helpers (`get_household_free_window()`, `get_household_self_sufficiency()`, `get_household_min_retail()`). The appliance-cost side and the solar/battery side both read the same values.

### 8.5 How self-consumed solar is costed

**Whole-household view.** Self-consumed solar kWh cost $0 at the appliance level. The PV system's capex is added once at the household level (§8.6). Exports earn the FiT. There is no "forgone FiT" charge on self-consumed kWh, because the export flow only applies to kWh that are actually exported. Supply charges are unaffected by solar: they depend on being connected, not on how much is drawn.

**Standalone appliance calls.** When `evaluate_option()` is called on its own with solar (`solar_marginal_cost = TRUE`), self-consumed kWh are charged at the state's solar LCOE, at a 6% finance rate (`solar_lcoe_interest_rate`; the LCOE table also has 0%, 5.5%, 7% and 8% rows). This spreads the panel cost over the kWh, because a single-appliance comparison has no other place to put it. Neither `evaluate_household()` nor the `compare_*()` context rows use this mode.

**Value of the solar system** (`compare_*()` "solar" row) is the household's actual avoided grid cost. It is calculated as the household's energy cost on grid only minus its energy cost with solar, at the tariff's import rates (not flat retail), plus FiT export income.

### 8.6 Solar and battery system costs

| Component | Rule | AUS, 10 kW / 15 kWh house | AUS, 2 kW / 5 kWh apartment |
|---|---|---|---|
| PV panels | $/kW by state × kWp (AUS $690/kW; ACT $533 to NT $1,490) | $6,903 | $1,381 |
| Inverter (> 5 kWp) | One string inverter, $1,800, shared by PV and battery; replaced in year 12 | $1,800 + $1,800 | — |
| Inverter (≤ 5 kWp) | Microinverters at $300/kWp; 50% replaced in year 12 | — | $600 + $300 |
| Battery | $620/kWh with PV (subsidised); $940/kWh + $2,300 installation if retrofitted without PV | $9,300 | $3,100 |
| Subsidies | §7.3 | — | NSW/VIC/ACT −$2,400 |

The battery has no separate installation charge when it is installed with PV, because the crew is already on site. The year-12 inverter replacement is financed from year 0 along with the rest of the system. This is a simplification.

Inverter capacity, which limits evening export, is 5 kW, or 10 kW for systems of 10 kWp and above.

### 8.7 Battery model (`battery_model.R`)

The battery is modelled as a **typical day in each of four seasons**. Seasonal solar weights are spring 2, summer 3, autumn 2 and winter 1, normalised to a mean of 1 (multipliers 1.0 / 1.5 / 1.0 / 0.5). Results are annualised at 365/4 days per season.

**Effective capacity:**

$$C_{\text{eff}} = C_{\text{rated}} \times u \times \frac{1 + (1-d)^{t}}{2}$$

- $u$ = useable capacity, 0.9
- $d$ = annual degradation: 4% for the 1-year horizon, 2.2% for 15 years
- $t$ = horizon in years

The last term is the linear average of start-of-life and end-of-horizon capacity. For a 15 kWh battery over 15 years this gives about 11.6 kWh.

**Daily flows (each season):**

1. Load met by direct solar: $L_{\text{solar}} = \min(E \times s,\ S_{\text{season}})$, where $s$ is the household self-sufficiency from §8.4.
2. Stored solar: $E_{\text{stored}} = \min\big((S_{\text{season}} - L_{\text{solar}}) \times \eta,\ C_{\text{eff}}\big)$, with round-trip efficiency $\eta = 0.90$ applied on charge.
3. Exported solar: $S_{\text{season}} - E_{\text{stored}}/\eta - L_{\text{solar}}$, valued at the FiT.
4. The usable stored pool, $E_{\text{stored}} \times (1 - \text{safeguard})$ (safeguard default 0), is **split three ways with no overlap**:
   - **(a) House load** that solar didn't cover: appliance load only, net of free-window and `min_retail` kWh. Credited at the tariff's import rate.
   - **(b) EV charging**, up to the EV's unmet home-charging load. Credited at the rate the EV would otherwise have paid.
   - **(c) Evening export.** Valued at the FiT, or for `amber` at tiered 4–8 pm wholesale prices (`evening_peak_prices_monthly.csv`, seasonal means, at most one inverter-capacity of kWh per peak hour, FiT as a floor).

   Between (b) and (c), the surplus goes to whichever is worth more at current prices. The EV's avoided import (24–33 c/kWh) always beats evening export (2–14 c/kWh), so in practice it goes to the EV first.

**Why the credits net to zero.** `evaluate_option()` has no visibility of the battery, so it bills every non-solar kWh as a grid import. The battery credits in (a) and (b) undo exactly that charge for the kWh the battery actually supplied. They are struck at the same rate the kWh were billed at, so they cancel rather than create income. A kWh can't be credited twice, because the free window, direct solar and battery each draw only on load the previous step left.

**Grid-to-battery arbitrage** (charging from the grid off-peak to discharge at peak) isn't modelled. All battery charge comes from solar.

`free_battery_kwh` can add storage at zero capex to represent vehicle-to-grid use of the EV's battery. It is 0 in standard runs.

---

## 9 Supply charges and financing

### 9.1 Supply charges

- **Whole-household view.** Each connected fuel is charged one full daily supply charge × 365. Every scenario pays the electricity charge, because lights and the fridge keep every home grid-connected; under `solar_sharer` it is the higher Solar Sharer charge. Gas and LPG charges apply only if the scenario uses that fuel. **Dropping the gas connection is therefore a real saving**: about $330/yr in NSW.
- **Per-appliance view.** Each option is allocated a share of its fuel's supply charge in proportion to its share of household consumption of that fuel. The electricity denominator is a fully electrified household, and includes home EV charging for the vehicles row. The gas denominator is a household using gas for heating, hot water and cooking. This lets single appliances be compared on the same basis, but per-appliance supply charges won't add up to the whole-household figure.

### 9.2 Financing

By default (`use_loan = TRUE`), capex (appliances, switchboard, PV, battery) is replaced by repayments on a standard amortising loan:

$$\text{annual repayment} = 12 \times P\,\frac{r}{1-(1+r)^{-n}}, \quad r = \frac{\text{rate}}{12},\ n = 12 \times \text{term}$$

The model default is **6% over 15 years**, which matches the horizon, so the loan is fully repaid within it. Total interest is reported separately. When `use_loan = FALSE`, capex is counted upfront in full. Neither option discounts future cash flows, and no residual value is credited for equipment still working at year 15. See §5.2 for the interaction between the nominal loan rate and real energy prices.

---

## 10 Total cost calculation (whole household)

For each scenario $s$ over horizon $T$ (1 or 15 years):

$$
\begin{aligned}
\text{total cost}_s =\ & \sum_{\text{rows}} \big(\text{capex or repayments}\big) + T \sum_{\text{rows}} \big(\text{paid energy} + \text{maintenance}\big) \\
& + \text{PV capex} + \text{battery capex} + \text{switchboard capex} \quad (\text{or their repayments}) \\
& + T \times \text{supply charges} \\
& - T \times \big(\text{FiT export} + \text{evening export} + \text{battery house credit} + \text{battery EV credit}\big)
\end{aligned}
$$

The rows are heating, cooling, hot water, cooktop, vehicles and other electric loads.

The summary also reports `annual_solar_self_value`, which is self-consumed kWh × flat retail. It is for **reporting only** and is not part of total cost, because those savings already show up as lower appliance bills.

**Why savings can move in unexpected directions.** The Gas baseline isn't electricity-free: it uses about 9.5 kWh/day for cooling and baseload. Any assumption that makes always-electric load cheaper lowers *both* bills and can *shrink* the savings figure. Report absolute costs alongside savings so this isn't mistaken for an error.

---

## 11 Key assumptions, limitations and open issues

### 11.1 Assumptions with the most leverage

| Assumption | Where | Sensitivity note |
|---|---|---|
| Per-load solar, free-window and `min_retail` fractions | §8.2 | Judgement-based. The Vehicles free-window row moves savings by about $1,050/yr. |
| TOU = all imports off-peak (for solar/battery homes) | §6.1 | Generous. Moderated by the flat-tariff rules in §6.3. |
| Fossil and grid-only homes on flat tariff | §6.3 | Each worth several hundred $/yr to the savings figure. |
| Vehicle count and class mix | §3.4, §7.2 | SUV EV costs about $12.8k more than the ICE SUV; the hatchback EV is about $1k cheaper. Fleet mix drives the capex side. |
| Real price growth (electricity 2.2%, gas 3.1%, petrol 1.7%) | §5.2 | Gas rising faster than electricity favours electrification over 15 years. |
| Constraint order | §8.4 | `min_retail`, the 24 kWh free cap and battery capacity all compete for the same load. Whichever binds first hides the others, so the same parameter can be worth $1/yr in one household and $280/yr in another. Sensitivity tables should state the household they were run on. |

### 11.2 Known limitations

- **Average households only.** Energy use is the state average for each appliance type, scaled by occupancy and dwelling type. Climate zones within a state, building fabric and individual behaviour aren't modelled.
- **Static grid emissions factor.** Grid decarbonisation isn't projected (§4).
- **Seasonal-average battery model.** Four typical days. Weather variability, multi-day cloudy periods and location-specific seasonal profiles aren't captured.
- **No discounting or residual value** (§9.2).
- **Amber import exposure.** Amber's 4–8 pm import price isn't modelled, so as built Amber can't come out worse than TOU. Close this before publishing Amber-specific claims.
- **TOU peak rates unused.** The DMO peak and shoulder legs, and the Solar Sharer peak leg, are in the data but not applied.

### 11.3 Open issues to resolve before publication

1. **Real vs nominal price forecasts** (§5.2). The code and its documentation disagree, and the energy prices aren't consistent with the nominal loan rate.
2. **`ev_tariff_share` is currently a no-op.** `get_tariff_spec()` sets the EV rate equal to the appliance import rate under every tariff. A dedicated EV tariff row is needed for this parameter to have any effect.
3. **State-level ICE calibration isn't used.** `ice_fleet_consumption_calibration.R` writes `ice_consumption_by_state.csv`, but the model reads only the national ICE Wh/km. Either wire it in or describe ICE efficiency as national only (as above).
4. **`min_retail` interpretation.** If the floor means "the stored energy isn't there at 4 am", it should arguably reduce the battery's dispatchable pool rather than reroute that energy to the EV (about $190/yr difference).
5. **No used ICE vehicle prices.** `vehicle_costs_2026.csv` has used EV prices but no used petrol or diesel rows, so a used-vehicle comparison prices the fossil car at $0. Only the default (new) comparison is valid until these are added.

---

## 12 Quality assurance

| Check | What it does |
|---|---|
| `validate_compare_vs_household.R` | Rebuilds the whole-household total from per-appliance and system rows across 256 combinations (7 states × 3 occupancies × 2 horizons × loan on/off × 3 tariffs, plus 4 size and class variants) and compares total cost, annual opex and savings vs Gas with `evaluate_household()`. Must agree within $1; the two paths agree to floating-point precision. |
| `test_household_object.R` | Checks that a `household()` object gives exactly the same results as the same settings passed as arguments, for every public function, and that invalid settings are rejected. |
| `energy_savings_tests.R` | Unit-level checks on the model functions. |
| Visualiser parity (`npm run regression`) | Runs the R model and the TypeScript port over 14 scenarios × 11 output columns. Fails on any cell differing by more than $20 over the horizon. |
| Energy-balance invariants | The battery's three outputs split the usable stored pool exactly (checked to 3.6 × 10⁻¹⁵). Credited kWh equal billed kWh. |

---

## Appendix A — Input files

All in `Research/Data/Clean data - for internal use/Energy savings report data/Model input data/` on the shared drive, and in `data/` in the GitHub repository.

| File | Used for | Built by |
|---|---|---|
| `average_energy_use_by_appliance_and_state.csv` | Appliance kWh/day by state | Residential Baseline Study / DISER |
| `energy_consumption_scaling_factors.csv` | Occupancy scaling | AER benchmarks |
| `fuel_prices_by_state_simple.csv` | All retail prices, supply charges, FiT, 15-yr forecasts | `fuel_prices_by_state.R` |
| `TOU_tariffs_DMO2026.csv` | Off-peak rates | AER DMO 2026–27 |
| `evening_peak_prices_monthly.csv` | 4–8 pm wholesale prices (Amber export) | `evening_peak_export_prices.R` |
| `solar_lcoe_by_state.csv` | PV $/kW, capacity factor, inverter cost, LCOE | `solar_lcoe_by_state.R` |
| `Energy Savings 2026 - input data - FINAL product prices.csv` | Appliance, battery and installation capex | Product price research |
| `Appliance_subsidies.csv` | State subsidies | Program websites |
| `Heater_numbers_by_state.csv` | Heater count per home (capex) | — |
| `vehicle_costs_2026.csv` | Vehicle prices | `vehicle_prices_sep2026_update.R` |
| `vehicle_average_efficiency_by_class.csv` | EV and ICE Wh/km | `ice_fleet_consumption_calibration.R` |
| `average_km_per_day_by_state.csv` | Driving distance tiers | ABS SMVU |
| `Vehicle_maintenance.csv` | Annual vehicle maintenance | — |
| `emissions_factors_by_state.csv` | Emissions factors | National Greenhouse Accounts |
| `state_population_weights.csv` | AUS averages | ABS |
| `state_average_values.csv` | Vehicles per household by state (default vehicle count) | `state_average_values.R` (ABS) |
| `model_defaults.csv` | Defaults and modelling constants (§2.6) | Set by hand |
| `solar_fraction_table.csv` | Per-load solar, free-window and `min_retail` shares (§8.2) | Set by hand |
| `WA_postcode_network.csv` *(optional)* | WA battery subsidy network | — |

## Appendix B — Main changes from the April 2026 draft

- Added the whole-household scenarios, the per-appliance system contexts and the attribution rules (§2).
- Added the tariff framework, Solar Sharer, and the flat-tariff rules for fossil and grid-only homes (§6).
- Changed the solar fractions: water heating `solar_optimised` 85% → 90%; vehicles 0% → 10% / 25%; added free-window and `min_retail` columns (§8.2).
- Rewrote the dispatch order and battery dispatch, including the EV credit and price-based routing of surplus solar (§8.4, §8.7).
- Battery capacity now uses 90% useable capacity and averages degradation over the horizon. The April draft's default of 1.0 was out of date.
- PV is sized from kWp × state capacity factor and costed per kW, with microinverters for systems of 5 kWp and below and a default system size that depends on dwelling type (§8.6). This replaces the flat $5,500 PV figure.
- Supply charges are now one charge per connected fuel in the whole-household view (§9.1).
- The default loan is now 6% / 15 years (was 7% / 10).
- Corrected the emissions factors to match the input CSV: gas 0.219 (was 0.19), petrol 0.305 (was 0.24), diesel 0.316 (was 0.25), LPG 0.291 (was 0.22), wood 0.004 (was 0.40).
- Added vehicle maintenance, BYD reference models, fast-charging share, switchboard upgrade, cooling-only AC for fossil homes, heaters per home and state subsidies.
- Price forecasts are now described as real-terms, matching the code, with the inconsistency flagged (§5.2).

## Appendix C — Changes since the 14 October draft

- Defaults and modelling constants moved out of the code into `model_defaults.csv`, and the per-load fractions into `solar_fraction_table.csv` (§2.6). No numbers changed.
- Runs are described with a validated `household()` settings object (§2.6).
- When no vehicle count is given, the model uses the state's average vehicles per household rather than a flat 1.8 (§2.5).
- Loans are on by default (`use_loan = TRUE`), matching the standard runs (§9.2).
- Standalone solar LCOE pricing uses a 6% finance rate rather than 8%, matching the loan rate (§8.5).
- Rounding is applied only to output tables. Previously some rounded values fed later calculations, which moved results by at most $2/yr (§2.6).
- The validation check now passes within $1 rather than $10 (§12).
