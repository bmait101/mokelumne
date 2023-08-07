

library(tidyverse)
library(eia2)

# set API key if not already set globally
Sys.getenv("EIA_KEY")
eia_get_key()

# explore available datasets
eia2()

# explore routes within datasets
eia2("electricity")

# explore power ops data
# explore routes within datasets
eia2("electricity/electric-power-operational-data")
# facets = location, sector, fueltype

elec_power_generation <- 
  eia2(
    "electricity/electric-power-operational-data",
    frequency = "monthly",
    data_cols = c("generation")
  )
distinct(elec_power_generation, sectorid, sectorDescription) |> arrange(sectorid)
distinct(elec_power_generation, fueltypeid, fuelTypeDescription) |> print(n=Inf)

# electricity generation from residential sections, all solar
elec_power_generation_hps <- 
  eia2(
    "electricity/electric-power-operational-data",
    frequency = "monthly",
    data_cols = c("generation"),
    facets = list(
      location = c("WI","CA"),
      sectorid = c("8"),   # 
      fueltypeid = c("TSN")  # estimated total solar
    ) 
    )

elec_power_generation_hps |> 
  mutate(period = parse_date_time(elec_power_generation_hps$period, "ym")) |> 
  ggplot(aes(x = period, y = generation)) + 
  geom_line() + 
  labs(x = "", y = "Thousand MWh") + 
  facet_wrap(vars(location), scales = "free")


# Energy consumption 
eia2()
eia2("total-energy")

df_energy <- 
  eia2(
    "total-energy",
    frequency = "annual",
    data_cols = c("value")
  )

sources <- c(
  "Nuclear Electric Power Consumption/Production in Quadrillion Btu", 
  "Petroleum Consumption (Excluding Biofuels) in Quadrillion Btu", 
  "Natural Gas Consumption (Excluding Supplemental Gaseous Fuels) in Quadrillion Btu", 
  "Coal Consumption in Quadrillion Btu",
  "Total Renewable Energy Consumption in Trillion Btu"
)

msns <- df_energy |> 
  filter(seriesDescription %in% sources) |>
  distinct(msn, seriesDescription) |> 
  pull()

df_energy2 <- 
  eia2(
    "total-energy",
    frequency = "annual",
    data_cols = c("value"), 
    facets = list(
      msn = msns
    )
  )

xref <- tibble(
  msn = msns, 
  label = c("renewable","petroleum","nuclear","natural gas","coal")
)

# prep data
df_energy2 <- df_energy2 |> 
  left_join(xref, by = "msn") |> 
  mutate(label = factor(label, levels = c("renewable","nuclear","petroleum","natural gas","coal"))) |> 
  mutate(value = as.double(value)) |> 
  mutate(value = value*0.001) 

# plot
df_energy2 |> 
  ggplot(aes(period, value, fill = label)) + 
  geom_area() + 
  MetBrewer::scale_fill_met_d("Degas", direction = -1) + 
  scale_x_continuous(breaks = seq(1950, 2020, 10)) + 
  labs(x = "", y = "", fill = "Energy source",
       title = "U.S. primary energy consumption by energy source", 
       subtitle = "quadrillion British thermal units", 
       caption = "Data source: U.S. EIA") + 
  theme_classic() + 
  theme(
    plot.title.position = "plot"
  )


