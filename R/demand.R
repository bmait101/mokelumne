library(tidyverse)
library(here)
library(scales)

path_demand <- here("data", "CAISO-netdemand-20230719.csv")

demand <- read_csv(path_demand)

demand <- demand |>
  rename("variable" = `Net Demand 07/19/2023undefined`) |> 
  pivot_longer(-variable, names_to = "time", values_to = "MW") |> 
  mutate(time = case_when(
    time == "00:00...2" ~ "00:00", 
    time == "00:00...290" ~ "23:59", 
    TRUE ~ time
  )) 

demand$time <- as_date(hm(demand$time), origin = "2023-07-19")


demand |> 
  ggplot() + 
  geom_line(aes(x = time, y = MW, group = variable)) + 
  scale_x_datetime(breaks = date_breaks("1 hour"), labels = date_format("%H"))
  