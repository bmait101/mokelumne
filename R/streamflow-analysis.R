## Summarize and analyze stream flow data for study streams
## Bryan Maitland
## Created: 1 Nov 2023 | Updated: 6 Nov 2023

## Prep ===========

library(tidyverse)
library(dataRetrieval) 
library(fasstr) 
library(sf) 
library(leaflet)
library(mapview)


## Pull data from NWIS =====

# # Create function to download NWIS data and convert to metric for use with fasstr
# readNWISdv_clean <- function(x){
#   # pull daily streamflow data for given site numbers
#   data <- readNWISdv(
#     siteNumbers = x, 
#     parameterCd = "00060"  # discharge param code
#   )
#   
#   # tidy up
#   data <- data |> select(-agency_cd)
#   names(data)[names(data) == 'site_no'] <- 'STATION_NUMBER'
#   names(data)[names(data) == 'X_00060_00003'] <- 'Value'
#   names(data)[names(data) == 'X_00060_00003_cd'] <- 'Symbol'
#   
#   # convert cfs to cms
#   data$Value <- data$Value * 0.028317
#   
#   # get basin areas
#   basin_area <- readNWISsite(siteNumbers = x)
#   basin_area$Basin_Area_sqkm <- basin_area$drain_area_va * 2.58999
#   basin_area <- data.frame(STATION_NUMBER = basin_area$site_no,
#                            Basin_Area_sqkm = basin_area$Basin_Area_sqkm)
#   # merge basin area back to data
#   data <- merge(data, basin_area, by = "STATION_NUMBER")
#   
#   return(data)
# }
# 
# # List of sites
# site_numbers <- c(
#   "11314000",  # TIGER C PH COND BL SALT SPRINGS DAM CA
#   "11314500",  # NF MOKELUMNE R BL SALT SPRINGS DAM CA
#   "11315000",  # COLE C NR SALT SPRINGS DAM CA
#   "11315030",  # COLE C BL DIV DAM NR SALT SPRINGS DAM CA
#   "11315900",  # BEAR R BL LO BEAR R DAM CA
#   "11316100",  # BEAR R BL BEAR R DIV DAM CA
#   "11316670",  # NF MOKELUMNE R BL TIGER C RES NR WEST PT CA
#   # "11317000",  # MF MOKELUMNE R A WEST POINT CA
#   # "11318500",  # SF MOKELUMNE R NR WEST POINT CA 
#   "11319500",  # MOKELUMNE R NR MOKELUMNE HILL CA (above Parde Res)
#   "11325500"  # MOKELUMNE R A WOODBRIDGE CA (below Camanche Res, near Lodi)
# )
# 
# # download site metadata
# metadata_sites <- 
#   readNWISsite(siteNumbers = site_numbers) |> 
#   as_tibble()
# 
# # download daily discharge data
# data_usgs <- 
#   readNWISdv_clean(site_numbers)
# 
# # check data download
# head(metadata_sites)
# head(data_usgs)
# 
# # make a site number - site name xref
# xref <- metadata_sites |> select(site_no, station_nm)
# 
# # Add names, water year and date variables
# data_usgs <- data_usgs |> 
#   fill_missing_dates(water_year_start = 10) |>
#   add_date_variables()
# 
# data_usgs <- data_usgs |> 
#   left_join(xref, by = c("STATION_NUMBER" = "site_no")) |>
#   relocate(station_nm, .after = STATION_NUMBER)
# 
# # Convert to list for each site
# data_usgs_lst <- data_usgs |> 
#   group_by(STATION_NUMBER) |>  
#   group_map(~.x, .keep = TRUE)
# 
# # Name list elements
# names(data_usgs_lst) <- data_usgs |> 
#   group_by(STATION_NUMBER) |> 
#   group_map(~.y) |> 
#   unlist()
# 
# # Assign basin areas
# basin_areas <- c(
#   "11314000" = NA,
#   "11314500" = metadata_sites[2,"drain_area_va"]$drain_area_va,
#   "11315000" = metadata_sites[3,"drain_area_va"]$drain_area_va,
#   "11315030" = metadata_sites[4,"drain_area_va"]$drain_area_va,
#   "11315900" = metadata_sites[5,"drain_area_va"]$drain_area_va,
#   "11316100" = metadata_sites[6,"drain_area_va"]$drain_area_va,
#   "11316670" = metadata_sites[7,"drain_area_va"]$drain_area_va,
#   "11319500" = metadata_sites[8,"drain_area_va"]$drain_area_va,
#   "11325500" = metadata_sites[9,"drain_area_va"]$drain_area_va
# )
# 
# # save data
# save(
#   data_usgs, data_usgs_lst, metadata_sites, xref, basin_areas,
#   file = here::here("out","data","usgs_flows.Rdata")
#   )

# load data
load(here::here("out","data","usgs_flows.Rdata"))

## Plot sites ===========

# NWIS coords are in NAD83, a local datum (crs = 4269)
# Leaflet needs WGS84 (crs = 4326)
sf_sites <- st_as_sf(
  metadata_sites, 
  coords = c("dec_long_va", "dec_lat_va"), 
  crs = 4326
  )

# Plot
m1 <- sf_sites |> 
  leaflet() |> 
  addTiles() |> 
  addMarkers(popup = ~ station_nm, label = ~ station_nm)
m1

m2 <- sf_sites |> 
  filter(! site_no %in% c("11325500","11319500")) |> 
  leaflet() |> 
  addTiles() |> 
  addMarkers(popup = ~ station_nm, label = ~ station_nm)
m2

m3 <- sf_sites |> 
  filter(! site_no %in% c("11325500","11319500","11316670")) |> 
  leaflet() |> 
  addTiles(options = providerTileOptions(maxZoom = 13)) |> 
  addMarkers(popup = ~ station_nm, label = ~ station_nm)
m3

# Save static images
mapshot(m1, file = here::here("out", "maps", "usgs-gages.png"))
mapshot(m2, file = here::here("out", "maps", "usgs-gages_upper.png"))
mapshot(m3, file = here::here("out", "maps", "usgs-gages_local.png"))

## Test full analysis =====

# i = 1
# plot_flow_data(data_usgs_lst[[i]], start_year = 2015, end_year = 2020)
# plot_data_screening(data_usgs_lst[[i]], start_year = 2015, end_year = 2020)
# plot_missing_dates(data_usgs_lst[[i]], start_year = 2015, end_year = 2020)
# tmp <- compute_full_analysis(
#   data_usgs_lst[[i]], 
#   basin_area = basin_areas[i],
#   water_year_start = 10, 
#   start_year = 2015, 
#   end_year = 2020, 
#   ignore_missing = TRUE
# )
# 
# tmp$Daily$Daily_Summary_Stats_Plot

## Full analyses ========

# fst_nfm_01 <- compute_full_analysis(
#   data_usgs_lst[["11314500"]], 
#   basin_area = basin_areas["11314500"],
#   water_year_start = 10, ignore_missing = TRUE
#   )
# 
# fst_nfm_02 <- compute_full_analysis(
#   data_usgs_lst[["11316670"]], 
#   basin_area = basin_areas["11316670"],
#   water_year_start = 10, ignore_missing = TRUE
# )
# 
# fst_moke_01 <- compute_full_analysis(
#   data_usgs_lst[["11319500"]], 
#   basin_area = basin_areas["11319500"],
#   water_year_start = 10, ignore_missing = TRUE
# )
# 
# fst_moke_02 <- compute_full_analysis(
#   data_usgs_lst[["11325500"]], 
#   basin_area = basin_areas["11325500"],
#   water_year_start = 10, ignore_missing = TRUE
# )
# 
# fst_br_01 <- compute_full_analysis(
#   data_usgs_lst[["11315900"]], 
#   basin_area = basin_areas["11315900"],
#   water_year_start = 10, ignore_missing = TRUE
# )
# 
# # save outputs
# save(
#   fst_nfm_01, fst_nfm_02, fst_moke_01, fst_moke_02, fst_br_01, 
#   file = here::here("out","data","fasstr_obj_lists.Rdata")
#   )
# 
# # load outputs
# load(here::here("out","data","fasstr_obj_lists.Rdata"))

## Explore ==========

## North Fork Mokelumne ----------

df <- data_usgs_lst[[2]]

plot_flow_data(df)
plot_missing_dates(df)
plot_data_screening(df)
# mins increase markedly around 1975...
plot_data_screening(df, start_year = 1970, end_year = 1995)
# looks like shift occurs in 1986

plot_annual_stats(df, water_year_start = 10, log_discharge = TRUE)
plot_annual_stats2(df, water_year_start = 10)
plot_annual_extremes(df, water_year_start = 10)
plot_annual_means(df, water_year_start = 10)
plot_annual_normal_days(df, water_year_start = 10)

plot_flow_duration(df, water_year_start = 10)
plot_flow_duration(df, water_year_start = 10, end_year = 1980)
plot_flow_duration(df, water_year_start = 10, start_year = 1990)


plot_daily_stats(df, water_year_start = 10, end_year = 1980)
plot_daily_stats(df, water_year_start = 10, start_year = 1990)
