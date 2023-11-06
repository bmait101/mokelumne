## Summarize and analyze stream flow data for study streams
## Bryan Maitland
## Ctreated: 1 Nov 2023 | Updated: 6 Nov 2023

# Prep
library(tidyverse)
library(dataRetrieval)  # Retrieval functions for USGS and EPA data
library(fasstr)  # Analyze, Summarize, and Visualize Daily Streamflow Data
library(sf)  # simple features in R 


## Pull data from NWIS =====

# Create function to download NWIS data and convert to metric for use with fasstr
readNWISdv_clean <- function(x){
  # pull daily streamflow data for given site numbers
  data <- readNWISdv(
    siteNumbers = x, 
    parameterCd = "00060"  # discharge param code
  )
  
  # tidy up
  data <- data |> select(-agency_cd)
  names(data)[names(data) == 'site_no'] <- 'STATION_NUMBER'
  names(data)[names(data) == 'X_00060_00003'] <- 'Value'
  names(data)[names(data) == 'X_00060_00003_cd'] <- 'Symbol'
  
  # convert cfs to cms
  data$Value <- data$Value * 0.028317
  
  # get basin areas
  basin_area <- readNWISsite(siteNumbers = x)
  basin_area$Basin_Area_sqkm <- basin_area$drain_area_va * 2.58999
  basin_area <- data.frame(STATION_NUMBER = basin_area$site_no,
                           Basin_Area_sqkm = basin_area$Basin_Area_sqkm)
  # merge basin area back to data
  data <- merge(data, basin_area, by = "STATION_NUMBER")
  
  return(data)
}

# List of sites
site_numbers <- c(
  "11314000",  # TIGER C PH COND BL SALT SPRINGS DAM CA
  "11314500",  # NF MOKELUMNE R BL SALT SPRINGS DAM CA
  "11315000",  # COLE C NR SALT SPRINGS DAM CA
  "11315030",  # COLE C BL DIV DAM NR SALT SPRINGS DAM CA
  "11315900",  # BEAR R BL LO BEAR R DAM CA
  "11316100",  # BEAR R BL BEAR R DIV DAM CA
  "11316670",  # NF MOKELUMNE R BL TIGER C RES NR WEST PT CA
  "11317000",  # MF MOKELUMNE R A WEST POINT CA
  "11318500",  # SF MOKELUMNE R NR WEST POINT CA 
  "11319500",  # MOKELUMNE R NR MOKELUMNE HILL CA (above Parde Res)
  "11325500"  # MOKELUMNE R A WOODBRIDGE CA (below Camanche Res, near Lodi)
)

# Site data
metadata_sites <- 
  readNWISsite(siteNumbers = site_numbers) |> 
  as_tibble()

# Get daily discharge data
data_usgs <- readNWISdv_clean(site_numbers)
head(data_usgs)

# plot sites
# usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE), crs = 4269)
# sf_sites <- st_as_sf(metadata_sites, coords = c("dec_long_va", "dec_lat_va"), crs = 4269)
# ggplot() +
#   geom_sf(data = usa[ usa$ID == "california" ,]) +
#   geom_sf(data = sf_sites) + 
#   xlab(NULL)+
#   ylab(NULL)+
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5))


### Screening

data_usgs <- data_usgs |> 
  fill_missing_dates(water_year_start = 10) |>
  add_date_variables()

plot_flow_data(data_usgs, water_year_start = 10)[[2]]
plot_data_screening(data_usgs)
plot_missing_dates(data_usgs)

# Convert to list for each site
data_usgs_lst <- data_usgs |> 
  group_by(STATION_NUMBER) |>  
  group_map(~.x, .keep = TRUE)

# Name list elements
names(data_usgs_lst) <- data_usgs |> 
  group_by(STATION_NUMBER) |> 
  group_map(~.y) |> 
  unlist()

## Test full analysis =====

# Assign basin areas
basin_areas <- c(
  "11314000" = NA,
  "11314500" = metadata_sites[2,"drain_area_va"]$drain_area_va,
  "11315000" = metadata_sites[3,"drain_area_va"]$drain_area_va,
  "11315030" = metadata_sites[4,"drain_area_va"]$drain_area_va,
  "11315900" = metadata_sites[5,"drain_area_va"]$drain_area_va,
  "11316100" = metadata_sites[6,"drain_area_va"]$drain_area_va,
  "11316670" = metadata_sites[7,"drain_area_va"]$drain_area_va,
  "11317000" = metadata_sites[8,"drain_area_va"]$drain_area_va,
  "11318500" = metadata_sites[9,"drain_area_va"]$drain_area_va,
  "11319500" = metadata_sites[10,"drain_area_va"]$drain_area_va,
  "11325500" = metadata_sites[11,"drain_area_va"]$drain_area_va
  )

# test

plot_flow_data(data_usgs_lst[[4]])

tmp <- compute_full_analysis(
  data_usgs_lst[[11]], 
  basin_area = basin_areas[11],
  water_year_start = 10, 
  start_year = 2015, end_year = 2020, 
  ignore_missing = TRUE
)

tmp$Screening$Daily_Flows_Plot
tmp$Longterm$Flow_Duration_Curves
tmp$Daily$Daily_Summary_Stats_Plot

## Full analyses - MOKELUMNE ========

moke <- compute_full_analysis(
  data_usgs_lst[["11314500"]], 
  basin_area = basin_areas["11314500"],
  water_year_start = 10
  )

write_full_analysis(
  data_usgs_lst[["11314500"]], 
  basin_area = basin_areas["11314500"],
  water_year_start = 10, 
  file_name = here::here("out","fasstr","11314500")
)

## Full analyses - Bear R BL LW Bear Res ========

write_full_analysis(
  data_usgs_lst[["11315900"]], 
  basin_area = basin_areas["11315900"],
  water_year_start = 10, 
  file_name = here::here("out","fasstr","11315900")
)


## Working =======

plot_daily_stats(
  data_usgs_lst[["11314500"]], 
  water_year_start = 10, 
  start_year = 2010
  )

plot_flow_duration(
  data_usgs_lst[["11314500"]], 
  water_year_start = 10, 
  start_year = 2010
)
