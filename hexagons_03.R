## Notes ----
# Calculate zonal stats for 10k HA hexagons that cover HUC12 area (faster than loading LTAs)
# Randy Swaty
# July 29, 2025

## Dependencies ----
library(nhdplusTools)
library(sf)
library(terra)
library(dplyr)
library(exactextractr)
library(rlandfire)
library(foreign)

## Define forest name and output directory ----
forest_name <- "Arapaho and Roosevelt National Forests"
output_dir <- paste0("outputs_", forest_name)
dir.create(output_dir, showWarnings = FALSE)

## Load in data ----
national_forests <- st_read("inputs/S_USA.AdministrativeForest.shp")
bps_conus_atts <- read.csv("inputs/LF20_BPS_220.csv")
evt_conus_atts <- read.csv("inputs/LF23_EVT_240.csv")

## Select AOI ----
selected_forest <- national_forests %>%
  filter(FORESTNAME == "Arapaho and Roosevelt National Forests") %>%
  st_make_valid() %>%
  st_union() %>%
  st_transform(5070)

## Get HUC12s ----
huc12s <- get_huc(AOI = selected_forest, type = "huc12")

## Load LANDFIRE raster stack ----
stacked_rasters <- rast("inputs/landfire_data.tif")
for (lyr in names(stacked_rasters)) assign(lyr, stacked_rasters[[lyr]])

## Hex Grid Zonal Stats ----

# Union of HUC12s
huc12_union <- st_union(huc12s)

# Hexagon side length for ~25,000 ha hexagons
hex_side <- 9809.44

# Create full hex grid
hex_grid <- st_make_grid(huc12_union,
                         cellsize = hex_side,
                         square = FALSE,
                         what = "polygons") %>%
  st_sf()

# Filter hexagons that intersect the HUC12 union
hex_grid <- hex_grid[st_intersects(hex_grid, huc12_union, sparse = FALSE), ]
hex_grid$hex_id <- seq_len(nrow(hex_grid))

## Filter hexagons fully within raster extents ----
bps_extent <- as.polygons(ext(US_220BPS)) %>% st_as_sf()
evt_extent <- as.polygons(ext(US_250EVT)) %>% st_as_sf()

# 🔧 Assign CRS from original rasters
st_crs(bps_extent) <- crs(US_220BPS)
st_crs(evt_extent) <- crs(US_250EVT)


hex_grid <- st_transform(hex_grid, crs = st_crs(bps_extent))
hex_grid_inside <- hex_grid[
  st_within(hex_grid, bps_extent, sparse = FALSE) &
    st_within(hex_grid, evt_extent, sparse = FALSE),
]

## Plot raster and hexagons for inspection ----
plot(US_220BPS, main = "US_220BPS Raster with HUC12s and Hexagons")
plot(st_transform(huc12s, crs = crs(US_220BPS))$geometry, add = TRUE, border = "blue", lwd = 2)
plot(hex_grid_inside$geometry, add = TRUE, border = "green", lwd = 1)

## write hexagons

st_write(hex_grid_inside, file.path(output_dir, paste0("hexs_selected.shp")))

## Crop and mask BpS and EVT to hex grid extent ----
hex_vect <- vect(hex_grid_inside)

bps_hex_aoi <- US_220BPS %>%
  crop(hex_vect) %>%
  mask(hex_vect)

evt_hex_aoi <- US_250EVT %>%
  crop(hex_vect) %>%
  mask(hex_vect)

levels(bps_hex_aoi)[[1]] <- bps_conus_atts
activeCat(bps_hex_aoi) <- "VALUE"

levels(evt_hex_aoi)[[1]] <- evt_conus_atts
activeCat(evt_hex_aoi) <- "VALUE"

## Extract and summarize BpS per hex ----
bps_hex_extract <- terra::extract(bps_hex_aoi, hex_vect, weights = TRUE, exact = FALSE)
bps_hex_extract$hex_id <- hex_vect$hex_id[bps_hex_extract$ID]

bps_hex_summary <- bps_hex_extract %>%
  group_by(hex_id, VALUE) %>%
  summarise(weighted_pixels = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    VALUE = as.integer(as.character(VALUE)),
    hectares = weighted_pixels * res(bps_hex_aoi)[1] * res(bps_hex_aoi)[2] / 10000
  ) %>%
  left_join(bps_conus_atts %>%
              select(VALUE, BPS_NAME, GROUPVEG, FRI_REPLAC, FRI_MIXED, FRI_SURFAC,
                     FRI_ALLFIR, PRC_REPLAC, PRC_MIXED, PRC_SURFAC, FRG_NEW,
                     R, G, B, RED, GREEN, BLUE),
            by = "VALUE")

## Extract and summarize EVT per hex ----
evt_hex_extract <- terra::extract(evt_hex_aoi, hex_vect, weights = TRUE, exact = FALSE)
evt_hex_extract$hex_id <- hex_vect$hex_id[evt_hex_extract$ID]

evt_hex_summary <- evt_hex_extract %>%
  group_by(hex_id, VALUE) %>%
  summarise(weighted_pixels = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    VALUE = as.integer(as.character(VALUE)),
    hectares = weighted_pixels * res(evt_hex_aoi)[1] * res(evt_hex_aoi)[2] / 10000
  ) %>%
  left_join(evt_conus_atts, by = "VALUE")

## Write outputs ----
write.csv(bps_hex_summary, file.path(output_dir, paste0("bps_hex_summary_", forest_name, ".csv")), row.names = FALSE)
write.csv(evt_hex_summary, file.path(output_dir, paste0("evt_hex_summary_", forest_name, ".csv")), row.names = FALSE)

