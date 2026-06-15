

## Notes ----
# Calculate zonal stats for huc12s
# Randy Swaty
# July 29, 2025

## Dependencies ----
library(nhdplusTools)
library(sf)
library(terra)
library(dplyr)
library(exactextractr)
library(rlandfire)  # For landfireAPIv2
library(foreign)    # For write.dbf if needed

## Define forest name and output directory ----
forest_name <- "Arapaho and Roosevelt National Forests"
output_dir <- paste0("outputs_", forest_name)


## Load in data ----
national_forests <- st_read("inputs/S_USA.AdministrativeForest.shp")
bps_conus_atts <- read.csv("inputs/LF20_BPS_220.csv")
evt_conus_atts <- read.csv("inputs/LF23_EVT_240.csv")

## Select and buffer AOI and clip LTAs ----
unique(national_forests$FORESTNAME)  # Optional: view forest names

selected_forest <- national_forests %>%
  filter(FORESTNAME == "Arapaho and Roosevelt National Forests") %>%
  st_make_valid() %>%
  st_union() %>%
  st_transform(5070)

## Get HUC12s ----
huc12s <- get_huc(AOI = selected_forest, type = "huc12")

## Load and split LANDFIRE data ----
stacked_rasters <- rast("inputs/landfire_data.tif")
for (lyr in names(stacked_rasters)) assign(lyr, stacked_rasters[[lyr]])

# quick plot to make sure that hucs are captured by downloaded data

# Transform HUC12s to match raster CRS if needed
huc12s <- st_transform(huc12s, crs = crs(US_220BPS))

# Plot the raster
plot(US_220BPS, main = "HUC12 Outlines over USBPS220 Raster")

# Add HUC12 outlines
plot(st_geometry(huc12s), add = TRUE, border = "blue", lwd = 1)
     
## Write out hucs ----


# Keep only the 'huc12' and geometry columns
huc12s_minimal <- huc12s[, c("huc12", attr(huc12s, "sf_column"))]


# Calculate area in square meters and convert to hectares
huc12s_minimal <- huc12s_minimal %>%
  mutate(hectares = as.numeric(st_area(.)) / 10000)


# Write to shapefile
st_write(huc12s_minimal, file.path(output_dir, "huc12_selected.shp"))


## Process BpS ----## Process BpS ----TRUE
names(stacked_rasters)

# Crop and mask
bps_crop <- crop(US_220BPS, vect(huc12s))
bps_aoi  <- mask(bps_crop, vect(huc12s))

levels(bps_aoi)[[1]] <- bps_conus_atts
activeCat(bps_aoi) <- "VALUE"

bps_aoi_atts <- values(bps_aoi, dataframe = TRUE, na.rm = TRUE) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate(VALUE = as.integer(as.character(VALUE)),
         Freq = as.integer(Freq)) %>%
  filter(Freq != 0) %>%
  left_join(bps_conus_atts %>%
              select(VALUE, BPS_NAME, GROUPVEG, FRI_REPLAC, FRI_MIXED, FRI_SURFAC,
                     FRI_ALLFIR, PRC_REPLAC, PRC_MIXED, PRC_SURFAC, FRG_NEW,
                     R, G, B, RED, GREEN, BLUE),
            by = "VALUE") %>%
  mutate(
    ACRES = round((Freq * 900 / 4046.86), 0),
    REL_PERCENT = round((Freq / sum(Freq)), 3) * 100
  ) %>%
  arrange(desc(REL_PERCENT))

## Optional write out raster and attributes ----
# writeRaster(bps_aoi, file.path(output_dir, paste0("bps_aoi_", forest_name, ".tif")),
#             gdal = c("COMPRESS=NONE", "TFW=YES"),
#             datatype = "INT2S",
#             overwrite = TRUE)
# 
# write.dbf(bps_aoi_atts, file.path(output_dir, paste0("bps_aoi_", forest_name, ".tif.vat.dbf")))
# write.csv(bps_aoi_atts, file.path(output_dir, paste0("bps_aoi_attributes_", forest_name, ".csv")), row.names = FALSE)

## Process EVT ----
evt_aoi <- US_250EVT %>%
  crop(huc12s) %>%
  mask(huc12s)

levels(evt_aoi)[[1]] <- evt_conus_atts
activeCat(evt_aoi) <- "VALUE"

evt_aoi_atts <- values(evt_aoi, dataframe = TRUE, na.rm = TRUE) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate(VALUE = as.integer(as.character(VALUE)),
         Freq = as.integer(Freq)) %>%
  filter(Freq != 0) %>%
  left_join(evt_conus_atts, 
            by = "VALUE") %>%
  mutate(
    ACRES = round((Freq * 900 / 4046.86), 0),
    REL_PERCENT = round((Freq / sum(Freq)), 3) * 100
  ) %>%
  arrange(desc(REL_PERCENT))
# 
# ## Optional write out EVT raster and attributes ----
# writeRaster(evt_aoi, file.path(output_dir, paste0("evt_aoi_", forest_name, ".tif")),
#             gdal = c("COMPRESS=NONE", "TFW=YES"),
#             datatype = "INT2S",
#             overwrite = TRUE)
# 
# write.dbf(evt_aoi_atts, file.path(output_dir, paste0("evt_aoi_", forest_name, ".tif.vat.dbf")))
# 
# write.csv(evt_aoi_atts, file.path(output_dir, paste0("evt_aoi_attributes_", forest_name, ".csv")), row.names = FALSE)

## BpSs per HUC12 ----

## BpSs per HUC12 ----
# Step 1: Transform huc12s to match CRS of bps_aoi
huc12s_forest <- huc12s |>
  st_transform(crs = crs(bps_aoi)) |>
  mutate(huc12 = huc12)  # assuming 'huc12' is already a column in huc12s

# Step 2: Convert sf object to terra vector
huc12s_vect <- vect(huc12s_forest)

# Step 3: Extract BpS raster values by HUC12 polygons with weights
bps_extract <- terra::extract(bps_aoi, huc12s_vect, weights = TRUE, exact = FALSE)

# Step 4: Replace polygon index with actual huc12 values
bps_extract <- bps_extract |>
  mutate(ID = huc12s_forest$huc12[ID])  # replaces ID with huc12

# Step 5: Summarize BpS data per HUC12
bps_huc12_summary <- bps_extract |>
  group_by(ID, VALUE) |>
  summarise(weighted_pixels = sum(weight, na.rm = TRUE), .groups = "drop") |>
  mutate(
    VALUE = as.integer(as.character(VALUE)),
    hectares = weighted_pixels * res(bps_aoi)[1] * res(bps_aoi)[2] / 10000
  ) |>
  left_join(
    bps_conus_atts |>
      select(VALUE, BPS_NAME, GROUPVEG, FRI_REPLAC, FRI_MIXED, FRI_SURFAC,
             FRI_ALLFIR, PRC_REPLAC, PRC_MIXED, PRC_SURFAC, FRG_NEW,
             R, G, B, RED, GREEN, BLUE),
    by = "VALUE"
  )



write.csv(bps_huc12_summary, file.path(output_dir, paste0("bps_huc12_summary_", forest_name, ".csv")), row.names = FALSE)

## EVTs per HUC12 ----

# Step 1: Transform huc12s to match CRS of evt_aoi
huc12s_forest <- huc12s |>
  st_transform(crs = crs(evt_aoi)) |>
  mutate(huc12 = huc12)  # assuming 'huc12' is already a column in huc12s

# Step 2: Convert sf object to terra vector
huc12s_vect <- vect(huc12s_forest)

# Step 3: Extract EVT raster values by HUC12 polygons with weights
EVT_extract <- terra::extract(evt_aoi, huc12s_vect, weights = TRUE, exact = FALSE)

# Step 4: Replace polygon index with actual huc12 values
EVT_extract <- EVT_extract |>
  mutate(ID = huc12s_forest$huc12[ID])  # replaces ID with huc12

# Step 5: Summarize EVT data per HUC12
EVT_huc12_summary <- EVT_extract |>
  group_by(ID, VALUE) |>
  summarise(weighted_pixels = sum(weight, na.rm = TRUE), .groups = "drop") |>
  mutate(
    VALUE = as.integer(as.character(VALUE)),
    hectares = weighted_pixels * res(evt_aoi)[1] * res(evt_aoi)[2] / 10000
  ) |>
  left_join(evt_conus_atts, by = "VALUE")

# Step 6: Write output to CSV
write.csv(
  EVT_huc12_summary,
  file.path(output_dir, paste0("evt_huc12_summary_", forest_name, ".csv")),
  row.names = FALSE
)

