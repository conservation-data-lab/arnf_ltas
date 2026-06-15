
## Notes ----
# Calculate zonal stats for ltas
# Becky Lane (original code from Randy Swaty)
# January 12, 2026

## Dependencies ----

# Define forest name
forest_name <- "Arapaho and Roosevelt National Forests"

# Create output directory
output_dir <- paste0("outputs_", forest_name)
dir.create(output_dir, showWarnings = FALSE)

# Load libraries

library(sf)
library(terra)
library(dplyr)
library(exactextractr)
library(rlandfire)  # For landfireAPIv2
library(foreign)   # For write.dbf if needed
library(tibble)

# Load in data

ltas <- st_read("inputs/eCognition_LTA_20250520.shp") %>%
  select(UID, geometry) %>%
  st_transform(crs = 5070)

national_forests <- st_read("inputs/S_USA.AdministrativeForest.shp")

bps_conus_atts <- read.csv("inputs/LF20_BPS_220.csv")

evt_conus_atts <- read.csv("inputs/LF23_EVT_240.csv")

# Select and buffer AOI and clip ltas ----


# View unique forest names (optional)
unique(national_forests$FORESTNAME)  # Replace with actual column name


# Select one forest by name and transform CRS
selected_forest <- national_forests %>%
  filter(FORESTNAME == "Arapaho and Roosevelt National Forests") %>%
  st_transform(crs = 5070)

# Transform LTAs to match CRS
ltas_transformed <- st_transform(ltas, crs = 5070)

# Select LTAs that intersect (touch or overlap) the forest-will be used for extraction below.
ltas_selected <- ltas_transformed %>%
  filter(lengths(st_intersects(., selected_forest)) > 0)

# Quick base R plot
plot(st_geometry(ltas_selected), col = "lightgreen", border = "darkgreen", main = "LTAs Intersecting Ouachita National Forest")
plot(st_geometry(selected_forest), border = "#0063C7", lwd = 2, add = TRUE)

# Write out selected forest and LTAs as shapefiles

st_write(selected_forest, file.path(output_dir, paste0(forest_name, ".shp")))

st_write(ltas_selected, file.path(output_dir, paste0("ltas_selected.shp")))

# Use ltas_selected for extractions

# expand area for data download

# Buffer the selected LTAs by 1 km (adjust distance as needed)
ltas_buffered <- ltas_selected |>
  st_union() |>
  st_buffer(dist = 10000) |> # distance in meters
  st_as_sf()

# Plot the buffered LTAs with the forest boundary
plot(st_geometry(ltas_buffered), col = "lightblue", border = "blue", main = "Buffered LTAs (1 km)")
plot(st_geometry(selected_forest), border = "red", lwd = 2, add = TRUE)

## Download LANDFIRE data ----

download_aoi <- getAOI(ltas_buffered)
products <- c("220BPS", "250EVT")
projection <- 5070
resolution <- 30
email <- "relane@nmu.edu"

save_file <- tempfile(fileext = ".zip")
ncal <- landfireAPIv2(products, download_aoi, projection, resolution, path = save_file, email = email)

dest_file <- file.path("inputs", "landfire_data.zip")
file.rename(save_file, dest_file)

temp_dir <- tempfile()
dir.create(temp_dir)
unzip(dest_file, exdir = temp_dir)

unzipped_files <- list.files(temp_dir, full.names = TRUE)
for (file in unzipped_files) {
  file_name <- basename(file)
  file_extension <- sub("^[^.]*", "", file_name)
  new_file_path <- file.path("inputs", paste0("landfire_data", file_extension))
  file.rename(file, new_file_path)
}
unlink(temp_dir, recursive = TRUE)

# Read and split stacked raster
stacked_rasters <- rast("inputs/landfire_data.tif")  

for (lyr in names(stacked_rasters)) assign(lyr, stacked_rasters[[lyr]])

# Step 6: Process BpS

bps_aoi <- US_220BPS %>%
  crop(ltas_selected) %>%
  mask(ltas_selected)

levels(bps_aoi)[[1]] <- bps_conus_atts

activeCat(bps_aoi) <- "VALUE"

bps_aoi_atts <- values(bps_aoi, dataframe = TRUE, na.rm = TRUE) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(bps_aoi)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(
    ACRES = round((Freq * 900 / 4046.86), 0),
    REL_PERCENT = round((Freq / sum(Freq)), 3) * 100
  ) %>%
  arrange(desc(REL_PERCENT))


## Optional write out raster and attributes ----
# BpS raster
writeRaster(bps_aoi, file.path(output_dir, paste0("bps_aoi_", forest_name, ".tif")),
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = TRUE)

# BpS attributes DBF
write.dbf(bps_aoi_atts, file.path(output_dir, paste0("bps_aoi_", forest_name, ".tif.vat.dbf")))

# BpS attributes CSV
write.csv(bps_aoi_atts, file.path(output_dir, paste0("bps_aoi_attributes_", forest_name, ".csv")), row.names = FALSE)




## Process EVT ----

evt_aoi <- US_250EVT %>%
  crop(ltas_selected) %>%
  mask(ltas_selected)


levels(evt_aoi)[[1]] <- evt_conus_atts
activeCat(evt_aoi) <- "VALUE"


evt_aoi_atts <- values(evt_aoi, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(evt_aoi)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) %>%
  arrange(desc(REL_PERCENT))

## Optional write out EVT raster and attributes ----

# EVT raster 
writeRaster(evt_aoi, 
            filename = file.path(output_dir, paste0("evt_aoi_", forest_name, ".tif")),
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = TRUE)


# EVT attributes CSV
write.csv(evt_aoi_atts, file.path(output_dir, paste0("evt_aoi_attributes_", forest_name, ".csv")), row.names = FALSE)

# EVT attributes DBF
write.dbf(evt_aoi_atts, file.path(output_dir, paste0("evt_aoi_", forest_name, ".tif.vat.dbf")))

## BpSs per LTA ----
#Read in files

bps_conus_atts <- read.csv("outputs_Arapaho and Roosevelt National Forests/bps_aoi_attributes_Arapaho and Roosevelt National Forests.csv")

ltas_selected <- st_read("outputs_Arapaho and Roosevelt National Forests/ltas_selected.shp")
plot(ltas_selected)


bps_aoi<- rast("outputs_Arapaho and Roosevelt National Forests/bps_aoi_Arapaho and Roosevelt National Forests.tif")
plot(bps_aoi)

# Ensure CRS match
ltas_selected <- st_transform(ltas_selected, crs = crs(bps_aoi))

# Convert sf to SpatVector
ltas_vect <- vect(ltas_selected)

# Extract raster values with weights (fractional pixel coverage)
bps_extract <- terra::extract(bps_aoi, ltas_vect, weights = TRUE, exact = FALSE)

# Add UID from polygons
bps_extract$UID <- ltas_selected$UID[bps_extract$ID]

# Summarize by UID and BpS value
bps_lta_summary <- bps_extract %>%
  group_by(UID, BPS_MODEL) %>%
  summarise(weighted_pixels = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    hectares = weighted_pixels * res(bps_aoi)[1] * res(bps_aoi)[2] / 10000
  )

bps_lta_summary$VALUE <- as.numeric(as.character(bps_lta_summary$VALUE ))


# Optional: Join with BpS attribute table
# Select only necessary columns from bps_conus_atts
bps_conus_atts_clean <- bps_conus_atts %>%
  select(VALUE, BPS_MODEL, BPS_NAME, GROUPVEG, FRI_REPLAC, FRI_MIXED, FRI_SURFAC, FRI_ALLFIR,
         PRC_REPLAC, PRC_MIXED, PRC_SURFAC, FRG_NEW, R, G, B, RED, GREEN, BLUE)

# Join without duplication
bps_lta_summary <- bps_lta_summary %>%
  left_join(bps_conus_atts_clean, by = "BPS_MODEL")

# BpS lta summary
write.csv(bps_lta_summary, file = file.path(output_dir, paste0("bps_lta_summary_", forest_name, ".csv")), row.names = FALSE)

## EVTs per LTA ----lta, evt_aio, evt_attributes*
evt_extract <- evt_extract %>%
  left_join(evt_conus_atts, by = "VALUE")

# Ensure CRS match
ltas_clipped_forest <- st_transform(ltas_selected, crs = crs(evt_aoi))

# Convert sf to SpatVector
ltas_vect <- vect(ltas_selected)

# Extract raster values with weights (fractional pixel coverage)
evt_extract <- terra::extract(evt_aoi, ltas_vect, weights = TRUE, exact = FALSE)

# Add UID from polygons
evt_extract$UID <- ltas_selected$UID[evt_extract$ID]

# Summarize by UID and BpS value
evt_lta_summary <- evt_extract %>%
  group_by(UID, VALUE) %>%
  summarise(weighted_pixels = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    hectares = weighted_pixels * res(bps_aoi)[1] * res(bps_aoi)[2] / 10000
  )

evt_lta_summary$VALUE <- as.numeric(as.character(evt_lta_summary$VALUE ))




# Optional: Join with BpS attribute table
evt_lta_summary <- evt_lta_summary %>%
  left_join(evt_conus_atts, by = c("VALUE" = "VALUE"))

# View result
head(evt_lta_summary)

# Write out
write.csv(evt_lta_summary, file = file.path(output_dir, paste0("evt_lta_summary_", forest_name, ".csv")), row.names = FALSE)

##  color file for use in QGIS
evt_color_file <- evt_aoi_atts %>%
  subset(EVT_NAME != "Open Water" & EVT_NAME != "Barren-Rock/Sand/Clay") %>%
  slice_max(REL_PERCENT, n = 10) %>% 
  add_column(z = 255) %>%
  dplyr::select(VALUE,
                R,
                G,
                B,
                z,
                EVT_NAME)
write.table(evt_color_file , file = file.path(output_dir, "evt_color_file.txt"), sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)



