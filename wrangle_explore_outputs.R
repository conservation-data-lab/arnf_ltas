

## Notes ----
# Wrangle and explore outputs for LTAs, hexs and watersheds
# Randy Swaty
# September 9, 2025

## Dependencies ----

# Define forest name
forest_name <- "Arapaho and Roosevelt National Forests"

install.packages("janitor")
# Load libraries
library(janitor)
library(tidyverse)

# Load in and wrangle datasets

bps_ltas <- read_csv("outputs_Arapaho and Roosevelt National Forests/bps_lta_summary_Arapaho and Roosevelt National Forests.csv") |>
  select(c(UID,
           hectares,
           BPS_NAME)) |>
  rename("id" = "UID") |>
  mutate(UNIT = "LTA",
         hectares = round(hectares, 0))|>
  clean_names()


bps_huc12s <- read_csv("outputs_Arapaho and Roosevelt National Forests/bps_huc12_summary_Arapaho and Roosevelt National Forests.csv") |>
  select(ID, hectares, BPS_NAME) |>
  rename(id = ID) |>
  mutate(
    id = as.numeric(as.factor(id)),  # convert 'id' column to numeric
    UNIT = "HUC12",
    hectares = round(hectares, 0)
  ) |>
  clean_names()


bps_hexs <- read_csv("outputs_Arapaho and Roosevelt National Forests/bps_hex_summary_Arapaho and Roosevelt National Forests.csv")|>
  select(c(hex_id,
           hectares,
           BPS_NAME)) |>
  rename("id" = "hex_id") |>
  mutate(UNIT = "HEX",
         hectares = round(hectares, 0))|>
  clean_names()

bps_merged <- bind_rows(bps_hexs,
                        bps_huc12s,
                        bps_ltas)


## Visualize for BpSs



# Step 1: Summarize unique BPS names per unit and id
bps_summary <- bps_merged %>%
  group_by(unit, id) %>%
  summarise(unique_bps = n_distinct(bps_name), .groups = "drop")

# Step 2: Calculate summary stats for mean and SD
bps_stats <- bps_summary %>%
  group_by(unit) %>%
  summarise(
    mean_bps = mean(unique_bps),
    sd_bps = sd(unique_bps),
    label = paste0("Mean = ", round(mean_bps, 2), "\nSD = ", round(sd_bps, 2)),
    .groups = "drop"
  )

# Step 3: Create violin plot with mean, SD bars, and annotation
ggplot(bps_summary, aes(x = unit, y = unique_bps, fill = unit)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.8) +
  geom_point(data = bps_stats, aes(x = unit, y = mean_bps), color = "black", size = 3) +
  geom_errorbar(data = bps_stats,
                aes(x = unit, ymin = mean_bps - sd_bps, ymax = mean_bps + sd_bps),
                inherit.aes = FALSE,
                width = 0.2, color = "black", linewidth = 0.8) +
  geom_text(data = bps_stats,
            aes(x = unit, y = 0.5, label = label),
            inherit.aes = FALSE,
            hjust = 0.5, vjust = 1,
            size = 4, color = "black") +
  labs(
    title = "Distribution of Unique BPS Names per Unit Type",
    subtitle = "Violin plot with mean and standard deviation",
    x = "Unit Type",
    y = "Number of Unique BPS Names"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2")

install.packages("vegan")
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)

# Step 1: Aggregate area per bps_name within each unit and id
bps_area <- bps_merged %>%
  group_by(unit, id, bps_name) %>%
  summarise(area = sum(hectares), .groups = "drop")

# Step 2: Pivot to wide format for diversity calculation
bps_matrix <- bps_area %>%
  pivot_wider(names_from = bps_name, values_from = area, values_fill = 0)

# Step 3: Calculate Shannon Diversity Index
bps_matrix$shannon <- diversity(bps_matrix[ , !(names(bps_matrix) %in% c("unit", "id"))], index = "shannon")

# Step 4: Kruskal-Wallis test
kruskal.test(shannon ~ unit, data = bps_matrix)

# Step 5: Visualize with violin plot
ggplot(bps_matrix, aes(x = unit, y = shannon, fill = unit)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.8) +
  labs(
    title = "Shannon Diversity Index by Unit Type",
    subtitle = "Area-weighted ecological diversity",
    x = "Unit Type",
    y = "Shannon Diversity Index"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2")

