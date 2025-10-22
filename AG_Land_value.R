# Load required libraries
library(sf)         # Simple Features for spatial vector data
library(tmap)       # Thematic mapping
library(ggplot2)    # Data visualization
library(dplyr)      # Data manipulation
library(lubridate)  # Makes dealing with dates easier
library(tidyverse)  #opinionated collection of R packages designed for data science. 
library(viridis)
library(stringr)
# Load and clean each file without adding a year column

ds11_1920 <- read_csv("data/ds11_county_1920.csv")
ds11_1950 <- read_csv("data/ds11_county_1950.csv")

# Combine and rename "AE7001" to VALUE_PER_ACRE

az_land_value <- bind_rows(ds11_1920,ds11_1950) %>%
    rename(VALUE_PER_ACRE = AE7001)

##load the later data

usda_ag <- read_csv("data/USDA_AG_Survey_1970_2020_County.csv")

usda_ag_clean <- usda_ag %>%
  select(YEAR, COUNTY, VALUE_PER_ACRE, `ACRES IN FARMS`)
## create a table to adjust for inflation
# This table provides Consumer Price Index values for selected years
# Used to normalize land values to constant dollars (1920 and 2025)


cpi_table <- tibble(
  YEAR = c(1910, 1920, 1950, 1960, 1970, 1980, 1997, 2000, 2010, 2020, 2025),
  cpi  = c(9.5, 19.3, 23.5, 29.1, 38.8, 82.4, 160.5, 172.2, 218.1, 258.8, 317.6)
)
# ── Normalize Arizona land values to 1920 dollars ──
# CPI in 1920 was 15.6; this re scales VALUE_PER_ACRE accordingly

az_land_value_20<- az_land_value %>%
  left_join(cpi_table, by = "YEAR") %>%
  mutate(value_1920 = round(VALUE_PER_ACRE * (15.6 / cpi), 2))%>%
  select(YEAR, COUNTY, value_1920)

# ── Normalize USDA land values to 1920 dollars ──

usda_ag_clean_20  <- usda_ag_clean %>%
  left_join(cpi_table, by = "YEAR") %>%
  mutate(value_1920 = round(VALUE_PER_ACRE * (15.6 / cpi), 2))%>%
  select(YEAR, COUNTY, value_1920)

# ── Combine both sources into one table for 1920-dollar comparison

land_value_20 <- bind_rows(az_land_value_20, usda_ag_clean_20)

# ── Normalize Arizona land values to 2025 dollars ──
# CPI in 2025 is projected as 317.6

az_land_value_25 <- az_land_value %>%
  left_join(cpi_table, by = "YEAR") %>%
  mutate(value_2025 = round(VALUE_PER_ACRE * (317.6 / cpi), 2))

usda_ag_clean_25 <- usda_ag_clean %>%
  left_join(cpi_table, by = "YEAR") %>%
  mutate(value_2025 = round(VALUE_PER_ACRE * (317.6 / cpi), 2))

land_value_25 <- bind_rows(
  az_land_value_25 %>% select(YEAR, COUNTY, value_2025),
  usda_ag_clean_25 %>% select(YEAR, COUNTY, value_2025)
)

# select the decades of data for visualization

selected_years <- c(1920, 1970, 1980, 2020)# Target years for comparison

# Filter CPI-adjusted land value data (2025 dollars) to selected years

land_value_selected <- land_value_25 %>%
  filter(YEAR %in% selected_years)

#  Read Arizona county boundaries 
az_cnty <- st_read("Data/Arizona_County_Boundary")
# Standardize county name formatting for join compatibility
az_cnty <- az_cnty %>%
  mutate(COUNTY = str_to_title(str_trim(NAME)))# Title case and trim white space
# Standardize county names in land value data (1920 dollars)
land_value_20 <- land_value_20 %>%
  mutate(COUNTY = str_to_title(str_trim(COUNTY)))# Match formatting to spatial layer

#  Join land value data to county geometries 
map_data20 <- az_cnty %>%
  left_join(land_value_20, by = "COUNTY")# Spatial join for mapping

# Create inflation-adjusted land value map (1920 dollars)
map_20 <- ggplot(map_data20) +
  geom_sf(aes(fill = value_1920), color = "white", size = 0.1) + # Draw county polygons with white borders
  scale_fill_viridis(
    name = "Land Value per Acre",# Legend title
    option = "C",# Viridis color palette (option C = perceptually uniform)
    na.value = "grey80"# Fill color for counties with missing data
  ) +
  facet_wrap(~YEAR, ncol = 3, strip.position = "bottom") +# Facet by year, 3 columns, labels below
  theme_minimal(base_size = 12) +# Clean base theme
  labs(
    title = "Arizona County Land Value per Acre",# Main title
    subtitle = "Inflation-Adjusted to 1920 Dollars",# Subtitle for context
    caption = "Data: DS11 and USDA AG Survey",# Source attribution
    x = NULL,# Remove axis labels
    y = NULL
  ) +
  theme(
    strip.placement = "outside",# Place facet labels outside plot panels
    strip.text = element_text(face = "bold"),# Bold facet labels
    legend.position = "right",# Place legend on the right
    axis.text = element_blank(),# Hide axis text
    axis.ticks = element_blank(),# Hide axis ticks
    panel.grid = element_blank()# Remove grid lines
  )
# ── Display the map in RStudio viewer

map_20
## export as PNG and PDF
pdf("plot/map_20.pdf", width = 12, height = 8)
print(map_20)
dev.off()


png("plot/map_20.png", width = 3600, height = 2400, res = 300)  # 12*300, 8*300 - this is good.
print(map_20)
dev.off()

## Duplicate workflow  Adjusted for 2025 

selected_years <- c(1920, 1970, 1980, 2020)

land_value_selected <- land_value_25 %>% 
  filter(YEAR %in% selected_years)

az_cnty2 <- st_read("Data/Arizona_County_Boundary")

az_cnty2 <- az_cnty2 %>%
  mutate(COUNTY = str_to_title(str_trim(NAME)))

# Patch historical land value data for La Paz County ──
# La Paz County was formed from Yuma County in 1983
# This patch reassigns pre-1983 Yuma data to La Paz for continuity in visualizations

la_paz_patch <- land_value_selected %>%
  filter(COUNTY == "Yuma", YEAR < 1983) %>%
  mutate(COUNTY = "La Paz")

land_value_selected <- bind_rows(land_value_selected, la_paz_patch)

la_paz_patch <- la_paz_patch %>%
  mutate(note = "Pre-1983 La Paz value inherited from Yuma")

land_value_selected <- bind_rows(
  land_value_selected,
  la_paz_patch
)
land_value_selected <- land_value_selected %>%
  mutate(COUNTY = str_to_title(str_trim(COUNTY)))

map_data3 <- az_cnty2 %>%
  left_join(land_value_selected, by = "COUNTY")


map2_25 <- ggplot(map_data3) +
  geom_sf(aes(fill = value_2025), color = "white", size = 0.2) +
  scale_fill_viridis(
    name = "AG Land Value\n          per Acre",
    option = "C",
    na.value = "grey80",
    limits = c(50, 11000),
    labels = scales::label_dollar()
  ) +
  facet_wrap(~YEAR, ncol = 2, strip.position = "bottom") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Arizona County Land Value per Acre",
    subtitle = "Inflation-Adjusted to 2025 Dollars",
    caption = "Note: Pre-1983 La Paz values are inherited from Yuma County",
    x = NULL,
    y = NULL
  ) +
  theme(
    strip.placement = "outside",
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    plot.caption = element_text(hjust = 1, size = 9, face = "italic"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

map2_25

pdf("plot/map2_25.pdf", width = 12, height = 8)
print(map2_25)
dev.off()

png("plot/map2_25.png", width = 3600, height = 2400, res = 300)  # 12*300, 8*300 - this is good.
print(map2_25)
dev.off()


