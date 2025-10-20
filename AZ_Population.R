# Load required libraries
library(sf)         # Simple Features for spatial vector data
library(ggplot2)    # Data visualization
library(dplyr)      # Data manipulation
library(lubridate)  # Makes dealing with dates easiler
library(tidyverse) # tidy data
library(scales)    ## for legend formatting
library(viridis) # viz - color
library(tidyr)


# ── Import historical county census shape files ──
# Each file represents county boundaries and census attributes for a given year

cnty_1900 <- st_read("Data/us_county_1900/US_county_1900.shp")
cnty_1920 <- st_read("Data/us_county_1920/US_county_1920.shp")
cnty_1950 <- st_read("Data/us_county_1950/US_county_1950.shp")
cnty_1970 <- st_read("Data/us_county_1970/US_county_1970.shp")
cnty_1980 <- st_read("Data/us_county_1980/US_county_1980.shp")
cnty_2000 <- st_read("Data/us_county_2000/US_county_2000.shp")
cnty_2020 <- st_read("Data/us_county_2020/US_county_2020.shp")
# ── Import Arizona groundwater basin shapefile 
gwb_2024 <- st_read("Data/AZ_groundwater_subbasin_24/ADWR_Groundwater_Subbasin_2024.shp")

## Inspect column names to prepare for joins ──
names(cnty_1900)
names(cnty_2020)

## Standardize state name for Arizona in 1900 ──
# Arizona was listed as "Arizona Territory" in 1900 census shape file
# This ensures consistency for filtering and joining across years

unique(cnty_1900$STATENAM)
cnty_1900 <- cnty_1900 %>%
  mutate(STATENAM = ifelse(STATENAM == "Arizona Territory", "Arizona", STATENAM))

## now we can extract Arizona
az_1900 <- cnty_1900 %>% filter(STATENAM == "Arizona")
az_1920 <- cnty_1920 %>% filter(STATENAM == "Arizona")
az_1950 <- cnty_1950 %>% filter(STATENAM == "Arizona")
az_1970 <- cnty_1970 %>% filter(STATENAM == "Arizona")
az_1980 <- cnty_1980 %>% filter(STATENAM == "Arizona")
az_2000 <- cnty_2000 %>% filter(STATENAM == "Arizona")
az_2020 <- cnty_2020 %>% filter(STATEFP == "04")

## Add STATENAM column to spatial layer ──
# Ensures compatibility for left joins with population data

az_2020 <- az_2020 %>%
  mutate(STATENAM = "Arizona")

## Import population data 
pop_all <- read_csv("Data/pop_all.csv")
pop_1900 <- read_csv("Data/pop_1900.csv")
# ── Quick structure check ──
glimpse(pop_all)
glimpse(pop_1900)
# ── Filter for Arizona only 
pop_all <- pop_all %>% filter(STATE == "Arizona")
pop_1900 <- pop_1900 %>% filter(STATE == "Arizona")

# ── Reshape pop_all from wide to long format ──
# Columns like A00AA1920 become rows with year and population values
# Removes "A00AA" prefix to extract clean year labels


pop_all <- pop_all %>%
  pivot_longer(
    cols = starts_with("A00AA"),# Select year columns
    names_to = "year",# New column for year labels
    values_to = "population"# New column for population values
  ) %>%
  mutate(year = str_remove(year, "A00AA"))# Strip prefix to get numeric year

# ── Reshape 1900 population data from wide to long format ──
# Columns like A00AA1900 become rows with year and population values
# Removes "A00AA" prefix to extract clean year labels

pop_1900 <- pop_1900 %>%
  pivot_longer(
    cols = starts_with("A00AA"),   # Select year columns (e.g., A00AA1900)
    names_to = "year",              # New column for year labels
    values_to = "population"        # New column for population values
  ) %>%
  mutate(year = str_remove(year, "A00AA"))

## ── Check column names for join compatibility 
names(pop_all)
names(pop_1900)

# ── Handle unmatched 1900 population record ──
# San Carlos Indian Reservation appears in 1900 data but not in spatial layer
# We'll exclude it from the join but preserve it for metadata/reference

## ID the unmatched row

unmatched_1900 <- pop_1900 %>%
  filter(GISJOIN %in% setdiff(pop_1900$GISJOIN, az_1900$GISJOIN))
# Remove unmatched row from population table
pop_1900_clean <- pop_1900 %>%
  filter(!(GISJOIN %in% unmatched_1900$GISJOIN))

## ── Join cleaned 1900 population data to spatial layer 
az_1900 <- az_1900 %>%
  left_join(pop_1900_clean, by = "GISJOIN")

# ── Save unmatched record for documentation 
dir.create("Data/metadata", recursive = TRUE, showWarnings = FALSE)
write_csv(unmatched_1900, "Data/metadata/unmatched_1900_san_carlos.csv")

## ── Join population data for other decades 
az_1920 <- az_1920 %>%
  left_join(pop_all %>% filter(year == "1920"), by = "GISJOIN")

az_1950 <- az_1950 %>%
  left_join(pop_all %>% filter(year == "1950"), by = "GISJOIN")

az_1970 <- az_1970 %>%
  left_join(pop_all %>% filter(year == "1970"), by = "GISJOIN")

az_1980 <- az_1980 %>%
  left_join(pop_all %>% filter(year == "1980"), by = "GISJOIN")

az_2000 <- az_2000 %>%
  left_join(pop_all %>% filter(year == "2000"), by = "GISJOIN")

az_2020 <- az_2020 %>%
  left_join(pop_all %>% filter(year == "2020"), by = "GISJOIN")


## ── Check current CRS of 1900 layer ── the groundwater basins are on NAD 83 UTM 12N 

st_crs(az_1900)# Inspect to confirm before transforming

# ── Reproject all county layers to NAD83 / UTM Zone 12N ──
# EPSG:26912 is ideal for Arizona spatial analysis (meters, minimal distortion)

az_1900 <- st_transform(az_1900, crs = 26912)
az_1920 <- st_transform(az_1920, crs = 26912)
az_1950 <- st_transform(az_1950, crs = 26912)
az_1970 <- st_transform(az_1970, crs = 26912)
az_1980 <- st_transform(az_1980, crs = 26912)
az_2000 <- st_transform(az_2000, crs = 26912)
az_2020 <- st_transform(az_2020, crs = 26912)

## Confirm the CRS alignment with groundwater basins 
st_crs(az_2020)
st_crs(gwb_2024)

# ── Create population trend table across decades ──
# Drops geometry for plotting and selects key columns

pop_trend <- bind_rows(
  az_1900 %>% mutate(year = 1900),
  az_1920 %>% mutate(year = 1920),
  az_1950 %>% mutate(year = 1950),
  az_1970 %>% mutate(year = 1970),
  az_1980 %>% mutate(year = 1980),
  az_2000 %>% mutate(year = 2000),
  az_2020 %>% mutate(year = 2020)
) %>%
  st_drop_geometry() %>%
  select(NAME, year, population)

# ── Plot county-level population trends over time ──
ggplot(pop_trend, aes(x = year, y = population, group = NAME, color = NAME)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Arizona County Population Over Time",
    x = "Year",
    y = "Population",
    caption = "Data joined from historical county shapefiles"
  ) +
  theme(legend.position = "none")  # or use legend if you want to highlight specific counties


# ── Combine all spatial layers into one for mapping or analysis ──

az_all <- bind_rows(
  az_1900 %>% mutate(year = 1900),
  az_1920 %>% mutate(year = 1920),
  az_1950 %>% mutate(year = 1950),
  az_1970 %>% mutate(year = 1970),
  az_1980 %>% mutate(year = 1980),
  az_2000 %>% mutate(year = 2000),
  az_2020 %>% mutate(year = 2020)
)

## Test 1-Minimal theme with default layout: Good for quick inspection
test1 <- ggplot(az_all) +
  geom_sf(aes(fill = population), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "C", trans = "log", name = "Population") +
  facet_wrap(~ year, ncol = 4) +
  theme_minimal() +
  labs(
    title = "Arizona County Population Over Time",
    subtitle = "Log-scaled fill for clarity across decades",
    caption = "Data harmonized to NAD83 / UTM Zone 12N"
  )

## Test 1 Strip labels are top-aligned and may crowd the title
## test 1 Grid lines and axes may distract from spatial focus


## test 2: Void theme with bottom legend
##cleaner visual focus
##bottom legend improves layout balance
## Strip labels still top-aligned


test2 <- ggplot(az_all) +
  geom_sf(aes(fill = population), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "C", trans = "log", name = "Population") +
  facet_wrap(~ year, ncol = 4) +
  theme_void() +  # removes axes and background
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  ) +
  labs(
    title = "Arizona County Population Over Time",
    subtitle = "Log-scaled fill for clarity across decades",
    caption = "CRS: NAD83 / UTM Zone 12N"
  )
## test 3: Caption refinement

test3 <- ggplot(az_all) +
  geom_sf(aes(fill = population), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "C", trans = "log", name = "Population") +
  facet_wrap(~ year, ncol = 4) +
  theme_void() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 1, size = 9, face = "italic")  # right-align caption
  ) +
  labs(
    title = "Arizona County Population Over Time",
    subtitle = "Log-scaled fill for clarity across decades",
    caption = "CRS: NAD83 / UTM Zone 12N"
  )
## Strip labels still top-aligned pay attention to the year labels "strip.placement and strip.position"

## Test 4 - most expressive version
test4 <- ggplot(az_all) +
  geom_sf(aes(fill = population), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "C", trans = "log", name = "Population") +
  facet_wrap(~ year, ncol = 4, strip.position = "bottom") +
  theme_void() +
  theme(
    strip.placement = "outside",
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right",  # move legend to the right
    plot.caption = element_text(hjust = 1, size = 9, face = "italic")
  ) +
  labs(
    title = "Arizona County Population Over Time",
    subtitle = "Log-scaled fill for clarity across decades",
    caption = "CRS: NAD83 / UTM Zone 12N\nData source: IPUMS USA, University of Minnesota, www.ipums.org."
  )
test4

##  Full historical range (1900–2020)


pop_map1 <- ggplot(az_all) +
  geom_sf(aes(fill = population), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "C",
    trans = "log",
    name = "Population",
    labels = scales::label_number(accuracy = 1, big.mark = ",")
  ) +
  facet_wrap(~ year, ncol = 4, strip.position = "bottom") +
  theme_void() +
  theme(
    strip.placement = "outside",
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right",
    plot.caption = element_text(hjust = 1, size = 9, face = "italic")
  ) +
  labs(
    title = "Arizona County Population Over Time",
    subtitle = "Log-scaled fill with whole-number legend",
    caption = "CRS: NAD83 / UTM Zone 12N\nData source: IPUMS USA, University of Minnesota, www.ipums.org."
  )
pop_map1
## Export steps

pdf("plot/pop_map1.pdf", width = 10, height = 7)  # open PDF device
print(pop_map1)                                   # print the ggplot object
dev.off()                                     # close device

png("plot/pop_map1.png",  width = 3600, height = 2400, res = 300)
print(pop_map1)
dev.off()

## Focused range (1920–2020, excluding 1900)

az_all_2d <- bind_rows(
  az_1920 %>% mutate(year = 1920),
  az_1950 %>% mutate(year = 1950),
  az_1970 %>% mutate(year = 1970),
  az_1980 %>% mutate(year = 1980),
  az_2000 %>% mutate(year = 2000),
  az_2020 %>% mutate(year = 2020)
)


pop_map2 <- ggplot(az_all_2d) +
  geom_sf(aes(fill = population), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "C",
    trans = "log",
    name = "Population",
    labels = scales::label_number(accuracy = 1, big.mark = ",")
  ) +
  facet_wrap(~ year, ncol = 3, strip.position = "bottom") +
  theme_void() +
  theme(
    strip.placement = "outside",
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right",
    plot.caption = element_text(hjust = 1, size = 9, face = "italic")
  ) +
  labs(
    title = "Arizona County Population Over Time (1920–2020)",
    subtitle = "Log-scaled fill for clarity across decades",
    caption = "CRS: NAD83 / UTM Zone 12N\nData source: IPUMS USA, University of Minnesota, www.ipums.org."
  )
pop_map2

## Export steps

pdf("plot/pop_map2.pdf", width = 12, height = 8)
print(pop_map2)
dev.off()

png("plot/pop_map2.png", width = 1200, height = 800, res = 300) ## this resolution is off
print(pop_map2)
dev.off()

png("plot/pop_map2.png", width = 3600, height = 2400, res = 300)  # 12*300, 8*300 - this is good.
print(pop_map2)
dev.off()


## Create truncated dataset (1920, 1970, 1980, 2020)
az_all_3 <- bind_rows(
  az_1920 %>% mutate(year = 1920),
  az_1970 %>% mutate(year = 1970),
  az_1980 %>% mutate(year = 1980),
  az_2020 %>% mutate(year = 2020)
)

highlighted_counties <- c("Graham", "Cochise", "Mohave")  # Example counties
az_highlight <- az_all_3 %>%
  filter(NAME %in% highlighted_counties)

pop_map3_highlight <- ggplot(az_all_3) +
  geom_sf(aes(fill = population), color = "white", size = 0.2) +  # Base map
  geom_sf(data = az_highlight, fill = NA, color = "black", size = 0.9) +  # Highlighted outlines
  scale_fill_viridis_c(
    option = "C",
    trans = "log",
    name = "Population",
    labels = scales::label_number(accuracy = 1, big.mark = ",")
  ) +
  facet_wrap(~ year, ncol = 2, strip.position = "bottom") +
  theme_void() +
  theme(
    strip.placement = "outside",
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right",
    plot.caption = element_text(hjust = 1, size = 9, face = "italic")
  ) +
  labs(
    title = "Arizona County Population (1920–2020)",
    subtitle = "Highlighted: Graham, Cochise, Mohave",
    caption = "CRS: NAD83 / UTM Zone 12N"
  )



pop_map3_highlight
## this only highlighted the counties in 2020.

##  Build faceted population map for use in the poster.
pop_map3 <- ggplot(az_all_3) +
  geom_sf(aes(fill = population), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "C",
    trans = "log",
    name = "Population",
    labels = scales::label_number(accuracy = 1, big.mark = ",")
  ) +
  facet_wrap(~ year, ncol = 2, strip.position = "bottom") +
  theme_void() +
  theme(
    strip.placement = "outside",
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right",
    plot.caption = element_text(hjust = 1, size = 9, face = "italic")
  ) +
  labs(
    title = "Arizona County Population (1920–2020)",
    caption = "CRS: NAD83 / UTM Zone 12N"
  )
pop_map3

## Export to PDF and high-resolution PNG

pdf("plot/pop_map3.pdf", width = 12, height = 8)
print(pop_map3)
dev.off()

png("plot/pop_map3.png", width = 3600, height = 2400, res = 300) 
print(pop_map3)
dev.off()


##  pop density experiment -  unsuccessful for this scale

az_all_density <- az_all %>%
  mutate(
    area_km2 = st_area(.) / 10^6,
    pop_density_km2 = pmax(population / as.numeric(area_km2), 0)
  ) %>%
  filter(year != 1900)

geom_sf(aes(fill = pop_density_km2))


scale_fill_viridis_c(
  trans = "log",
  name = "Population Density\n(people/km²)",
  labels = scales::label_number(accuracy = 1, big.mark = ",")
)

az_all_density <- az_all %>%
  mutate(
    area_km2 = st_area(.) / 10^6,
    pop_density_km2 = pmax(population / as.numeric(area_km2), 0)
  ) %>%
  filter(year != 1900)

pop_dens1 <- ggplot(az_all_density) +
  geom_sf(aes(fill = pop_density_km2), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    trans = "log",
    name = "Population Density\n(people/km²)",
    labels = scales::label_number(accuracy = 1, big.mark = ",")
  ) +
  facet_wrap(~ year, ncol = 3, strip.position = "bottom") +
  theme_void() +
  theme(
    strip.placement = "outside",
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right",
    plot.caption = element_text(hjust = 1, size = 9, face = "italic", lineheight = 1.2)
  ) +
  labs(
    title = "Arizona County Population Density Over Time",
    subtitle = "Log-scaled fill with whole-number legend",
    caption = "CRS: NAD83 / UTM Zone 12N\nData source: IPUMS USA, University of Minnesota, www.ipums.org."
  )
pop_dens1
