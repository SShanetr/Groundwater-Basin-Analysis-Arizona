# Load required libraries
library(sf)         # Simple Features for spatial vector data
library(tmap)       # Thematic mapping
library(ggplot2)    # Data visualization
library(dplyr)      # Data manipulation
library(lubridate)  # Makes dealing with dates easiler


# Load well data for Hualapai  Groundwater Basins 
# Ensure longitudes are negative (Western Hemisphere)
# This avoids misplacement on the map due to positive longitudes
# Convert Hualapai wells to spatial features using WGS84 (EPSG:4326)
# Convert before loading the next set

wells_hua <- read.csv("data/Automated_well_hualapai.csv")
wells_hua$longitude <- -abs(wells_hua$longitude)  
wells_hua <- st_as_sf(wells_hua, coords = c("longitude", "latitude"), crs = 4326)

# Load well data for  Willcox Groundwater Basins
# Ensure longitudes are negative (Western Hemisphere)
# Convert Willcox wells to spatial features using NAD27 (EPSG:4267)
# NAD27 is common in older USGS datasets; we'll reproject it next

wells_wil <- read.csv("data/Automated_well_willcox.csv")
wells_wil$longitude <- -abs(wells_wil$longitude)
wells_wil <-st_as_sf(wells_wil, coords = c("longitude", "latitude"), crs = 4267)


# Reproject both data sets to WGS84 for consistency in mapping and analysis
# This ensures alignment with base maps and other spatial layers
wells_hua <- st_transform(wells_hua, crs = 4326)
wells_wil <- st_transform(wells_wil, crs = 4326)

## Visualize wells interactively on a map
tmap_mode("view")  # Switch to interactive viewing mode (pan, zoom, inspect)

tm_shape(wells_hua) + tm_dots(col = "black", size = 0.2) + # Plot Hualapai wells as small black dots
  tm_shape(wells_wil) + tm_dots(col = "black", size = 0.2) # Add Willcox wells to the same map

## ── Inspect structure and available columns

class(wells_hua)  # Should include "sf"
names(wells_hua)  # Check available columns

## ── Convert water level dates to Date format. Use mdy() if dates are in "MM/DD/YYYY" format
wells_hua$WL.Date <- mdy(wells_hua$WL.Date)  # Convert Hualapai WL dates

str(wells_hua$WL.Date)  # Confirm conversion to Date class
wells_wil$WL.Date <- mdy(wells_wil$WL.Date)  # Convert Willcox WL dates.

## Filter out unused wells and Extract year from water level date for both HUA and WIL
## Group by year and water use
## Summarize mean depth to water


hua_trend <- wells_hua |>
  filter(Water.Use != "UNUSED") |>
  mutate(year = year(WL.Date)) |>
  group_by(year, Water.Use) |>
  summarise(mean_dtw = mean(`DTW..ft.`, na.rm = TRUE), .groups = "drop")

 png("plot/hualapai_trend.png", width = 3600, height = 2400, res = 300)
 print(hua_trend)
 dev.off()

wil_trend <- wells_wil |>
  filter(Water.Use != "UNUSED") |>
  mutate(year = year(WL.Date)) |>
  group_by(year, Water.Use) |>
  summarise(mean_dtw = mean(`DTW..ft.`, na.rm = TRUE), .groups = "drop")



## the following plot of ALL water use by type get too messy to be of use.
ggplot(hua_trend, aes(x = year, y = mean_dtw, color = Water.Use)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Hualapai Basin – Avg Depth to Water by Use Type",
       x = "Year", y = "Depth to Water (ft)") +
  theme_minimal()

## the following plot tells  more of a story but would need explanation because it is to well depth
## some may find this type of visualization confusing (this is audience dependent)
## create data set of  observation and unused (monitoring) 
## all data forward will be active wells

monitoring_hua <- wells_hua |> filter(Water.Use %in% c("UNUSED", "OBSERVATION"))

monitoring_trend <- monitoring_hua |>
  mutate(year = year(WL.Date)) |>
  group_by(year) |>
  summarise(mean_dtw = mean(`DTW..ft.`, na.rm = TRUE), .groups = "drop")


ggplot(monitoring_trend, aes(x = year, y = mean_dtw)) +
  geom_line(linewidth = 1.2, color = "darkgreen") +
  labs(title = "Hualapai Basin – Monitoring Wells (Avg Depth to Water)",
       x = "Year", y = "Depth to Water (ft)") +
  theme_minimal()

## duplicate for Willcox

monitoring_wil <- wells_wil |> filter(Water.Use %in% c("UNUSED", "OBSERVATION"))

monitoring_trend2 <- monitoring_wil |>
  mutate(year = year(WL.Date)) |>
  group_by(year) |>
  summarise(mean_dtw = mean(`DTW..ft.`, na.rm = TRUE), .groups = "drop")


ggplot(monitoring_trend2, aes(x = year, y = mean_dtw)) +
  geom_line(linewidth = 1.2, color = "darkgreen") +
  labs(title = "Willcox Basin – Monitoring Wells (Avg Depth to Water)",
       x = "Year", y = "Depth to Water (ft)") +
  theme_minimal()





## now lets look at well elevation over time - this may illustrate volume better

wells_hua$year <- year(wells_hua$WL.Date)

elev_trend <- wells_hua |>
  filter(!is.na(`WL.Elev..ft.amsl.`)) |>
  group_by(year) |>
  summarise(mean_elev = mean(`WL.Elev..ft.amsl.`, na.rm = TRUE), .groups = "drop")

hua_elev <- ggplot(elev_trend, aes(x = year, y = mean_elev)) +
  geom_line(linewidth = 2.0, color = "#1E90FF") +
  labs(title = "Hualapai Basin – Avg Water Level Elevation Over Time",
       x = "Year", y = "Water Level Elevation (ft amsl)") +
  theme_grey()
hua_elev 

## export as PNG and PDF
png("plot/hua_elev.png", width = 3600, height = 2400, res = 300)
print(hua_elev)
dev.off()


## Duplicate for Willcox

wells_wil$year <- year(wells_wil$WL.Date)

elev_trend2 <- wells_wil |>
  filter(!is.na(`WL.Elev..ft.amsl.`)) |>
  group_by(year) |>
  summarise(mean_elev = mean(`WL.Elev..ft.amsl.`, na.rm = TRUE), .groups = "drop")

Will_elev <- ggplot(elev_trend2, aes(x = year, y = mean_elev)) +
  geom_line(linewidth = 2.0, color = "#1E90FF") +
  labs(title = "Willcox Basin – Avg Water Level Elevation Over Time",
       x = "Year", y = "Water Level Elevation (ft amsl)") +
  theme_gray()

Will_elev

## export as PNG and PDF
png("plot/Willcox_elev.png", width = 3600, height = 2400, res = 300)
print(Will_elev)
dev.off()

## Creating Maps of wells over the ground water Basins
## import county and groundwater basin shape files
county_az <- st_read("Data/Arizona_County_Boundary/Arizona_County_Boundary.shp")
ADWR <- st_read("Data/AZ_groundwater_subbasin_24/ADWR_Groundwater_Subbasin_2024.shp")

## Reproject data sets to NAD83 / UTM zone 12N (26912) for consistency in mapping and analysis
# This ensures alignment with base maps and other spatial layers
county_az <- st_transform(county_az, crs = 26912)
ADWR <- st_transform(ADWR, crs = 26912)
wells_hua <- st_transform(wells_hua, crs = 26912)
wells_wil <- st_transform(wells_wil, crs = 26912)

## Create custom county labels

county_labels <- tibble(
  NAME = c("Mohave", "Graham", "Cochise"),
  x = c(-113.5, -109.9, -109.7),
  y = c(35.2, 32.9, 31.6)
) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(county_az))

tm_shape(county_labels) +
  tm_text("NAME", size = 0.8, col = "gray20")

## Create county well map - Hualapai Basin - Mohave County

mohave <- county_az %>% 
  filter(NAME == "Mohave")  # adjust column name if needed
basin_mohave <- st_intersection(ADWR, mohave)

hualapai_basin <- ADWR %>%
  filter(SUBBASIN_N == "HUALAPAI VALLEY") %>%
  st_intersection(mohave)

basin_outline <- tm_shape(hualapai_basin) +
  tm_borders(col = "darkblue", lwd = 2)

tmap_mode("plot")

hua_map <- tm_shape(mohave) +
  tm_borders(col = "gray40") +
  tm_shape(wells_hua) +
  tm_dots(col = "black", size = 0.2) +
  basin_outline +
  tm_title("Hualapai Groundwater Basin\n Groundwater Wells") +
  tm_scalebar(position = c("right", "bottom")) +
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_layout(frame = FALSE) 

hua_map <- tm_shape(mohave) +
  tm_borders(col = "gray40") +
  tm_shape(mohave) +
  tm_text("NAME", size = 0.8, col = "red") +
  tm_shape(wells_hua) +
  tm_dots(col = "black", size = 0.2) +
  tm_shape(hualapai_basin) +
  tm_borders(col = "blue", lwd = 2) +
  tm_title("Hualapai Groundwater Basin\n Groundwater Wells") +
  tm_scalebar(position = c("right", "bottom")) +
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_layout(frame = FALSE)

hua_map <- tm_shape(mohave) +
  tm_borders(lwd = 2.0, col = "gray40") +
  tm_shape(county_labels %>% filter(NAME == "Mohave")) +
  tm_text("NAME", size = 0.8, col = "red") +
  tm_shape(wells_hua) +
  tm_dots(col = "black", size = 0.2) +
  tm_shape(hualapai_basin) +
  tm_borders(col = "blue", lwd = 2) +
  tm_title("Hualapai Groundwater Basin\n Groundwater Wells") +
  tm_scalebar(position = c("right", "bottom")) +
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_layout(frame = FALSE)

hua_map
## export as PNG and PDF
pdf("plot/hualapai_map.pdf", width = 12, height = 8)
print(hua_map)
dev.off()

png("plot/hualapai_map.png", width = 3600, height = 2400, res = 300)
print(hua_map)
dev.off()

## Duplicate workflow for Willcox Basin - Graham and Cochise Counties

willcox_counties <- county_az %>%
  filter(NAME %in% c("Graham", "Cochise"))

willcox_basin <- ADWR %>%
  filter(SUBBASIN_N == "WILLCOX") %>%
  st_intersection(willcox_counties)

tmap_mode("plot")

wil_map <- tm_shape(willcox_counties) +
  tm_borders(col = "gray40") +
  tm_shape(wells_wil) +
  tm_dots(col = "black", size = 0.2) +
  tm_shape(willcox_basin) +
  tm_borders(col = "blue", lwd = 2) +
  tm_title("Willcox Groundwater Basin\n Groundwater Wells") +
  tm_scalebar(position = c("left", "bottom")) +   # ✅ updated function
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_layout(frame = FALSE)

wil_map <- tm_shape(willcox_counties) +
  tm_borders(lwd = 2.0, col = "gray40") +
  tm_shape(willcox_counties) +
  tm_text("NAME", size = 0.8, col = "red") +
  tm_shape(wells_wil) +
  tm_dots(col = "black", size = 0.2) +
  tm_shape(willcox_basin) +
  tm_borders(col = "blue", lwd = 2) +
  tm_title("Willcox Groundwater Basin\n Groundwater Wells") +
  tm_scalebar(position = c("left", "bottom")) +
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_layout(frame = FALSE)

wil_map
## export as PNG and PDF
png("plot/willcox_map.png", width = 3600, height = 2400, res = 300)
print(wil_map)
dev.off()

pdf("plot/willcox_map.pdf", width = 12, height = 8)
print(wil_map)
dev.off()

wells_wil_trimmed <- st_intersection(wells_wil, willcox_basin)

## the following maps are zoomed in to the basins only
wil_zoom <- ggplot() +
  geom_sf(data = willcox_basin, fill = NA, color = "blue", linewidth = 1.2) +
  geom_sf(data = wells_wil_trimmed, color = "black", size = 0.8) +
  coord_sf(xlim = st_bbox(willcox_basin)[c("xmin", "xmax")],
           ylim = st_bbox(willcox_basin)[c("ymin", "ymax")],
           expand = FALSE) +
  labs(title = "Willcox Basin – Groundwater Wells") +
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

wil_zoom 
## export as PNG and PDF
png("plot/wil_zoom.png", width = 3600, height = 2400, res = 300)
print(wil_zoom)
dev.off()

pdf("plot/wil_zoom.pdf", width = 12, height = 8)
print(wil_zoom)
dev.off()

wells_hua_trimmed <- st_intersection(wells_hua, hualapai_basin)

hua_zoom <- ggplot() +
  geom_sf(data =hualapai_basin, fill = NA, color = "blue", linewidth = 1.2) +
  geom_sf(data = wells_hua_trimmed, color = "black", size = 0.8) +
  coord_sf(xlim = st_bbox(hualapai_basin)[c("xmin", "xmax")],
           ylim = st_bbox(hualapai_basin)[c("ymin", "ymax")],
           expand = FALSE) +
  labs(title = "Hualapai Basin – Groundwater Wells") +
  theme_minimal()+
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

hua_zoom

hua_zoom2 <- ggplot() +
  geom_sf(data = hualapai_basin, fill = NA, color = "blue", linewidth = 1.2) +
  geom_sf(data = wells_hua_trimmed, color = "black", size = 0.8) +
  coord_sf(xlim = st_bbox(hualapai_basin)[c("xmin", "xmax")],
           ylim = st_bbox(hualapai_basin)[c("ymin", "ymax")],
           expand = FALSE) +
  scale_x_continuous(breaks = seq(114.2, 113.9, by = -0.2)) +  # adjust as needed
  labs(title = "Hualapai Basin\n                   – Groundwater Wells") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

hua_zoom2
## export as PNG and PDF
png("plot/hua_zoom2.png", width = 3600, height = 2400, res = 300)
print(hua_zoom2)
dev.off()

pdf("plot/hua_zoom2.pdf", width = 12, height = 8)
print(hua_zoom2)
dev.off()


wil_zoom2 <- ggplot() +
  geom_sf(data = willcox_basin, fill = NA, color = "blue", linewidth = 1.2) +
  geom_sf(data = wells_wil_trimmed, color = "black", size = 0.8) +
  coord_sf(xlim = st_bbox(willcox_basin)[c("xmin", "xmax")],
           ylim = st_bbox(willcox_basin)[c("ymin", "ymax")],
           expand = FALSE) +
  scale_x_continuous(breaks = seq(114.2, 113.9, by = -0.2)) +  # adjust as needed
  labs(title = "Willcox Basin\n                   – Groundwater Wells") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

wil_zoom2 
## export as PNG and PDF
png("plot/wil_zoom2.png", width = 3600, height = 2400, res = 300)
print(wil_zoom2)
dev.off()

pdf("plot/wil_zoom2.pdf", width = 12, height = 8)
print(wil_zoom2)
dev.off()

## minor changes in graphics

wil_zoom3 <- ggplot() +
  geom_sf(data = willcox_basin, fill = NA, color = "blue", linewidth = 1.2) +
  geom_sf(data = wells_wil_trimmed, color = "black", size = 0.8) +
  coord_sf(xlim = st_bbox(willcox_basin)[c("xmin", "xmax")],
           ylim = st_bbox(willcox_basin)[c("ymin", "ymax")],
           expand = FALSE) +
  scale_x_continuous(breaks = seq(114.2, 113.9, by = -0.2)) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering) +
  labs(title = "Willcox Basin – Groundwater Wells") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

wil_zoom3 
## export as PNG and PDF

png("plot/wil_zoom3.png", width = 3600, height = 2400, res = 300)
print(wil_zoom3)
dev.off()


hua_zoom3 <- ggplot() +
  geom_sf(data = hualapai_basin, fill = NA, color = "blue", linewidth = 1.2) +
  geom_sf(data = wells_hua_trimmed, color = "black", size = 0.8) +
  coord_sf(xlim = st_bbox(hualapai_basin)[c("xmin", "xmax")],
           ylim = st_bbox(hualapai_basin)[c("ymin", "ymax")],
           expand = FALSE) +
  scale_x_continuous(breaks = seq(114.2, 113.9, by = -0.2)) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering) +
  labs(title = "Hualapai Basin – Groundwater Wells") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

hua_zoom3 
## export as PNG and PDF
png("plot/hua_zoom3.png", width = 3600, height = 2400, res = 300)
print(hua_zoom3)
dev.off()
