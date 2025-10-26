library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Load the dataset
pollution <- read_csv("global_air_pollution_dataset_cleaned.csv")

# See column names
glimpse(pollution)

# Stats about the dataset
summary(pollution)

# Checking for missing values
colSums(is.na(pollution))

# Converting text columns into factors for easier grouping
pollution <- pollution %>%
  mutate(
    Region = as.factor(Region),
    Country = as.factor(Country),
    City = as.factor(City),
    `AQI Category` = as.factor(`AQI Category`)
  )

# Avg AQI by country
aqi_country <- pollution %>%
  group_by(Country) %>%
  summarise(Avg_AQI = mean(`AQI Value`, na.rm = TRUE))
print(aqi_country)

# Calculating mean AQI by regions
mean_aqi_region <- pollution %>%
  group_by(Region) %>%
  summarise(Avg_AQI = mean(`AQI Value`, na.rm = TRUE)) %>%
  arrange(desc(Avg_AQI))
print(mean_aqi_region)

# Finding top 15 most polluted cities
top15_cities <- pollution %>%
  arrange(desc(`AQI Value`)) %>%
  select(Country, City, `AQI Value`, `AQI Category`) %>%
  head(15)
print(top15_cities)

ggplot(mean_aqi_region, aes(x = reorder(Region, -Avg_AQI), y = Avg_AQI, fill = Region)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Average AQI by Region",
    x = "Region",
    y = "Mean AQI Value"
  ) +
  theme_minimal()

ggplot(pollution, aes(x = `PM2.5 AQI Value`, y = `AQI Value`, color = `AQI Category`)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Relationship between PM2.5 and Overall AQI Value",
    x = "PM2.5 AQI Value",
    y = "Overall AQI Value"
  ) +
  theme_minimal()


ggplot(pollution, aes(x = Region, y = `AQI Value`, fill = Region)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.5) +
  labs(
    title = "AQI Distribution by Region",
    x = "Region",
    y = "AQI Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


pollution %>%
  group_by(`AQI Category`) %>%
  summarise(City_Count = n_distinct(City)) %>%
  ggplot(aes(x = `AQI Category`, y = City_Count, fill = `AQI Category`)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Number of Cities per AQI Category",
    x = "AQI Category",
    y = "Number of Cities"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Load world map (spatial data)
world <- ne_countries(scale = "medium", returnclass = "sf")

aqi_country <- aqi_country %>%
  mutate(Country = case_when(
    Country == "Bolivia (Plurinational State of)" ~ "Bolivia",
    Country == "Bosnia and Herzegovina" ~ "Bosnia and Herz.",
    Country == "Central African Republic" ~ "Central African Rep.",
    Country == "Democratic Republic of the Congo" ~ "Dem. Rep. Congo",
    Country == "Dominican Republic" ~ "Dominican Rep.",
    Country == "Equatorial Guinea" ~ "Eq. Guinea",
    Country == "Iran (Islamic Republic of)" ~ "Iran",
    Country == "Kingdom of Eswatini" ~ "eSwatini",
    Country == "Lao People's Democratic Republic" ~ "Laos",
    Country == "Republic of Korea" ~ "South Korea",
    Country == "Republic of Moldova" ~ "Moldova",
    Country == "Republic of North Macedonia" ~ "North Macedonia",
    Country == "Russian Federation" ~ "Russia",
    Country == "Saint Kitts and Nevis" ~ "St. Kitts and Nevis",
    Country == "Solomon Islands" ~ "Solomon Is.",
    Country == "South Sudan" ~ "S. Sudan",
    Country == "Syrian Arab Republic" ~ "Syria",
    Country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
    Country == "United Republic of Tanzania" ~ "Tanzania",
    Country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    TRUE ~ Country
  ))


# Join pollution data with map data
world_aqi <- left_join(world, aqi_country, by = c("name" = "Country"))

ggplot(data = world_aqi) +
  geom_sf(aes(fill = Avg_AQI), color = "white", size = 0.1) +
  scale_fill_gradient(
    name = "Average AQI",
    low = "lightblue",
    high = "red",
    na.value = "grey90"
  ) +
  labs(
    title = "Average Air Quality Index (AQI) by Country",
    subtitle = "Darker red indicates higher pollution levels",
    caption = "Source: Global Air Pollution Dataset"
  ) +
  theme_minimal()
