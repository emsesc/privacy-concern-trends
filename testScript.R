library_survey <- read.csv("./data/library-survey-2021.csv")
pen_index <- read.csv("./data/pen-index-2023.csv")
school_districts <- read.csv("./data/sdlist-23.csv")

library(dplyr)
library(stringr)

# Convert 'District' and 'CNTY' columns to lowercase
pen_index <- pen_index %>% 
  mutate(school_district = tolower(District))

school_districts <- school_districts %>%
  mutate(school_district = tolower(School.District.Name)) %>%
  mutate(County_Merge = tolower(County.Names))

library_survey <- library_survey %>%
  mutate(County_Merge = paste(tolower(CNTY), "county", sep = " "))

# Perform cross join
pen_index_counties <- left_join(pen_index, school_districts, by = "school_district", relationship = "many-to-many")
merged <- full_join(pen_index_counties, library_survey, by = "County_Merge", relationship = "many-to-many")


# Drop the intermediate lowercase columns
merged <- select(merged, -CNTY, -school_district, -Secondary.Author.s., -Illustrator.s., -Translator.s., -Series.Name)

################################## mapping

# Load necessary libraries
library(leaflet)
library(tidygeocoder)

merged <- merged %>%
  mutate(full_address = paste(ADDRESS, CITY, ZIP, sep = ", "))

# Define a color palette for the districts
district_colors <- rainbow(n_distinct(merged$District))

# Plotting
leaflet(merged) %>%
  addTiles() %>%
  addCircleMarkers(
    ~LONGITUD, ~LATITUDE,
    color = district_colors[as.factor(merged$District)],
    radius = sqrt(merged$TOTSTAFF[!is.na(merged$TOTSTAFF)]) * 0.1,  # Size scaled by TOTSTAFF, ignoring NaN values
    popup = ~paste(ADDRESS, "<br>", CITY, ", ", ZIP, sep = "")
  )