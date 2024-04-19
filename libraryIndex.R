# Assuming you have loaded the library_data dataframe with your survey data

# Step 1: Define Weights
weights <- c(
  Circulation = 0.4,            # Weight for circulation-related variables
  Technology_Use = 0.3,         # Weight for technology-related variables
  Service = 0.3                 # Weight for service-related variables
)

# Step 2: Normalize Data
library(dplyr)
library(tidyverse)

library_data <- read.csv("./data/library-survey-2021.csv")

# Step 3: Aggregate Variables
composite_scores <- library_data %>%
  mutate(
    Circulation = (TOTCIR + ELMATCIR) / 2,
    Technology_Use = (PITUSR + ELINFO + ELCONT) / 3,
    Service = (VISITS + REFERENC + LOANTO) / 3
  )

# Step 4: Calculate Weighted Composite Scores for Each Library
library_data$Library_Usage_Index <- rowSums(composite_scores[, c("Circulation", "Technology_Use", "Service")] * weights)

# Step 5: Output the Resulting Library Quality Index for Each Library
library_data <- library_data %>% select(CNTY, Library_Usage_Index, STABR) %>%
  mutate(county_state = paste(tolower(trimws(CNTY)), trimws(STABR), sep = ", "))

clean_pen_index_2 <- read.csv("./outputs/clean_pen_index.csv") %>%
  select(code, county_state) %>%
  distinct() %>%
  left_join(library_data, by = "county_state") %>%
  group_by(code) %>%
  summarise(avg_index = mean(Library_Usage_Index)) %>%
  ungroup() %>%
  mutate(std_avg_index = scale(avg_index))


