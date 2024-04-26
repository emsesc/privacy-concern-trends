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
    Circulation = rowMeans(select(., c("TOTCIR", "ELMATCIR", "PHYSCIR", "TOTCOLL")), na.rm = TRUE),
    Technology_Use = rowMeans(select(., c("PITUSR", "ELINFO", "ELCONT")), na.rm = TRUE),
    Service = rowMeans(select(., c("VISITS", "REFERENC", "LOANTO", "LIBRARIA")), na.rm = TRUE)
  )

# Step 4: Calculate Weighted Composite Scores for Each Library
library_data$Library_Usage_Index <- rowSums(composite_scores[, c("Circulation", "Technology_Use", "Service")] * weights, na.rm = TRUE)
library_data <- library_data %>%
  rowwise() %>%
  mutate(Library_Revenue = sum(CAP_REV, TOTINCM, na.rm = TRUE), Library_Expenses = sum(TOTOPEXP, CAPITAL, na.rm = TRUE))

# Step 5: Output the Resulting Library Quality Index for Each Library
library_data <- library_data %>% select(CNTY, Library_Usage_Index, STABR, Library_Revenue, Library_Expenses) %>%
  mutate(county_state = paste(tolower(trimws(CNTY)), trimws(STABR), sep = ", "))

library_index_2021 <- read.csv("./outputs/clean_pen_index.csv") %>%
  select(code, county_state) %>%
  distinct() %>%
  left_join(library_data, by = "county_state") %>%
  group_by(code) %>%
  summarise(avg_index_2021 = mean(Library_Usage_Index, na.rm = TRUE), 
            num_lib = n(),
            avg_revenue_2021 = mean(Library_Revenue, na.rm = TRUE),
            avg_expenses_2021 = mean(Library_Expenses, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(std_avg_index_2021 = scale(avg_index_2021))


