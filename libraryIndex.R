# Assuming you have loaded the library_data dataframe with your survey data

# Step 1: Define Weights
weights <- c(
  POPU_LSA17 = 0.1,             # Population served
  TOTCIR = 0.15,                # Total circulation
  LIBRARIA = 0.1,               # Total librarians
  VISITS = 0.1,                 # Number of library visits
  REFERENC = 0.05,              # Number of reference transactions
  LOANTO = 0.05,                # Inter-library loans out
  PITUSR = 0.1,                 # Public internet computer uses per year
  SQ_FEET = 0.1,                # Square footage
  ELMATCIR = 0.1,               # Circulation of electronic materials
  ELINFO = 0.05,                # Retrieval of electronic information
  ELCONT = 0.1                  # Electronic content use
)

# Step 2: Normalize Data
library(dplyr)

normalized_data <- library_data %>%
  mutate_at(vars(-Grouping_Variables), scale)

# Step 3: Aggregate Variables
composite_scores <- library_data %>%
  mutate(
    Circulation = (TOTCIR + ELMATCIR) / 2,
    Technology_Use = (PITUSR + ELINFO + ELCONT) / 3,
    Service = (VISITS + REFERENC + LOANTO) / 3
  )

# Step 4: Calculate Sub-Indices
sub_indices <- composite_scores %>%
  select(Circulation, Technology_Use, Service) %>%
  rowSums()

# Step 5: Weighted Aggregation
library_quality_index <- sum(weights * sub_indices)

# Step 6: Sensitivity Analysis - Not demonstrated in this example

# Step 7: Validation - Not demonstrated in this example

# Step 8: Index Interpretation
print(paste("Library Quality Index:", library_quality_index))