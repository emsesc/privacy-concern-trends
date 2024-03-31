library(dplyr)
library(tidyverse)

pen_index_2023 <- read.csv("./data/pen-index-2023.csv") %>% 
  select(Title, State, Date.of.Challenge.Removal) 
pen_index_2022 <- read.csv("./data/pen-index-2022.csv") %>% 
  select(Title, State, Date.of.Challenge.Removal) 

# Goals:
# 1. Remove unnecessary rows
# 2. Mark each book by DMA
# 3. Standardize by time

# Create a function to map month abbreviations to numbers
translate_date <- function(date_str) {
  dates <- case_when(
    date_str == "Fall 2022" ~ c("9", "2022"),
    date_str == "Spring 2023" ~ c("1", "2023"),
    date_str == "AY 2022-2023" ~ c("8", "2022"),
    (substr(date_str, 1, 1) == "2") ~ {
      parts <- strsplit(date_str, "-")[[1]]
      year <- paste0("20", parts[1])
      month <- parts[2]
      month_number <- match(month, month.abb)
      c(as.character(month_number), year)
    },
    TRUE ~ {
      parts <- strsplit(date_str, "-")[[1]]
      month <- parts[1]
      month_number <- match(month, month.abb)
      year <- as.character(parts[2])
      c(as.character(month_number), year)
    }
  )
  return(dates)
}

clean_pen_index <- pen_index_2022 %>%
  rbind(pen_index_2023) %>%
  rowwise() %>%
  mutate(Month = translate_date(Date.of.Challenge.Removal)[1],  # Translate month abbreviations to numbers
         Year = translate_date(Date.of.Challenge.Removal)[2]) # Adding '20' as prefix to year
