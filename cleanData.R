library(dplyr)
library(tidyverse)
library(purrr)

# Notes on cleaning: some of the book ban dates were only specified as "Fall" or "Spring"

library_survey <- library_survey %>%
  mutate(County_Merge = paste(tolower(CNTY), "county", sep = " "))
pen_index_2023 <- read.csv("./data/pen-index-2023.csv") %>% 
  select(Title, State, District, Date.of.Challenge.Removal) 
pen_index_2022 <- read.csv("./data/pen-index-2022.csv") %>% 
  select(Title, State, District, Date.of.Challenge.Removal) 
school_districts <- read.csv("./data/sdlist-23.csv") %>%
  mutate(school_district = tolower(School.District.Name)) %>%
  mutate(County_Merge = tolower(trimws(County.Names))) %>%
  select(County_Merge, County.FIPS, school_district)
county_dma <- read.csv("./data/county_dma_mapping.csv") %>%
  mutate(county_state = paste(tolower(trimws(COUNTY)), trimws(STATE), sep = ", ")) %>%
  mutate(merger = ifelse(DMAINDEX == 143, "ERIE", ifelse(DMAINDEX == 35, "GREENVILLE-SPARTA-ASHEVILLE", 
                         ifelse(DMAINDEX == 178, "HARRISONBURG", 
                                ifelse(DMAINDEX == 189, "LAFAYETTE, IN", 
                                       ifelse(DMAINDEX == 37, "SAN ANTONIO", 
                                              ifelse(DMAINDEX == 77, "ROCHESTER, NY", 
                                                     ifelse(DMAINDEX == 126, "COLUMBUS, GA-OPELIKA, AL",
                                                            ifelse(DMAINDEX == 131, "COLUMBUS-TUPELO-WEST POINT",
                                                                   ifelse(DMAINDEX == 139, "COLUMBIA-JEFFERSON CITY",
                                                                          ifelse(DMAINDEX == 84, "COLUMBIA, SC",
                                                                                 ifelse(DMAINDEX == 34, "COLUMBUS, OH", 
                                                                                        ifelse(DMAINDEX == 519, "CHARLESTON, SC", 
                                                                                               ifelse(DMAINDEX == 561, "JACKSONVILLE", 
                                                                                                      ifelse(DMAINDEX == 639, "JACKSON, TN", 
                                                                                                             ifelse(DMAINDEX == 582, "LAFAYETTE, IN", 
                                                                                                                    ifelse(DMAINDEX == 627, "WICHITA FALLS & LAWTON", substr(DMA, 1, 6))))))))))))))))))
         
dma_codes <- read.csv("./data/dma-codes.csv") %>%
  mutate(merger = ifelse(area %in% c("ERIE", "GREENVILLE-SPARTA-ASHEVILLE", "HARRISONBURG", "LAFAYETTE, IN", "SAN ANTONIO", "ROCHESTER, NY", "COLUMBUS, GA-OPELIKA, AL", "COLUMBUS-TUPELO-WEST POINT", "COLUMBIA-JEFFERSON CITY", "COLUMBIA, SC", "COLUMBUS, OH", "CHARLESTON, SC", "JACKSONVILLE", "JACKSON, TN", "WICHITA FALLS & LAWTON"), area, substr(area, 1, 6)))

# Goals:
# 1. Remove unnecessary rows
# 2. Mark each book by DMA
# 3. Standardize by time
# 4. Merge in DMA demographic statistics?

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
      year <- paste0("20", parts[2])
      c(as.character(month_number), year)
    }
  )
  return(dates)
}

# Bind two pen indexes together and split their dates
clean_pen_index <- pen_index_2022 %>%
  rbind(pen_index_2023) %>%
  rowwise() %>%
  mutate(Month = translate_date(Date.of.Challenge.Removal)[1],  # Translate month abbreviations to numbers
         Year = translate_date(Date.of.Challenge.Removal)[2]) # Adding '20' as prefix to year

# create counties and DMA google trends
clean_pen_index <- clean_pen_index %>%
  mutate(school_district = tolower(District)) %>%
  left_join(school_districts, by = "school_district") %>%
  mutate(county_state = paste(sub(" county$", "", County_Merge), state.abb[match(State, state.name)], sep = ", ")) %>%
  left_join(county_dma, by = "county_state") %>%
  left_join(dma_codes, by = "merger", relationship = "many-to-many") %>%
  select(-GOOGLE_DMA, -merger, -area) %>%
  mutate(Trends_DMA = paste0("US-", str_trim(STATE), "-", code))

google_trends_analysis <- clean_pen_index %>%
  select(Month, Year, Trends_DMA) %>%
  group_by(Month, Year, Trends_DMA) %>%
  summarize(count = n()) %>%
  distinct()

# Remove anything with NA and less than 30 bans
google_trends_analysis <- google_trends_analysis[!(grepl("NA", google_trends_analysis$Trends_DMA, fixed = TRUE) | google_trends_analysis$count < 30), ]

write.csv(clean_pen_index, "clean_pen_index.csv")


# Only run if you want to create pen index merged with local libraries
pen_index_libraries <- clean_pen_index %>%
  left_join(library_survey, by = "County_Merge", relationship = "many-to-many")
  