setwd("/Users/hulyaseyidoglu/Library/CloudStorage/OneDrive-UniversityofLeeds/Phd Projects/Police.uk/")
main_dir <- "/Users/hulyaseyidoglu/Library/CloudStorage/OneDrive-UniversityofLeeds/Phd Projects/Police.uk/"
#dir.create('data')

library(dplyr)
library(data.table)
library(readr)
library(lubridate)
library(sf)
library(purrr)
library(tibble)
library(janitor)
library(tidyr)
library(stringr)

`%nin%` <- Negate(`%in%`)

process_year_data <- function(year, police_data_dir = "police_data") {
  file_pattern <- paste0(year, "*street.csv")
  csv_files <- list.files(police_data_dir, pattern = glob2rx(file_pattern), recursive = TRUE, full.names = TRUE)
  
  data <- lapply(csv_files, read_csv)
  
  full_data <- data %>%
    bind_rows() %>%
    clean_names() %>%
    mutate(year = as.character(year))
  
    sub_data <- full_data %>%
      filter(reported_by != "Greater Manchester Police", 
             reported_by != "Police Service of Northern Ireland")

  setDT(sub_data)
  sub_data_agg <- sub_data[, .(crime_count = .N), by = .(crime_type, month, year, lsoa_code, lsoa_name, reported_by)]
  
  return(sub_data_agg)
}


police_data_dir <- "/Users/hulyaseyidoglu/Library/CloudStorage/OneDrive-UniversityofLeeds/Phd Projects/Police.uk/police_data"
all_years_data_1517 <- lapply(2015:2017, process_year_data, police_data_dir = police_data_dir) 
all_years_data_1820 <- lapply(2018:2020, process_year_data, police_data_dir = police_data_dir)
all_years_data_2123 <- lapply(2021:2023, process_year_data, police_data_dir = police_data_dir)
all_years_data_24 <- lapply(2024, process_year_data, police_data_dir = police_data_dir)



# Combine all years into 1 df
full_dataset_1517 <- bind_rows(all_years_data_1517)
full_dataset_1820 <- bind_rows(all_years_data_1820)
full_dataset_2123 <- bind_rows(all_years_data_2123)
full_dataset_24 <- bind_rows(all_years_data_24)
rm(all_years_data_1517, all_years_data_1820, all_years_data_2123, all_years_data_24)

lsoa_sf <- st_read("/Users/hulyaseyidoglu/Library/CloudStorage/OneDrive-UniversityofLeeds/Phd Projects/Police.uk/infuse_lsoa_lyr_2011_clipped/infuse_lsoa_lyr_2011_clipped.shp")

# Subset LSOAs for E&W and then removing GM data
lsoa_ew_sf <- lsoa_sf %>% 
  mutate(country_cd = str_extract(geo_code, "^.{1}")) %>% 
  filter(country_cd == "E" | country_cd == "W",
         !str_detect(geo_label, "Manchester|Bolton|Oldham|Rochdale|Stockport|Tameside|Salford|Bury|Trafford|Wigan"))

# Remove GM and NI geometries
gm_lsoa <- lsoa_sf %>% 
  filter(str_detect(geo_label, "Manchester|Bolton|Oldham|Rochdale|Stockport|Tameside|Salford|Bury|Trafford|Wigan")) %>% 
  select(geo_code) %>% 
  pluck(1)

ni_lsoa <- lsoa_sf %>%
  mutate(country_cd = str_extract(geo_code, "^.{1}")) %>% 
  filter(country_cd == "9") %>% 
  select(geo_code) %>% 
  pluck(1)

# Remove objects to save space if needed.
rm(lsoa_sf, lsoa_ew_sf)

# Remove the GM and NI LSOA from the crime data.
full_dataset_1517 <- full_dataset_1517 %>% 
  filter(lsoa_code %nin% gm_lsoa, lsoa_code %nin% ni_lsoa)

full_dataset_1820 <- full_dataset_1820 %>% 
  filter(lsoa_code %nin% gm_lsoa, lsoa_code %nin% ni_lsoa)

full_dataset_2123 <- full_dataset_2123 %>% 
  filter(lsoa_code %nin% gm_lsoa, lsoa_code %nin% ni_lsoa)

full_dataset_24 <- full_dataset_24 %>% 
  filter(lsoa_code %nin% gm_lsoa, lsoa_code %nin% ni_lsoa)

## NON-VALIDATED FULL DATASET IS REQUIRED ONLY FOR RQ1 ##
full_dataset_nv <- bind_rows(full_dataset_1517, full_dataset_1820, full_dataset_2123, full_dataset_24)
write_csv(full_dataset_nv, file = '/Users/hulyaseyidoglu/Library/CloudStorage/OneDrive-UniversityofLeeds/Phd Projects/Police.uk/full_dataset_nv.csv')

# Continue from here if geo validated dataset is needed

# Check validity of the remaining LSOAs.
validity_check <- st_is_valid(lsoa_ew_sf)

# N = 4 invalid geometries.
table(validity_check) 

# Identify them
lsoa_ew_sf <- lsoa_ew_sf %>% 
  mutate(valid = validity_check)

# Correct invalid geometries
lsoa_ew_valid_sf <- st_make_valid(lsoa_ew_sf)

# Check validity again
validity_check <- st_is_valid(lsoa_ew_valid_sf)
table(validity_check)

# Identify which geometries are still invalid (if any)
invalid_geometries <- lsoa_ew_valid_sf %>% filter(!validity_check)
print(invalid_geometries)

full_dataset_1517 <- full_dataset_1517 %>% 
  drop_na(lsoa_code)
full_dataset_1820 <- full_dataset_1820 %>% 
  drop_na(lsoa_code)
full_dataset_2123 <- full_dataset_2123 %>% 
  drop_na(lsoa_code)
full_dataset_24 <- full_dataset_24 %>% 
  drop_na(lsoa_code)

# Check. 
table(st_is_valid(lsoa_ew_valid_sf))
length(unique(lsoa_ew_valid_sf$geo_code))
length(unique(full_dataset_1517$lsoa_code))
length(unique(full_dataset_1820$lsoa_code))
length(unique(full_dataset_2123$lsoa_code))
length(unique(full_dataset_24$lsoa_code))


missing_lsoa <- lsoa_ew_valid_sf %>% 
  filter(lsoa_ew_valid_sf$geo_code %nin% full_dataset_1820$lsoa_code) 

lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  filter(geo_code %nin% missing_lsoa$geo_code)

full_dataset_1517 <- full_dataset_1517 %>%
  semi_join(lsoa_ew_valid_sf, by = c("lsoa_code" = "geo_code"))
full_dataset_1820 <- full_dataset_1820 %>%
  semi_join(lsoa_ew_valid_sf, by = c("lsoa_code" = "geo_code"))

missing_lsoa <- lsoa_ew_valid_sf %>% 
  filter(lsoa_ew_valid_sf$geo_code %nin% full_dataset_1820$lsoa_code) 

lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  filter(geo_code %nin% missing_lsoa$geo_code)

full_dataset_2123 <- full_dataset_2123 %>%
  semi_join(lsoa_ew_valid_sf, by = c("lsoa_code" = "geo_code"))
full_dataset_24 <- full_dataset_24 %>%
  semi_join(lsoa_ew_valid_sf, by = c("lsoa_code" = "geo_code"))

length(unique(lsoa_ew_valid_sf$geo_code))
length(unique(full_dataset_1517$lsoa_code))
length(unique(full_dataset_1820$lsoa_code))
length(unique(full_dataset_2123$lsoa_code))
length(unique(full_dataset_24$lsoa_code))

rm(missing_lsoa)

full_dataset <- bind_rows(full_dataset_1517, full_dataset_1820, full_dataset_2123)
rm(full_dataset_1517, full_dataset_1820, full_dataset_2123)

# Save the combined dataset
write_csv(full_dataset, file = '/Users/hulyaseyidoglu/Library/CloudStorage/OneDrive-UniversityofLeeds/Phd Projects/Police.uk/full_dataset.csv')

## GREATER MANCHESTER DATA ##

process_year_data <- function(year, police_data_dir = "police_data") {
  file_pattern <- paste0(year, "*street.csv")
  csv_files <- list.files(police_data_dir, pattern = glob2rx(file_pattern), recursive = TRUE, full.names = TRUE)
  
  data <- lapply(csv_files, read_csv)
  
  full_data <- data %>%
    bind_rows() %>%
    clean_names() %>%
    mutate(year = as.character(year))
  
  sub_data <- full_data %>%
    filter(reported_by == "Greater Manchester Police") %>%
    drop_na(lsoa_code)
  
  setDT(sub_data)
  sub_data_agg <- sub_data[, .(crime_count = .N), by = .(crime_type, month, year, lsoa_code, lsoa_name, reported_by)]
  
  return(sub_data_agg)
}


police_data_dir <- "/Users/hulyaseyidoglu/Library/CloudStorage/OneDrive-UniversityofLeeds/Phd Projects/Police.uk/police_data"
all_years_data_1517 <- lapply(2015:2017, process_year_data, police_data_dir = police_data_dir) 

all_years_data_1820 <- lapply(2018:2020, process_year_data, police_data_dir = police_data_dir)
all_years_data_2123 <- lapply(2021:2023, process_year_data, police_data_dir = police_data_dir)



# Combine all years into 1 df
full_dataset_1517 <- bind_rows(all_years_data_1517)
full_dataset_1820 <- bind_rows(all_years_data_1820)
full_dataset_2123 <- bind_rows(all_years_data_2123)

GM_police <- bind_rows(full_dataset_1517, full_dataset_1820, full_dataset_2123)
write_csv(GM_police, file = '/Users/hulyaseyidoglu/Library/CloudStorage/OneDrive-UniversityofLeeds/Phd Projects/Police.uk/GM_police_full.csv')

## try shoplifting data fro 2021-2023 to explain difference in counts between ons report (drop_na lsoa_code is removed) ##

shoplifting_full <- full_dataset_2123 %>% filter(crime_type == "Shoplifting") %>% 
  group_by(crime_type, month, year) %>% 
  summarise(crime_count = sum(crime_count, na.rm = TRUE)) %>% 
  ungroup()

write_csv(shoplifting_full, file = '/Users/hulyaseyidoglu/Library/CloudStorage/OneDrive-UniversityofLeeds/Phd Projects/Police.uk/shoplifting_full_2123.csv')
