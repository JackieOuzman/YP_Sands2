
Day_start_GS_rainfall <- 1
Month_start_GS_rainfall <- 4


Day_end_GS_rainfall <- 15
Month_end_GS_rainfall <- 10

Day_start_Summer_rainfall <- 1
Month_start_summer_rainfall <- 11


Day_end_summer_rainfall <- 30
Month_end_summer_rainfall <- 3


site_selected <- "25006"


path <-  file.path("H:","Output-2", "Analysis", "Scripts", "Jackie", "Downloaded_files")

df <- read.csv(paste0(path, "/Neater_file_", site_selected,  ".csv")) 

## add a clm with two dates marked as start and end of GS
df <- df %>% mutate(
  start_end_GS_date = case_when(
    month == Month_start_GS_rainfall & day_of_month == Day_start_GS_rainfall ~ "start_gs",
    month == Month_end_GS_rainfall & day_of_month == Day_end_GS_rainfall ~ "end_gs",
    TRUE ~ NA))

### fill the blanks

df <- df %>% fill(start_end_GS_date) %>%
  mutate(
    season_GS = case_when(
      start_end_GS_date == "start_gs" ~ "gs",
      TRUE ~ "other"))

df <- df %>% select(-start_end_GS_date)


## add a clm with two dates marked as start and end of Summer
df <- df %>% mutate(
  start_end_summer_date = case_when(
    month == Month_start_summer_rainfall & day_of_month == Day_start_Summer_rainfall ~ "start_summer",
    month == Month_end_summer_rainfall & day_of_month == Day_end_summer_rainfall ~ "end_summer",
    TRUE ~ NA))

### fill the blanks

df <- df %>% fill(start_end_summer_date) %>%
  mutate(
    season_summer = case_when(
      start_end_summer_date == "start_summer" ~ "summer",
      TRUE ~ "other"))

df <- df %>% select(-start_end_summer_date)


## summer seems to be one day out
## next job is to merge the season_GS and saeson_summer into one clm called season
