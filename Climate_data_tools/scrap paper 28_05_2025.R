
Day_start_GS_rainfall <- 1
Month_start_GS_rainfall <- 4


Day_end_GS_rainfall <- 16 #if you want the range to include the 15th this needs to be 16th
Month_end_GS_rainfall <- 10


### summer dates after GS
Day_start_Summer_rainfall_after_GS <- 1
Month_start_summer_rainfall_after_GS <- 11

Day_end_summer_rainfall_after_GS <- 31 
Month_end_summer_rainfall_after_GS <- 12

### summer dates before GS
Day_start_Summer_rainfall_before_GS <- 1
Month_start_summer_rainfall_before_GS <- 1

Day_end_summer_rainfall_before_GS <- 1 # if you want this to be the 31 /3 you need to add a day ie 1/4
Month_end_summer_rainfall_before_GS <- 4

site_selected <- "25006"


path <-  file.path("H:","Output-2", "Analysis", "Scripts", "Jackie", "Downloaded_files")

df <- read.csv(paste0(path, "/Neater_file_", site_selected,  ".csv")) 
#df2000 <- df %>%  filter(year == 2000) %>%  select(Date, month_name, month, day_of_month )


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

#df <- df %>% select(-start_end_GS_date)


## add a clm with two dates marked as start and end of Summer

df <- df %>% mutate(
  start_end_summer_date = case_when(
    #before GS
    month == Month_start_summer_rainfall_before_GS & 
      day_of_month == Day_start_Summer_rainfall_before_GS ~ "start_summer", 
    month == Month_end_summer_rainfall_before_GS & 
      day_of_month == Day_end_summer_rainfall_before_GS ~ "end_summer",
    
    #after GS
    month == Month_start_summer_rainfall_after_GS & 
      day_of_month == Day_start_Summer_rainfall_after_GS ~ "start_summer", 
    month == Month_end_summer_rainfall_after_GS & 
      day_of_month == Day_end_summer_rainfall_after_GS ~ "end_summer", 
    
    #after GS to end of year
    TRUE ~ NA))

### fill the blanks

df <- df %>% fill(start_end_summer_date) %>%
  mutate(
    season_summer = case_when(
      start_end_summer_date == "start_summer" ~ "summer",
      month == 12 & day_of_month == 31 ~ "summer",
      TRUE ~ "other"))

#df <- df %>% select(-start_end_summer_date)

test_df <- df %>%  filter(year == 2000) %>%  select(Date, month_name, start_end_GS_date, start_end_summer_date, season_GS, season_summer )


test_df <- test_df %>% mutate(
  season = (case_when(
    season_summer == "summer" ~ "summer",
    season_GS == "gs" ~ "gs",
    TRUE ~ "other"
  )))

## summer seems to be one day out
## next job is to merge the season_GS and saeson_summer into one clm called season - good its working now - put in back into R markdown


#File start date and end date
paste("Start date in file is: ",
      min(df$Date),
      ". End date in file is: ",
      max(df$Date))


paste("Site Name (BOM): ", unique(df$Name),
      ". Site Number (BOM): ", unique(df$Number)
)


GS_defined_as <- paste0(
  Day_start_GS_rainfall, "/", Month_start_GS_rainfall,
  " to ", Day_end_GS_rainfall, "/", Month_end_GS_rainfall)

summer_defined_as <- paste0(
  Day_start_Summer_rainfall_after_GS, "/", Month_start_summer_rainfall_after_GS,
  " to ", Day_end_summer_rainfall_before_GS, "/", Month_end_summer_rainfall_before_GS)

paste("summer month: ", summer_defined_as)
paste("GS season: ", GS_defined_as)
