# Step 1 ----------------------------------------------------------------------


##Background   ####
### Rainfall expressed as decile.
#https://www.longpaddock.qld.gov.au/silo/point-data/

# Station point datasets
# Station point datasets are a time series of data at a station location, consisting of station records which have been supplemented by interpolated estimates when observed data are missing. 
# Station point datasets are available at approximately 8,000 station locations around Australia. These datasets were formerly known as SILO Patched Point datasets.
# Grid point datasets
# Grid point datasets are a time series of data at a grid point location consisting entirely of interpolated estimates. The data are taken from our gridded datasets and are available at any grid point over the land area of Australia (including some islands). The nominal grid location (where the interpolated surface is evaluated) is the centre of the corresponding grid cell. These datasets were formerly known as SILO Data Drill datasets.
# The data are taken from our gridded datasets and are available at any grid point over the land area of Australia (including some islands). 
# The nominal grid location (where the interpolated surface is evaluated) is the centre of the corresponding grid cell. These datasets were formerly known as SILO Data Drill datasets.
# "How are grided rainfall datasets created?
#  "
# Daily rainfall grided datasets are derived from interpolated monthly rainfall by partitioning the monthly total onto individual days. Partitioning requires estimation of the daily distribution throughout the month. 
# The distribution is obtained by direct interpolation of daily rainfall data throughout the month. At the end of the month, the interpolated monthly rainfall is then partitioned onto individual days according to the computed distribution.


## This script will automate the calculation of decile year ##
## point it to the folder the point data set has been downloaded
## I have selected a APSIM format starting with 1/1/1960 and saved as a .csv file


## Load Libraries -----------------------------------------------------------------

library(ggplot2)
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)


## Import met files -----------------------------------------------------------------

#Folder for met files
current.folder <- "H:/Output-2/Analysis/Scripts/Jackie/"

# find the files that you want
list.of.files <- list.files(current.folder, ".csv",full.names=T) #the trick is getting the full name
list.of.files #with path

#list.of.files <- c(
#  "H:/Output-2/Analysis/Scripts/Jackie/metfile_ Karoonda_025006.csv")

### Download all of the climate data
climate <- read.table(list.of.files, 
                      skip = 22, header = TRUE, sep ="")
climate <- climate [-1,]
climate <- climate %>% 
  mutate(date = strptime(paste(year, day), format="%Y %j"))

climate <- climate %>% 
  rename(day_of_year = day) %>% 
  mutate(year = year(date),
         month =month(date),
         day_of_month = lubridate::day(date),
         month_name = lubridate::month(date, label = TRUE)
         )

## convert the numeric clm from a char to double
str(climate)
climate <- climate %>% mutate_at(c("day_of_year" ,"radn","maxt", "mint", "rain", "evap", "vp"), as.numeric)

#---

Cliamte_file_start_date <- min(climate$date)
Cliamte_file_end_date <- max(climate$date)

#alternative 
Cliamte_file_start_end_date <- climate %>% 
  summarise(Cliamte_file_start_date =  min(date),
            Cliamte_file_end_date =    max(date),
              ) %>% 
  mutate(Cliamte_file_start_Year =       year(Cliamte_file_start_date),#get the year
         Cliamte_file_end_Year =         year(Cliamte_file_end_date)
         ) 

Year_before_end <- Cliamte_file_start_end_date %>% 
  mutate(Cliamte_file_date_before_end = ymd(Cliamte_file_end_date)-years(1) ,
         Cliamte_file_Year_before_end = year(Cliamte_file_date_before_end))%>% 
  select(Cliamte_file_date_before_end,Cliamte_file_Year_before_end )

str(Year_before_end)
str(Cliamte_file_start_end_date)

numb_days_start <- climate %>% filter(year==Cliamte_file_start_end_date$Cliamte_file_start_Year) %>% 
  summarise(first_day = min(day_of_year),
            last_day = max(day_of_year)
            ) %>% 
  mutate(Cliamte_file_position = "Cliamte_file_start",
         Cliamte_file_date  = Cliamte_file_start_end_date$Cliamte_file_start_date,
         Cliamte_file_year  = Cliamte_file_start_end_date$Cliamte_file_start_Year,
         numb_days = (last_day -first_day) +1) %>% 
           select(- last_day,  -first_day)

numb_days_end <- climate %>% filter(year==Cliamte_file_start_end_date$Cliamte_file_end_Year) %>% 
  summarise(first_day = min(day_of_year),
            last_day = max(day_of_year)
  ) %>% 
  mutate(Cliamte_file_position = "Cliamte_file_end",
         Cliamte_file_date  = Cliamte_file_start_end_date$Cliamte_file_end_date,
         Cliamte_file_year  = Cliamte_file_start_end_date$Cliamte_file_end_Year,
         numb_days = (last_day -first_day) +1) %>% 
  select(- last_day,  -first_day)   

str(Year_before_end)

numb_days_year_before_end <- climate %>% filter(year==Year_before_end$Cliamte_file_Year_before_end) %>% 
  summarise(first_day = min(day_of_year),
            last_day = max(day_of_year)
  ) %>% 
  mutate(Cliamte_file_position = "Cliamte_file_year_before_end",
         Cliamte_file_date  = Year_before_end$Cliamte_file_date_before_end,
         Cliamte_file_year  = Year_before_end$Cliamte_file_Year_before_end,
         numb_days = (last_day -first_day) +1) %>% 
  select(- last_day,  -first_day) 







numb_days_start
numb_days_end
numb_days_year_before_end

## merge them ###

details_about_start_end_climate_data <- rbind(numb_days_start, numb_days_end,numb_days_year_before_end )
details_about_start_end_climate_data



## Loop to import files -----------------------------------------------------------------


#for (list.of.files in list.of.files){
##Add some meta data back into the file  
   ## station number
  station_number <- read_csv(list.of.files,
                             col_names = FALSE, skip = 1)
  station_number <-station_number[1,1] #just the row with the download date
  station_number <-stringr::str_extract(station_number, "[[:digit:]]+") #just the numbers
  
  ## station name
  station_name <- read_csv(list.of.files, 
                           col_names = FALSE, skip = 2)
  station_name <-station_name[1,1]
  station_name <- station_name %>% stringr::str_replace("!station name =", "")
  station_name<-str_trim(station_name) #remove the white spaces

  
climate <- climate %>% 
  mutate(station_number = station_number,
         station_name = station_name)
 



### ---- stop step 1 here and save outputs
 
  
  ## EXTRA STEP Trim dates to fixed period
  
  str(Cliamte)
  TESTCliamte <- Cliamte %>% filter(year=="2025" & day > 0)
  
  TESTCliamte <- TESTCliamte %>% 
    mutate(date = seq(as.Date(start_date), download_date, "days"))
  # set date as a date
  TESTCliamte <- TESTCliamte %>% 
    mutate(year = year(date),
           month =month(date),
           day = lubridate::day(date),
           month_name = lubridate::month(date, label = TRUE),
           site = paste0(station_name,"_", station_number))
  str(TESTCliamte)
  
  #how much rain for each month and year
  Summary_rain_month_yr <- TESTCliamte %>% 
    group_by(year, month, month_name, site) %>% 
    summarise(sum_rain_month = sum(rain))
  str(Summary_rain_month_yr)
  Summary_rain_month_yr <- ungroup(Summary_rain_month_yr)
  
  # define summer month and GS months
  Summary_rain_month_yr <- Summary_rain_month_yr %>% 
    mutate(season = case_when(
      month >= 1 & month <= 3 ~ "summer" ,
      month >= 11 & month <= 12 ~ "summer" ,
      month >= 4 & month <= 10 ~ "gs" ,
      TRUE ~ "no_class"
    ))
  
  
  summer_GS_rain <- Summary_rain_month_yr %>% 
    group_by(year, season, site) %>% 
    summarise(season_rain = sum(sum_rain_month))
  
  ## for each year what is the sume of GS + 0.25 summer rain
  
  
  
  str(summer_GS_rain)
  summer_GS_rain <- ungroup(summer_GS_rain)
  
  summer <- summer_GS_rain  %>% 
    filter(season == "summer") %>% 
    mutate(summer_rain0.25 = season_rain * 0.25) %>% 
    select(year,summer_rain0.25 )
  gs <- summer_GS_rain  %>% 
    filter(season == "gs") %>%  
    select(year,gs_rain =season_rain, site )
  
  gs_rain_with_summer <- left_join(gs, summer)
  
  gs_rain_with_summer <- gs_rain_with_summer %>% 
    mutate(rain = gs_rain+ summer_rain0.25 ) %>% 
    select(year, site, rain)
  
  #remove the unwated files
  rm(Cliamte, gs, summer, Summary_rain_month_yr, summer_GS_rain)
  
  #the percentiles for each year.
  
  decile_max_rain <- quantile(gs_rain_with_summer$rain, c(.1, .2, .3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
  deciles_names <- c("decile_1", "decile_2", "decile_3", "decile_4", "decile_5", "decile_6", "decile_7", "decile_8", "decile_9", "decile_10")
  
  decile_table <- data.frame(decile_max_rain, deciles_names, row.names = NULL)
  decile_table <- mutate(decile_table, decile_min_rain = lag(decile_max_rain+0.1))
  
  
  #add in the min value to table
  decile_table[1,3] <-  min(gs_rain_with_summer$rain)
  decile_table <- decile_table %>%  select (deciles_names, decile_min_rain, decile_max_rain)
  
  
  