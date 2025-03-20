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


## Load Libraries --------------------------------------------------------------

library(ggplot2)
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)


## Set location of file --------------------------------------------------------

#Folder for met files
current.folder <- "H:/Output-2/Analysis/Scripts/Jackie/"

# find the files that you want
list.of.files <- list.files(current.folder, ".csv",full.names=T) #the trick is getting the full name
list.of.files #with path

#list.of.files <- c(
#  "H:/Output-2/Analysis/Scripts/Jackie/metfile_ Karoonda_025006.csv")

## Download all of the climate data and format it -------------------------------

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
climate <- climate %>% 
  mutate_at(c("day_of_year" ,"radn","maxt", "mint", "rain", "evap", "vp"), as.numeric)

## Get metadata and add to met file --------------------------------------------


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


## Additional information about the years of data - supplementary data-------


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

rm(numb_days_end, numb_days_start, numb_days_year_before_end, Year_before_end, Cliamte_file_start_end_date)

## Save the df ready for Step 2 --------------------------------------------------------


write_csv(climate, file = paste0(current.folder,"output_of_steps/","Step1.csv"))
write_csv(details_about_start_end_climate_data, 
          file = paste0(current.folder,"output_of_steps/","Step1_details_about_start_end_climate_data.csv"))


