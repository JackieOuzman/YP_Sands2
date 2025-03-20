# Step 2 summary of rainfall per year ------------------------------------------


## Load Libraries --------------------------------------------------------------

library(ggplot2)
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)


## Set location of file --------------------------------------------------------

#Folder for met files
location_files <- "H:/Output-2/Analysis/Scripts/Jackie/output_of_steps/"

# find the files that you want
list.of.files <- list.files(location_files, ".csv",full.names=T) #the trick is getting the full name
list.of.files #with path


## Download all of the climate data and format it -------------------------------
Step1 <- read_csv("H:/Output-2/Analysis/Scripts/Jackie/output_of_steps/Step1.csv")

Step1_details_about_start_end_climate_data <- read_csv("H:/Output-2/Analysis/Scripts/Jackie/output_of_steps/Step1_details_about_start_end_climate_data.csv")

## set / check the date clms as dates

str(Step1$date)
str(Step1_details_about_start_end_climate_data$Cliamte_file_date)

## What years do you want to calculate the deciles for? ------------------------
Step1_details_about_start_end_climate_data
start_year <- "1920-01-01" # 1920 #
end_year <- "2024-12-31" # 2024 #

Step1_set_date <- Step1 %>% filter(between(date, as.Date(start_year), as.Date(end_year)))
 str(Step1_set_date)

 
## Calculate the sum of rain for each month in the year  ------------------------

Summary_rain_month_yr <- Step1_set_date %>% 
  group_by(year, month, month_name) %>% 
  summarise(sum_rain_month = sum(rain, na.rm = TRUE))
Summary_rain_month_yr <- ungroup(Summary_rain_month_yr)
str(Summary_rain_month_yr)



# define summer month and GS months
Summary_rain_month_yr <- Summary_rain_month_yr %>% 
  mutate(season = case_when(
    month >= 1 & month <= 3 ~ "summer" ,
    month >= 11 & month <= 12 ~ "summer" ,
    month >= 4 & month <= 10 ~ "gs" ,
    TRUE ~ "no_class"
  ))

## Calculate the sum of rain for summer and GS per year + plot -----------------
summer_GS_rain <- Summary_rain_month_yr %>% 
  group_by(year, season) %>% 
  summarise(season_rain = sum(sum_rain_month))
summer_GS_rain <- ungroup(summer_GS_rain)

ggplot(data = summer_GS_rain,aes(x = year, y = season_rain, fill = season) )+
       geom_bar(stat = "identity")+
  labs(y = "sum of rain")


## Calculate the sum of rain for 0.25 summer + GS per year + plot -----------------


str(summer_GS_rain)


summer <- summer_GS_rain  %>% 
  filter(season == "summer") %>% 
  mutate(summer_rain0.25 = season_rain * 0.25) %>% 
  select(year,summer_rain0.25 )
gs <- summer_GS_rain  %>% 
  filter(season == "gs") %>%  
  select(year,gs_rain =season_rain )

gs_rain_with_summer <- left_join(gs, summer)

gs_rain_with_summer <- gs_rain_with_summer %>% 
  mutate(rain_GS_plus_25summer = gs_rain+ summer_rain0.25 ) %>% 
  select(rain_GS_plus_25summer, year)

#remove the unwanted files
rm(gs, summer, Summary_rain_month_yr, summer_GS_rain)

ggplot(data = gs_rain_with_summer,aes(x = year, y = rain_GS_plus_25summer) )+
  geom_bar(stat = "identity")+
  labs(y = "sum of rain per year (GS plus 0.25 of summer rain)")


## Calculate the percentiles for each year  ------------------------------------

#the percentiles for each year.

decile_max_rain <- quantile(gs_rain_with_summer$rain_GS_plus_25summer, c(.1, .2, .3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
deciles_names <- c("decile_1", "decile_2", "decile_3", "decile_4", "decile_5", "decile_6", "decile_7", "decile_8", "decile_9", "decile_10")

decile_table <- data.frame(decile_max_rain, deciles_names, row.names = NULL)
decile_table <- mutate(decile_table, decile_min_rain = lag(decile_max_rain+0.1))


#add in the min value to table
decile_table[1,3] <-  min(gs_rain_with_summer$rain_GS_plus_25summer)
decile_table <- decile_table %>%  select (deciles_names, decile_min_rain, decile_max_rain)



