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
location_files <- "H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/"

# find the files that you want
list.of.files <- list.files(location_files, ".csv",full.names=T) #the trick is getting the full name
list.of.files #with path


## Download all of the climate data and format it -------------------------------
Step1 <- read_csv("H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/Step1.csv")

Step1_details_about_start_end_climate_data <- read_csv("H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/Step1_details_about_start_end_climate_data.csv")

## set / check the date clms as dates
Step1
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

year(start_year)

plot1 <- ggplot(data = summer_GS_rain,aes(x = year, y = season_rain, fill = season) )+
       geom_bar(stat = "identity")+
  labs(title = "Sum of rain per year",
       subtitle = paste0("Station name: ", distinct(Step1, station_name), 
                         " - ", distinct(Step1, station_number)),
       caption = paste0("Years included : ",year(start_year) ," to ",  year(end_year)),
       y = "sum of rain"
    )

plot1
summer_GS_rain
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
rm(gs, summer, Summary_rain_month_yr)

plot2 <- ggplot(data = gs_rain_with_summer,aes(x = year, y = rain_GS_plus_25summer) )+
  geom_bar(stat = "identity")+
  labs(title = "Sum of rain per year (GS plus 0.25 of summer rain)",
       subtitle = paste0("Station name: ", distinct(Step1, station_name), 
                         " - ", distinct(Step1, station_number)),
       caption = paste0("Years included : ",year(start_year) ," to ",  year(end_year)),
       y = "sum of rain"
  )
  
plot2
gs_rain_with_summer
## Calculate the percentiles for each year  ------------------------------------

#the percentiles for each year.

decile_max_rain <- quantile(gs_rain_with_summer$rain_GS_plus_25summer, c(.1, .2, .3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
deciles_names <- c("decile_1", "decile_2", "decile_3", "decile_4", "decile_5", "decile_6", "decile_7", "decile_8", "decile_9", "decile_10")

decile_table <- data.frame(decile_max_rain, deciles_names, row.names = NULL)
decile_table <- mutate(decile_table, decile_min_rain = lag(decile_max_rain+0.1))


#add in the min value to table
decile_table[1,3] <-  min(gs_rain_with_summer$rain_GS_plus_25summer)
decile_table <- decile_table %>%  select (deciles_names, decile_min_rain, decile_max_rain)


decile_table <- decile_table %>% 
  mutate(Station_name = paste0(distinct(Step1, station_name)),
         Station_number = paste0(distinct(Step1, station_number)) ,
         Years_included = paste0(year(start_year) ," to ",  year(end_year))
         )
                         
decile_table
## Assign the year to decile l  ------------------------------------


str(gs_rain_with_summer$rain_GS_plus_25summer)
decile_table[1,2]
decile_table[1,3]

gs_rain_with_summer <- gs_rain_with_summer %>%
  mutate(
    decile = case_when(
      between(round(rain_GS_plus_25summer, 1), round(decile_table[1,2],1) ,round(decile_table[1,3],1) ) ~ "decile_1",
      between(round(rain_GS_plus_25summer, 1), round(decile_table[2,2],1) ,round(decile_table[2,3],1) ) ~ "decile_2",
      between(round(rain_GS_plus_25summer, 1), round(decile_table[3,2],1) ,round(decile_table[3,3],1) ) ~ "decile_3",
      between(round(rain_GS_plus_25summer, 1), round(decile_table[4,2],1) ,round(decile_table[4,3],1) ) ~ "decile_4",
      between(round(rain_GS_plus_25summer, 1), round(decile_table[5,2],1) ,round(decile_table[5,3],1) ) ~ "decile_5",
      between(round(rain_GS_plus_25summer, 1), round(decile_table[6,2],1) ,round(decile_table[6,3],1) ) ~ "decile_6",
      between(round(rain_GS_plus_25summer, 1), round(decile_table[7,2],1) ,round(decile_table[7,3],1) ) ~ "decile_7",
      between(round(rain_GS_plus_25summer, 1), round(decile_table[8,2],1) ,round(decile_table[8,3],1) ) ~ "decile_8",
      between(round(rain_GS_plus_25summer, 1), round(decile_table[9,2],1) ,round(decile_table[9,3],1) ) ~ "decile_9",
      between(round(rain_GS_plus_25summer, 1), round(decile_table[10,2],1) ,round(decile_table[10,3],1) ) ~ "decile_10",
      
      TRUE                      ~ "other"
    )
  )
gs_rain_with_summer <- gs_rain_with_summer %>% 
  mutate(Station_name = paste0(distinct(Step1, station_name)),
         Station_number = paste0(distinct(Step1, station_number)) ,
         Years_included = paste0(year(start_year) ," to ",  year(end_year))
  )

gs_rain_with_summer




## Save / export outputs  ------------------------------------------------------


##files
write_csv(summer_GS_rain, 
          file = paste0("H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/","summer_GS_rain.csv"))
write_csv(gs_rain_with_summer, 
          file = paste0("H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/","sum_of_gs_rain_plus_25percent_summer.csv"))
write_csv(decile_table, 
          file = paste0("H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/","decile_table.csv"))
##plots
ggsave(plot = plot1,
       filename =  paste0("H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/","plot1_sum_rain_per_yr_summer_GS.png"), 
       width = 20, height = 12, units = "cm")
ggsave(plot = plot2,
       filename =  paste0("H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/","plot2_sum_GS_rain_plus_25percen_summer_yr.png"), 
       width = 20, height = 12, units = "cm")
