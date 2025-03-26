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

#### set / check the date clms as dates
Step1
str(Step1$date)
str(Step1_details_about_start_end_climate_data$Cliamte_file_date)

# 1.What years do you want to calculate the deciles for? ------------------------
Step1_details_about_start_end_climate_data
start_year <- "1920-01-01" # 1920 #
end_year <- "2024-12-31" # 2024 #

# 2. How to defined the GS rainfall  ---------------------------------------------

DAY_start_GS_rainfall <- 1
Month_start_GS_rainfall <- 4 #April

DAY_end_GS_rainfall <- 15
Month_end_GS_rainfall <- 10 #Oct

GS_Defined <- paste0(DAY_start_GS_rainfall,"/", Month_start_GS_rainfall, " to ", 
                     DAY_end_GS_rainfall,"/", Month_end_GS_rainfall)

### Filter dataset based on years  ---------------------------------------------
Step1_set_date <- Step1 %>% filter(between(date, as.Date(start_year), as.Date(end_year)))
str(Step1_set_date)


# 3.Create season clm per day as GS or summer ---------------------------------------------

Step1_set_date <- Step1_set_date %>% 
  mutate(
    start_end_GS_date = case_when(
      month == Month_start_GS_rainfall & 
        day_of_month  == DAY_start_GS_rainfall ~ "start_gs" ,
      month == Month_end_GS_rainfall & 
        day_of_month  == DAY_end_GS_rainfall+1 ~ "end_gs" ,
      TRUE ~ NA
    ))
Step1_set_date <- Step1_set_date %>% fill(start_end_GS_date) %>% 
  mutate(
    season = case_when(
      start_end_GS_date == "start_gs" ~ "gs",
      TRUE ~ "summer"))

Step1_set_date <- Step1_set_date %>% select(-start_end_GS_date)


# 4.Calculate the sum of rain for each season in the year  ------------------------

Summary_rain_season_yr <- Step1_set_date %>% 
  group_by(year, season) %>% 
  summarise(sum_rain_season = sum(rain, na.rm = TRUE))
Summary_rain_season_yr <- ungroup(Summary_rain_season_yr)
str(Summary_rain_season_yr)





### Plot summer and GS per year  ------------------------------------------------


plot1 <- ggplot(data = Summary_rain_season_yr,aes(x = year, y = sum_rain_season, fill = season) )+
       geom_bar(stat = "identity")+
  labs(title = "Sum of rain per year",
       subtitle = paste0("Station name: ", distinct(Step1, station_name), 
                         " - ", distinct(Step1, station_number)),
       caption = paste0("Years included : ",year(start_year) ," to ",  year(end_year),
                        ". GS defined as: ", GS_Defined),
       y = "sum of rain"
    )

plot1


# 5. Calculate the percentiles for each year per season type -------------------
str(Summary_rain_season_yr)
gs_rain <- Summary_rain_season_yr %>% filter(season == "gs")
summer_rain <- Summary_rain_season_yr %>% filter(season == "summer")

## 5a. GS ----------------------------------------------------------------------
#the percentiles for each year.

GS_decile_max_rain <- quantile(gs_rain$sum_rain_season, c(.1, .2, .3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
GS_deciles_names <- c("decile_1", "decile_2", "decile_3", "decile_4", "decile_5", "decile_6", "decile_7", "decile_8", "decile_9", "decile_10")

GS_decile_table <- data.frame(GS_decile_max_rain, GS_deciles_names, row.names = NULL)
GS_decile_table <- mutate(GS_decile_table, GS_decile_min_rain = lag(GS_decile_max_rain+0.1))


#add in the min value to table
GS_decile_table[1,3] <-  min(gs_rain$sum_rain_season)
GS_decile_table <- GS_decile_table %>%  select (GS_deciles_names, GS_decile_min_rain, GS_decile_max_rain)


GS_decile_table <- GS_decile_table %>% 
  mutate(Station_name = paste0(distinct(Step1, station_name)),
         Station_number = paste0(distinct(Step1, station_number)) ,
         Years_included = paste0(year(start_year) ," to ",  year(end_year)),
         GS_period =   GS_Defined
         )
                         
GS_decile_table
## Assign the year to decile l  ------------------------------------


# str(gs_rain)
# GS_decile_table[1,2]
# GS_decile_table[1,3]

gs_rain <- gs_rain %>%
  mutate(
    decile = case_when(
      between(round(sum_rain_season, 1), round(GS_decile_table[1,2],1) ,round(GS_decile_table[1,3],1) ) ~ "decile_1",
      between(round(sum_rain_season, 1), round(GS_decile_table[2,2],1) ,round(GS_decile_table[2,3],1) ) ~ "decile_2",
      between(round(sum_rain_season, 1), round(GS_decile_table[3,2],1) ,round(GS_decile_table[3,3],1) ) ~ "decile_3",
      between(round(sum_rain_season, 1), round(GS_decile_table[4,2],1) ,round(GS_decile_table[4,3],1) ) ~ "decile_4",
      between(round(sum_rain_season, 1), round(GS_decile_table[5,2],1) ,round(GS_decile_table[5,3],1) ) ~ "decile_5",
      between(round(sum_rain_season, 1), round(GS_decile_table[6,2],1) ,round(GS_decile_table[6,3],1) ) ~ "decile_6",
      between(round(sum_rain_season, 1), round(GS_decile_table[7,2],1) ,round(GS_decile_table[7,3],1) ) ~ "decile_7",
      between(round(sum_rain_season, 1), round(GS_decile_table[8,2],1) ,round(GS_decile_table[8,3],1) ) ~ "decile_8",
      between(round(sum_rain_season, 1), round(GS_decile_table[9,2],1) ,round(GS_decile_table[9,3],1) ) ~ "decile_9",
      between(round(sum_rain_season, 1), round(GS_decile_table[10,2],1) ,round(GS_decile_table[10,3],1) ) ~ "decile_10",
      
      TRUE                      ~ "other"
    )
  )


gs_rain <- gs_rain %>% 
  mutate(Station_name = paste0(distinct(Step1, station_name)),
         Station_number = paste0(distinct(Step1, station_number)) ,
         Years_included = paste0(year(start_year) ," to ",  year(end_year)),
         GS_period =   GS_Defined                      
  )

gs_rain <- gs_rain %>%  rename(gs_sum_rain = sum_rain_season,
                               gs_decile = decile) %>% 
  select(- season)


gs_rain
GS_decile_table

write_csv(GS_decile_table, 
          file = paste0("H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/","GS_decile_table.csv"))

## 5b. Summer ----------------------------------------------------------------------
#the percentiles for each year.
str(summer_rain)
### I loose my GS_decile_table here? why
Summer_decile_max_rain <- quantile(summer_rain$sum_rain_season, c(.1, .2, .3, 0.4, 
                                                                  0.5, 0.6, 
                                                                  0.7, 0.8, 0.9, 1.0))
Summer_deciles_names <- c("decile_1", "decile_2", "decile_3", "decile_4", 
                          "decile_5", "decile_6", 
                          "decile_7", "decile_8", "decile_9", "decile_10")

Summer_decile_table <- data.frame(Summer_decile_max_rain, Summer_deciles_names, row.names = NULL)
Summer_decile_table <- mutate(Summer_decile_table, Summer_decile_min_rain = lag(Summer_decile_max_rain+0.1))


#add in the min value to table
Summer_decile_table[1,3] <-  min(summer_rain$sum_rain_season)
Summer_decile_table <- Summer_decile_table %>%  select (Summer_deciles_names, Summer_decile_min_rain, Summer_decile_max_rain)

Summer_decile_table <- Summer_decile_table %>% 
  mutate(Station_name = paste0(distinct(Step1, station_name)),
         Station_number = paste0(distinct(Step1, station_number)) ,
         Years_included = paste0(year(start_year) ," to ",  year(end_year)),
         GS_period =   GS_Defined
  )

Summer_decile_table
## Assign the year to decile l  ------------------------------------


str(summer_rain)
Summer_decile_table[1,2]
Summer_decile_table[1,3]

summer_rain <- summer_rain %>%
  mutate(
    decile = case_when(
      between(round(sum_rain_season, 1), round(Summer_decile_table[1,2],1) ,round(Summer_decile_table[1,3],1) ) ~ "decile_1",
      between(round(sum_rain_season, 1), round(Summer_decile_table[2,2],1) ,round(Summer_decile_table[2,3],1) ) ~ "decile_2",
      between(round(sum_rain_season, 1), round(Summer_decile_table[3,2],1) ,round(Summer_decile_table[3,3],1) ) ~ "decile_3",
      between(round(sum_rain_season, 1), round(Summer_decile_table[4,2],1) ,round(Summer_decile_table[4,3],1) ) ~ "decile_4",
      between(round(sum_rain_season, 1), round(Summer_decile_table[5,2],1) ,round(Summer_decile_table[5,3],1) ) ~ "decile_5",
      between(round(sum_rain_season, 1), round(Summer_decile_table[6,2],1) ,round(Summer_decile_table[6,3],1) ) ~ "decile_6",
      between(round(sum_rain_season, 1), round(Summer_decile_table[7,2],1) ,round(Summer_decile_table[7,3],1) ) ~ "decile_7",
      between(round(sum_rain_season, 1), round(Summer_decile_table[8,2],1) ,round(Summer_decile_table[8,3],1) ) ~ "decile_8",
      between(round(sum_rain_season, 1), round(Summer_decile_table[9,2],1) ,round(Summer_decile_table[9,3],1) ) ~ "decile_9",
      between(round(sum_rain_season, 1), round(Summer_decile_table[10,2],1) ,round(Summer_decile_table[10,3],1) ) ~ "decile_10",
      
      TRUE                      ~ "other"
    )
  )


summer_rain <- summer_rain %>%  rename(Summer_sum_rain = sum_rain_season,
                                       Summer_decile = decile) %>% 
  select(- season)


## Save / export outputs  ------------------------------------------------------
Summer_decile_table
write_csv(Summer_decile_table, 
          file = paste0("H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/","summer_decile_table.csv"))

summer_rain
gs_rain

Year_with_Deciles <- left_join(gs_rain, summer_rain)
Year_with_Deciles <- Year_with_Deciles %>% select(
  year,
  Station_name ,
  Station_number,
  gs_decile,
  gs_sum_rain,
  Summer_decile ,
  Summer_sum_rain,
  GS_period ,
  Years_included )
Year_with_Deciles

rm(gs_rain, summer_rain)
Year_with_Deciles

########STOP#######################################################################

##files

write_csv(Year_with_Deciles, 
          file = paste0("H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/","Year_with_Deciles.csv"))

##plots
ggsave(plot = plot1,
       filename =  paste0("H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/","plot1_sum_rain_per_yr_summer_GS.png"), 
       width = 20, height = 12, units = "cm")

