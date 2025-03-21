# Step 3 cal the estimaiated per decile  ------------------------------------------
# Current Baseline cals
# ES Estimated evaporation
# YP Yield potential
# EYP Estimated yield potential
# 
# Frontier Potential
# ES Estimated evaporation
# YP Yield potential
# EYP Estimated yield potential


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


## Download all of the decile table  -------------------------------------------
decile_table <- read_csv("H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/decile_table.csv")
decile_table


## add calulations to the tabel ------------------------------------------------


decile_table <- decile_table %>%
  mutate(
    Cal_Est_Evaporation_base = decile_max_rain *0.4,
    Cal_YP_base = ((decile_max_rain+27)-(Cal_Est_Evaporation_base))*20,
    Cal_EYP_base = Cal_YP_base*0.8,

    Cal_Est_Evaporation_Front = 0.18*decile_max_rain+15,
    Cal_YP_Front = ((decile_max_rain+27-Cal_Est_Evaporation_Front)*26),
    Cal_EYP_Front = Cal_YP_Front*0.8
    )
  
  
decile_table
decile_table_long <- decile_table %>% 
  pivot_longer(
    cols = starts_with("Cal"),
    names_to = "Cal_type",
    values_to = "value",
    values_drop_na = TRUE
  )

decile_table_long

decile_table_long$deciles_names <- factor(decile_table_long$deciles_names, ordered = TRUE, 
                                levels = c("decile_1", "decile_2", "decile_3", "decile_4",
                                           "decile_5", "decile_6", "decile_7", "decile_8",
                                           "decile_9","decile_10" ))



decile_table_long <- decile_table_long %>% 
  mutate(
    Cal_type = case_when(
      Cal_type ==  "Cal_EYP_base" ~  "Baseline Practice",
      Cal_type ==  "Cal_EYP_Front" ~   "New Frontier",
      Cal_type ==  "Cal_YP_Front" ~  "New Frontier Long term",
      .default = Cal_type
    ))


plot1 <-decile_table_long %>% 
  filter(Cal_type =="Baseline Practice" | Cal_type =="New Frontier" | Cal_type == "New Frontier Long term") %>% 
  
  ggplot(aes(x = deciles_names , y = value, group=Cal_type) )+
  geom_point(aes(color=Cal_type))+
  geom_line(aes(color=Cal_type))+
  labs(title = "YP based on histrical decile years",
       subtitle = paste0("Station name: ", distinct(decile_table, Station_name ), 
                         " - ", distinct(decile_table, Station_number )),
       caption = paste0("Years included : ", distinct(decile_table,Years_included)),
       y = "Yield kg/ha",
       x = "",
       colour = "" #removed the legend title
  )
plot1

ggsave(plot = plot1,
       filename =  paste0("H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/","YP based on decile years.png"), 
       width = 20, height = 12, units = "cm")
