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
GS_decile_table <- read_csv("H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/GS_decile_table.csv")
GS_decile_table
Summer_decile_table <- read_csv("H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/Summer_decile_table.csv")
Summer_decile_table

## add calculations to the table ------------------------------------------------
str(Summer_decile_table)

Summer_decile_table <- Summer_decile_table %>%
  mutate(Baseline_Estimated_Starting_water = Summer_decile_max_rain * 0.25, # Estimated Starting water = (25% of Summer Rainfall)
         Frontier_Estimated_Starting_water  = Summer_decile_max_rain * 0.30) # Estimated Starting water = (30% of Summer Rainfall)
################################################################################
#get decile 5 Baseline_Estimated_Starting_water

D5_Baseline_Est_Start_water <- Summer_decile_table %>% 
  filter(Summer_deciles_names =="decile_5" ) %>% 
  select(Baseline_Estimated_Starting_water)

D5_Baseline_Est_Start_water <-   D5_Baseline_Est_Start_water$Baseline_Estimated_Starting_water
D5_Baseline_Est_Start_water


D5_Frontier_Est_Start_water <- Summer_decile_table %>% 
  filter(Summer_deciles_names =="decile_5" ) %>% 
  select(Frontier_Estimated_Starting_water)

D5_Frontier_Est_Start_water <- D5_Frontier_Est_Start_water$Frontier_Estimated_Starting_water
D5_Frontier_Est_Start_water

str(GS_decile_table)
################################################################################
D5_Baseline_Est_Start_water
D5_Frontier_Est_Start_water
################################################################################
GS_decile_table <- GS_decile_table %>%
  mutate(
    Cal_Est_Evaporation_base = (GS_decile_max_rain *0.4), # (40% of GSR)
      Cal_YP_base = ((GS_decile_max_rain +
                       D5_Baseline_Est_Start_water)-
                       (Cal_Est_Evaporation_base))*20 ,
     Cal_EYP_base = (Cal_YP_base*0.8), # 80% of Yield potential 

    Cal_Est_Evaporation_Front = (0.18*GS_decile_max_rain+15), # using regression equation 
      Cal_YP_Front = ((GS_decile_max_rain+
                         D5_Frontier_Est_Start_water)-
                         (Cal_Est_Evaporation_Front))*26,
     Cal_EYP_Front = Cal_YP_Front*0.8 #80% of Yield potential 
    )
  
  
GS_decile_table
################################################################################
# make df long to help with plotting different approaches in calculating YP
GS_decile_table_long <- GS_decile_table %>% 
  pivot_longer(
    cols = starts_with("Cal"),
    names_to = "Cal_type",
    values_to = "value",
    values_drop_na = TRUE
  )

GS_decile_table_long

################################################################################
# order the deciles to help with plotting

GS_decile_table_long$GS_deciles_names <- factor(GS_decile_table_long$GS_deciles_names , ordered = TRUE, 
                                levels = c("decile_1", "decile_2", "decile_3", "decile_4",
                                           "decile_5", "decile_6", "decile_7", "decile_8",
                                           "decile_9","decile_10" ))


## assign names to match Kenton names in his excel sheets
GS_decile_table_long <- GS_decile_table_long %>% 
  mutate(
    Cal_type = case_when(
      Cal_type ==  "Cal_EYP_base" ~  "Baseline Practice",
      Cal_type ==  "Cal_EYP_Front" ~   "New Frontier",
      Cal_type ==  "Cal_YP_Front" ~  "New Frontier Long term",
      .default = Cal_type
    ))


################################################################################
#Plot YP
str(GS_decile_table_long)
plot1 <-GS_decile_table_long %>% 
  filter(Cal_type =="Baseline Practice" | Cal_type =="New Frontier" | Cal_type == "New Frontier Long term") %>% 
  
  ggplot(aes(x = GS_deciles_names , y = value, group=Cal_type) )+
  geom_point(aes(color=Cal_type))+
  geom_line(aes(color=Cal_type))+
  labs(title = "YP based on historical  decile years",
       subtitle = paste0("Station name: ", distinct(GS_decile_table, Station_name ), 
                         " - ", distinct(GS_decile_table, Station_number )),
       caption = paste0("Years included : ", distinct(GS_decile_table,Years_included),
                        ". GS period:",  distinct(GS_decile_table,GS_period)),
       y = "Yield kg/ha",
       x = "",
       colour = "" #removed the legend title
  )
plot1

ggsave(plot = plot1,
       filename =  paste0("H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/","Plot_YP based on GS decile years.png"), 
       width = 20, height = 12, units = "cm")


write_csv(GS_decile_table_long, 
          file = paste0("H:/Output-2/Analysis/Scripts/Jackie/Karoonda_025006/","GS_decile_table_withYP_cals_long.csv"))
