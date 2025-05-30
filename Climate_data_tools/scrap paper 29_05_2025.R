Day_end_GS_rainfall <- 15
Month_end_GS_rainfall <- 10


Day_defined_date1 <- 1 
Month_defined_date1 <- 4 

Day_defined_date2 <- 1 
Month_defined_date2 <- 5

Day_defined_date3 <- 1 
Month_defined_date3 <- 6 

Day_defined_date4 <- 1 
Month_defined_date4 <- 7

Day_defined_date5 <- 1 
Month_defined_date5 <- 8 

Day_defined_date6 <- 1 
Month_defined_date6 <- 9 

Day_defined_date7 <- 1 
Month_defined_date7 <- 10



site_selected <- "25006"

date1 <- paste0(Day_defined_date1, "-", Month_defined_date1)
date2 <- paste0(Day_defined_date2, "-", Month_defined_date2)
date3 <- paste0(Day_defined_date3, "-", Month_defined_date3)
date4 <- paste0(Day_defined_date4, "-", Month_defined_date4)
date5 <- paste0(Day_defined_date5, "-", Month_defined_date5)
date6 <- paste0(Day_defined_date6, "-", Month_defined_date6)
date7 <- paste0(Day_defined_date7, "-", Month_defined_date7)
#date8 <- paste0(Day_defined_date8, "-", Month_defined_date8)

################################################################################
path <-  file.path("H:","Output-2", "Analysis", "Scripts", "Jackie", "Downloaded_files")

df <- read.csv(paste0(path, "/Neater_file_", site_selected,  ".csv")) 

################################################################################
### I need to make this into a loop 
## add a clm with two dates marked as start and end of GS
df <- df  %>% select(Date,year, month, month_name, day_of_month,Rain)

df <- df %>% mutate(
  start_end_GS_date1 = case_when(
    month == Month_defined_date1 & day_of_month == Day_defined_date1 ~ "start_gs",
    month == Month_end_GS_rainfall & day_of_month == Day_end_GS_rainfall ~ "end_gs",
    TRUE ~ NA))
df <- df %>% mutate(
  start_end_GS_date2 = case_when(
    month == Month_defined_date2 & day_of_month == Day_defined_date2 ~ "start_gs",
    month == Month_end_GS_rainfall & day_of_month == Day_end_GS_rainfall ~ "end_gs",
    TRUE ~ NA))
df <- df %>% mutate(
  start_end_GS_date3 = case_when(
    month == Month_defined_date3 & day_of_month == Day_defined_date3 ~ "start_gs",
    month == Month_end_GS_rainfall & day_of_month == Day_end_GS_rainfall ~ "end_gs",
    TRUE ~ NA))
df <- df %>% mutate(
  start_end_GS_date4 = case_when(
    month == Month_defined_date4 & day_of_month == Day_defined_date4 ~ "start_gs",
    month == Month_end_GS_rainfall & day_of_month == Day_end_GS_rainfall ~ "end_gs",
    TRUE ~ NA))
df <- df %>% mutate(
  start_end_GS_date5 = case_when(
    month == Month_defined_date5 & day_of_month == Day_defined_date5 ~ "start_gs",
    month == Month_end_GS_rainfall & day_of_month == Day_end_GS_rainfall ~ "end_gs",
    TRUE ~ NA))
df <- df %>% mutate(
  start_end_GS_date6 = case_when(
    month == Month_defined_date6 & day_of_month == Day_defined_date6 ~ "start_gs",
    month == Month_end_GS_rainfall & day_of_month == Day_end_GS_rainfall ~ "end_gs",
    TRUE ~ NA))
df <- df %>% mutate(
  start_end_GS_date7 = case_when(
    month == Month_defined_date7 & day_of_month == Day_defined_date7 ~ "start_gs",
    month == Month_end_GS_rainfall & day_of_month == Day_end_GS_rainfall ~ "end_gs",
    TRUE ~ NA))


### fill the blanks

df <- df %>% 
  fill(start_end_GS_date1) %>%
  mutate(season_GS1 = case_when(
      start_end_GS_date1 == "start_gs" ~ "gs",
      TRUE ~ "other"))
df <- df %>% 
  fill(start_end_GS_date2) %>%
  mutate(  season_GS2 = case_when(
    start_end_GS_date2 == "start_gs" ~ "gs",
    TRUE ~ "other"))
df <- df %>% 
  fill(start_end_GS_date3) %>%
  mutate(  season_GS3 = case_when(
    start_end_GS_date3 == "start_gs" ~ "gs",
    TRUE ~ "other"))
df <- df %>% 
  fill(start_end_GS_date4) %>%
  mutate(  season_GS4 = case_when(
    start_end_GS_date4 == "start_gs" ~ "gs",
    TRUE ~ "other"))
df <- df %>% 
  fill(start_end_GS_date5) %>%
  mutate(  season_GS5 = case_when(
    start_end_GS_date5 == "start_gs" ~ "gs",
    TRUE ~ "other"))
df <- df %>% 
  fill(start_end_GS_date6) %>%
  mutate(  season_GS6 = case_when(
    start_end_GS_date6 == "start_gs" ~ "gs",
    TRUE ~ "other"))
df <- df %>% 
  fill(start_end_GS_date7) %>%
  mutate(  season_GS7 = case_when(
    start_end_GS_date7 == "start_gs" ~ "gs",
    TRUE ~ "other"))

df <- df %>% select(-start_end_GS_date1, 
                              -start_end_GS_date2,
                              -start_end_GS_date3,
                              -start_end_GS_date4,
                              -start_end_GS_date5,
                              -start_end_GS_date6,
                              -start_end_GS_date7)


gs_rain1 <- df %>%
  group_by(year, season_GS1) %>%
  summarise(sum_rain_season = sum(Rain, na.rm = TRUE)) %>%
  filter(season_GS1 == "gs")
gs_rain2 <- df %>%
  group_by(year, season_GS2) %>%
  summarise(sum_rain_season = sum(Rain, na.rm = TRUE)) %>%
  filter(season_GS2 == "gs")
gs_rain3 <- df %>%
  group_by(year, season_GS3) %>%
  summarise(sum_rain_season = sum(Rain, na.rm = TRUE)) %>%
  filter(season_GS3 == "gs")
gs_rain4 <- df %>%
  group_by(year, season_GS4) %>%
  summarise(sum_rain_season = sum(Rain, na.rm = TRUE)) %>%
  filter(season_GS4 == "gs")
gs_rain5 <- df %>%
  group_by(year, season_GS5) %>%
  summarise(sum_rain_season = sum(Rain, na.rm = TRUE)) %>%
  filter(season_GS5 == "gs")
gs_rain6 <- df %>%
  group_by(year, season_GS6) %>%
  summarise(sum_rain_season = sum(Rain, na.rm = TRUE)) %>%
  filter(season_GS6 == "gs")
gs_rain7 <- df %>%
  group_by(year, season_GS7) %>%
  summarise(sum_rain_season = sum(Rain, na.rm = TRUE)) %>%
  filter(season_GS7 == "gs")


## 5a. GS ----------------------------------------------------------------------
#the percentiles for each year.

#date1
GS_decile_max_rain1 <- quantile(gs_rain1$sum_rain_season, c(.1, .2, .3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
GS_deciles_names1 <- c("decile_1", "decile_2", "decile_3", "decile_4", "decile_5", "decile_6", "decile_7", "decile_8", "decile_9", "decile_10")

GS_decile_table1 <- data.frame(
  GS_decile_max_rain = GS_decile_max_rain1,
  GS_deciles_names = GS_deciles_names1,
  row.names = NULL
) %>%
  mutate(GS_decile_min_rain = lag(GS_decile_max_rain1 + 0.1)) %>%
  mutate(defined_date = paste0("from ", date1, "to 14-10")) %>%
  select(GS_deciles_names,GS_decile_min_rain, GS_decile_max_rain, defined_date)
#add in the min value to table
GS_decile_table1[1,2] <-  min(gs_rain1$sum_rain_season)

#date2
GS_decile_max_rain2 <- quantile(gs_rain2$sum_rain_season, c(.1, .2, .3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
GS_deciles_names2 <- c("decile_1", "decile_2", "decile_3", "decile_4", "decile_5", "decile_6", "decile_7", "decile_8", "decile_9", "decile_10")

GS_decile_table2 <- data.frame(
  GS_decile_max_rain = GS_decile_max_rain2,
  GS_deciles_names = GS_deciles_names2,
  row.names = NULL
) %>%
  mutate(GS_decile_min_rain = lag(GS_decile_max_rain2 + 0.1)) %>%
  mutate(defined_date = paste0("from ", date2, "to 14-10")) %>%
  select(GS_deciles_names,GS_decile_min_rain, GS_decile_max_rain, defined_date)
#add in the min value to table
GS_decile_table2[1,2] <-  min(gs_rain2$sum_rain_season)

#date3
GS_decile_max_rain3 <- quantile(gs_rain3$sum_rain_season, c(.1, .2, .3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
GS_deciles_names3 <- c("decile_1", "decile_2", "decile_3", "decile_4", "decile_5", "decile_6", "decile_7", "decile_8", "decile_9", "decile_10")

GS_decile_table3 <- data.frame(
  GS_decile_max_rain = GS_decile_max_rain3,
  GS_deciles_names = GS_deciles_names3,
  row.names = NULL
) %>%
  mutate(GS_decile_min_rain = lag(GS_decile_max_rain3 + 0.1)) %>%
  mutate(defined_date = paste0("from ", date3, "to 14-10")) %>%
  select(GS_deciles_names,GS_decile_min_rain, GS_decile_max_rain, defined_date)
#add in the min value to table
GS_decile_table3[1,2] <-  min(gs_rain3$sum_rain_season)


#date4
GS_decile_max_rain4 <- quantile(gs_rain4$sum_rain_season, c(.1, .2, .3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
GS_deciles_names4 <- c("decile_1", "decile_2", "decile_3", "decile_4", "decile_5", "decile_6", "decile_7", "decile_8", "decile_9", "decile_10")

GS_decile_table4 <- data.frame(
  GS_decile_max_rain = GS_decile_max_rain4,
  GS_deciles_names = GS_deciles_names4,
  row.names = NULL
) %>%
  mutate(GS_decile_min_rain = lag(GS_decile_max_rain4 + 0.1)) %>%
  mutate(defined_date = paste0("from ", date4, "to 14-10")) %>%
  select(GS_deciles_names,GS_decile_min_rain, GS_decile_max_rain, defined_date)
#add in the min value to table
GS_decile_table4[1,2] <-  min(gs_rain4$sum_rain_season)

#date5
GS_decile_max_rain5 <- quantile(gs_rain5$sum_rain_season, c(.1, .2, .3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
GS_deciles_names5 <- c("decile_1", "decile_2", "decile_3", "decile_4", "decile_5", "decile_6", "decile_7", "decile_8", "decile_9", "decile_10")

GS_decile_table5 <- data.frame(
  GS_decile_max_rain = GS_decile_max_rain5,
  GS_deciles_names = GS_deciles_names5,
  row.names = NULL
) %>%
  mutate(GS_decile_min_rain = lag(GS_decile_max_rain5 + 0.1)) %>%
  mutate(defined_date = paste0("from ", date5, "to 14-10")) %>%
  select(GS_deciles_names,GS_decile_min_rain, GS_decile_max_rain, defined_date)
#add in the min value to table
GS_decile_table5[1,2] <-  min(gs_rain5$sum_rain_season)

#date6
GS_decile_max_rain6 <- quantile(gs_rain6$sum_rain_season, c(.1, .2, .3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
GS_deciles_names6 <- c("decile_1", "decile_2", "decile_3", "decile_4", "decile_5", "decile_6", "decile_7", "decile_8", "decile_9", "decile_10")

GS_decile_table6 <- data.frame(
  GS_decile_max_rain = GS_decile_max_rain6,
  GS_deciles_names = GS_deciles_names6,
  row.names = NULL
) %>%
  mutate(GS_decile_min_rain = lag(GS_decile_max_rain6 + 0.1)) %>%
  mutate(defined_date = paste0("from ", date6, "to 14-10")) %>%
  select(GS_deciles_names,GS_decile_min_rain, GS_decile_max_rain, defined_date)
#add in the min value to table
GS_decile_table6[1,2] <-  min(gs_rain6$sum_rain_season)


#date7
GS_decile_max_rain7 <- quantile(gs_rain7$sum_rain_season, c(.1, .2, .3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
GS_deciles_names7 <- c("decile_1", "decile_2", "decile_3", "decile_4", "decile_5", "decile_6", "decile_7", "decile_8", "decile_9", "decile_10")

GS_decile_table7 <- data.frame(
  GS_decile_max_rain = GS_decile_max_rain7,
  GS_deciles_names = GS_deciles_names7,
  row.names = NULL
) %>%
  mutate(GS_decile_min_rain = lag(GS_decile_max_rain7 + 0.1)) %>%
  mutate(defined_date = paste0("from ", date7, "to 14-10")) %>%
  select(GS_deciles_names, GS_decile_min_rain, GS_decile_max_rain, defined_date)
#add in the min value to table
GS_decile_table7[1,2] <-  min(gs_rain6$sum_rain_season)



GS_decile_table <- rbind(GS_decile_table1,
                         GS_decile_table2,
                         GS_decile_table3,
                         GS_decile_table4,
                         GS_decile_table5,
                         GS_decile_table6,
                         GS_decile_table7)

str(GS_decile_table)
GS_decile_table_decile5 <- GS_decile_table %>% filter(GS_deciles_names == "decile_5")


# order the deciles to help with plotting

GS_decile_table$GS_deciles_names <- factor(GS_decile_table$GS_deciles_names , ordered = TRUE, 
                                      levels = c("decile_1", "decile_2", "decile_3", "decile_4",
                                                 "decile_5", "decile_6", "decile_7", "decile_8",
                                                 "decile_9","decile_10" ))
distinct(GS_decile_table,defined_date )
GS_decile_table$defined_date <- factor(
  GS_decile_table$defined_date ,
  ordered = TRUE,
  levels = c(
    "from 1-4to 14-10",
    "from 1-5to 14-10",
    "from 1-6to 14-10",
    "from 1-7to 14-10",
    "from 1-8to 14-10",
    "from 1-9to 14-10",
    "from 1-10to 14-10"
  )
)

GS_decile_table %>% 
  ggplot(aes(x = GS_deciles_names , y = GS_decile_max_rain, group=defined_date) )+
  geom_point(aes(color=defined_date))+
  geom_line(aes(color=defined_date))+
  theme_bw()+
   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
   labs(title = "GS rainfall by decile years",
        y = "sum of rainfall within GS starting at different dates",
        x = "",
        colour = "" #removed the legend title
  )





GS_decile_table <- as.data.frame(GS_decile_table)
GS_decile_table <- GS_decile_table_df %>% mutate(across(where(is.numeric), round, 2))
#won't save because its reactive - I cant work it out

names(df_orginal)


GS_decile_table_TEST <- GS_decile_table %>% 
  mutate(Station_name = unique(df_orginal$Name),
         Station_number = unique(df_orginal$Number),
         Years_included = paste0(
           year(min(df_orginal$Date)) ,  " to ", 
           year(max(df_orginal$Date))))
