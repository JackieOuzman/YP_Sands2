
summer_rain_decile5 <- df_summer %>% 
  filter(Summer_deciles_names =="decile_5" ) %>% 
  select(Summer_decile_max_rain)
summer_rain_decile5 <-   summer_rain_decile5$Summer_decile_max_rain

df_GS <- df_GS %>%
  mutate(Baseline_Est_Start_water = D5_Baseline_Est_Start_water,
         Frontier_Est_Start_water = D5_Frontier_Est_Start_water,
         summer_rain_decile5 = summer_rain_decile5)
names((df_GS))

### get rainfall to defined date.

defined_date <- 


df_GS <- df_GS %>%
  mutate(
    Cal_Est_Evaporation_base = (GS_decile_max_rain *0.4), # (40% of GSR)
    Cal_YP_base = ((GS_decile_max_rain +
                      Baseline_Est_Start_water)-
                     (Cal_Est_Evaporation_base))*20 ,
    Cal_EYP_base = (Cal_YP_base*0.8), # 80% of Yield potential 
    
    Cal_Est_Evaporation_Front = (0.18*GS_decile_max_rain+15), # using regression equation 
    Cal_YP_Front = ((GS_decile_max_rain+
                       Frontier_Est_Start_water)-
                      (Cal_Est_Evaporation_Front))*26,
    Cal_EYP_Front = Cal_YP_Front*0.8 #80% of Yield potential 
  )