### create a function and loop through a list
str(daily_file_sum_rain_all)

defined_date_list <- daily_file_sum_rain_all$Defined_date
defined_date_list
#defined_date_list <- c("2024-04-01", "2024-05-28")


for(defined_date_list in defined_date_list){
  
   df_gs_front_values <- df_gs %>% select(
    Station_name,
    Station_number,
    Years_included ,
    GS_period,
    GS_deciles_names, 
    GS_decile_max_rain
  )
  
  ## get the rainfall for summer rainfall decile 5
  summer_rain_decile5 <- df_summer %>% 
    filter(summer_deciles_names =="decile_5" ) %>% 
    select(summer_decile_max_rain)
  summer_rain_decile5 <-   summer_rain_decile5$summer_decile_max_rain
  #summer_rain_decile5
  df_gs_front_values <- df_gs_front_values %>% mutate(summer_rain_decile5 = summer_rain_decile5)
  #df_gs_front_values
  
  # Est start water
  df_gs_front_values <- df_gs_front_values %>%
    mutate(Frontier_Est_Start_water = D5_Frontier_Est_Start_water
    )
  ## get the rainfall for summer rainfall decile 5
  summer_rain_decile5 <- df_summer %>% 
    filter(summer_deciles_names =="decile_5" ) %>% 
    select(summer_decile_max_rain)
  summer_rain_decile5 <-   summer_rain_decile5$summer_decile_max_rain
  #summer_rain_decile5
  df_gs_front_values <- df_gs_front_values %>% mutate(summer_rain_decile5 = summer_rain_decile5)
  #df_gs_front_values
  
  # Est start water
  df_gs_front_values <- df_gs_front_values %>%
    mutate(Frontier_Est_Start_water = D5_Frontier_Est_Start_water
    )
  
  
  ## get the rainfall to date
  rainfall_to_date <- daily_file_sum_rain_all %>% 
    filter(Defined_date == defined_date_list ) %>% 
    select(sum_rain)
  rainfall_to_date <-   rainfall_to_date$sum_rain
  
  df_gs_front_values <- df_gs_front_values %>% 
    mutate(
      defined_date = defined_date_list,
      rainfall_to_date = rainfall_to_date
    )
  
  ## rainfall to maturity
  df_gs_front_values <- df_gs_front_values %>% 
    mutate(rainfall_to_end_GS = GS_decile_max_rain - rainfall_to_date)
  
  
  #Estimated_crop_water_use = `Baseline_Est_Start_water` + `Rainfall_to_date` + `Rainfall_to_maturity`
  df_gs_front_values <- df_gs_front_values %>% 
    mutate(estimated_crop_water_use = Frontier_Est_Start_water + rainfall_to_date  + rainfall_to_end_GS)
  
  
  #Baseline_Evaporation_on_term = `Baseline_Estimated_crop_water_use` * 0.18 + 15
  df_gs_front_values <- df_gs_front_values %>% 
    mutate(Evaporation_on_term = estimated_crop_water_use * 0.18 + 15)
  
  
  #Baseline_TE_term = 26
  df_gs_front_values <- df_gs_front_values %>% 
    mutate(Front_TE_term = 26)
  
  
  #Potential Yield (Baseline_PY) = ((`Baseline_Estimated_crop_water_use` - `Baseline_Evaporation_on_term`)* `Baseline_TE_term`)/1000
  df_gs_front_values <- df_gs_front_values %>% 
    mutate(PY = ((estimated_crop_water_use - Evaporation_on_term)* Front_TE_term)/1000)
  
  
  #Yield baseline = 80% * `Baseline_PY` 
  df_gs_front_values <- df_gs_front_values %>% 
    mutate(Yield_forecast = 0.8* PY)
  
  ## final dataset for outputs
  forecast_yld <- df_gs_front_values %>% 
    select(GS_deciles_names, Yield_forecast, defined_date) %>% 
    mutate(method = "frontier")
    

  name <- paste0("f_forecast_yld_", defined_date_list)
  assign(name,forecast_yld)
}

## merge my forecast_yld file


f_forecast_yld <- rbind(`f_forecast_yld_2024-04-01` ,
                      `f_forecast_yld_2024-05-28` ,
                      `f_forecast_yld_2024-06-30` ,
                      `f_forecast_yld_2024-07-24` ,
                      `f_forecast_yld_2024-08-30` ,
                      `f_forecast_yld_2024-09-30` ,
                      `f_forecast_yld_2024-10-24`)
rm(`f_forecast_yld_2024-04-01` ,
   `f_forecast_yld_2024-05-28` ,
   `f_forecast_yld_2024-06-30` ,
   `f_forecast_yld_2024-07-24` ,
   `f_forecast_yld_2024-08-30` ,
   `f_forecast_yld_2024-09-30` ,
   `f_forecast_yld_2024-10-24`)



