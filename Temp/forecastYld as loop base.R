### create a function and loop through a list
str(daily_file_sum_rain_all)

defined_date_list <- daily_file_sum_rain_all$Defined_date
defined_date_list
#defined_date_list <- c("2024-04-01", "2024-05-28")


for(defined_date_list in defined_date_list){
  
df_gs_baseline_values <- df_gs %>% select(
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
df_gs_baseline_values <- df_gs_baseline_values %>% mutate(summer_rain_decile5 = summer_rain_decile5)


# Est start water
df_gs_baseline_values <- df_gs_baseline_values %>%
  mutate(Baseline_Est_Start_water = D5_Baseline_Est_Start_water
  )


## get the rainfall to date
rainfall_to_date <- daily_file_sum_rain_all %>% 
  filter(Defined_date == defined_date_list ) %>% 
  select(sum_rain)
rainfall_to_date <-   rainfall_to_date$sum_rain
df_gs_baseline_values <- df_gs_baseline_values %>% 
  mutate(
    defined_date = defined_date_list,
    rainfall_to_date = rainfall_to_date
  )

## rainfall to maturity
df_gs_baseline_values <- df_gs_baseline_values %>% 
  mutate(rainfall_to_end_GS = GS_decile_max_rain - rainfall_to_date)


#Estimated_crop_water_use = `Baseline_Est_Start_water` + `Rainfall_to_date` + `Rainfall_to_maturity`
df_gs_baseline_values <- df_gs_baseline_values %>% 
  mutate(estimated_crop_water_use = Baseline_Est_Start_water + rainfall_to_date  + rainfall_to_end_GS)


#Baseline_Evaporation_on_term = `Baseline_Estimated_crop_water_use` * 0.267 + 11.7
df_gs_baseline_values <- df_gs_baseline_values %>% 
  mutate(Evaporation_on_term = estimated_crop_water_use * 0.267 + 11.7)


#Baseline_TE_term = 21
df_gs_baseline_values <- df_gs_baseline_values %>% 
  mutate(Baseline_TE_term = 21)


#Potential Yield (Baseline_PY) = ((`Baseline_Estimated_crop_water_use` - `Baseline_Evaporation_on_term`)* `Baseline_TE_term`)/1000
df_gs_baseline_values <- df_gs_baseline_values %>% 
  mutate(PY = ((estimated_crop_water_use - Evaporation_on_term)* Baseline_TE_term)/1000)


#Yield baseline = 80% * `Baseline_PY` 
df_gs_baseline_values <- df_gs_baseline_values %>% 
  mutate(Yield_forecast = 0.8* PY)

## final dataset for outputs
forecast_yld <- df_gs_baseline_values %>% 
  select(GS_deciles_names, Yield_forecast, defined_date) %>% 
  mutate(method = "baseline")


name <- paste0("b_forecast_yld_", defined_date_list)
assign(name,forecast_yld)
}

## merge my forecast_yld file


b_forecast_yld <- rbind(`b_forecast_yld_2024-04-01` ,
                      `b_forecast_yld_2024-05-28` ,
                      `b_forecast_yld_2024-06-30` ,
                      `b_forecast_yld_2024-07-24` ,
                      `b_forecast_yld_2024-08-30` ,
                      `b_forecast_yld_2024-09-30` ,
                      `b_forecast_yld_2024-10-24`)
rm(`b_forecast_yld_2024-04-01` ,
   `b_forecast_yld_2024-05-28` ,
   `b_forecast_yld_2024-06-30` ,
   `b_forecast_yld_2024-07-24` ,
   `b_forecast_yld_2024-08-30` ,
   `b_forecast_yld_2024-09-30` ,
   `b_forecast_yld_2024-10-24`)
