## Download the **Growing season for different dates** decile file 

#```{r sites_selction_download gs file, echo=FALSE, warning=FALSE}
#GS_decile_table_df_25006 

df_gs <- read.csv(
  paste0(
    file_path_input_data,
    "/GS_decile_table_df_fixed_dates_",
    site_selected, 
    ".csv") )
df_gs <- df_gs %>% mutate(across(where(is.numeric), round, 2))

DT::datatable(df_gs)

DT::datatable(distinct(df_gs, defined_date))

### make a new clm called Defined date from the date range clm defined_date.

distinct(df_gs,defined_date )
distinct(daily_file_sum_rain_all_GS,Defined_date )

df_gs <- df_gs %>% 
  mutate(Defined_date_common_year = case_when(
    defined_date == "from 1-4to 14-10" ~ paste0(year, "-04-01"),
    defined_date == "from 1-5to 14-10" ~ paste0(year, "-05-01"),
    defined_date == "from 1-6to 14-10" ~ paste0(year, "-06-01"),
    defined_date == "from 1-7to 14-10" ~ paste0(year, "-07-01"),
    defined_date == "from 1-8to 14-10" ~ paste0(year, "-08-01"),
    defined_date == "from 1-9to 14-10" ~ paste0(year, "-09-01"),
    defined_date == "from 1-10to 14-10" ~ paste0(year, "-10-01")
  ))

## join the decile GS historical data 
names(daily_file_sum_rain_all_GS)
df_gs <- left_join(df_gs , daily_file_sum_rain_all_GS,  by = join_by(Defined_date_common_year == Defined_date))


### add this years summer rainfall


df_gs <- df_gs %>% 
  mutate(summer_rain_sum = daily_file_summer_sum_rain$sum_rain)

#rename some clm to make it clear
names(df_gs)
#GS_rainfall_defined_date_historical
#summer_rain_current_yr
#GS_rainfall_defined_date_current_yr

df_gs <- df_gs %>% rename(GS_rainfall_defined_date_historical =GS_decile_max_rain,
                          GS_rainfall_defined_date_current_yr = sum_rain,
                          summer_rain_current_yr = summer_rain_sum)


# * **Baseline**						
# * 1. ` Estimated Starting Water (mm) = Summer rainfall *.25		(est_sw_base)				
# * 2. `Estimated Evaporation` = (Estimated Rainfall to maturity + GSR to date) *0.4	(est_evap_base)					
# * 3. `Estimated Water Supply` = (GS Rain + Estimated rainfall to maturity+ Estimated Starting water) - Estimated Evaporation		(est_water_supp_base)	
# * 4. `Estimated Yield Potential (kg/ha) = Estimated Water Supply *20	 (est_yp_base)				
# * 5.  Yield_forecast = 80% * `EPY` 		Yld_forecast_base


names(df_gs)

df_gs <- df_gs %>% 
  mutate(est_sw_base = (summer_rain_current_yr*.25),
         
         est_evap_base = (GS_rainfall_defined_date_historical +GS_rainfall_defined_date_current_yr)*0.4,
         
         est_water_supp_base = ((GS_rainfall_defined_date_current_yr + 
                             GS_rainfall_defined_date_historical+ 
                             est_sw_base) -   est_evap_base),
         
         est_yp_base = (est_water_supp_base *20),
         Yld_forecast_base = (est_yp_base*.80)
         )


names(df_gs)



df_gs_base <- df_gs %>% select(Defined_date_common_year, GS_deciles_names, GS_rainfall_defined_date_historical,
                               Station_name, Station_number, summer_rain_current_yr, GS_rainfall_defined_date_current_yr,
                               
                               est_sw_base,
                               est_evap_base,
                               est_water_supp_base,
                               est_yp_base,
                               Yld_forecast_base)
                               





