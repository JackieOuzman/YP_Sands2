b_f_forecast_yld$GS_deciles_names <- factor(b_f_forecast_yld$GS_deciles_names , ordered = TRUE,
                                            levels = c("decile_1", "decile_2", "decile_3", "decile_4",
                                                       "decile_5", "decile_6", "decile_7", "decile_8",
                                                       "decile_9","decile_10" ))
b_f_forecast_yld
df_gs
plot1 <-b_f_forecast_yld %>%http://127.0.0.1:16783/graphics/plot_zoom_png?width=1280&height=658
  ggplot(aes(x = defined_date , y = Yield_forecast, group=method) )+
  geom_point(aes(color=method))+
  geom_line(aes(color=method))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(.~GS_deciles_names )+
  labs(title = "Yield_forecast",
       subtitle = paste0("Station name: ", distinct(df_gs, Station_name ),
                         " - ", distinct(df_gs, Station_number )),
       caption = paste0("Years included to make deciles : ", distinct(df_gs,Years_included),
                        ". GS period:",  distinct(df_gs,GS_period)),
       y = "Yield kg/ha",
       x = "",
       colour = "" #removed the legend title
  )
plot1
