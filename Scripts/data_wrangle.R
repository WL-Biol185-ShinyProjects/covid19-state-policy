time_series_list <- list.files('~/Project/covid19-state-policy/Data/csse_covid_19_daily_reports_us', full.names = TRUE)

df <- lapply(time_series_list, function(file){
  read.csv(file)
})

binded_df <- do.call(rbind, df)
