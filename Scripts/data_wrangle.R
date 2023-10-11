library(lubridate)
library(tidyverse)


time_series_list <- list.files('../Data/csse_covid_19_daily_reports_us', full.names = TRUE)


df <- lapply(time_series_list, function(file){
  table <- read.csv(file)[,1:18]
  table$Date <- substring(file, 90,99) 
  
  table
})

binded_df <- do.call(rbind, df)

binded_df <- filter(binded_df, !(binded_df$Province_State %in% c("American Samoa",
                                                             "Diamond Princess",
                                                             "Grand Princess",
                                                             "Guam",
                                                             "Northern Mariana Islands",
                                                             "Puerto Rico",
                                                             "Virgin Islands")))

#sort from least recent to most recent 
sorted_df <- binded_df %>% arrange(mdy(binded_df$Date))
