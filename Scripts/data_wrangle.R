library(lubridate)
library(tidyverse)
library(readxl)


time_series_list <- list.files('Data/csse_covid_19_daily_reports_us', full.names = TRUE)


df <- lapply(time_series_list, function(file){
  table <- read.csv(file)[,1:18]
  table$Date <- substring(file, 37,46)
  table$Converted_Date <- as_date(table$Date, format = "%m-%d-%Y")

  table
})

binded_df <- do.call(rbind, df)

binded_df <- filter(binded_df, !(binded_df$Province_State %in% c("American Samoa",
                                                             "Diamond Princess",
                                                             "Grand Princess",
                                                             "Guam",
                                                             "Northern Mariana Islands",
                                                             "Puerto Rico",
                                                             "Virgin Islands",
                                                             "Recovered")))

#sort from least recent to most recent 
sorted_df <- binded_df %>% arrange(ymd(binded_df$Converted_Date))

#save cleaned data
write.csv(sorted_df, file = "Data/csse_covid_19_daily_reports_us_CLEAN.csv")



#Joining in time series data - Stringency Index
StringencyIndex <- read_excel('Data/OxCGRT_timeseries_us_subnational_indices.xlsx', 
                              sheet = "StringencyIndex")

StringencyIndex_clean <- StringencyIndex %>%
  filter(!is.na(RegionName)) %>%
  pivot_longer(cols= c(colnames(StringencyIndex[8:1162])),
                    names_to='Date',
                    values_to='StringencyIndex')

  
StringencyIndex_clean$Converted_Date <- as_date(StringencyIndex_clean$Date, format = "%d%b%Y")

StringencyIndex_clean$RegionName[StringencyIndex_clean$RegionName == "Washington DC"] <- "District of Columbia"

tidyData <- left_join(sorted_df, 
                       StringencyIndex_clean, 
                       by = c("Province_State" = "RegionName", "Converted_Date"))


