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

#save cleaned data, Step 1 Done
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

StringencyIndex_clean$Date <- NULL

tidyData_v1 <- left_join(sorted_df, 
                       StringencyIndex_clean, 
                       by = c("Province_State" = "RegionName", "Converted_Date"))

#Joining in time series data - ContainmentHealthIndex
ContainmentHealthIndex <- read_excel('Data/OxCGRT_timeseries_us_subnational_indices.xlsx', 
                              sheet = "ContainmentHealthIndex")

ContainmentHealthIndex_clean <- ContainmentHealthIndex %>%
  filter(!is.na(RegionName)) %>%
  pivot_longer(cols= c(colnames(ContainmentHealthIndex[8:1162])),
               names_to='Date',
               values_to='ContainmentHealthIndex')

ContainmentHealthIndex_clean <- ContainmentHealthIndex_clean[,c(4,8,9)]

ContainmentHealthIndex_clean$Converted_Date <- as_date(ContainmentHealthIndex_clean$Date, format = "%d%b%Y")

ContainmentHealthIndex_clean$RegionName[ContainmentHealthIndex_clean$RegionName == "Washington DC"] <- "District of Columbia"

ContainmentHealthIndex_clean$Date <- NULL

tidyData_v2 <- left_join(tidyData_v1, 
                      ContainmentHealthIndex_clean, 
                      by = c("Province_State" = "RegionName", "Converted_Date"))


#Joining in time series data - GovernmentResponseIndex
GovernmentResponseIndex <- read_excel('Data/OxCGRT_timeseries_us_subnational_indices.xlsx', 
                                     sheet = "GovernmentResponseIndex")

GovernmentResponseIndex_clean <- GovernmentResponseIndex %>%
  filter(!is.na(RegionName)) %>%
  pivot_longer(cols= c(colnames(GovernmentResponseIndex[8:1162])),
               names_to='Date',
               values_to='GovernmentResponseIndex')

GovernmentResponseIndex_clean <- GovernmentResponseIndex_clean[,c(4,8,9)]

GovernmentResponseIndex_clean$Converted_Date <- as_date(GovernmentResponseIndex_clean$Date, format = "%d%b%Y")

GovernmentResponseIndex_clean$RegionName[GovernmentResponseIndex_clean$RegionName == "Washington DC"] <- "District of Columbia"

GovernmentResponseIndex_clean$Date <- NULL

tidyData_v3 <- left_join(tidyData_v2, 
                         GovernmentResponseIndex_clean, 
                         by = c("Province_State" = "RegionName", "Converted_Date"))

#Joining in time series data - EconomicSupportIndex
EconomicSupportIndex <- read_excel('Data/OxCGRT_timeseries_us_subnational_indices.xlsx', 
                                      sheet = "EconomicSupportIndex")

EconomicSupportIndex_clean <- EconomicSupportIndex %>%
  filter(!is.na(RegionName)) %>%
  pivot_longer(cols= c(colnames(EconomicSupportIndex[8:1162])),
               names_to='Date',
               values_to='EconomicSupportIndex')

EconomicSupportIndex_clean <- EconomicSupportIndex_clean[,c(4,8,9)]

EconomicSupportIndex_clean$Converted_Date <- as_date(EconomicSupportIndex_clean$Date, format = "%d%b%Y")

EconomicSupportIndex_clean$RegionName[EconomicSupportIndex_clean$RegionName == "Washington DC"] <- "District of Columbia"

EconomicSupportIndex_clean$Date <- NULL

tidyData_v4 <- left_join(tidyData_v3, 
                         EconomicSupportIndex_clean, 
                         by = c("Province_State" = "RegionName", "Converted_Date"))


#save cleaned data
write.csv(tidyData_v4, file = "Data/covid19_state_policy_tidydata.csv")


