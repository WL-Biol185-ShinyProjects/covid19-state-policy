library(dplyr)
library(readxl)
library(lubridate)

detailedPolicyData <- read_xlsx("~/Project/covid19-state-policy/Data/OxCGRT_USA_detailed_policy.xlsx")

detailedPolicyData_tidy <- detailedPolicyData %>%
  mutate(Date = ymd(Date))

write.csv(detailedPolicyData_tidy, file = "~/Project/covid19-state-policy/Data/OxCGRT_USA_detailed_policy_tidy.csv")
