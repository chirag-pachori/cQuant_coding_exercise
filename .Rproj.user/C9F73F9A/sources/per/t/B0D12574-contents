#PACKAGES INSTALL AND LOAD###############################################
install.packages("tidyverse","lubridate","stringr", "data.table")
install.packages("data.table")
library(tidyverse, lubridate, stringr)
library(data.table)



#TASK - 1 ######################################################################
data_16 <- as_tibble(read.csv("historicalPriceData/ERCOT_DA_Prices_2016.csv"))
data_17 <- as_tibble(read.csv("historicalPriceData/ERCOT_DA_Prices_2017.csv"))
data_18 <- as_tibble(read.csv("historicalPriceData/ERCOT_DA_Prices_2018.csv"))
data_19 <- as_tibble(read.csv("historicalPriceData/ERCOT_DA_Prices_2019.csv"))


data_all <- data_16 %>%
  add_row(data_17) %>%
  add_row(data_18) %>%
  add_row(data_19)

data_all
#TASK - 2 ######################################################################

average_price_by_month <- data_all %>%
  mutate(Year = (lubridate::year(data_all $ Date)), Month = (lubridate :: month(data_all$Date))) %>%
  group_by(SettlementPoint, Year, Month) %>%
  summarize(AveragePrice = mean(Price, na.rm=TRUE)) %>%
  select(SettlementPoint, Year, Month, AveragePrice)

average_price_by_month

#TASK - 3 ######################################################################
write_csv(average_price_by_month, "AveragePriceByMonth.csv")

#TASK - 4 ######################################################################

hourly_volatility <- data_all %>%
  mutate(Year = (lubridate::year(data_all $ Date)), Hour = (lubridate :: hour(data_all$Date))) %>%
  dplyr::filter(Price > 0, str_detect(SettlementPoint,"^HB")) %>%
  group_by(SettlementPoint, Year) %>%
  select(SettlementPoint, Year, Hour, Price) %>%
  summarize(HourlyVolatility = sd(log(Price)))
         
hourly_volatility  

#TASK - 5 ######################################################################
write_csv(hourly_volatility, "HourlyVolatilityByYear.csv")

#TASK - 6 ######################################################################

max_volatility_by_year <- hourly_volatility %>%
  group_by(Year) %>%
  summarize(HourlyVolatility = max(HourlyVolatility)) 

max_volatility_by_year

write_csv(max_volatility_by_year, "MaxVolatilityByYear.csv")
  
#TASK- 7 #######################################################################

spot_history_format <- data_all %>%
  mutate(Hour = (lubridate::hour(data_all $ Date)), Date = (lubridate::as_date(Date))) %>%
  pivot_wider(names_from = Hour, values_from = Price) %>%
  rename(Variable = SettlementPoint) %>%
  relocate(Variable) %>%
  arrange(Variable) %>%
  as_data_frame()

spot_history_format  

split_spot_history_format <- split(spot_history_format, spot_history_format$Variable)
split_spot_history_format
split_names = names(split_spot_history_format)
split_names
new_column_names <- names(spot_history_format)

new_column_names

# split_spot_history_format[1]
# 
for(i in seq_along(split_names)){
  # rename(names(split_spot_history_format[i]) = new_column_names)    #Tried renaming columns
  file_name <-  paste("formattedSpotHistory/",split_names[i],".csv") 
  write.csv(split_spot_history_format[i], file_name)
}
# split_names[1]
# 
# is_list(split_names)
# 
# write_csv(split_spot_history_format, "temp.csv")
# lapply(names(split_spot_history_format), function(x){write.table(spt1[[x]], file = paste("spot_", x, sep = ""))})
# 
# df <- split_spot_history_format %>%
#   unnest(cols = split_names)
#  
# df
# lapply(split_names, function(nm){
#   write.csv(x = split_spot_history_format[[nm]], file = paste0(nm, ".csv"), row.names = FALSE)
# })


