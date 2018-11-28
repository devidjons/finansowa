library(dplyr)
library(openxlsx)
data = read.xlsx("googl_opt.xlsx")
data$r = 0.027
head(data)
data_by_ask = data %>% filter(Open.Interest > 0)
data_by_last_price = data %>% filter(Open.Interest == 0, Volume > 0)
    