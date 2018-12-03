#############################
#opcje na google 2019/02/15
#############################
library(dplyr)
library(openxlsx)
data = read.xlsx("googl_opt.xlsx")
data$r = 0.027
head(data)
data_by_ask = data %>% filter(Ask > 0) %>% mutate(price=(Bid+Ask)/2)
data_by_last_price = data %>% filter( Volume > 0)
d1 = function(S, K, r, sigma, Time)
{
    #d1 from BS formula
    return((log(S / K) + (r + (sigma ^ 2) / 2)*Time) / (sigma*sqrt(Time)))
}

d2 = function(S, K, r, sigma, Time)
{
    #d2 from BS formula
    return(d1(S, K, r, sigma,Time) - sigma*sqrt(Time))
}

call_opt_price = function(S, K, r, sigma, Time)
{
    #call option price from BS formula
    return(S * pnorm(d1(S, K, r, sigma, Time)) - K * exp(-r * Time) * pnorm(d2(S, K, r, sigma, Time)))
}


get_volatility = function(n, data, price_colname)
{
    #wylicza volatility na postawie (BS_formula(vol)-call_price)^2 ->min
    ################################
    
    #initial parameters
    S = data$S[n]
    K = data$Strike[n]
    r = data$r[n]
    Time = data$T[n]
    price = data[n, price_colname]
    #
    
    #ciag volatility
    vol = seq(0.01, 0.9, by = 0.01)
    #
    
    #wartosci (BS_formula(vol)-call_price)^2
    vols=sapply(vol, function(x) (call_opt_price(S, K, r, x, Time) - price) ^ 2)
    #
    
    #wybieramy volatility o najmniejszej roznice
    vol[which.min(vols)] %>%
        return
}

#dla kazdego wiersza wyliczyc volatility implikowana
result = sapply(1:dim(data_by_ask)[1], function(x)
    c(
        data_by_ask[x, "Strike"],
        get_volatility(x, data_by_ask, "price")
    )) %>% t
plot(result[result[,2]>0.1,]) #wyrzucam outliers
