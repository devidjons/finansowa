library(dplyr)
library(openxlsx)
data = read.xlsx("googl_opt.xlsx")
data$r = 0.027
head(data)
data_by_ask = data %>% filter(Open.Interest > 0)
data_by_last_price = data %>% filter(Open.Interest == 0, Volume > 0)
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

get_volatility=function(n, data, price_colname)
{
    S=data$S[n]
    K=data$Strike[n]
    r=data$r[n]
    Time=data$T[n]
    price=data[n,price_colname]
    #return(uniroot(function(vol) call_opt_price(S,K,r,vol,Time)-price, interval = c(0.001,8.8))$root)
    print(paste0(call_opt_price(S,K,r,0.000001,Time)," ", price))
}
get_volatility(3,data_by_ask, "Ask")
data_by_ask[5,]
