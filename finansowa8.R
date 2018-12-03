#############################
library(dplyr)
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
