library(dplyr)
#############################
#initial parameters
S=100
K=90
r=0.03
sigma=0.2
Time=1
N_step=1000
N_simulations=10000
#############################

get_stock_path = function(Time, sigma, N_step, S0,r)
{
        dt=Time/N_step
        winner_diff=rnorm(N_step)*sigma*sqrt(dt)
        S=S0*cumprod(1+r*dt+winner_diff)
        return(c(S0,S))
}
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

#cena opcji metoda monte carlo


(sapply(1:N_simulations, function(x)
                        (get_stock_path(Time, sigma, N_step, S, r)[N_step + 1]) - K) * exp(-r * Time)) %>% 
    mean ->
    simulation_price
theoretical_price=call_opt_price(S,K,r,sigma,Time)
print(paste("theoretical price =", theoretical_price, " simulation_price =", simulation_price))
