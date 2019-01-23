source("finansowa6.R")
colnames(result) = c("K", "sigma")
result
#initialization
K1 = 1025
K2 = 1240
sigma = result[result[, 1] == K1, 2]
S = data$S[1]
r = data$r[1]
Time = data$T[1]
#


#delta
delta1 = 1 #akcja
delta2 = pnorm(d1(S, K1, r, sigma, Time)) #pierwsza opcja
delta3 = pnorm(d1(S, K2, r, sigma, Time)) #druga opcjca
#

#gamma
gamma1 = 0 #akcja
gamma2 = dnorm(d1(S, K1, r, sigma, Time)) / (S * sigma * sqrt(Time)) #pierwsza opcja
gamma3 = dnorm(d1(S, K2, r, sigma, Time)) / (S * sigma * sqrt(Time)) #druga opcja

#szukamy hedge dla 1 akcji

##
A = matrix(c(delta2, delta3, gamma2, gamma3), 2, 2, byrow = T)
b = c(-delta1, -gamma1)
portfel = c(1, solve(A, b)) #odpowiedz= 1.00000 -2.45094  5.08314

#check hedge
portfel %*% c(gamma1, gamma2, gamma3) # 0
portfel %*% c(delta1, delta2, delta3) # 0
