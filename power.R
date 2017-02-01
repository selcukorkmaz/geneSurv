## 1. Exponential Model
## 1.1 Test for Equality

hazardRate1 = 1
hazardRate2 = 2
T=3
T0=1
k = 1
alpha = 0.05
beta = 0.2



sigma_hazardRate1 = (hazardRate1^2)*(1+(exp(-hazardRate1*T)-exp(-hazardRate1*(T+T0)))/hazardRate1*T0)^(-1)
sigma_hazardRate2 = (hazardRate2^2)*(1+(exp(-hazardRate2*T)-exp(-hazardRate2*(T+T0)))/hazardRate2*T0)^(-1)

za = qnorm(alpha/2,lower.tail = F)
zb = qnorm(beta,lower.tail = F)

n = round((((za+zb)^(2))/(hazardRate2-hazardRate1)^(2))*((sigma_hazardRate1/k)+sigma_hazardRate2),0)

## 1.1.1 Unconditional Versus Conditional (Lachin, 1981)

e = hazardRate1-hazardRate2
hazardRateMean = (k*hazardRate1+hazardRate2)/(k+1)  
sigma_hazardRateMean = (hazardRateMean^2)*(1+(exp(-hazardRateMean*T)-exp(-hazardRateMean*(T+T0)))/hazardRateMean*T0)^(-1)

n = (1/e^(2))*(za*sigma_hazardRateMean*((1/k)+1)+ zb*((sigma_hazardRate1/k)+sigma_hazardRate2)^(0.5))^(2)

## 1.2 Test for Non-Inferiority/Superiority

hazardRate1 = 1
hazardRate2 = 2
superiorityMargin = 0.2
T=3
T0=1
k = 1
alpha = 0.05
beta = 0.2


sigma_hazardRate1 = (hazardRate1^2)*(1+(exp(-hazardRate1*T)-exp(-hazardRate1*(T+T0)))/hazardRate1*T0)^(-1)
sigma_hazardRate2 = (hazardRate2^2)*(1+(exp(-hazardRate2*T)-exp(-hazardRate2*(T+T0)))/hazardRate2*T0)^(-1)

za = qnorm(alpha,lower.tail = F)
zb = qnorm(beta,lower.tail = F)

n = round((((za+zb)^(2))/(hazardRate2-hazardRate1-superiorityMargin)^(2))*((sigma_hazardRate1/k)+sigma_hazardRate2),0)


## 1.2 Test for Equivalence

hazardRate1 = 1
hazardRate2 = 1
equivalenceMargin = 0.5
T=3
T0=1
k = 1
alpha = 0.05
beta = 0.2


sigma_hazardRate1 = (hazardRate1^2)*(1+(exp(-hazardRate1*T)-exp(-hazardRate1*(T+T0)))/hazardRate1*T0)^(-1)
sigma_hazardRate2 = (hazardRate2^2)*(1+(exp(-hazardRate2*T)-exp(-hazardRate2*(T+T0)))/hazardRate2*T0)^(-1)

za = qnorm(alpha,lower.tail = F)
zb = qnorm(beta/2,lower.tail = F)

n = round((((za+zb)^(2))/(equivalenceMargin-abs(hazardRate1-hazardRate2))^(2))*((sigma_hazardRate1/k)+sigma_hazardRate2),0)



