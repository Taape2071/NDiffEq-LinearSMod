library(readr)
library(ggplot2)
library(pid)
library(unrepx)
library(FrF2)
library(pracma)

LinStat_Data_Rep_1 <- read_delim("LinStat/LinStat Data Rep 1.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

LinStat_Data_Rep_2 <- read_delim("LinStat/LinStat Data Rep 2.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

Y1 = as.matrix(LinStat_Data_Rep_1[8])
Y2 = as.matrix(LinStat_Data_Rep_2[8])

X_vals = c(1,-1,-1,-1,1,1,1,-1,
           1,-1,-1,1,1,-1,-1,1,
           1,-1,1,-1,-1,1,-1,1,
           1,-1,1,1,-1,-1,1,-1,
           1,1,-1,-1,-1,-1,1,1,
           1,1,-1,1,-1,1,-1,-1,
           1,1,1,-1,1,-1,-1,-1,
           1,1,1,1,1,1,1,1)

X <- matrix(X_vals,nrow = 8, ncol = 8,byrow=TRUE)

beta_1 = inv(t(X)%*%X)%*%t(X)%*%Y1
beta_2 = inv(t(X)%*%X)%*%t(X)%*%Y2
beta_2

S2 = mean((Y1-Y2)^2/2)
S2_effect = 4*S2/16
S2_effect

effects1 = beta_1*2
effects2 = beta_2*2

SME = qt((1-(1-0.05)^(1/8))/2,8,lower.tail = FALSE)*sqrt(S2_effect)
ME = qt(0.025,8,lower.tail = FALSE)*sqrt(S2_effect)

cv = c(ME,SME)

parplot((effects1[2:8]+effects2[2:8])/2, critvals=cv)
parplot(effects1[2:8], critvals = cv)
parplot(effects2[2:8], critvals = cv)

plan <- FrF2(8,3,factor.names=c("Damaged","Size","Type"),randomize = FALSE, replications = 2)
plan <- add.response(plan,c(Y1,Y2))

MEPlot(plan)
IAPlot(plan)

