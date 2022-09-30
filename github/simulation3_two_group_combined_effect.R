
# This script provides the code and plot for two groups with no differences


# use functions
source("t_test_functions.R")

# seed 117
set.seed(117)

sim_number <- 1000


# G1
M1 <- 60
Nn1 <- 20
Ns1 <- 20
d_mean1 <- 2.5
d_sd1 <- 0.8
c_mean1 <- 0.4
c_sd1 <- 0

# G2
M2 <- 60
Nn2 <- 20
Ns2 <- 20
d_mean2 <- 2.5
d_sd2 <- 0.5
c_mean2 <- 0.2
c_sd2 <- 0



result <- group_comparison_t_test(d_mean1=d_mean1,
                                  c_mean1=c_mean1,
                                  d_sd1=d_sd1,
                                  c_sd1=c_sd1,
                                  d_mean2=d_mean2,
                                  c_mean2=c_mean2,
                                  d_sd2=d_sd2,
                                  c_sd2=c_sd2,
                                  Nn1=Nn1,
                                  Ns1=Ns1,
                                  M1=M1,
                                  Nn2=Nn2,
                                  Ns2=Ns2,
                                  M2=M2,
                                  sim_number=sim_number,
                                  replacement_value=0.5, 
                                  two.groups=1) 
prop.t <- sum(ifelse(result$p.t<0.05,1,0))/sim_number
prop.rep <- sum(ifelse(result$p.rep<0.05,1,0))/sim_number
prop.ll <- sum(ifelse(result$p.ll<0.05,1,0))/sim_number

prop.t
prop.rep
prop.ll



