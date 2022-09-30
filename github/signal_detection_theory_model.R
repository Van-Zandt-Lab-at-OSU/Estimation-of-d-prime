

# This function is used to perform signal detection theory simulation

sdt_simulation <- function(Nn = 20,    # number of noise trial
                           Ns = 20,    # number of signal trials
                           sim_number = 100,  # number of simulations
                           d_mean.t,    # true d' mean used for simulations
                           c_mean.t,    # true response bias mean used for simulations
                           d_sd.t,      # true d' standard deviation for simulations
                           c_sd.t,       # true response bias standard deviations for simulations
                           replacement_values = c(0.5), # one or more values for the replacement method,
                           replacement = 1, # 1: perform replacement; 0: don't perform
                           log_linear = 1  # 1: perform log-linear; 0: don't perform
                           ){
  
# In this function, based on the d' and c distributions specified,
# it first simulates a list of true d' and c values (list length based on sim_number),
# then it simulated signal detection responses based each pair of d' and c value,
# and trial numbers. Finally, it estimates SDT parameters based on the selected correction method,
# and selected correction values if applicable

# a list of perfect responses  
  x <- c(rep(0,Nn),rep(1,Ns))

# obtain number of values to be used in replacement  
  L <- length(replacement_values)

# define the outcomes  
  d.t <- rep(0,sim_number)             # the list of true d' to be sampled
  c.t <- rep(0,sim_number)             # the list of true c to be sampled  
  d.rep <- array(0,dim=c(L,sim_number))  # estimated values using replacement correction
  c.rep <- array(0,dim=c(L,sim_number))
  d.ll <- rep(0,sim_number)            # estimated values using log-linear correction
  c.ll <- rep(0,sim_number)
  accuracy <- rep(0,sim_number)        # response accuracy
  
# simulate true values
  
  d.t <- rnorm(sim_number,d_mean.t,d_sd.t)
  c.t <- rnorm(sim_number,c_mean.t,c_sd.t)
  
# signal detection theory simulation

  for(i in 1:sim_number){
    
    # select values to be used to generate simulated data   
    d <- d.t[i]
    c <- c.t[i]
    
    # generate simulated data from the true SDT distributions
    # res is the simulated responses
    res <- ifelse(x==0,rnorm(Ns+Nn,0,1),rnorm(Ns+Nn,d,1))
    res <- ifelse(res<d/2+c,0,1)
    accu <- ifelse(res==x,1,0)    
    
    # response accuracy
    accuracy[i] <- mean(accu)
    
    # replacement: compute estimated values when replacement=1
    if(replacement == 1){
      # computation based on each replacement value specified by replacement_values
      for(l in 1:L){
        rep.v <- replacement_values[l]
        # Hit
        h.mean <- mean(accu[(Nn+1):(Nn+Ns)])
        H <- ifelse(h.mean==1,1-rep.v/Ns,ifelse(h.mean==0,rep.v/Ns,
                                                h.mean))
        # False alarm
        f.mean <- mean(accu[1:Nn])
        FA <- 1 - ifelse(f.mean==1,1-rep.v/Nn,ifelse(f.mean==0,rep.v/Nn,f.mean))
        
        # SDT parameter estimations
        d.rep[l,i] <- qnorm(H)+qnorm(1-FA)
        c.rep[l,i] <- -(qnorm(H)+qnorm(FA))/2
      }
    }

    # log-linear: compute estimated values when log_linear=1
    if(log_linear == 1){
      h.mean <- mean(accu[(Nn+1):(Nn+Ns)])
      f.mean <- mean(accu[1:Nn])
      H <- (Ns*h.mean + 0.5)/(Ns + 1)
      FA <- 1 - (Nn*f.mean + 0.5)/(Nn + 1)
      d.ll[i] <- qnorm(H)+qnorm(1-FA)
      c.ll[i] <- -(qnorm(H)+qnorm(FA))/2
    }
  }

# list the results 
  result <- list(d.t=d.t,
                 c.t=c.t,
                 d.rep=d.rep,
                 c.rep=c.rep,
                 d.ll=d.ll,
                 c.ll=c.ll,
                 replacement_values=replacement_values)
  
  return(result)
  
}

