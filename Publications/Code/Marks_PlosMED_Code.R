###########################################################
###########################################################
###########################################################
####                                                   ####
#### PRIMER Injection Initiation Model                 ####
#### Code Written by Charles Marks                     ####
####                                                   ####
###########################################################
###########################################################
###########################################################

### Required Libraries

library(deSolve)
library(minpack.lm)
library(tidyverse)
library(kableExtra)
library(triangle)
library(epiR)
library(prevalence)

## Setting Number of Simulations ##

## for testing functionality, drop this number down to 2
## for actual running, should be equal to the number of iteration you want to run
## running 100 rounds will take about an hour
## to run more than 1,000 iterations, likely best to run in smaller chunks and merge results

sampling_number <- 2

########### Function Definitions ##########################

### ODE Model Definitions ###

de_fun <- function(t, states, parameters){
  with(as.list(c(states,parameters)),
       {
         ## For full details of model schematic and equations, see manuscript
         ## First necessary to construct some additional variables for readability
         
         ## f is our overal initiatation rate, the flow from v to a
         f <- beta*(A + RR_Init*B + RR_OAT*C + RR_OAT*RR_Init*D)/(E+V+A+B+C+D+Ex)
         
         ## g is the number of A PWID who initiate during the time step and thus
         ## transition to B
         g <- m*beta*A/(E+V+A+B+C+D+Ex)
         
         ## h is the number of C PWID who initiate during the time step and thus
         ## transition to D
         h <- m*beta*C*RR_OAT/(E+V+A+B+C+D+Ex)
         
         ## rho modifier
         ## accounts for the increased risk of mortality
         ## during the four weeks after OAT dropout
         delta_exit <- 1 - ((mu_A-mu_E)*(RR_muOATout - 1)*(4/52))
       
         ##alpha modifier
         ##accounts for the increased risk of mortality
         ##during the first four weeks of OAT enrollment
         delta_enter <- 1 - ((mu_C-mu_E)*(RR_muOATin - 1)*(4/52))

         ### sigma: the replacement rate, overdoses are not replaced
         sigma <- mu_E*E + mu_E*V + mu_E*A + mu_E*B + mu_E*C + mu_E*D + mu_E*Ex 
         
         #Our full ODEs are defined here
         dE <- sigma - theta*E + gamma*V - mu_E*E
         dV <- theta*E - gamma*V - f*V - tau - mu_V*V
         dA <- tau + f*V - g*V + rho*C*delta_exit - alpha*A - xi*A - mu_A*A
         dB <- g*V + rho*D*delta_exit - alpha*B - xi*B - mu_B*B
         dC <- alpha*A*delta_enter - rho*C - h*V - xi*C - mu_C*C
         dD <- alpha*B*delta_enter + h*V - rho*D - xi*D - mu_D*D
         dEx <- xi*A + xi*B + xi*C + xi*D - mu_Ex*Ex
         
         return(list(c(dE,dV,dA,dB,dC,dD,dEx)))
       })
}



de_fun_two <- function(t, states, parameters, new_parameters){
  if(t < 900){
    with(as.list(c(states,parameters)),
         {
          ## This portion of the code is identical to the previous function
           ## it runs the baseline OAT coverage scenario
           
           f <- beta*(A + RR_Init*B + RR_OAT*C + RR_OAT*RR_Init*D)/(E+V+A+B+C+D+Ex)
           g <- m*beta*A/(E+V+A+B+C+D+Ex)
           h <- m*beta*C*RR_OAT/(E+V+A+B+C+D+Ex)
           delta_exit <- 1 - ((mu_A-mu_E)*(RR_muOATout - 1)*(4/52))
           delta_enter <- 1 - ((mu_C-mu_E)*(RR_muOATin - 1)*(4/52))
           sigma <- mu_E*E + mu_E*V + mu_E*A + mu_E*B + mu_E*C + mu_E*D + mu_E*Ex 
           
           #Our full ODEs are defined here
           
           dE <- sigma - theta*E + gamma*V - mu_E*E
           dV <- theta*E - gamma*V - f*V - tau - mu_V*V
           dA <- tau + f*V - g*V + rho*C*delta_exit - alpha*A - xi*A - mu_A*A
           dB <- g*V + rho*D*delta_exit - alpha*B - xi*B - mu_B*B
           dC <- alpha*A*delta_enter - rho*C - h*V - xi*C - mu_C*C
           dD <- alpha*B*delta_enter + h*V - rho*D - xi*D - mu_D*D
           dEx <- xi*A + xi*B + xi*C + xi*D - mu_Ex*Ex
           
           return(list(c(dE,dV,dA,dB,dC,dD,dEx)))
         })
  } else {
    
    with(as.list(c(states,new_parameters)),
         {
           
           ## Here a new value for alpha (OAT scale-up) is supplied at t = 900
           
           f <- beta*(A + RR_Init*B + RR_OAT*C + RR_OAT*RR_Init*D)/(E+V+A+B+C+D+Ex)
           g <- m*beta*A/(E+V+A+B+C+D+Ex)
           h <- m*beta*C*RR_OAT/(E+V+A+B+C+D+Ex)
           delta_exit <- 1 - ((mu_A-mu_E)*(RR_muOATout - 1)*(4/52))
           delta_enter <- 1 - ((mu_C-mu_E)*(RR_muOATin - 1)*(4/52))
           sigma <- mu_E*E + mu_E*V + mu_E*A + mu_E*B + mu_E*C + mu_E*D + mu_E*Ex 
           
           #Our full ODEs are defined here
           dE <- sigma - theta*E + gamma*V - mu_E*E
           dV <- theta*E - gamma*V - f*V - tau - mu_V*V
           dA <- tau + f*V - g*V + rho*C*delta_exit - alpha*A - xi*A - mu_A*A
           dB <- g*V + rho*D*delta_exit - alpha*B - xi*B - mu_B*B
           dC <- alpha*A*delta_enter - rho*C - h*V - xi*C - mu_C*C
           dD <- alpha*B*delta_enter + h*V - rho*D - xi*D - mu_D*D
           dEx <- xi*A + xi*B + xi*C + xi*D - mu_Ex*Ex
           
           return(list(c(dE,dV,dA,dB,dC,dD,dEx)))
         })
  }
}

### This first calibration function is responsible for calibrating
### beta, gamma, tau, and aloha for our basline coverage scenario

calib_function <- function(x, parameters){
  
  #define states, starting with a population of a million
  E <- 901000
  V <- 89000
  A <- 9000
  B <- 1000
  C <- 0
  D <- 0
  Ex <- 0
  
  states = c(E = E, 
             V = V, 
             A = A, 
             B = B, 
             C = C, 
             D = D, 
             Ex = Ex)
  
  ## Calibration Variables
  beta <- x[1]
  gamma <- x[2]
  tau <- x[3]
  oat_enroll <- x[4]
  
  ## Parameters
  muBaseline <- parameters[1]
  muInjection <- parameters[2]
  RR_Init <- parameters[3]
  RR_OAT <- parameters[4]
  average_assisted <- parameters[5]
  injection_cessation <- parameters[6]
  theta <- parameters[7]
  rho <- parameters[8]
  tau_target <- parameters[9]
  PWID_target <- parameters[10]
  V_target <- parameters[11]
  RR_muOATout <- parameters[12]
  RR_muOATin <- parameters[13]
  RR_oat_protection <- parameters[14]
  OAT_target <- parameters[15]
  
  
  ## Computed Variables
  m <- 1/average_assisted
  mu_E <- muBaseline
  mu_V <- muBaseline
  mu_A <- muBaseline + muInjection
  mu_B <- muBaseline + muInjection
  mu_C <- muBaseline + muInjection*RR_oat_protection
  mu_D <- muBaseline + muInjection*RR_oat_protection
  mu_Ex <- muBaseline
  alpha <- oat_enroll
  xi <- injection_cessation

  ## Parameters to be fed to the ODE solver  
  parameters = c(mu_E = mu_E,
                 mu_V = mu_V, 
                 mu_A = mu_A, 
                 mu_B = mu_B, 
                 mu_C = mu_C, 
                 mu_D = mu_D, 
                 mu_Ex = mu_Ex, 
                 beta = beta, 
                 RR_Init = RR_Init, 
                 RR_OAT = RR_OAT, 
                 theta = theta, 
                 gamma = gamma, 
                 alpha = alpha, 
                 rho = rho, 
                 m = m, 
                 xi = xi, 
                 tau = tau,
                 RR_muOATout = RR_muOATout,
                 RR_muOATin = RR_muOATin)
  
  #define time
  t = seq(0,1000, by = 1)
  
  
  ## Run the ODE solver with the first 
  result <- ode(y = states, 
                times = t, 
                func = de_fun, 
                parms = parameters)
  
  result_df <- data.frame(result)
  
  
  ## The following values measure how good a fit our current calibration attempt is
  PWID_prop <- (result_df$A[900] + result_df$B[900] + result_df$C[900] + result_df$D[900])/(result_df$E[900] +result_df$V[900] +result_df$A[900] + result_df$B[900] + result_df$C[900] + result_df$D[900] + result_df$Ex[900])
  
  V_prop <- result_df$V[900]/(result_df$E[900] +result_df$V[900] +result_df$A[900] + result_df$B[900] + result_df$C[900] + result_df$D[900] + result_df$Ex[900])
  
  initiations <- result_df$V[900]*beta*(result_df$A[900] + RR_Init*result_df$B[900] + RR_OAT*result_df$C[900] + RR_OAT*RR_Init*result_df$D[900])/(result_df$E[900]+result_df$V[900] +result_df$A[900]+result_df$B[900] +result_df$C[900]+result_df$D[900]+result_df$Ex[900])
  
  OAT_coverage <- (result_df$C[900] + result_df$D[900])/(result_df$A[900] + result_df$B[900] + result_df$C[900] + result_df$D[900])
  
  ##This vector returns the distance our calibration fit is it the target
  formula <- c((PWID_prop - PWID_target)^2, 
               (V_prop - V_target)^2,
               (tau/(tau + initiations) - tau_target)^2,
               (OAT_coverage - OAT_target)^2)
  
  return(formula)
  
}

### Different OAT level calibration function

calib_function2 <- function(x, parameters, target_coverage){
  
  #define states
  E <- 901000
  V <- 89000
  A <- 9000
  B <- 1000
  C <- 0
  D <- 0
  Ex <- 0
  
  states = c(E = E, 
             V = V, 
             A = A, 
             B = B, 
             C = C, 
             D = D, 
             Ex = Ex)
  
  ## Calibration Values
  alpha <- x[1]
  
  ## Model Parameters
  muBaseline <- parameters[1]
  muInjection <- parameters[2]
  RR_Init <- parameters[3]
  RR_OAT <- parameters[4]
  average_assisted <- parameters[5]
  injection_cessation <- parameters[6]
  theta <- parameters[7]
  rho <- parameters[8]
  RR_muOATout <- parameters[12]
  RR_muOATin <- parameters[13]
  RR_oat_protection <- parameters[14]
  beta <- parameters[16]
  gamma <- parameters[17]
  tau <- parameters[18]
  
  
  ## Calculated Variables
  mu_E <- muBaseline
  mu_V <- muBaseline
  mu_A <- muBaseline + muInjection
  mu_B <- muBaseline + muInjection
  mu_C <- muBaseline + muInjection*RR_oat_protection
  mu_D <- muBaseline + muInjection*RR_oat_protection
  mu_Ex <- muBaseline
  m <- 1/average_assisted
  xi <- injection_cessation
  
  parameters = c(mu_E = mu_E,
                 mu_V = mu_V, 
                 mu_A = mu_A, 
                 mu_B = mu_B, 
                 mu_C = mu_C, 
                 mu_D = mu_D, 
                 mu_Ex = mu_Ex, 
                 beta = beta, 
                 RR_Init = RR_Init, 
                 RR_OAT = RR_OAT, 
                 theta = theta, 
                 gamma = gamma, 
                 alpha = alpha, 
                 rho = rho, 
                 m = m, 
                 xi = xi, 
                 tau = tau,
                 RR_muOATout = RR_muOATout,
                 RR_muOATin = RR_muOATin)
  
  #define time
  t = seq(0,1000, by = 1)
  
  result <- ode(y = states, 
                times = t, 
                func = de_fun, 
                parms = parameters)
  
  result_df <- data.frame(result)
  
  OAT_coverage <- (result_df$C[900] + result_df$D[900])/(result_df$A[900] + result_df$B[900] + result_df$C[900] + result_df$D[900])
  
  formula <- c((OAT_coverage - target_coverage)^2)
  
  return(formula)
  
}

########### End Function Definitions ######################

########### Parameter Sampling ################################

## First we want to define our data frame of parameters. 
## Each row is going to represent a different sampling
## In the next step we will run our auto-calibration on each row



samplings_df <- as.data.frame(matrix(nrow = sampling_number, ncol = 0))

## depending on the type of distribution we have a set of different functions
## normal distribution: rnorm()
## uniform distribution: runif()
## beta distribution: rbeta()
## we used the betaExpert() function to fit beta distributions
## for additional distribution samplings, go to the Stats Package Index and go to the "r" section

## Parameters table is available in the manuscript
## Parameters listed here in same order as in the manuscript

samplings_df$muBaseline <- rnorm(sampling_number, mean = 1/69.3, sd = .00)
samplings_df$muInjection <-rpois(sampling_number, lambda = 62)/(10000)
samplings_df$theta <- rbeta(sampling_number, 31.46398, 1239.868)
samplings_df$injection_cessation <- 1/rtriangle(sampling_number,a = 5,b = 25, c= 15)
samplings_df$OAT_dropout <- 1/runif(sampling_number, min = 1/4, max = 5/4)
samplings_df$average_assisted <- 1 + rgamma(sampling_number, shape = 0.8296943)
samplings_df$RR_Init <- exp(rnorm(sampling_number, mean = 1.5963, sd = 0.1882))
samplings_df$RR_OAT <- exp(rnorm(sampling_number, mean =  -0.5902, sd = 0.2135))
samplings_df$RR_muOATout <- exp(rnorm(sampling_number, mean = log(2.38), sd = 0.23))
samplings_df$RR_muOATin <- exp(rnorm(sampling_number, mean = log(1.97), sd = 0.37))
samplings_df$RR_oat_protection <- 1/exp(rnorm(sampling_number, mean = log(4.80), sd = 0.26))
samplings_df$V_prevalence <-rbeta(sampling_number, 999.9999, 10102)
samplings_df$PWID_prevalence <- rbeta(sampling_number, 13.34261,1153.055)
samplings_df$tau_proportion <- rbeta(sampling_number, 18.91557, 93.3341)
samplings_df$baseline_coverage <- rbeta(sampling_number, 9.937847, 36.75739)

########### End 1000 Samplings ############################

########### Calibration ###################################

## There will be two stages to the calibration
### First, we are going to calibrate beta, gamma, tau, and alpha for baseline coverage
### Then, we are going to calibrate OAT enrollment (alpha) for a series of different OAT coverage values (40,50,60%)

## Calibrating beta, gamma, tau, and alpha
## beta will be calibrated to the proportion of the population PWID represent (~1.06% based on Degenhardt paper)
## gamma will be calibrated to the proportion of non-injection drug users (excluding marijuana and alcohol) (~8.9% from NSDUH)
## tau will be calibrated to the proportion of annual IDU initiations which are not assisted
## alpha will be calibrated to baseline OAT coverage



## Vectors to extract the calibrated values
beta_vector <- c()
gamma_vector <- c()
tau_vector <- c()
oat_enroll_baseline <-c()
oat_enroll_40 <- c()
oat_enroll_50 <- c()
oat_enroll_60 <- c()


for(row in 1:sampling_number){
  
  
  ## we need to pull the starting parameters from the samples data frame we created
  calib_parameters <- c(samplings_df$muBaseline[row],
                        samplings_df$muInjection[row],
                        samplings_df$RR_Init[row],
                        samplings_df$RR_OAT[row],
                        samplings_df$average_assisted[row],
                        samplings_df$injection_cessation[row],
                        samplings_df$theta[row],
                        samplings_df$OAT_dropout[row],
                        samplings_df$tau_proportion[row],
                        samplings_df$PWID_prevalence[row],
                        samplings_df$V_prevalence[row],
                        samplings_df$RR_muOATout[row],
                        samplings_df$RR_muOATin[row],
                        samplings_df$RR_oat_protection[row],
                        samplings_df$baseline_coverage[row])
  
  ## we need to define our starting point for calibrating beta, gamma, tau, and alpha (oat_enroll)
  beta <- 0.6
  gamma <- 0.2
  tau <- 0
  oat_enroll <- 1
  
  ## this is our vector of starting values
  x0 <- c(beta, gamma, tau, oat_enroll)
  
  ## we also want to set lower and upper bounds for these variables
  upper <- c(2,1,1000, 10)
  lower <- c(0,0,25,0)
  
  ## at this stage we can run the calibration function
  ## for baseline we will calibrate to OAT population baseline
  calibration_results <- nls.lm(x0, 
                                lower = lower, 
                                upper = upper, 
                                fn = calib_function,
                                parameters = calib_parameters)
  
  ## extract calibrated values
  beta <- calibration_results$par[1] 
  gamma <- calibration_results$par[2] 
  tau <- calibration_results$par[3]
  oat_enroll <- calibration_results$par[4]
  
  ## store calibrated values
  beta_vector <- c(beta_vector, beta)
  gamma_vector <- c(gamma_vector, gamma)
  tau_vector <- c(tau_vector, tau)
  oat_enroll_baseline <- c(oat_enroll_baseline, oat_enroll)
  
  ## for the remaining calibrations we want to update parameters
  ## to include the calibrated parameters beta, gamma, and tau
  calib_parameters <- c(calib_parameters, c(beta, gamma, tau))
  ## we will start calibrations with baseline alpha value
  x0 <- c(oat_enroll)
  
  ## Calibrating different OAT enrollment
  
  
  
  ## 40% coverage
  oat_enroll <- .4
  lower <- c(0)
  upper <- c(10)
  
  calibration_results <- nls.lm(x0, 
                                lower = lower, 
                                upper = upper, 
                                fn = calib_function2,
                                parameters = calib_parameters,
                                target_coverage = oat_enroll)
  
  oat_enroll <- calibration_results$par[1]
  oat_enroll_40 <- c(oat_enroll_40, oat_enroll)
  
  ## 50% coverage
  oat_enroll <- .5
  lower <- c(0)
  upper <- c(10)
  
  calibration_results <- nls.lm(x0, 
                                lower = lower, 
                                upper = upper, 
                                fn = calib_function2,
                                parameters = calib_parameters,
                                target_coverage = oat_enroll)
  
  oat_enroll <- calibration_results$par[1]
  oat_enroll_50 <- c(oat_enroll_50, oat_enroll)
  
  ## 60% coverage
  oat_enroll <- .6
  lower <- c(0)
  upper <- c(10)
  
  calibration_results <- nls.lm(x0, 
                                lower = lower, 
                                upper = upper, 
                                fn = calib_function2,
                                parameters = calib_parameters,
                                target_coverage = oat_enroll)
  
  oat_enroll <- calibration_results$par[1]
  oat_enroll_60 <- c(oat_enroll_60, oat_enroll)
  
}

## store all of the calibrated values into the parameters data frame
samplings_df$beta <- beta_vector
samplings_df$gamma <- gamma_vector
samplings_df$tau <- tau_vector
samplings_df$oat_enroll_baseline <- oat_enroll_baseline
samplings_df$oat_enroll_40 <- oat_enroll_40
samplings_df$oat_enroll_50 <- oat_enroll_50
samplings_df$oat_enroll_60 <- oat_enroll_60

## At the stage it is a good idea to save these values
## Calibrating is a computationally intensive process
## So as to not need to repeat this process, uncomment the following code

write.csv(samplings_df, "calibrated_model_parameters.csv")

########### End Calibration ###############################

########### Run The Model #################################

## so we want to run our models and collect data so that 
## we can get values related to the impact of OAT coverage
## on initiation rates

## so for each row in the samplings_df table
## we will create 4 sets of results
## one for each OAT coverage: baseline, 20%, 40%, 60%
## and we will save the results (compartment populations)
## for 25 years following the implementation of different OAT levels

simulations_results <- NA

for(i in 1:nrow(samplings_df)){
  
  ## set the starting conditions
  ## note same as the calibration functions
  E <- 901000
  V <- 89000
  A <- 9000
  B <- 1000
  C <- 0
  D <- 0
  Ex <- 0
  
  states = c(E = E, 
             V = V, 
             A = A, 
             B = B, 
             C = C, 
             D = D, 
             Ex = Ex)
  
  #define parameters
  muBaseline <- samplings_df$muBaseline[i]
  muInjection <- samplings_df$muInjection[i]
  RR_oat_protection <- samplings_df$RR_oat_protection[i]
  
  mu_E <- muBaseline
  mu_V <- muBaseline
  mu_A <- muBaseline + muInjection
  mu_B <- muBaseline + muInjection
  mu_C <- muBaseline + muInjection*RR_oat_protection
  mu_D <- muBaseline + muInjection*RR_oat_protection
  mu_Ex <- muBaseline
  
  ## Initiation Function Parameters
  tau <- samplings_df$tau[i]
  beta <- samplings_df$beta[i]
  RR_Init <- samplings_df$RR_Init[i]
  RR_OAT <- samplings_df$RR_OAT[i]
  
  average_assisted <- samplings_df$average_assisted[i]
  m <- 1/average_assisted
  
  ## other flows
  injection_cessation <- samplings_df$injection_cessation[i]
  
  theta <- samplings_df$theta[i]
  gamma <- samplings_df$gamma[i]
  alpha <- 0
  rho <- samplings_df$OAT_dropout[i]
  xi <- injection_cessation
  
  RR_muOATin <- samplings_df$RR_muOATin[i]
  RR_muOATout <- samplings_df$RR_muOATout[i]
  
  t = seq(0,1000, by = 1)
  
  ## for each of the four scenarios (baseline,40,50,60)
  ## we define an array of parameters
  ## note for each the value of alpha (which has been calibrated to each coverage level)
  ## is varied as OAT is scaled up
  
  parameters_baseline = c(mu_E = mu_E,
                          mu_V = mu_V, 
                          mu_A = mu_A, 
                          mu_B = mu_B, 
                          mu_C = mu_C, 
                          mu_D = mu_D, 
                          mu_Ex = mu_Ex, 
                          beta = beta, 
                          RR_Init = RR_Init, 
                          RR_OAT = RR_OAT, 
                          theta = theta, 
                          gamma = gamma, 
                          alpha = samplings_df$oat_enroll_baseline[i], 
                          rho = rho, 
                          m = m, 
                          xi = xi,
                          RR_muOATout = RR_muOATout,
                          RR_muOATin = RR_muOATin)
  
  parameters_forty = c(mu_E = mu_E,
                       mu_V = mu_V, 
                       mu_A = mu_A, 
                       mu_B = mu_B, 
                       mu_C = mu_C, 
                       mu_D = mu_D, 
                       mu_Ex = mu_Ex, 
                       beta = beta, 
                       RR_Init = RR_Init, 
                       RR_OAT = RR_OAT, 
                       theta = theta, 
                       gamma = gamma, 
                       alpha = samplings_df$oat_enroll_40[i], 
                       rho = rho,
                       m = m, 
                       xi = xi,
                       RR_muOATout = RR_muOATout,
                       RR_muOATin = RR_muOATin)
  
  parameters_fifty = c(mu_E = mu_E,
                       mu_V = mu_V, 
                       mu_A = mu_A, 
                       mu_B = mu_B, 
                       mu_C = mu_C, 
                       mu_D = mu_D, 
                       mu_Ex = mu_Ex, 
                       beta = beta, 
                       RR_Init = RR_Init, 
                       RR_OAT = RR_OAT, 
                       theta = theta, 
                       gamma = gamma, 
                       alpha = samplings_df$oat_enroll_50[i], 
                       rho = rho,
                       m = m, 
                       xi = xi,
                       RR_muOATout = RR_muOATout,
                       RR_muOATin = RR_muOATin)
  
  parameters_sixty = c(mu_E = mu_E,
                       mu_V = mu_V, 
                       mu_A = mu_A, 
                       mu_B = mu_B, 
                       mu_C = mu_C, 
                       mu_D = mu_D, 
                       mu_Ex = mu_Ex, 
                       beta = beta, 
                       RR_Init = RR_Init, 
                       RR_OAT = RR_OAT, 
                       theta = theta, 
                       gamma = gamma, 
                       alpha = samplings_df$oat_enroll_60[i], 
                       rho = rho, 
                       m = m, 
                       xi = xi,
                       RR_muOATout = RR_muOATout,
                       RR_muOATin = RR_muOATin)
  
  ## run model for baseline OAT
  
  result <- ode(y = states, 
                times = t, 
                func = de_fun_two, 
                parms = parameters_baseline, 
                new_parameters = parameters_baseline)
  ## extract results
  result_baseline <- data.frame(result)
  
  ## calculate the number of ASSISTED initiations
  result_baseline <- as.data.frame(result_baseline) %>% 
    dplyr::mutate(initiation = V*beta*(A + RR_Init*B + RR_OAT*C + RR_OAT*RR_Init*D)/(E+V+A+B+C+D+Ex) )
  
  
  ## run model for 40% OAT
  result <- ode(y = states, 
                times = t, 
                func = de_fun_two, 
                parms = parameters_baseline, 
                new_parameters = parameters_forty)
  result_forty <- data.frame(result)
  result_forty <- as.data.frame(result_forty) %>% 
    dplyr::mutate(initiation = V*beta*(A + RR_Init*B + RR_OAT*C + RR_OAT*RR_Init*D)/(E+V+A+B+C+D+Ex) )
  
  ## run model for 50% OAT
  
  result <- ode(y = states, 
                times = t, 
                func = de_fun_two, 
                parms = parameters_baseline, 
                new_parameters = parameters_fifty)
  result_fifty <- data.frame(result)
  result_fifty <- as.data.frame(result_fifty) %>% 
    dplyr::mutate(initiation = V*beta*(A + RR_Init*B + RR_OAT*C + RR_OAT*RR_Init*D)/(E+V+A+B+C+D+Ex) )
  
  ## run model for 60% OAT
  
  result <- ode(y = states, 
                times = t, 
                func = de_fun_two, 
                parms = parameters_baseline, 
                new_parameters = parameters_sixty)
  result_sixty <- data.frame(result)
  result_sixty <- as.data.frame(result_sixty) %>% 
    dplyr::mutate(initiation = V*beta*(A + RR_Init*B + RR_OAT*C + RR_OAT*RR_Init*D)/(E+V+A+B+C+D+Ex) )
  
  ## we want the 25 years where their parameters change
  
  result_baseline <- result_baseline[901:925,]
  result_forty <- result_forty[901:925,]
  result_fifty <- result_fifty[901:925,]
  result_sixty <- result_sixty[901:925,]
  
  result_baseline$OAT_coverage <- "Baseline"
  result_forty$OAT_coverage <- "Forty"
  result_fifty$OAT_coverage <- "Fifty"
  result_sixty$OAT_coverage <- "Sixty"
  
  results <- rbind(rbind(result_baseline,result_forty),rbind(result_fifty,result_sixty))
  results$simulation_number <- i
  
  if(is.na(simulations_results)){
    simulations_results <- results
  }else{
    simulations_results <- rbind(simulations_results, results)
  }
}

## After running simulations, the results can be saved by uncommenting the following line of code

write.csv(simulations_results, "Simulation_Results.csv") 

########### End Run The Model #############################

########### Lastly, Formatting Data for Analysis ##########

## Note, that we need to do two things 
### 1) add the number of self-initiations to the number of assisted initiations
### 2) standardize this number to a population of 1,000,000
### because our model does not replace PWID overdose deaths, the population is likely less than 1,000,000

simulations_results$population <- simulations_results$E + 
  simulations_results$V +
  simulations_results$A +
  simulations_results$B +
  simulations_results$C +
  simulations_results$D +
  simulations_results$Ex

simulations_results$PWID <-  simulations_results$A +
  simulations_results$B +
  simulations_results$C +
  simulations_results$D 

simulations_results$unassisted <- simulations_results$initiation
simulations_results$assisted <- 0

## Need to add that number who self-initiate (tau)
## Need to standardize rate to population of 1,000,000

for(i in 1:nrow(simulations_results)){
  
  sim_number <- simulations_results$simulation_number[i]
  
  ## get the standardized (per 1,000,000) for both assisted and unassisted initiations
  simulations_results$unassisted[i] <- 1000000*samplings_df$tau[sim_number]/simulations_results$population[i]
  simulations_results$assisted[i] <- 1000000*simulations_results$initiation[i]/simulations_results$population[i]
  
  ## add tau (self_initiations)
  simulations_results$initiation[i] <- simulations_results$initiation[i] + samplings_df$tau[sim_number]
  
  ## standardize to population of 1,000,000
  simulations_results$initiation[i] <- 1000000*(simulations_results$initiation[i]/simulations_results$population[i])
}

## Now you have the data!
write.csv(simulations_results, "Final_Results_Ready_For_Analysis.csv")


