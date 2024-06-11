library(tidyverse)
library(patchwork)
library(rstan)
library(shinystan)
library(bayesplot)
library(broom)
library(dplyr)
library(gtools)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#get median parameter estimates from joint model posteriors
joint_posterior <- read.csv('Joint_posterior.csv')
joint_median <- joint_posterior %>% 
  summarize_all(median) #joint model media
beta_joint_median <- joint_median$beta #estimated beta median
phi_joint_median <- joint_median$phi #estimated phi median
p10_joint_median <- joint_median$p10 #estimated p10 median

#create simulated scenarios
#true mu
mu <- c(0,0.02,0.05,0.1,0.15,0.25,0.5,1,3)
#true p11
p11 <- rep(NA, length(mu))
for(i in 1:length(mu)){
  p11[i] <- mu[i]/(beta_joint_median+mu[i])
}
#traps
traps <- c(3,4,5,7,10,12,15,20,30,40,60)

#create input dataframe
input <- expand.grid(mu,traps) #expand grid
input$ID <- seq(from = 1, to = length(input$Var1), by = 1)
colnames(input) <- c('mu', 'traps', 'ID')
input <- merge(input, cbind(mu,p11)) #add associated p11
input <- input[order(input$ID),] #sort by ID

#create empty output dataframes
#50 replicates (columns), 4 chains & 2500 sampling iterations/chain (rows)
output_joint <- as.data.frame(matrix(NA, nrow = 10001, ncol = length(input$ID)*50))
output_joint[1,] <- rep(input$ID, times=50) #for joint model output
output_trap <- as.data.frame(matrix(NA, nrow = 10001, ncol = length(input$ID)*50))
output_trap[1,] <- rep(input$ID, times=50) #for trap model output

#compile models
jointm <- stan_model(file = "Simulation_Joint.stan")
trapm <- stan_model(file = "Simulation_Trap-only.stan")


#Define the MCMC
N_CHAIN = 4 #number of chains
Warm = 500 #number of warm-up iterations
Iter = 2500 #number of sampling iterations

#create function for initial values
stan_init <- function(n.chain){
  A <- list()
  for(i in 1:n.chain){
    A[[i]] <- list(
      mu = runif(length(unique(qPCR_data$L)), .01, 5)
    )
  }
  return(A)
}




for(j in 1:50){
  
  #create empty dataframes for simulated data
  qPCR_data <- data.frame(Bio_rep=integer(),
                          K=integer(), 
                          N=integer(),
                          L=integer(),
                          stringsAsFactors=FALSE)
  trap_data <- data.frame(rep=integer(),
                          Count=integer(),
                          L=integer())
  
  for(i in 1:length(input$mu)){
    
    #set up grid for simulated qPCR data
    qPCR <- expand.grid(
      Bio_rep = c(1:5),
      N = 3,
      K = NA,
      L = input[i,'ID']
    )
    
    #set up grid for simulated trap data
    nreps <- input[i,'traps']
    trap <- expand.grid(
      rep = c(1:nreps),
      Count = NA,
      L = input[i,'ID']
    )
    
    #simulate data
    qPCR[,'K'] <- rbinom(5, 3, input[i,'p11']+plogis(p10_joint_median))
    trap[,'Count'] <- rnbinom(n = input[i,'traps'], 
                              size = phi_joint_median,
                              mu = input[i,'mu'])
    
    #add simulated data to qPCR and trap data
    qPCR_data <- rbind(qPCR_data, qPCR)
    trap_data <- rbind(trap_data, trap)
    
    
  }
  
    ##Stan Model -- joint model
  model_joint <- sampling(object = jointm,
                          data = list(
                            S = nrow(qPCR_data),
                            L = qPCR_data$L,
                            Nloc = length(unique(qPCR_data$L)),
                            K = qPCR_data$K,
                            N = qPCR_data$N,
                            C = nrow(trap_data),
                            R = trap_data$L,
                            E = trap_data$Count,
                            beta = beta_joint_median,
                            phi = phi_joint_median,
                            p10 = p10_joint_median,
                            mupriors = c(0.05, 0.05),
                            control = list(adapt_delta = 0.999,
                                           stepsize = 0.5)
                          ), 
                          chains = N_CHAIN, 
                          thin = 1, 
                          warmup = Warm, 
                          iter = Warm + Iter,
                          init = stan_init(n.chain=N_CHAIN)
  )
  
  model_trap <- sampling(object = trapm,
                         data = list(
                           Nloc = length(unique(qPCR_data$L)),
                           C = nrow(trap_data),
                           R = trap_data$L,
                           E = trap_data$Count,
                           phi = phi_joint_median,
                           mupriors = c(0.05, 0.05),
                           control = list(adapt_delta = 0.999,
                                          stepsize = 0.5)
                         ), 
                         chains = N_CHAIN, 
                         thin = 1, 
                         warmup = Warm, 
                         iter = Warm + Iter,
                         init = stan_init(n.chain=N_CHAIN)
  )
  
  #save model output
  joint_posterior <- as.data.frame(model_joint)
  trap_posterior <- as.data.frame(model_trap)
  
  #add mu posteriors to output dataframes
  output_joint[2:10001,((j-1)*99+1):((j-1)*99+99)] <- joint_posterior[,1:99]
  output_trap[2:10001,((j-1)*99+1):((j-1)*99+99)] <- trap_posterior[,1:99]
  
}

#create summaries of simulation posteriors
ID <- joint_summary[1,] #extract simulation ID
#joint model summary -- mean, sd, and cv for each mu posterior
output_joint <- output_joint[-1,] #remove first row (simulation ID)
joint_summary <- output_joint %>% #calculate mean, sd, and cv of each mu posterior 
  summarize_all(list(mean, sd)) %>% 
  mutate(cv=sd/mean)
joint_summary <- cbind(ID,joint_summary) #add simulation ID to summary
#trap-only model summary -- mean, sd, and cv for each mu posterior
output_trap <- output_trap[-1,] #remove first row (simulation ID)
trap_summary <- output_trap %>% #calculate mean, sd, and cv of each mu posterior 
  summarize_all(list(mean, sd)) %>% 
  mutate(cv=sd/mean)
trap_summary <- cbind(ID,trap_summary) #add simulation ID to summary

#save input and output data frames
write.csv(input, 'simulation_scenarios.csv')
write.csv(joint_summary, 'simulation_joint_summary.csv') 
write.csv(trap_summary, 'simulation_trap_summary.csv')




