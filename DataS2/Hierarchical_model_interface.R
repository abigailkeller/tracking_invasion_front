######################
library(tidyverse)
library(rstan)
library(shinystan)
library(bayesplot)
library(broom)
library(dplyr)
library(boot)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
######################

#import data
qPCR_data <- read.csv("qPCR_results.csv")
trap_data <- read.csv("trap_data.csv")

#replace "No Ct" with 0
qPCR_data <- mutate_if(qPCR_data,
                  is.character,
                  str_replace_all, pattern = "No Ct", replacement = "0")

#create N column, number of technical replicates
qPCR_data$N <- 3

#create K column, number of positive detections per water bottle
qPCR_data[,c(4:6)] <- apply(qPCR_data[,c(4:6)], MARGIN = 2, FUN = as.numeric)
K <- vegan::specnumber(qPCR_data[,c(4:6)])  
qPCR_data$K <- K

#create index for each site
sites <- as.data.frame(matrix(NA, length(unique(trap_data$Site)), 2))
colnames(sites) <- c('Site', 'L')
sites$Site <- unique(trap_data$Site)
sites$L <- match(sites$Site, unique(trap_data$Site)) 
qPCR_data <- merge(qPCR_data, sites, by = 'Site')

#add indexing to site info for traps
trap_data <- trap_data %>% 
  left_join(sites) %>% 
  arrange(L)

#p10 prior: convert beta(1,28) to lognormal distribution
#moment match from beta(alpha,beta) to normal(mu, sigma^2)
alpha <- 1
beta <- 28
mu <- alpha/(alpha+beta)
sigma_2 <- (alpha*beta)/((alpha+beta)^2*(alpha+beta+1))

#convert normal(mu, sigma^2) to lognormal(mu, sigma^2)
mu_ln <- log(mu^2/sqrt(mu^2+sigma_2))
sigma_2_ln <- log(1+sigma_2/mu^2)
sigma_ln <- sqrt(sigma_2_ln)

#create function for initial values -- joint model
stan_init_joint <- function(n.chain){
  A <- list()
  for(i in 1:n.chain){
    A[[i]] <- list(
      log_mu = runif(length(unique(qPCR_data$Site)), -4, 1),
      sigma = runif(1, .01, .05),
      p10 = -4,
      beta = runif(1, 1, 3)
    )
  }
  return(A)
}

#create function for initial values -- trap-only model
stan_init_trap <- function(n.chain){ 
  A <- list()
  for(i in 1:n.chain){
    A[[i]] <- list(
      mu = runif(length(unique(qPCR_data$Site)), 0.1, 5)
    )
  }
  return(A)
}

# Define the MCMC
N_CHAIN = 4 #number of chains
Warm = 500 #number of warm-up iterations
Iter = 2500 #number of sampling iterations

##Stan Model-- hierarchical joint model
hier_joint_model <- stan(file = "Hierarchical_joint_model.stan", 
              data = list(
                S = nrow(qPCR_data),
                L = qPCR_data$L,
                Nloc = length(unique(qPCR_data$Site)),
                K = qPCR_data$K,
                N = qPCR_data$N,
                C = nrow(trap_data),
                R = trap_data$L,
                E = trap_data$Count,
                betapriors = c(2,1),
                p10priors = c(mu_ln,sigma_ln), #log scale
                phipriors = c(0.25,0.25),
                mupriors = c(-2, 5), #log scale
                sigmapriors = c(2,10),
                control = list(adapt_delta = 0.999,
                               stepsize = 0.5)
              ), 
              chains = N_CHAIN, 
              thin = 1, 
              warmup = Warm, 
              iter = Warm + Iter,
              init = stan_init_joint(n.chain=N_CHAIN)
)



#save model output
hier_joint_posterior <- as.data.frame(joint_model)
write.csv(hier_joint_posterior, 'Hierarchical_joint_posterior.csv')
