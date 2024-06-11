data{/////////////////////////////////////////////////////////////////////
    int<lower=1> S;    // number of qPCR samples
    int<lower=1> C;    // number of trap samples
    int<lower=1> L[S];   // index of locations for qPCR samples
    int<lower=1> R[C];   // index of locations for trap samples
    int<lower=1> Nloc;   // number of locations
    int<lower=1> Nloc_crab;   // number of locations with historic trapped crab
    int<lower=1> Z[Nloc_crab];   // index of locations with historic trapped crab
    int<lower=1> Nloc_nocrab;   // number of locations with no historic trapped crab
    int<lower=1> X[Nloc_nocrab];   // index of locations with no historic trapped crab
    int<lower=0> E[C];   // number of crabs trapped in trap sample C
    int<lower=1> N[S];   // number of qPCR replicates per site
    int<lower=0> K[S]; // number of qPCR detections among these replicates
    real<lower=0> phipriors[2]; // priors for gamma distrib on phi
    real<lower=0> betapriors[2]; // priors for gamma distrib on beta
    real p10priors[2]; // priors for normal distrib on (log) p10
    real<lower=0> mu_crab_priors[2]; // priors for gamma distrib on mu_crab
    real<lower=0> mu_nocrab_priors[2]; // priors for gamma distrib on mu_nocrab

}

parameters{/////////////////////////////////////////////////////////////////////
    real<lower=0> mu_crab[Nloc_crab];  // expected crabs/trap at each site
    real<lower=0> mu_nocrab[Nloc_nocrab];  // expected crabs/trap at each site
    real<lower=0> phi;  // dispersion parameter
    real beta; // scaling coefficient in saturation function
    real<upper=0> p10;  // p10, false-positive rate. Note it's on a log scale, so max is 0
}

transformed parameters{/////////////////////////////////////////////////////////////////////
  real<lower=0, upper = 1> p11[Nloc]; // true-positive detection probability
  real<lower=0, upper = 1> p[Nloc];   // total detection probability
  real<lower=0> mu[Nloc]; // crabs/trap at all sites
  
  for(i in 1:Nloc_crab){
    mu[Z[i]] = mu_crab[i]; // index mu_crab into estimated mu vector
  }
  
  for(i in 1:Nloc_nocrab){
    mu[X[i]] = mu_nocrab[i]; // index mu_nocrab into estimated mu vector
  }
  
  for (i in 1:Nloc){
    p11[i] = mu[i] / (mu[i] + beta); // Eq. 1.2
    p[i] = p11[i] + exp(p10); // Eq. 1.3
  }
}

model{/////////////////////////////////////////////////////////////////////


    for(j in 1:C){
        E[j] ~ neg_binomial_2(mu[R[j]], phi); // Eq. 1.1
    }
    
    for (i in 1:S){
        K[i] ~ binomial(N[i], p[L[i]]); // Eq. 1.4
    }
    
    
  //priors
  beta ~ gamma(betapriors[1],betapriors[2]); // beta prior
  phi ~ gamma(phipriors[1],phipriors[2]); // phi prior
  mu_crab ~ gamma(mu_crab_priors[1],mu_crab_priors[2]); // mu_crab prior
  mu_nocrab ~ gamma(mu_nocrab_priors[1],mu_nocrab_priors[2]); // mu_nocrab prior
  p10 ~ normal(p10priors[1], p10priors[2]); // p10 prior
  
}

generated quantities{
  
}

