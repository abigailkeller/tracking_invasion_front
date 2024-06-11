data{/////////////////////////////////////////////////////////////////////
    int<lower=1> C;    // number of trap samples
    int<lower=1> R[C];   // index of locations for trap samples
    int<lower=1> Nloc;   // total number of locations 
    int<lower=1> Nloc_crab;   // number of locations with historic trapped crab
    int<lower=1> Z[Nloc_crab];   // index of locations with historic trapped crab
    int<lower=1> Nloc_nocrab;   // number of locations with no historic trapped crab
    int<lower=1> X[Nloc_nocrab];   // index of locations with no historic trapped crab
    int<lower=0> E[C];   // number of crabs trapped in trap sample C
    real<lower=0> phipriors[2]; // priors for gamma distrib on phi
    real<lower=0> mu_crab_priors[2]; // priors for gamma distrib on mu_crab
    real<lower=0> mu_nocrab_priors[2]; // priors for gamma distrib on mu_nocrab
    
}

parameters{/////////////////////////////////////////////////////////////////////
    real<lower=0> mu_crab[Nloc_crab];  // expected crabs/trap at each site with historic trapped crabs
    real<lower=0> mu_nocrab[Nloc_nocrab];  // expected crabs/trap at each site w/out historic trapped crabs
    real<lower=0> phi;  // dispersion parameter
    }

transformed parameters{/////////////////////////////////////////////////////////////////////
  real<lower=0> mu[Nloc]; // crabs/trap at all sites
  
  for(i in 1:Nloc_crab){
    mu[Z[i]] = mu_crab[i]; // index mu_crab into estimated mu vector
  }
  
  for(i in 1:Nloc_nocrab){
    mu[X[i]] = mu_nocrab[i]; // index mu_nocrab into estimated mu vector
  }
  
}

model{/////////////////////////////////////////////////////////////////////

    for(j in 1:C){
        E[j] ~ neg_binomial_2(mu[R[j]], phi); // Eq. 1.1
    }
    
    
  //priors
  phi ~ gamma(phipriors[1],phipriors[2]); // phi prior
  mu_crab ~ gamma(mu_crab_priors[1],mu_crab_priors[2]); // mu_crab prior
  mu_nocrab ~ gamma(mu_nocrab_priors[1],mu_nocrab_priors[2]); // mu_nocrab prior
  
}

generated quantities{
  
}

