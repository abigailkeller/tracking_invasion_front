data{/////////////////////////////////////////////////////////////////////
    int<lower=1> S;    // number of qPCR samples
    int<lower=1> C;    // number of trap samples
    int<lower=1> L[S];   // index of locations for qPCR samples
    int<lower=1> R[C];   // index of locations for trap samples
    int<lower=1> Nloc;   // number of locations
    int<lower=0> E[C];   // number of crabs trapped in trap sample C
    int<lower=1> N[S];   // number of qPCR replicates per site
    int<lower=0> K[S]; // number of qPCR detections among these replicates
    real phi; // set value for phi (median estimate from joint model posterior)
    real beta; // set value for beta (median estimate from joint model posterior)
    real p10; // set value for p10 (median estimate from joint model posterior)
    real mupriors[2]; // priors for gamma distrib on mu

}

parameters{/////////////////////////////////////////////////////////////////////
    real<lower=0> mu[Nloc];  // expected crabs/trap at each site
}

transformed parameters{/////////////////////////////////////////////////////////////////////
  real<lower=0, upper = 1> p11[Nloc]; // true-positive detection probability
  real<lower=0, upper = 1> p[Nloc];   // total detection probability
  
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
    
   
    mu ~ gamma(mupriors[1], mupriors[2]); // mu prior
  
}

generated quantities{
   
  
}

