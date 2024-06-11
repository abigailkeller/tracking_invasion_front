data{/////////////////////////////////////////////////////////////////////
    int<lower=1> C;    // number of trap samples
    int<lower=1> R[C];   // index of locations for trap samples
    int<lower=1> Nloc;   // number of locations
    int<lower=0> E[C];   // number of crabs trapped in trap sample C
    real phi; // set value for phi (median estimate from joint model posterior)
    real mupriors[2]; // priors for gamma distrib on mu
    
    
}

parameters{/////////////////////////////////////////////////////////////////////
    real<lower=0> mu[Nloc];  // expected crabs/trap at each site
    }

transformed parameters{/////////////////////////////////////////////////////////////////////
  
}

model{/////////////////////////////////////////////////////////////////////

    for(j in 1:C){
        E[j] ~ neg_binomial_2(mu[R[j]], phi); // Eq. 1.1
    }
    
    
    mu ~ gamma(mupriors[1], mupriors[2]); // mu prior
  
}

generated quantities{
  
}

