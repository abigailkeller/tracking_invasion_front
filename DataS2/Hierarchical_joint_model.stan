data{/////////////////////////////////////////////////////////////////////
    int<lower=1> S;    // number of qPCR samples
    int<lower=1> C;    // number of trap samples
    int<lower=1> L[S];   // index of locations for qPCR samples
    int<lower=1> R[C];   // index of locations for trap samples
    int<lower=1> Nloc;   // number of locations
    int<lower=0> E[C];   // number of crabs trapped in trap sample C
    int<lower=1> N[S];   // number of qPCR replicates per site
    int<lower=0> K[S]; // number of qPCR detections among these replicates
    real mupriors[2]; //priors for normal distrib on mu
    real<lower=0> phipriors[2]; //priors for gamma distrib on phi
    real<lower=0> betapriors[2]; //priors for gamma distrib on beta
    real<lower=0> sigmapriors[2]; //priors for gamma distrib on sigma, the shared variance among bottles for p
    real p10priors[2]; //priors for normal distrib on (log) p10
}

parameters{/////////////////////////////////////////////////////////////////////
    real log_mu[Nloc];  // expected crabs/trap at each site, log scale
    real<lower=0> phi;  // dispersion parameter
    real<lower=0> beta; // scaling coefficient in saturation function
    real<upper=0> p10;  //p10, false-positive rate. Note it's on a log scale, so max is 0
    real<upper=0> log_p_bottle[S]; //bottle-level prob, log scale
    real<lower=0> sigma; // variance across bottles, within site
}

transformed parameters{/////////////////////////////////////////////////////////////////////
  real<upper=0> log_p11[Nloc]; // true-positive detection probability, log scale
  real<upper=0> log_site_mean_p[Nloc];  // site total detection probability, log scale
  
  for (i in 1:Nloc){
    log_p11[i] = log(exp(log_mu[i]) / (exp(log_mu[i]) + beta)); // Eq. 2.2
    log_site_mean_p[i] = log_sum_exp(log_p11[i] , p10); // Eq. 2.3
  }


}

model{/////////////////////////////////////////////////////////////////////

    for(j in 1:C){
        E[j] ~ neg_binomial_2(exp(log_mu[R[j]]), phi); // Eq. 2.1
    }
    
    // get ML bottle-level prob
    for (i in 1:S){
          K[i] ~ binomial(N[i], exp(log_p_bottle[i])); // Eq. 2.5
          log_p_bottle[i] ~ normal(log_site_mean_p[L[i]], sigma); // Eq. 2.4
    }
    
  //priors
  beta ~ gamma(betapriors[1],betapriors[2]);
  log_mu ~ normal(mupriors[1],mupriors[2]);
  phi ~ gamma(phipriors[1],phipriors[2]);
  p10 ~ normal(p10priors[1], p10priors[2]);
  sigma ~ gamma(sigmapriors[1],sigmapriors[2]);
}

generated quantities{
  real mu_real[Nloc];

  mu_real = exp(log_mu); // estimated mu on real scale

}

