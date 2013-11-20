data {
  int<lower=0> N;
  int<lower=0,upper=1> y[N];
  real x[N];
  int<lower=1,upper=5> cond[N];
  int<lower=1,upper=2> io[N];
  real aMu1[6];
  real aMu2[6];
  real<lower=0> bMu1[6];
  real<lower=0> bMu2[6];
  real<lower=0> aSigma1[6];
  real<lower=0> aSigma2[6];
  real<lower=0> bSigma1[6];
  real<lower=0> bSigma2[6];
  real<lower=0,upper=1> proMu;
  real proSigma;
  real<lower=0,upper=1> heavyMu;
  real heavySigma;
}
parameters {
  real<lower=-5,upper=5> a1[5];
  real<lower=-5,upper=5> a2[5];
  real<lower=0,upper=10> b1[5];
  real<lower=0,upper=10> b2[5];
  real<lower=0,upper=1> heavy[2];
  real<lower=0,upper=1> pro[2];
}

transformed parameters {
  real<lower=0.001,upper=0.999> mu[N];
  for (n in 1:N) {
    if (io[n] == 1) {
      if (cond[n] == 1) {
        mu[n] <- (1 / (1 + exp(-(a1[1] + b1[1] * x[n]))));
      }
      else if (cond[n] == 2) {
        mu[n] <- (1 / (1 + exp(-(a1[2] + b1[2] * x[n]))))*(1-(heavy[1] / (1 + exp(-(a1[3] + b1[3] * x[n])))));
      }
      else if (cond[n] == 3) {
        mu[n] <- (1 / (1 + exp(-(a1[4] + b1[4] * x[n]))));
      }
      else if (cond[n] == 4) {
        mu[n] <- (1 / (1 + exp(-(a1[5] + b1[5] * x[n]))));
      }
      else if (cond[n] == 5) {
        mu[n] <- (1 / (1 + exp(-(a1[6] + b1[6] * x[n]))));
      }
    }
    else if (io[n] == 2) {
      if (cond[n] == 1) {
        mu[n] <- (pro[1] / (1 + exp(-(a2[1] + b2[1] * x[n]))));
      }
      else if (cond[n] == 2) {
        mu[n] <- (1 / (1 + exp(-(a2[2] + b2[2] * x[n]))))*(1-((heavy[2]) / (1 + exp(-(a2[3] + b2[3] * x[n])))));
      }
      else if (cond[n] == 3) {
        mu[n] <- (pro[2] / (1 + exp(-(a2[4] + b2[4] * x[n]))));
      }
      else if (cond[n] == 4) {
        mu[n] <- (1 / (1 + exp(-(a2[5] + b2[5] * x[n]))));
      }
      else if (cond[n] == 5) {
        mu[n] <- (1 / (1 + exp(-(a2[6] + b2[6] * x[n]))));
      }
    }
    if (mu[n] > .999) mu[n] <- .999;
    else if (mu[n] < .001) mu[n] <- .001;
  }
}

model {
  a1[1] ~ normal(aMu1[1], aSigma1[1]) T[-5,5];
  a1[2] ~ normal((aMu1[2]+aMu1[3])/2, (aSigma1[2]+aSigma2[3])/2) T[-5,5];
  a1[3] ~ normal(aMu1[4], aSigma1[4]) T[-5,5];
  a1[4] ~ normal(aMu1[5], aSigma1[5]) T[-5,5];
  a1[5] ~ normal(aMu1[6], aSigma1[6]) T[-5,5];

  a2[1] ~ normal(aMu2[1], aSigma2[1]) T[-5,5];
  a2[2] ~ normal((aMu2[2]+aMu2[3])/2, (aSigma2[2]+aSigma2[3])/2) T[-5,5];
  a2[3] ~ normal(aMu2[4], aSigma2[4]) T[-5,5];
  a2[4] ~ normal(aMu2[5], aSigma2[5]) T[-5,5];
  a2[5] ~ normal(aMu2[6], aSigma2[6]) T[-5,5];

  b1[1] ~ normal(bMu1[1], bSigma1[1]) T[0,10];
  b1[2] ~ normal((bMu1[2]+bMu1[3])/2, (bSigma1[2]+bSigma1[3])/2) T[0,10];
  b1[3] ~ normal(bMu1[4], bSigma1[4]) T[0,10];
  b1[4] ~ normal(bMu1[5], bSigma1[5]) T[0,10];
  b1[5] ~ normal(bMu1[6], bSigma1[6]) T[0,10];

  b2[1] ~ normal(bMu2[1], bSigma2[1]) T[0,10];
  b2[2] ~ normal((bMu2[2]+bMu2[3])/2, (bSigma2[2]+bSigma2[3])/2) T[0,10];
  b2[3] ~ normal(bMu2[4], bSigma2[4]) T[0,10];
  b2[4] ~ normal(bMu2[5], bSigma2[5]) T[0,10];
  b2[5] ~ normal(bMu2[6], bSigma2[6]) T[0,10];

  heavy[1] ~ normal(heavyMu, heavySigma) T[0,1.0];
  heavy[2] ~ normal(heavyMu, heavySigma) T[0,1.0];
 
  pro[1] ~ normal(proMu, proSigma) T[0,1.0];
  pro[2] ~ normal(proMu, proSigma) T[0,1.0];

  y ~ bernoulli(mu);
}
