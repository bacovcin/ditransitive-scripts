data {
  int<lower=0> N;
  int<lower=0,upper=1> y[N];
  real x[N];
  int<lower=1,upper=4> cond[N];
  real aMu[5];
  real<lower=0> bMu[5];
  real<lower=0> aSigma[5];
  real<lower=0> bSigma[5];
  real<lower=0,upper=1> heavyMu;
  real heavySigma;
}

parameters {
  real<lower=-5,upper=5> a[4,4];
  real<lower=0,upper=10> b[4,4];
  real<lower=0,upper=1> heavy[4];
}

transformed parameters {
  real<lower=0.00001,upper=0.99999> mu[4,N];
  for (i in 1:6) {
    for (n in 1:N) {
      if (cond[n] == 1) {
        mu[i,n] <- (1 / (1 + exp(-(a[i,1] + b[i,1] * x[n]))));
      }
      else if (cond[n] == 2) {
        mu[i,n] <- (1 / (1 + exp(-(a[i,2] + b[i,2] * x[n]))));
      }
      else if (cond[n] == 3) {
        mu[i,n] <- (1 / (1 + exp(-(a[i,3] + b[i,3] * x[n]))))*(1-(heavy[i] / (1 + exp(-(a[i,4] + b[i,4] * x[n])))));
      }
      else if (cond[n] == 4) {
        mu[i,n] <- (1 / (1 + exp(-(a[i,5] + b[i,5] * x[n]))));
      }
      if (mu[i,n] > .99999) mu[i,n] <- .99999;
      else if (mu[i,n] < .00001) mu[i,n] <- .00001;
    }
  }
}

model {
  a[1,1] ~ normal(aMu[1], aSigma[1]) T[-5,5];
  a[1,2] ~ normal(aMu[2], aSigma[2]) T[-5,5];
  a[1,3] ~ normal(aMu[3], aSigma[3]) T[-5,5];
  a[1,4] ~ normal(aMu[4], aSigma[4]) T[-5,5];
  a[1,5] ~ normal(aMu[5], aSigma[5]) T[-5,5];

  a[2,1] ~ normal(aMu[1], aSigma[1]) T[-5,5];
  a[2,2] ~ normal(aMu[2], aSigma[2]) T[-5,5];
  a[2,3] ~ normal(aMu[3], aSigma[3]) T[-5,5];
  a[2,4] ~ normal(aMu[4], aSigma[4]) T[-5,5];
  a[2,5] ~ normal(aMu[5], aSigma[5]) T[-5,5];

  a[3,1] ~ normal(aMu[1], aSigma[1]) T[-5,5];
  a[3,2] ~ normal(aMu[2], aSigma[2]) T[-5,5];
  a[3,3] ~ normal(aMu[3], aSigma[3]) T[-5,5];
  a[3,4] ~ normal(aMu[4], aSigma[4]) T[-5,5];
  a[3,5] ~ normal(aMu[5], aSigma[5]) T[-5,5];

  a[4,1] ~ normal(aMu[1], aSigma[1]) T[-5,5];
  a[4,2] ~ normal(aMu[2], aSigma[2]) T[-5,5];
  a[4,3] ~ normal(aMu[3], aSigma[3]) T[-5,5];
  a[4,4] ~ normal(aMu[4], aSigma[4]) T[-5,5];
  a[4,5] ~ normal(aMu[5], aSigma[5]) T[-5,5];

  a[5,1] ~ normal(aMu[1], aSigma[1]) T[-5,5];
  a[5,2] ~ normal(aMu[2], aSigma[2]) T[-5,5];
  a[5,3] ~ normal(aMu[3], aSigma[3]) T[-5,5];
  a[5,4] ~ normal(aMu[4], aSigma[4]) T[-5,5];
  a[5,5] ~ normal(aMu[5], aSigma[5]) T[-5,5];

  a[6,1] ~ normal(aMu[1], aSigma[1]) T[-5,5];
  a[6,2] ~ normal(aMu[2], aSigma[2]) T[-5,5];
  a[6,3] ~ normal(aMu[3], aSigma[3]) T[-5,5];
  a[6,4] ~ normal(aMu[4], aSigma[4]) T[-5,5];
  a[6,5] ~ normal(aMu[5], aSigma[5]) T[-5,5];

  b[1,1] ~ normal(bMu[1], bSigma[1]) T[0,10];
  b[1,2] ~ normal(bMu[2], bSigma[2]) T[0,10];
  b[1,3] ~ normal(bMu[3], bSigma[3]) T[0,10];
  b[1,4] ~ normal(bMu[4], bSigma[4]) T[0,10];
  b[1,5] ~ normal(bMu[5], bSigma[5]) T[0,10];

  b[2,1] ~ normal(bMu[1], bSigma[1]) T[0,10];
  b[2,2] ~ normal(bMu[2], bSigma[2]) T[0,10];
  b[2,3] ~ normal(bMu[1], bSigma[1]) T[0,10];
  b[2,4] ~ normal(bMu[4], bSigma[4]) T[0,10];
  b[2,5] ~ normal(bMu[5], bSigma[5]) T[0,10];  

  b[3,1] ~ normal(bMu[1], bSigma[1]) T[0,10];
  b[3,2] ~ normal(bMu[2], bSigma[2]) T[0,10];
  b[3,3] ~ normal(bMu[1], bSigma[1]) T[0,10];
  b[3,4] ~ normal(bMu[5], bSigma[5]) T[0,10];
  b[3,5] ~ normal(bMu[5], bSigma[5]) T[0,10];

  b[4,1] ~ normal(bMu[1], bSigma[1]) T[0,10];
  b[4,2] ~ normal(bMu[2], bSigma[2]) T[0,10];
  b[4,3] ~ normal(bMu[2], bSigma[2]) T[0,10];
  b[4,4] ~ normal(bMu[4], bSigma[4]) T[0,10];
  b[4,5] ~ normal(bMu[5], bSigma[5]) T[0,10];

  b[5,1] ~ normal(bMu[1], bSigma[1]) T[0,10];
  b[5,2] ~ normal(bMu[2], bSigma[2]) T[0,10];
  b[5,3] ~ normal(bMu[2], bSigma[2]) T[0,10];
  b[5,4] ~ normal(bMu[5], bSigma[5]) T[0,10];
  b[5,5] ~ normal(bMu[5], bSigma[5]) T[0,10];

  b[5,1] ~ normal(bMu[1], bSigma[1]) T[0,10];
  b[5,2] ~ normal(bMu[2], bSigma[2]) T[0,10];
  b[5,3] ~ normal(bMu[3], bSigma[3]) T[0,10];
  b[5,4] ~ normal(bMu[5], bSigma[5]) T[0,10];
  b[5,5] ~ normal(bMu[5], bSigma[5]) T[0,10];

  heavy[1] ~ normal(heavyMu, heavySigma) T[0,1.0];
  heavy[2] ~ normal(heavyMu, heavySigma) T[0,1.0];
  heavy[3] ~ normal(heavyMu, heavySigma) T[0,1.0];
  heavy[4] ~ normal(heavyMu, heavySigma) T[0,1.0];
  heavy[5] ~ normal(heavyMu, heavySigma) T[0,1.0];
  heavy[6] ~ normal(heavyMu, heavySigma) T[0,1.0];

  for (i in 1:4) y ~ bernoulli(mu[i]);
}
