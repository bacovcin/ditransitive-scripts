data {
  int<lower=0> N;
  int<lower=0,upper=1> y[N];
  real x[N];
  int<lower=1,upper=4> cond[N];
  real aMu[4];
  real<lower=0> bMu[4];
  real<lower=0> aSigma[4];
  real<lower=0> bSigma[4];
}
parameters {
  real<lower=-5,upper=5> a[4];
  real<lower=0,upper=10> b[4];
}

transformed parameters {
  real<lower=0.001,upper=0.999> mu[N];
  for (n in 1:N) {
    if (cond[n] == 1) {
      mu[n] <- (1 / (1 + exp(-(a[1] + b[1] * x[n]))));
    }
    else if (cond[n] == 2) {
      mu[n] <- (1 / (1 + exp(-(a[2] + b[2] * x[n]))));
    }
    else if (cond[n] == 3) {
      mu[n] <- (1 / (1 + exp(-(a[3] + b[3] * x[n]))));
    }
    else if (cond[n] == 4) {
      mu[n] <- (1 / (1 + exp(-(a[4] + b[4] * x[n]))));
    }
    if (mu[n] > .999) mu[n] <- .999;
    else if (mu[n] < .001) mu[n] <- .001;
  }
}

model {
  a[1] ~ normal(aMu[1], aSigma[1]) T[-5,5];
  a[2] ~ normal(aMu[2], aSigma[2]) T[-5,5];
  a[3] ~ normal(aMu[3], aSigma[3]) T[-5,5];
  a[4] ~ normal(aMu[4], aSigma[4]) T[-5,5];

  b[1] ~ normal(bMu[1], bSigma[1]) T[0,10];
  b[2] ~ normal(bMu[2], bSigma[2]) T[0,10];
  b[3] ~ normal(bMu[3], bSigma[3]) T[0,10];
  b[4] ~ normal(bMu[4], bSigma[4]) T[0,10];

  y ~ bernoulli(mu);
}
