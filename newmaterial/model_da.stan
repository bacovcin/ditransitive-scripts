data {
  int<lower=0> N;
  int<lower=0,upper=1> y[N];
  real x[N];
  int<lower=-1,upper=1> io[N];
  real aMu1;
  real aMu2;
  real<lower=0> b1;
  real<lower=0> b2;
  real gMu1;
  real gMu2;
  real<lower=0> aSigma1;
  real<lower=0> aSigma2;
  real<lower=0> gSigma1;
  real<lower=0> gSigma2;
  real<lower=0,upper=1> heavyMu;
  real<lower=0> heavySigma;
}
parameters {
  real<lower=-5,upper=5> a1;
  real<lower=-5,upper=5> a2;
  real<lower=-5,upper=5> g1;
  real<lower=-5,upper=5> g2;
  real<lower=0,upper=1> heavy;
}

transformed parameters {
  real<lower=0.001,upper=0.999> mu[N];
  for (n in 1:N) {
    mu[n] <- (1 / (1 + exp(-(a1 + b1 * x[n] + g1 * io[n]))))*(1-((heavy) / (1 + exp(-(a2 + b2 * x[n] + g2 * io[n])))));
    if (mu[n] > .999) mu[n] <- .999;
    else if (mu[n] < .001) mu[n] <- .001;
  }
}

model {
  a1 ~ normal(aMu1, aSigma1) T[-5,5];
  a2 ~ normal(aMu2, aSigma2) T[-5,5];

  g1 ~ normal(gMu1, gSigma1) T[-5,5];
  g2 ~ normal(gMu2, gSigma2) T[-5,5];

  heavy ~ normal(heavyMu, heavySigma) T[0,1.0];

  y ~ bernoulli(mu);
}
