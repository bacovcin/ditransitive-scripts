data {
  int<lower=0> N;
  int<lower=0,upper=1> y[N];
  real x[N];
  int<lower=1,upper=3> cond[N];
  int<lower=-1,upper=1> io[N];
  real aMu1;
  real aMu2;
  real aMu3;
  real aMu4;
  real<lower=0> bMu1;
  real<lower=0> bMu2;
  real gMu1;
  real gMu2;
  real gMu3;
  real<lower=0> aSigma1;
  real<lower=0> aSigma2;
  real<lower=0> aSigma3;
  real<lower=0> aSigma4;
  real<lower=0> bSigma1;
  real<lower=0> bSigma2;
  real<lower=0> gSigma1;
  real<lower=0> gSigma2;
  real<lower=0> gSigma3;
  real<lower=0,upper=1> heavyMu;
  real<lower=0> heavySigma;
}
parameters {
  real<lower=-5,upper=5> a1;
  real<lower=-5,upper=5> a2;
  real<lower=-5,upper=5> a3;
  real<lower=-5,upper=5> a4;
  real<lower=0,upper=10> b1;
  real<lower=0,upper=10> b2;
  real<lower=-5,upper=5> g1;
  real<lower=-5,upper=5> g2;
  real<lower=-5,upper=5> g3;
  real<lower=0,upper=1> heavy;
}

transformed parameters {
  real<lower=0.001,upper=0.999> mu[N];
  for (n in 1:N) {
    if (cond[n] == 1) {
        mu[n] <- (1 / (1 + exp(-(a1 + b1 * x[n] + g1 * io[n]))));
      }
    else if (cond[n] == 2) {
        mu[n] <- (1 / (1 + exp(-(a2 + b1 * x[n] + g2 * io[n]))))*(1-((heavy) / (1 + exp(-(a3 + b2 * x[n] + g3 * io[n])))));
      }
    else if (cond[n] == 3) {
        mu[n] <- (1 / (1 + exp(-(a4 + b2 * x[n]))));
      }
    if (mu[n] > .999) mu[n] <- .999;
    else if (mu[n] < .001) mu[n] <- .001;
  }
}

model {
  a1 ~ normal(aMu1, aSigma1) T[-5,5];
  a2 ~ normal(aMu2, aSigma2) T[-5,5];
  a3 ~ normal(aMu3, aSigma3) T[-5,5];
  a4 ~ normal(aMu4, aSigma4) T[-5,5];

  b1 ~ normal(bMu1, bSigma1) T[0,10];
  b2 ~ normal(bMu2, bSigma2) T[0,10];

  g1 ~ normal(gMu1, gSigma1) T[-5,5];
  g2 ~ normal(gMu2, gSigma2) T[-5,5];
  g3 ~ normal(gMu2, gSigma2) T[-5,5];

  heavy ~ normal(heavyMu, heavySigma) T[0,1.0];

  y ~ bernoulli(mu);
}
