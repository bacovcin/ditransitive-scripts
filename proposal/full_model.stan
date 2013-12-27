data {
  int<lower=0> N;
  int<lower=0,upper=1> y[N];
  real x[N];
  int<lower=1,upper=5> cond[N];
  int<lower=-1,upper=1> iosum[N];
  int<lower=0,upper=1> iopro[N];
  int<lower=0,upper=1> donoun[N];
  int<lower=0,upper=1> iodo[N];

  real aMuToAD;
  real aMuToDA;
  real aMuAccAct;
  real aMuAccPas;
  real aMuToPas;
  real<lower=0> aSigmaToAD;
  real<lower=0> aSigmaToDA;
  real<lower=0> aSigmaAccAct;
  real<lower=0> aSigmaAccPas;
  real<lower=0> aSigmaToPas;

  real<lower=0> bMuToAD;
  real<lower=0> bMuToDA;
  real<lower=0> bMuAccAct;
  real<lower=0> bMuAccPas;
  real<lower=0> bMuToPas;
  real<lower=0> bSigmaToAD;
  real<lower=0> bSigmaToDA;
  real<lower=0> bSigmaAccAct;
  real<lower=0> bSigmaAccPas;
  real<lower=0> bSigmaToPas;

  real gMuToAD;
  real gMuToDA;
  real gMuAccAct;
  real gMuAccPas;
  real gMuToPas;
  real<lower=0> gSigmaToAD;
  real<lower=0> gSigmaToDA;
  real<lower=0> gSigmaAccAct;
  real<lower=0> gSigmaAccPas;
  real<lower=0> gSigmaToPas;

  real dMuToAD;
  real dMuToDA;
  real dMuAccAct;
  real dMuAccPas;
  real dMuToPas;
  real<lower=0> dSigmaToAD;
  real<lower=0> dSigmaToDA;
  real<lower=0> dSigmaAccAct;
  real<lower=0> dSigmaAccPas;
  real<lower=0> dSigmaToPas;

  real<lower=0,upper=1> orderMu;
  real<lower=0> orderSigma;
  
  real<lower=0,upper=1> heavyMu;
  real<lower=0> heavySigma;
}
parameters {
  real<lower=-5,upper=5> aToAD;
  real<lower=-5,upper=5> aToDA;
  real<lower=-5,upper=5> aAccAct;
  real<lower=-5,upper=5> aAccPas;
  real<lower=-5,upper=5> aToPas;

  real<lower=0,upper=10> bToAD;
  real<lower=0,upper=10> bToDA;
  real<lower=0,upper=10> bAccAct;
  real<lower=0,upper=10> bAccPas;
  real<lower=0,upper=10> bToPas;
  
  real<lower=-5,upper=5> gToAD;
  real<lower=-5,upper=5> gToDA;
  real<lower=-5,upper=5> gAccAct;
  real<lower=-5,upper=5> gAccPas;
  real<lower=-5,upper=5> gToPas;

  real<lower=-5,upper=5> dToAD;
  real<lower=-5,upper=5> dToDA;
  real<lower=-5,upper=5> dAccAct;
  real<lower=-5,upper=5> dAccPas;
  real<lower=-5,upper=5> dToPas;

  real<lower=0,upper=1> order;

  real<lower=0,upper=1> heavy;
}

transformed parameters {
  real<lower=0.001,upper=0.999> mu[N];
  for (n in 1:N) {
    if (cond[n] == 1) {
      mu[n] <- ((1 - ((1 - order) * iodo[n]))/ (1 + exp(-(aToAD + bToAD * x[n] + gToAD * iosum[n] + dToAD * iosum[n] * x[n]))));
    }
    if (cond[n] == 2) {
      mu[n] <- (((1 - ((1 - order) * iopro[n])) * donoun[n])/ (1 + exp(-(aToDA + bToDA * x[n] + gToDA * iosum[n] + dToDA * iosum[n] * x[n])))) * (1 - (heavy / (1 + exp(-(aAccAct + bAccAct * x[n] + gAccAct * iosum[n] + dAccAct * iosum[n] * x[n])))));
    }
    if (cond[n] == 3) {
      mu[n] <- (donoun[n] / (1 + exp(-(aAccPas + bAccPas * x[n] + gAccPas * iosum[n] + dAccPas * iosum[n] * x[n]))));
    }
    if (cond[n] == 4) {
      mu[n] <- ((1 - ((1 - order) * iopro[n]))/ (1 + exp(-(aToPas + bToPas * x[n] + gToPas * iosum[n] + dToPas * iosum[n] * x[n]))));
    }
    if (mu[n] > .999) mu[n] <- .999;
    else if (mu[n] < .001) mu[n] <- .001;
  }
}

model {
  aToAD ~ normal(aMuToAD, aSigmaToAD) T[-5,5];
  aToDA ~ normal(aMuToDA, aSigmaToDA) T[-5,5];
  aAccAct ~ normal(aMuAccAct, aSigmaAccAct) T[-5,5];
  aAccPas ~ normal(aMuAccPas, aSigmaAccPas) T[-5,5];
  aToPas ~ normal(aMuToPas, aSigmaToPas) T[-5,5];

  bToAD ~ normal(bMuToAD, bSigmaToAD) T[-5,5];
  bToDA ~ normal(bMuToDA, bSigmaToDA) T[-5,5];
  bAccAct ~ normal(bMuAccAct, bSigmaAccAct) T[-5,5];
  bAccPas ~ normal(bMuAccPas, bSigmaAccPas) T[-5,5];
  bToPas ~ normal(bMuToPas, bSigmaToPas) T[-5,5];

  gToAD ~ normal(gMuToAD, gSigmaToAD) T[-5,5];
  gToDA ~ normal(gMuToDA, gSigmaToDA) T[-5,5];
  gAccAct ~ normal(gMuAccAct, gSigmaAccAct) T[-5,5];
  gAccPas ~ normal(gMuAccPas, gSigmaAccPas) T[-5,5];
  gToPas ~ normal(gMuToPas, gSigmaToPas) T[-5,5];

  dToAD ~ normal(dMuToAD, dSigmaToAD) T[-5,5];
  dToDA ~ normal(dMuToDA, dSigmaToDA) T[-5,5];
  dAccAct ~ normal(dMuAccAct, dSigmaAccAct) T[-5,5];
  dAccPas ~ normal(dMuAccPas, dSigmaAccPas) T[-5,5];
  dToPas ~ normal(dMuToPas, dSigmaToPas) T[-5,5];

  heavy ~ normal(heavyMu, heavySigma) T[0,1.0];
 
  y ~ bernoulli(mu);
}
