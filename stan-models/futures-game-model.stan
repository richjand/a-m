data {
  int<lower=0> J;         // number of observations
  int<lower=0> K;         // number of columns
  real y[J];              // final score difference
  matrix[J,K] x;            // predictors
}
parameters {
  real alpha;             // intercept, in this case the mean home field advantage
  vector[K] beta;         // coefficients
  real<lower = 0> sigma;   // residual sd
}
model {
  sigma ~ normal(14,7);   // BDA3 had this around 14 points but this is wide enough to learn
  beta ~ normal(0,1);     // coefficient priors
  alpha ~ normal(3,1.5);    // prior on intercept should be close to standard 3 point vegas line
  
  y ~ normal(alpha + x * beta, sigma);
}
generated quantities{
  vector[J] y_rep;
  vector[J] home_win;
  for (i in 1:J){
    y_rep[i] = normal_rng(alpha + x[i,] * beta, sigma);
    home_win[i] = y_rep[i] > 0;
  }
}
