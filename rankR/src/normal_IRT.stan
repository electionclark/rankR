//
// This Stan program defines a simple model, with a binary IRT 2pl model
data {
  int<lower=1> J;              // number of students
  int<lower=1> K;              // number of questions
  int<lower=1> N;              // number of observations
  int<lower=1,upper=J> jj[N];  // student for observation n
  int<lower=1,upper=K> kk[N];  // question for observation n
  int<lower=0,upper=1> y[N];   // correctness for observation n
}

parameters {
  real mu_beta;                // mean question difficulty
  vector[J] alpha;             // ability for j - mean
  vector[K] beta;              // difficulty for k
  vector[K] gamma;             // discrimination of k- unconstrained
  real<lower=0> sigma_beta;    // scale of difficulties
  real<lower=0> sigma_gamma;   // scale of log discrimination
}

model {
  alpha ~ std_normal();
  beta ~ normal(0, sigma_beta);
  gamma ~ normal(0, sigma_gamma);
  gamma[1] ~ normal(1, .001); //constraint
  gamma[2] ~ normal(-1, .001);
  mu_beta ~ cauchy(0, 5);
  sigma_beta ~ cauchy(0, 5);
  sigma_gamma ~ cauchy(0, 5);
  y ~ bernoulli_logit(gamma[kk] .* (alpha[jj] - (beta[kk] + mu_beta)));
}


generated quantities {
  // compute actual thresholds
  vector[K] idealpoint;
  for (q in 1:K){
idealpoint[q] = ((alpha[q]/ beta[q]) *-1) ;
}
}




