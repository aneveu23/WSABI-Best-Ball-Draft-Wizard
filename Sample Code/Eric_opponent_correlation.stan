// Stan model to estimate correlations between QB, RB, WR1, WR2, and TE fantasy points

data {
  int<lower=1> N;          // Number of observations (e.g., games or players)
  int<lower=1> K;          // Number of variables (e.g., QB, RB, WR1, WR2, TE)
  vector[K] y[N];
}

parameters {
  vector[K] mu;            // Means of fantasy points for QB, RB, WR1, WR2, and TE
  vector<lower=0>[K] sigma; // Standard deviations for QB, RB, WR1, WR2, and TE
  corr_matrix[K] Rho;      // Correlation matrix for the variables
}

transformed parameters {
  cov_matrix[K] Sigma;     // Covariance matrix

  // Construct the covariance matrix from standard deviations and correlations
  Sigma = quad_form_diag(Rho, sigma);
}

model {
  // Priors for means
  mu[1] ~ normal(12.6, 5);   // Prior for QB mean fantasy points
  mu[2] ~ normal(12, 5);   // Prior for RB1 mean fantasy points
  mu[3] ~ normal(5.5, 5);   // Prior for RB2 mean fantasy points
  mu[4] ~ normal(13.6, 5);   // Prior for WR1 mean fantasy points
  mu[5] ~ normal(8.2, 5);   // Prior for WR2 mean fantasy points (similar to WR1)
  mu[6] ~ normal(7.5, 5);    // Prior for TE mean fantasy points
  
  // Priors for opponent means
  mu[7] ~ normal(12.6, 5);   // Prior for QB mean fantasy points
  mu[8] ~ normal(12, 5);   // Prior for RB1 mean fantasy points
  mu[9] ~ normal(5.5, 5);   // Prior for RB2 mean fantasy points
  mu[10] ~ normal(13.6, 5);   // Prior for WR1 mean fantasy points
  mu[11] ~ normal(8.2, 5);   // Prior for WR2 mean fantasy points (similar to WR1)
  mu[12] ~ normal(7.5, 5);    // Prior for TE mean fantasy points
  
  // Priors for standard deviations
  sigma[1] ~ normal(9.6, 10);   // Prior for QB sigma
  sigma[2] ~ normal(8.9, 10);   // Prior for RB1 sigma
  sigma[3] ~ normal(6.1, 10);   // Prior for RB2 sigma
  sigma[4] ~ normal(9.7, 10);   // Prior for WR1 sigma
  sigma[5] ~ normal(7.6, 10);   // Prior for WR2 sigma
  sigma[6] ~ normal(6.8, 10);   // Prior for TE sigma
  
  // Priors for opponent standard deviations
  sigma[7] ~ normal(9.6, 10);   // Prior for QB sigma
  sigma[8] ~ normal(8.9, 10);   // Prior for RB1 sigma
  sigma[9] ~ normal(6.1, 10);   // Prior for RB2 sigma
  sigma[10] ~ normal(9.7, 10);   // Prior for WR1 sigma
  sigma[11] ~ normal(7.6, 10);   // Prior for WR2 sigma
  sigma[12] ~ normal(6.8, 10);   // Prior for TE sigma
  
  // Priors for correlations
  Rho ~ lkj_corr(2);       // LKJ prior for correlation matrix
  //print(Rho);
  
  // Likelihood: Multivariate normal distribution for observed data
  y ~ multi_normal(mu, Sigma);
}

generated quantities {
  corr_matrix[K] corr;     // Generated correlation matrix for output
  corr = Rho;              // Assign the correlation matrix for output
}


