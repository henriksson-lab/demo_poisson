input <- list(
  
  
  shrinkage_random_seed1=1,
  shrinkage_num_samples1=100,
  shrinkage_lambda1=1,
  
  shrinkage_random_seed2=1,
  shrinkage_num_samples2=100,
  shrinkage_lambda2=1,
  
  
  
  
  poisson_random_seed=1,
  poisson_num_points=1,
  poisson_lambda=1,
  
  sampling_random_seed=1,
  sampling_num_points=10,
  sampling_num_samples=20
  
)

reactive <- function(f) function() f

################################################################################



