generate_data <- function(n_labeled,
                          n_unlabeled,
                          true_rate,
                          tpr, # true positive rate
                          fpr) {# false positive rate

  # Ground-truth labels
  y <- rbinom(n_labeled + n_unlabeled, 1, true_rate)
  # Predicted labels
  z <- rbinom(n_labeled + n_unlabeled, 1, (y*tpr) + ((1-y)*fpr))

  # Split data into labeled and unlabeled sets
  y_l <- y[1:n_labeled]
  z_l <- z[1:n_labeled]
  y_u <- y[(n_labeled+1):length(y)]
  z_u <- z[(n_labeled+1):length(z)]

  return(list(y_l=y_l, z_l=z_l, y_u=y_u, z_u=z_u))
}


# Test mean estimation
dat <- generate_data(n_labeled=200,
                     n_unlabeled=2000,
                     true_rate=.1,
                     tpr=.8,
                     fpr=.1)
estimate <- ppi_mean(dat$y_l, dat$z_l, dat$z_u)

# Check error
test_ppi_mean <- function(n_labeled,
                          n_unlabeled,
                          true_rate,
                          tpr,
                          fpr) {
  # simulate data
  dat <- generate_data(n_labeled,
                       n_unlabeled,
                       true_rate,
                       tpr,
                       fpr)
  # Estimate mean
  ppi <- ppi_mean(dat$y_l, dat$z_l, dat$z_u)
  error <- ppi$estimated_mean - true_rate
  return(error)
}
errors <- replicate(1000, test_ppi_mean(200, 2000, .1, .8, .1))
mean(errors) # SEs also look approximately correct, so coverage is good

# Test with larger unlabeled sample
errors <- replicate(1000, test_ppi_mean(400, 2, .1, .8, .1))
sd(errors)
