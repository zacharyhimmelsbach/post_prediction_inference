calculate_optimal_weight <- function(labeled_data_Y,
                                     labeled_data_predictions,
                                     unlabeled_data_predictions,
                                     Y_pred_covariance,
                                     var_labeled_pred,
                                     var_unlabeled_pred,
                                     n_labeled,
                                     n_unlabeled) {


  # Return Optimal Weight
  optimal_weight <- Y_pred_covariance/(var_labeled_pred + ((n_labeled*var_unlabeled_pred)/n_unlabeled))
  return(optimal_weight)
}

estimate_mean <- function(labeled_data_Y,
                          labeled_data_predictions,
                          unlabeled_data_predictions,
                          optimal_weight) {

  # Calculate mean of each input
  mean_of_labels <- mean(labeled_data_Y)
  mean_of_labeled_predictions <- mean(labeled_data_predictions)
  mean_of_unlabeled_predictions <- mean(unlabeled_data_predictions)

  # Return point-estimate
  point_estimate <- mean_of_labels + optimal_weight*(mean_of_unlabeled_predictions - mean_of_labeled_predictions)
  return(point_estimate)
}

asymptotic_variance <- function(labeled_data_Y,
                                labeled_data_predictions,
                                unlabeled_data_predictions,
                                optimal_weight,
                                Y_pred_covariance,
                                var_labeled_pred,
                                var_unlabeled_pred,
                                n_labeled_data,
                                n_unlabeled_data) {


  # Calculate asymptotic variance
  variance <- (var(labeled_data_Y)/n_labeled_data) + ((optimal_weight^2)*(var_labeled_pred/n_labeled_data)) -
    (2*optimal_weight*(Y_pred_covariance/n_labeled_data)) + ((optimal_weight^2)*(var_unlabeled_pred/n_unlabeled_data))
  return(variance)
}

ppi_mean <- function(labeled_data_Y,
                     labeled_data_predictions,
                     unlabeled_data_predictions) {
  # Check inputs
  # Check arguments
  if (any(is.na(c(labeled_data_Y, labeled_data_predictions, unlabeled_data_predictions)))) {
    stop('Error: One or more inputs contains missing values')
  }

  if (!all(c(labeled_data_Y, labeled_data_predictions, unlabeled_data_predictions) %in% c(0,1))) {
    stop('Error: All inputs must be 0/1 values')
  }

  if (length(labeled_data_Y) != length(labeled_data_predictions)) {
    stop('Error: The first two arguments must be the same length (since they are the labels and predicted labels, respectively, of the labeled portion of the data)')
  }

  # Calculate inputs for sub-routines
  Y_pred_covariance <- cov(labeled_data_Y, labeled_data_predictions)
  var_labeled_pred <- var(labeled_data_predictions)
  var_unlabeled_pred <- var(unlabeled_data_predictions)

  n_labeled <- length(labeled_data_Y)
  n_unlabeled <- length(unlabeled_data_predictions)

  # Calculate optimal weight
  optimal_weight <- calculate_optimal_weight(labeled_data_Y,
                                             labeled_data_predictions,
                                             unlabeled_data_predictions)

  # Estimate Mean
  point_estimate <- estimate_mean(labeled_data_Y,
                                  labeled_data_predictions,
                                  unlabeled_data_predictions,
                                  optimal_weight)

  # Estimate standard error
  se <- sqrt(asymptotic_variance(labeled_data_Y,
                                 labeled_data_predictions,
                                 unlabeled_data_predictions,
                                 optimal_weight,
                                 Y_pred_covariance,
                                 var_labeled_pred,
                                 var_unlabeled_pred,
                                 n_labeled_data,
                                 n_unlabeled_data))

  # Calculate confidence interval
  CI_95 <- c(point_estimate - (qnorm(0.025)*se), point_estimate + (qnorm(0.025)*se))

  results <- list(estimated_mean = point_estimate,
                  labeled_mean = mean(labeled_data_Y),
                  standard_error = se,
                  confidence_interval = CI_95)

  class(results) <- 'ppi_mean_estimate'

  return(results)
}

print.ppi_mean_estimate <- function(x) {
  cat('Post-Prediction Inference Results: \n')
  cat('Mean of Labeled Data: ', x$labeled_mean, '\n')
  cat('Estimated Mean (all data): ', x$estimated_mean, '\n')
  cat('95% Confidence Interval: [', x$confidence_interval[1], ', ', x$confidence_interval[2], ']', '\n')
}
