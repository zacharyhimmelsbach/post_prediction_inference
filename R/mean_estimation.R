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
  variance <- (stats::var(labeled_data_Y)/n_labeled_data) + ((optimal_weight^2)*(var_labeled_pred/n_labeled_data)) -
    (2*optimal_weight*(Y_pred_covariance/n_labeled_data)) + ((optimal_weight^2)*(var_unlabeled_pred/n_unlabeled_data))
  return(variance)
}

#' Estimate proportion of binary outcome in full data set
#'
#' This function takes a vector of observed binary outcomes (for a labeled dataset), a vector of predicted outcomes
#' (for the same labeled dataset), and a vector of predicted outcomes for an unlabeled dataset. It returns an estimate
#' of the proportion of positive binary outcomes in the full dataset (labeled and unlabeled combined). The method implemented here
#' follows Miao, Miao, Wu, Zhao, and Lu (2024), calculating an optimal weight for adjusting the sample mean in the
#' labeled data by the difference between the mean predicted labels in the labeled and unlabeled datasets.
#' The output also provides 95% confidence intervals based on the asymptotic variance of the estimator.
#'
#' @param labeled_data_Y a vector of binary outcomes (treated as ground-truth labels)
#' @param labeled_data_predictions a vector of predicted binary outcomes (from e.g. an LLM) for the labeled data
#' @param unlabeled_data_predictions a vector of predicted binary outcomes for corresponding unlabeled data
#' @export
#' @return a list containing the point estimate, standard error, and confidence interval. The mean of the labeled data alone is also returned.
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
  Y_pred_covariance <- stats::cov(labeled_data_Y, labeled_data_predictions)
  var_labeled_pred <- stats::var(labeled_data_predictions)
  var_unlabeled_pred <- stats::var(unlabeled_data_predictions)

  n_labeled <- length(labeled_data_Y)
  n_unlabeled <- length(unlabeled_data_predictions)

  # Calculate optimal weight
  optimal_weight <- calculate_optimal_weight(labeled_data_Y,
                                             labeled_data_predictions,
                                             unlabeled_data_predictions,
                                             Y_pred_covariance,
                                             var_labeled_pred,
                                             var_unlabeled_pred,
                                             n_labeled,
                                             n_unlabeled)

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
                                 n_labeled,
                                 n_unlabeled))

  # Calculate confidence interval
  CI_95 <- c(point_estimate - (stats::qnorm(0.025)*se), point_estimate + (stats::qnorm(0.025)*se))

  results <- list(estimated_mean = point_estimate,
                  labeled_mean = mean(labeled_data_Y),
                  standard_error = se,
                  confidence_interval = CI_95)

  class(results) <- 'ppi_mean_estimate'

  return(results)
}

#' Print method for ppi_mean_estimate class
#'
#' @param x An object of class \code{ppi_mean_estimate}.
#' @param ... Additional arguments (currently unused).
#' @exportS3Method
print.ppi_mean_estimate <- function(x, ...) {
  cat('Post-Prediction Inference Results: \n')
  cat('Mean of Labeled Data: ', x$labeled_mean, '\n')
  cat('Estimated Mean (all data): ', x$estimated_mean, '\n')
  cat('95% Confidence Interval: [', x$confidence_interval[1], ', ', x$confidence_interval[2], ']', '\n')
}
