<img src="images/lab_logo.jpg" alt="lab_logo" width="200"/>

# Post-Prediction Inference (PPI) Package

## Introduction

The Post-Prediction Inference (PPI) package implements a range of methods for estimation using a mixture of human-labeled and machine-labeled data.

## Problem Statement

The expense of human-labeling unstructured data, such as text, frequently prohibits large-scale analytic projects. Recent advances in machine learning and AI, especially the advent of large language models, enable the low-cost prediction of these labels. 

For example, a researcher may be interested in studying the relationship between geographic region and hiring requirements in a large corpus of job postings. Are BA degrees more likely to be required for the same position in some locations? The researcher faces several challenges in conducting an analysis to answer this question:

1. **Job Title Ambiguity**: Ensuring that jobs with similar titles actually describe similar duties. Two jobs labeled "data analyst," for instance, may have vastly different requirements. The analyst wants to compare the degree requirements in postings that have similar duties.
2. **Varied Expressions**: A BA requirement may be expressed in many different ways in a job posting.

Both of these issues can be addressed using a large language model (LLM). For the first issue, an LLM can label whether a pair of postings has similar job requirements (operationalizing "similar" will require some thought and prompt-engineering, of course). For the second, an LLM can label each job posting as requiring an undergraduate degree or not.

## Solution Overview

With predicted labels in hand, we would like to conduct our analysis using a regression. However, fitting a regression with the predicted labels could lead to heavily biased estimates of the coefficients. Instead, a method must be employed to account for potential inaccuracies in the predicted labels.

The Post-Prediction Inference package presents a suite of functions to enable such analyses. In each case, the researcher provides a subsample of the data that has been human-labeled. This subsample is used to assess the accuracy of the predicted labels and provide unbiased estimation of statistical models (e.g., simple mean estimation or a regression model) combining data that have been human-labeled with (usually much more) data that have been machine-labeled. The methods in this package have asymptotically correct coverage, regardless of the prediction method.

## Features

- Unbiased point estimation using a mix of human and machine-labeled data.
- Methods to account for inaccuracies in predicted labels.
- Asymptotically correct interval coverage for various statistical models.

## Example

Here is a simple example demonstrating how to use the PPI package:

```R
# Example code snippet
library(ppi)

# Load your human-labeled and machine-labeled data
human_labeled_data_true_labels <- ...
human_labeled_data_predicted_labels <- ...
machine_labeled_data <- ...



# Estimate the mean of the binary outcome
results <- ppi_mean(human_labeled_data_true_labels, 
                    human_labeled_data_predicted_labels,
                    machine_labeled_data)


# Display results
print(results)
```
