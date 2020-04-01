##' Relative mean squared error averaged acorss simulations
##'
##' @details Relative average mean square error is
##' \deqn{\sum_{i = 1}^{N}{(obs - pred)^2} / N * (obs + 1)^2}
##' We add 1 to the observed vector to avoid dividing by 0.
##' @title Average relative mean squared error
##' @param obs observed vector T X 1
##' @param pred matrix of predicted observations. Each column is a
##' simulation. T X N where N is the number of simulations.
##' @return error T X 1. Each entry is the error averaged across the
##' simulations
##' @author Sangeeta Bhatia
##' @export
rel_mse <- function(obs, pred) {
    nsims <- ncol(pred)
    res_sq <- rowSums((obs - pred) ^ 2)
    avg_res_sq <- res_sq / (nsims * (obs^2 + 1))
    avg_res_sq
}

##' Residual averaged acorss simulations
##'
##' @details
##' \deqn{\sum_{i = 1}^{N}{obs - pred} / N }
##' @title Average Residual
##' @param obs observed vector T X 1
##' @param pred matrix of predicted observations. Each column is a
##' simulation. T X N where N is the number of simulations.
##' @return error T X 1. Each entry is the error averaged across the
##' simulations.
##' @author Sangeeta Bhatia
##' @export
avg_residual <- function(obs, pred) {

    nsims <- ncol(pred)
    avg_res <- rowSums(obs - pred) / nsims
    avg_res
}
##' Heaviside function
##'
##' @details Heaviside function H(x) is defined to be 1 if x is positive
##' and 0 otherwise. If x is 0, the function returns 0.5.
##'
##' @title Heaviside function
##' @param x number
##' @return 1 if x is positive, 0 if it is negative and 0.5 otherwise.
##' @author Sangeeta Bhatia
##' @keywords internal
heaviside <- function(x) {
    if (is.na(x)) NA
    else if (x > 0) 1
    else if (x < 0) 0
    else 0.5
}

##' Median absolute deviation about the median
##' @details \deqn{median(|pred - median(pred)|)}
##' @title MADM
##' @param pred T X N Matrix of predictions. Each column is
##' a simulation.
##' @return vector of length T.
##' @details Median absolute deviation about the median is a measure of
##' how clustered the forecasts are. A value of 0 indicates that all
##' the predicted values are the same, thus highly clustered. Large
##' values indicate more diffuse predictions.
##' @references https://bit.ly/2vPO0I9
##' @seealso [rel_madm()]
##' @export
abs_madm <- function(pred) {
    pred_median <- apply(pred, 1, stats::median)
    dvtn <- abs(pred - pred_median)
    dvtn_median <- apply(dvtn, 1, stats::median)
    dvtn_median
}


##' Relative sharpness: median absolute deviation about the median
##' @details \deqn{median(|(pred - median(pred))/pred|)}
##' @title Relative sharpness
##' @param pred T X N Matrix of predictions. Each column is
##' a simulation.
##' @return vector of length T.
##' @references https://bit.ly/2vPO0I9
##' @seealso [abs_madm()]
##' @export
rel_madm <- function(pred) {
    pred <- pred + 1 ## in case there are 0s.
    pred_median <- apply(pred, 1, stats::median)
    rel_dvtn <- abs(pred - pred_median)
    rel_dvtn_median <- apply(rel_dvtn, 1, stats::median) / pred_median
    rel_dvtn_median
}

##' Relative mean absolute deviation about the median
##' @details \deqn{median(|(pred - median(pred))/pred|)}
##' @title Relative sharpness
##' @param pred T X N Matrix of predictions. Each column is
##' a simulation.
##' @return vector of length T.
##' @references https://bit.ly/2vPO0I9
##' @export
rel_mean_dvtn <- function(pred) {
    pred <- pred + 1 ## in case there are 0s.
    pred_median <- apply(pred, 1, stats::median)
    rel_dvtn <- abs(pred - pred_median)
    rel_dvtn_mean <- apply(rel_dvtn, 1, mean) / pred_median
    rel_dvtn_mean
}


##' Bias in probabilistic forecasts
##' @title Bias
##' @details Bias is measured as
##' \deqn{2 * mean((heaviside(obs - pred)) - 0.5)}
##' where heaviside returns 1 if the arg is positive, 0 if this negative
##' and 0.5 if it is 0. The average is taken over all simulations.
##' @title Bias
##' @param obs observed vector T X 1
##' @param pred Simulated predictions T X N. Each column is a simulation.
##' @return vector of length T.
##' @author Sangeeta Bhatia
##' @references https://doi.org/10.1371/journal.pcbi.1006785
##' @export
bias <- function(obs, pred) {
    res <- pred - obs
    hvals <- apply(res, c(1, 2), heaviside)
    hvals <- apply(hvals, 1, mean, na.rm = TRUE)
    out <- 2 * (hvals - 0.5)
    out
}


##' Relative mean absolute error
##' @details Relative mean absolute error is defined as
##' \deqn{\sum_{i = 1}^{N}{|obs - pred|} / N * |obs + 1|}
##'
##' @title Relative mean absolute error
##' @param obs T X 1 vector of observations.
##' @param pred T X N matrix of predictions where each column is a simulation.
##' @return T X 1 vector of mean absolute error normalised by the observed value.
##' @author Sangeeta Bhatia
##' @export
rel_mae <- function(obs, pred) {
    nsims <- ncol(pred)
    res_abs <- rowSums(abs(obs - pred))
    avg_res_abs <- res_abs / (nsims * (abs(obs + 1)))
    avg_res_abs
}


##' Proportion of observations in given credible interval
##'
##' @details Proportion of observed values that fall within a
##' given interval
##' @title Proportion of observations in given credible interval
##' @param obs vector of observed values
##' @param min vector of the lower end of the interval. Either
##' length 1 vector or the same length as the that of obs.
##' @param max vector of the upper end of the interval. Either length
##' 1 or the same length as that of the obs vector.
##' @return proportion of values in obs vector that are greater than
##' or equal to min and less than or equal to max.
##' @author Sangeeta Bhatia
##' @export
prop_in_ci <- function(obs, min, max) {

    n <- length(obs)
    len_min <- length(min)
    len_max <- length(max)
    if (len_min != 1 & len_min != n)
        stop("Length of min vector should be 1 or same as obs.")

    if (len_max != 1 & len_max != n)
        stop("Length of max vector should be 1 or same as obs.")

    in_ci <- sum(min <= obs & obs <= max)
    in_ci / n
}


## https://tinyurl.com/y99x2jxq
log_accuracy_ratio <- function(obs, pred) {
    nsims <- ncol(pred)
    avg_pred <- apply(pred, 1, mean)
    log_accuracy <- log(avg_pred / (obs + 1))
    log_accuracy

}
