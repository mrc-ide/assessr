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
rel_mse <- function(obs, pred) {
    nsims <- ncol(pred)
    res_sq <- rowSums((obs - pred) ^ 2)
    avg_res_sq <- res_sq / (nsims * ((obs + 1) ^ 2))
    avg_res_sq
}
##' Resdiual averaged acorss simulations
##'
##'
##' @title Average Residual
##' @param obs observed vector T X 1
##' @param pred matrix of predicted observations. Each column is a
##' simulation. T X N where N is the number of simulations.
##' @return error T X 1. Each entry is the error averaged across the
##' simulations.
##' @author Sangeeta Bhatia
avg_residual <- function(obs, pred) {
    nsims <- ncol(pred)
    avg_res <- rowSums(obs - pred) / nsims
    avg_res
}

heaviside <- function(x) {
    if (is.na(x)) NA
    else if (x > 0) 1
    else if (x < 0) 0
    else 0.5
}

##' Median absolute deviation about the median
##' @details
sharpness <- function(pred) {
    pred_median <- apply(pred, 1, median)
    dvtn <- abs(pred - pred_median)
    dvtn_median <- apply(dvtn, 1, median)
    dvtn_median
}
##' Bias in probabilistic forecasts
##'
##' @details Bias is measured as
##' \deqn{2 * mean((heaviside(obs - pred)) - 0.5)}
##' where heaviside returns 1 if the arg is positive, 0 if this negative
##' and 0.5 if it is 0. The average is taken over all simulations.
##' @title
##' @param obs
##' @param pred
##' @return
##' @author Sangeeta Bhatia
bias <- function(obs, pred) {
    res <- pred - obs
    hvals <- apply(res, c(1, 2), heaviside)
    hvals <- apply(hvals, 1, mean, na.rm = TRUE)
    out <- 2 * (hvals - 0.5)
    out
}
