context("test-assess-test.R")

test_that("relative mean sqaure error works", {
    obs <- 1:4
    pred <- matrix(c(1, 10, 2, 4,
                     7, 3, 5, 6,
                     8, 9, 1, 10),
                   nrow = 4,
                   byrow = FALSE)
    correct <- c(7.08, 4.22, 0.19, 0.53)
    out <- round(rel_mse(obs, pred), 2)
    expect_true(all(correct == out))

})

test_that("Mean residual works", {
    obs <- 1:4
    pred <- matrix(c(1, 10, 2, 4,
                     7, 3,5, 6,
                     8, 9, 1, 10),
                   nrow = 4,
                   byrow = FALSE)
    correct <- c(-4.33, -5.33, 0.33, -2.67)
    out <- round(avg_residual(obs, pred), 2)
    expect_true(all(correct == out))

})


test_that("shaprness works", {
    pred <- matrix(c(6, 3, 4, 2, 1,
                     4, 6, 10, 8, 3,
                     2, 9, 9, 1, 3,
                     2, 8, 2, 1, 3),
                   nrow = 4,
                   ncol = 5,
                   byrow = TRUE)
    correct <- c(1, 2, 2, 1)
    out <- sharpness(pred)
    expect_equal(correct, out)
})

test_that("bias works", {
    pred <- matrix(c(6, 3, 4, 2, 1,
                     4, 6, 10, 8, 3,
                     2, 9, 9, 1, 3,
                     2, 8, 2, 1, 3),
                   nrow = 4,
                   ncol = 5,
                   byrow = TRUE)
    obs <- c(1, 7, 9, 5)
    correct <- c(0.8, -0.2, -0.6, -0.6)
    out <- bias(obs, pred)
    expect_equal(correct, out)
})

test_that("relative mean absolute error works", {
    obs <- 1:4
    pred <- matrix(c(1, 10, 2, 4,
                     7, 3, 5, 6,
                     8, 9, 1, 10),
                   nrow = 4,
                   byrow = FALSE)
    correct <- c(2.17, 1.78, 0.42, 0.53)
    out <- round(rel_mae(obs, pred), 2)
    expect_true(all(correct == out))

})

test_that("relative sharpness works", {
  pred <- c(1:9, 2, 11:16, 19, 20)
  pred <- matrix(pred, nrow = 2, ncol = 9, byrow = TRUE)
  correct <- c(0.33, 0.15)
  out <- round(rel_sharpness(pred), 2)
  expect_true(all(correct == out))
})
