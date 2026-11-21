test_that("RemoveSeasonal 去季节并还原", {
  t <- rep(1:3, each = 2)
  y <- c(10, 12, 20, 22, 30, 32)
  r <- RemoveSeasonal(y, t)
  expect_equal(r$mu, c(11, 21, 31))
  expect_equal(r$anomaly, c(-1, 1, -1, 1, -1, 1))
})

test_that("RemoveSeasonal 忽略 NA", {
  r <- RemoveSeasonal(c(1, NA, 3, 5), c(1, 1, 2, 2))
  expect_equal(r$mu, c(1, 4))
  expect_true(is.na(r$anomaly[2]))
})

test_that("LS_MultiBreaks 两段同斜率异截距", {
  X <- seq_len(10)
  Y <- X + c(rep(1, 5), rep(3, 5))
  r <- LS_MultiBreaks(Y, X, c(5, 10))
  expect_equal(as.vector(r$coef), c(mean(X) + 1, 1, 2))
  expect_equal(as.vector(r$fitted), Y)
  expect_equal(r$SSE, 0)
})

test_that("LS_OneBreak 单断点完美拟合", {
  X <- seq_len(10)
  Y <- X + c(rep(0, 4), rep(5, 6))
  r <- LS_OneBreak(Y, X, 4)
  expect_equal(as.vector(r$coef), c(0, 1, 5))
  expect_equal(as.vector(r$fitted), Y)
})

test_that("lm_solve 与 lm 一致", {
  X <- cbind(1, 1:4)
  y <- 2 * X[, 2] + 1
  r <- lm_solve(X, y)
  m <- lm(y ~ X - 1)
  expect_equal(as.vector(r$coef), unname(coef(m)))
  expect_equal(r$SSE, sum(resid(m)^2))
})

test_that("lm_solve only.SSE 只返回 SSE", {
  r <- lm_solve(cbind(1, 1:4), 1:4, only.SSE = TRUE)
  expect_named(r, "SSE")
})
