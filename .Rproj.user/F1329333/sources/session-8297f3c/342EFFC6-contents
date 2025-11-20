## tests/testthat/test-sparse_numeric.R

library(testthat)

## ---- Class + validity -------------------------------------------------------

test_that("validity method exists", {
  validity_method <- getValidity(getClassDef("sparse_numeric"))
  expect_false(is.null(validity_method))
})

test_that("validity accepts a correct object", {
  x <- new("sparse_numeric",
           value = c(1, 2, 3, 1),
           pos   = c(1L, 2L, 3L, 5L),
           length = 5L)
  expect_true(validObject(x))
})

test_that("validity catches bad length", {
  x <- new("sparse_numeric",
           value = c(1, 2, 3, 1),
           pos   = c(1L, 2L, 3L, 5L),
           length = 5L)
  x@length <- 2L
  expect_error(validObject(x))
})

test_that("validity catches NA positions", {
  expect_error(
    new("sparse_numeric",
        value  = c(1, 2),
        pos    = c(1L, NA_integer_),
        length = 5L),
    "NA"
  )
})

test_that("validity catches out-of-range positions", {
  expect_error(
    new("sparse_numeric",
        value  = 1,
        pos    = 10L,
        length = 5L),
    "within vector length range"
  )
})


## ---- Coercion ---------------------------------------------------------------

test_that("coercion from numeric returns sparse_numeric", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  expect_s4_class(x, "sparse_numeric")
})

test_that("coercion back and forth preserves values", {
  dense <- c(0, 5, 0, -2, 3)
  s <- as(dense, "sparse_numeric")
  back <- as(s, "numeric")
  expect_equal(back, dense)
})


## ---- Methods existence ------------------------------------------------------

test_that("show method exists", {
  expect_no_error(getMethod("show", "sparse_numeric"))
})

test_that("plot method exists for sparse_numeric pairs", {
  expect_no_error(getMethod("plot", c("sparse_numeric", "sparse_numeric")))
})

test_that("+ method exists", {
  expect_no_error(getMethod("+", c("sparse_numeric", "sparse_numeric")))
})

test_that("- method exists", {
  expect_no_error(getMethod("-", c("sparse_numeric", "sparse_numeric")))
})

test_that("* method exists", {
  expect_no_error(getMethod("*", c("sparse_numeric", "sparse_numeric")))
})

test_that("arithmetic generics have at least two arguments", {
  gens <- list(sparse_add, sparse_mult, sparse_sub, sparse_crossprod)
  for (g in gens) {
    expect_true(length(formals(g)) >= 2L)
  }
})


## ---- Arithmetic: add / sub / mult / crossprod -------------------------------

test_that("sparse_add returns sparse_numeric", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
  res <- sparse_add(x, y)
  expect_s4_class(res, "sparse_numeric")
})

test_that("sparse_add matches dense addition", {
  dense_x <- c(0, 0, 0, 1, 2)
  dense_y <- c(1, 1, 0, 0, 4)
  x <- as(dense_x, "sparse_numeric")
  y <- as(dense_y, "sparse_numeric")
  res_sparse <- as(sparse_add(x, y), "numeric")
  expect_equal(res_sparse, dense_x + dense_y)
})

test_that("sparse_add all-zero vectors keeps zeros", {
  x <- as(rep(0, 5), "sparse_numeric")
  y <- as(rep(0, 5), "sparse_numeric")
  res <- as(sparse_add(x, y), "numeric")
  expect_equal(res, rep(0, 5))
})

test_that("sparse_add errors on mismatched lengths", {
  x <- as(rep(0, 10), "sparse_numeric")
  y <- as(rep(0, 9), "sparse_numeric")
  expect_error(sparse_add(x, y))
})

test_that("sparse_sub matches dense subtraction", {
  dense_x <- c(0, 2, 0, 5)
  dense_y <- c(1, 0, 3, 1)
  x <- as(dense_x, "sparse_numeric")
  y <- as(dense_y, "sparse_numeric")
  res_sparse <- as(sparse_sub(x, y), "numeric")
  expect_equal(res_sparse, dense_x - dense_y)
})

test_that("sparse_mult matches dense elementwise product", {
  dense_x <- c(0, 2, 0, 5)
  dense_y <- c(1, 0, 3, 1)
  x <- as(dense_x, "sparse_numeric")
  y <- as(dense_y, "sparse_numeric")
  res_sparse <- as(sparse_mult(x, y), "numeric")
  expect_equal(res_sparse, dense_x * dense_y)
})

test_that("sparse_mult with non-overlapping support gives all zeros", {
  dense_x <- c(1, 0, 0, 0)
  dense_y <- c(0, 2, 3, 4)
  x <- as(dense_x, "sparse_numeric")
  y <- as(dense_y, "sparse_numeric")
  res <- as(sparse_mult(x, y), "numeric")
  expect_equal(res, rep(0, 4))
})

test_that("sparse_crossprod matches dense dot product", {
  dense_x <- c(0, 2, 0, 5)
  dense_y <- c(1, 0, 3, 1)
  x <- as(dense_x, "sparse_numeric")
  y <- as(dense_y, "sparse_numeric")
  res_sparse <- sparse_crossprod(x, y)
  expect_equal(res_sparse, sum(dense_x * dense_y))
})


## ---- Show and plot ----------------------------------------------------------

test_that("show produces output without error", {
  x <- as(c(0, 1, 0, 2), "sparse_numeric")
  expect_output(show(x))
})

test_that("plot produces no error for two sparse vectors", {
  x <- as(c(0, 1, 0, 2), "sparse_numeric")
  y <- as(c(1, 0, 3, 0), "sparse_numeric")
  expect_no_error(plot(x, y))
})


## ---- Norms, mean, standardize -----------------------------------------------

test_that("sparse_norm equals dense L2 norm", {
  dense <- c(0, 0, 3, 4)
  s <- as(dense, "sparse_numeric")
  expect_equal(sparse_norm(s), sqrt(sum(dense^2)))
})

test_that("norm for sparse matches dense L2 norm", {
  dense <- c(0, 0, 3, 4)
  s <- as(dense, "sparse_numeric")
  expect_equal(norm(s), sqrt(sum(dense^2)))
})

test_that("norm and sparse_norm agree", {
  dense <- c(1, -2, 0, 3)
  s <- as(dense, "sparse_numeric")
  expect_equal(norm(s), sparse_norm(s))
})

test_that("mean for sparse matches dense mean", {
  dense <- c(0, 0, 3, 0, -2, 0)
  s <- as(dense, "sparse_numeric")
  expect_equal(mean(s), mean(dense))
})

test_that("mean of all-zero sparse vector is zero", {
  dense <- rep(0, 5)
  s <- as(dense, "sparse_numeric")
  expect_equal(mean(s), 0)
})

test_that("mean of zero-length sparse vector is NA", {
  x <- new("sparse_numeric",
           value = numeric(0),
           pos   = integer(0),
           length = 0L)
  expect_true(is.na(mean(x)))
})

test_that("standardize gives mean ~ 0 and sd ~ 1", {
  dense <- c(0, 0, 3, 4, -2)
  s <- as(dense, "sparse_numeric")
  s_std <- standardize(s)
  dense_std <- as(s_std, "numeric")
  expect_equal(mean(dense_std), 0, tolerance = 1e-8)
  expect_equal(sd(dense_std),   1, tolerance = 1e-8)
})

test_that("standardize handles all-equal vector (nonzero)", {
  dense <- rep(5, 5)
  s <- as(dense, "sparse_numeric")
  s_std <- standardize(s)
  dense_std <- as(s_std, "numeric")
  expect_true(all(is.na(dense_std)))
})

test_that("standardize handles length-1 vector", {
  dense <- 7
  s <- as(dense, "sparse_numeric")
  s_std <- standardize(s)
  dense_std <- as(s_std, "numeric")
  expect_length(dense_std, 1L)
  expect_true(is.na(dense_std[1]))
})

test_that("standardize handles zero-length vector", {
  x <- new("sparse_numeric",
           value = numeric(0),
           pos   = integer(0),
           length = 0L)
  s_std <- standardize(x)
  dense_std <- as(s_std, "numeric")
  expect_length(dense_std, 0L)
})



