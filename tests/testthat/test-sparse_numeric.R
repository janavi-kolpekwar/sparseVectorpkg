## HW5_testscript.R

library(testthat)

test_that("check validity method exists", {
  expect_false({
    validity_method <- getValidity(getClassDef("sparse_numeric"))
    is.null(validity_method)
  })
})

test_that("check validity method", {
  expect_true({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    validObject(x)
  })
})

test_that("check validity method 2", {
  expect_error({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    x@length <- 2L
    validObject(x)
  })
})

test_that("check coercion return class", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  }, "sparse_numeric")
})

test_that("check for show method", {
  expect_no_error({
    getMethod("show", "sparse_numeric")
  })
})

test_that("check for plot method", {
  expect_no_error({
    getMethod("plot", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for + method", {
  expect_no_error({
    getMethod("+", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for - method", {
  expect_no_error({
    getMethod("-", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for * method", {
  expect_no_error({
    getMethod("*", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("sparse add generic", expect_true(isGeneric("sparse_add")))
test_that("sparse mult generic", expect_true(isGeneric("sparse_mult")))
test_that("sparse sub generic", expect_true(isGeneric("sparse_sub")))
test_that("sparse crossprod generic", expect_true(isGeneric("sparse_crossprod")))

test_that("sparse add formals", {
  expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse mult formals", {
  expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse sub formals", {
  expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse crossprod formals", {
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})

test_that("check returned class for add", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, "sparse_numeric")
})

test_that("sparse_add", {
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("sparse add dense", {
  result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
  expect_equal({
    x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("all zero wrong length", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")
    sparse_add(x, y)
  })
})

# Additional tests for coverage

test_that("coercion creates correct sparse structure", {
  x <- as(c(0, 0, 5, 0, 3), "sparse_numeric")
  expect_equal(x@value, c(5, 3))
  expect_equal(x@pos, c(3L, 5L))
  expect_equal(x@length, 5L)
})

test_that("coercion handles all zeros", {
  x <- as(rep(0, 5), "sparse_numeric")
  expect_equal(length(x@value), 0)
  expect_equal(length(x@pos), 0)
  expect_equal(x@length, 5L)
})

test_that("sparse_sub works correctly", {
  x <- as(c(5, 0, 3, 0, 2), "sparse_numeric")
  y <- as(c(1, 0, 1, 0, 1), "sparse_numeric")
  result <- sparse_sub(x, y)
  expected <- as(c(4, 0, 2, 0, 1), "sparse_numeric")
  expect_equal(result, expected)
})

test_that("sparse_mult works correctly", {
  x <- as(c(2, 0, 3, 0, 4), "sparse_numeric")
  y <- as(c(1, 0, 2, 0, 3), "sparse_numeric")
  result <- sparse_mult(x, y)
  expected <- as(c(2, 0, 6, 0, 12), "sparse_numeric")
  expect_equal(result, expected)
})

test_that("sparse_crossprod works correctly", {
  x <- as(c(1, 2, 3), "sparse_numeric")
  y <- as(c(4, 5, 6), "sparse_numeric")
  result <- sparse_crossprod(x, y)
  expect_equal(result, 32)  # 1*4 + 2*5 + 3*6
})

test_that("operator + works", {
  x <- as(c(1, 0, 2), "sparse_numeric")
  y <- as(c(3, 0, 4), "sparse_numeric")
  result <- x + y
  expected <- as(c(4, 0, 6), "sparse_numeric")
  expect_equal(result, expected)
})

test_that("operator - works", {
  x <- as(c(5, 0, 3), "sparse_numeric")
  y <- as(c(2, 0, 1), "sparse_numeric")
  result <- x - y
  expected <- as(c(3, 0, 2), "sparse_numeric")
  expect_equal(result, expected)
})

test_that("operator * works", {
  x <- as(c(2, 0, 3), "sparse_numeric")
  y <- as(c(4, 0, 5), "sparse_numeric")
  result <- x * y
  expected <- as(c(8, 0, 15), "sparse_numeric")
  expect_equal(result, expected)
})

test_that("show method executes without error", {
  x <- as(c(1, 0, 0, 2, 3), "sparse_numeric")
  expect_output(show(x))
})

test_that("plot method executes without error", {
  x <- as(c(1, 0, 2, 0, 3), "sparse_numeric")
  y <- as(c(0, 1, 0, 2, 0), "sparse_numeric")
  expect_no_error(plot(x, y))
})

test_that("length mismatch errors in subtraction", {
  x <- as(c(1, 2, 3), "sparse_numeric")
  y <- as(c(1, 2), "sparse_numeric")
  expect_error(sparse_sub(x, y))
})

test_that("length mismatch errors in multiplication", {
  x <- as(c(1, 2, 3), "sparse_numeric")
  y <- as(c(1, 2), "sparse_numeric")
  expect_error(sparse_mult(x, y))
})

test_that("length mismatch errors in crossprod", {
  x <- as(c(1, 2, 3), "sparse_numeric")
  y <- as(c(1, 2), "sparse_numeric")
  expect_error(sparse_crossprod(x, y))
})


