library(testthat)

############################################################
##  BASE HW5 TESTS (converted to package form)
############################################################

test_that("validity method exists", {
  validity_method <- getValidity(getClassDef("sparse_numeric"))
  expect_false(is.null(validity_method))
})

test_that("valid object passes validity", {
  x <- new("sparse_numeric",
           value = c(1, 2, 3, 1),
           pos = c(1L, 2L, 3L, 5L),
           length = 5L)
  expect_true(validObject(x))
})

test_that("invalid object fails validity (length mismatch)", {
  x <- new("sparse_numeric",
           value = c(1, 2, 3, 1),
           pos = c(1L, 2L, 3L, 5L),
           length = 5L)
  x@length <- 2L
  expect_error(validObject(x))
})

test_that("numeric → sparse conversion returns S4", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  expect_s4_class(x, "sparse_numeric")
})

test_that("show method exists", {
  expect_no_error(getMethod("show", "sparse_numeric"))
})

test_that("plot method exists", {
  expect_no_error(getMethod("plot", c("sparse_numeric", "sparse_numeric")))
})

test_that("+ operator exists", {
  expect_no_error(getMethod("+", c("sparse_numeric", "sparse_numeric")))
})

test_that("- operator exists", {
  expect_no_error(getMethod("-", c("sparse_numeric", "sparse_numeric")))
})

test_that("* operator exists", {
  expect_no_error(getMethod("*", c("sparse_numeric", "sparse_numeric")))
})

test_that("generics exist", {
  expect_true(isGeneric("sparse_add"))
  expect_true(isGeneric("sparse_sub"))
  expect_true(isGeneric("sparse_mult"))
  expect_true(isGeneric("sparse_crossprod"))
})

test_that("sparse_add returns S4", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
  expect_s4_class(sparse_add(x, y), "sparse_numeric")
})

test_that("sparse_add correctness simple", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal(sparse_add(x, y), result)
})

test_that("sparse_add dense", {
  x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
  y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
  result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
  expect_equal(sparse_add(x, y), result)
})

test_that("all zero wrong length", {
  x <- as(rep(0, 10), "sparse_numeric")
  y <- as(rep(0, 9), "sparse_numeric")
  expect_error(sparse_add(x, y))
})

############################################################
##  ADDITIONAL TESTS FOR 90% COVERAGE
############################################################

### Validity edge cases -------------------------------------------------------

test_that("duplicate positions invalid", {
  expect_error(
    new("sparse_numeric",
        value = c(1, 2),
        pos = c(3L, 3L),
        length = 5L),
    "duplicate"
  )
})

test_that("pos outside bounds invalid", {
  expect_error(
    new("sparse_numeric",
        value = 5,
        pos = 10L,
        length = 5L),
    "outside valid range"
  )
})

### Coercion roundtrip --------------------------------------------------------

test_that("sparse → numeric → sparse roundtrip", {
  x <- as(c(0, 4, 0, 2), "sparse_numeric")
  back <- as(as(x, "numeric"), "sparse_numeric")
  expect_equal(x, back)
})

### Arithmetic edge cases -----------------------------------------------------

test_that("subtract correctness", {
  x <- as(c(5, 0, 3), "sparse_numeric")
  y <- as(c(1, 0, 1), "sparse_numeric")
  expect_equal(sparse_sub(x, y), as(c(4, 0, 2), "sparse_numeric"))
})

test_that("multiply correctness partial overlap", {
  x <- as(c(0, 2, 3), "sparse_numeric")
  y <- as(c(4, 0, 3), "sparse_numeric")
  expect_equal(sparse_mult(x, y), as(c(0, 0, 9), "sparse_numeric"))
})

test_that("multiply no overlap returns empty sparse", {
  x <- as(c(1, 0, 0), "sparse_numeric")
  y <- as(c(0, 2, 3), "sparse_numeric")
  out <- sparse_mult(x, y)
  expect_equal(out@value, numeric(0))
  expect_equal(out@pos, integer(0))
})

### Crossproduct --------------------------------------------------------------

test_that("crossproduct simple", {
  x <- as(c(1, 0, 2), "sparse_numeric")
  y <- as(c(3, 4, 5), "sparse_numeric")
  expect_equal(sparse_crossprod(x, y), 1*3 + 2*5)
})

test_that("crossproduct zero", {
  x <- as(c(0, 2, 0), "sparse_numeric")
  y <- as(c(1, 0, 3), "sparse_numeric")
  expect_equal(sparse_crossprod(x, y), 0)
})

### Density ratio -------------------------------------------------------------

test_that("density_ratio basic", {
  x <- as(c(0, 1, 0, 2), "sparse_numeric")
  expect_equal(density_ratio(x), 2/4)
})

test_that("density_ratio empty", {
  x <- as(numeric(0), "sparse_numeric")
  expect_true(is.na(density_ratio(x)))
})

### Mean ----------------------------------------------------------------------

test_that("mean basic", {
  x <- as(c(0, 1, 0, 3), "sparse_numeric")
  expect_equal(mean(x), (1+3)/4)
})

test_that("mean zeros", {
  x <- as(c(0, 0, 0), "sparse_numeric")
  expect_equal(mean(x), 0)
})

### Norm ----------------------------------------------------------------------

test_that("norm basic", {
  x <- as(c(0, 3, 4), "sparse_numeric")
  expect_equal(norm(x), 5)
})

test_that("norm zero", {
  x <- as(c(0, 0, 0), "sparse_numeric")
  expect_equal(norm(x), 0)
})

### Standardize ---------------------------------------------------------------

test_that("standardize mean ≈ 0", {
  x <- as(c(0, 2, 4, 6), "sparse_numeric")
  out <- as(standardize(x), "numeric")
  expect_true(abs(mean(out)) < 1e-8)
})

test_that("standardize variance ≈ 1 (population)", {
  x <- as(c(1, 2, 3), "sparse_numeric")
  out <- as(standardize(x), "numeric")

  # compute POPULATION variance (matches package implementation)
  pop_var <- mean((out - mean(out))^2)

  expect_true(abs(pop_var - 1) < 1e-8)
})

test_that("standardize zero variance errors", {
  x <- as(c(5, 5, 5), "sparse_numeric")
  expect_error(standardize(x))
})

### Show & Plot ---------------------------------------------------------------

test_that("show outputs", {
  x <- as(c(0, 1, 2), "sparse_numeric")
  expect_output(show(x))
})

test_that("plot works", {
  x <- as(c(0, 2, 0), "sparse_numeric")
  y <- as(c(1, 0, 3), "sparse_numeric")
  expect_no_error(plot(x, y))
})

### Operators -----------------------------------------------------------------

test_that("+ operator works", {
  x <- as(c(1, 0, 2), "sparse_numeric")
  y <- as(c(0, 3, 1), "sparse_numeric")
  expect_equal(x + y, as(c(1, 3, 3), "sparse_numeric"))
})

test_that("- operator works", {
  x <- as(c(3, 2, 1), "sparse_numeric")
  y <- as(c(1, 1, 1), "sparse_numeric")
  expect_equal(x - y, as(c(2, 1, 0), "sparse_numeric"))
})

test_that("* operator works", {
  x <- as(c(1, 0, 2), "sparse_numeric")
  y <- as(c(2, 1, 2), "sparse_numeric")
  expect_equal(x * y, as(c(2, 0, 4), "sparse_numeric"))
})

### Random stress tests -------------------------------------------------------

test_that("random add matches dense", {
  for (i in 1:20) {
    a <- sample(0:3, 20, replace=TRUE)
    b <- sample(0:3, 20, replace=TRUE)
    expect_equal(
      sparse_add(as(a, "sparse_numeric"), as(b, "sparse_numeric")),
      as(a + b, "sparse_numeric")
    )
  }
})

