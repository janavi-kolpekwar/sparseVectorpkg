#' @keywords internal
"_PACKAGE"

############################################################
## 1. Class Definition
############################################################

#' Sparse Numeric Vector (S4 Class)
#'
#' An S4 class that stores numeric vectors efficiently by keeping
#' only the nonzero values and their positions.
#'
#' @slot value Numeric vector of nonzero values.
#' @slot pos Integer vector of positions of the nonzero values.
#' @slot length Integer giving the full length of the vector.
#' @export
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

############################################################
## 2. Validity Method
############################################################

setValidity("sparse_numeric", function(object) {
  if (length(object@value) != length(object@pos))
    return("Slots 'value' and 'pos' must have the same length.")
  if (any(object@pos < 1L | object@pos > object@length))
    return("Slot 'pos' contains indices outside valid range.")
  if (anyDuplicated(object@pos))
    return("Slot 'pos' contains duplicate indices.")
  TRUE
})

############################################################
## 3. Coercion Methods
############################################################

#' @export
setAs("numeric", "sparse_numeric", function(from) {
  nz <- which(from != 0)
  new("sparse_numeric",
      value = from[nz],
      pos = as.integer(nz),
      length = as.integer(length(from)))
})

#' @export
setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  if (length(from@pos) > 0L)
    out[from@pos] <- from@value
  out
})

############################################################
## 4. Generic Function Definitions
############################################################

#' Add Two Sparse Numeric Vectors
#'
#' Element-wise addition of two sparse vectors of equal length.
#'
#' @param x A sparse_numeric vector.
#' @param y A sparse_numeric vector.
#' @return A sparse_numeric representing the sum.
#' @export
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

#' Subtract Two Sparse Numeric Vectors
#'
#' Element-wise subtraction of two sparse vectors of equal length.
#' @export
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))

#' Multiply Two Sparse Numeric Vectors
#'
#' Computes element-wise products only where both vectors have nonzero entries.
#' @export
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' Sparse Crossproduct (Dot Product)
#'
#' Computes sum(x_i * y_i) for sparse vectors efficiently.
#' @export
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

#' Compute Density Ratio of Sparse Vector
#'
#' Proportion of nonzero entries.
#' @export
setGeneric("density_ratio", function(x) standardGeneric("density_ratio"))

#' Compute Mean of Sparse Vector
#'
#' Calculates mean including implicit zeros.
#' @export
setGeneric("mean")

#' Compute Euclidean Norm of Sparse Vector
#'
#' Computes sqrt(sum(x_i^2)).
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))

#' Standardize a Sparse Vector
#'
#' Subtracts mean and divides by SD.
#' @export
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

############################################################
## 5. Arithmetic Methods
############################################################

setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("Vectors must have the same length.")

            pos_all <- sort(unique(c(x@pos, y@pos)))
            val_x <- numeric(length(pos_all))
            val_y <- numeric(length(pos_all))

            if (length(x@pos) > 0)
              val_x[match(x@pos, pos_all)] <- x@value
            if (length(y@pos) > 0)
              val_y[match(y@pos, pos_all)] <- y@value

            vals <- val_x + val_y
            nz <- which(vals != 0)
            new("sparse_numeric",
                value = vals[nz],
                pos = as.integer(pos_all[nz]),
                length = x@length)
          })

setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("Vectors must have the same length.")

            pos_all <- sort(unique(c(x@pos, y@pos)))
            val_x <- numeric(length(pos_all))
            val_y <- numeric(length(pos_all))

            if (length(x@pos) > 0)
              val_x[match(x@pos, pos_all)] <- x@value
            if (length(y@pos) > 0)
              val_y[match(y@pos, pos_all)] <- y@value

            vals <- val_x - val_y
            nz <- which(vals != 0)
            new("sparse_numeric",
                value = vals[nz],
                pos = as.integer(pos_all[nz]),
                length = x@length)
          })

setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("Vectors must have the same length.")
            common <- intersect(x@pos, y@pos)
            if (length(common) == 0L)
              return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))

            x_vals <- x@value[match(common, x@pos)]
            y_vals <- y@value[match(common, y@pos)]
            prod_vals <- x_vals * y_vals
            nz <- which(prod_vals != 0)
            new("sparse_numeric",
                value = prod_vals[nz],
                pos = as.integer(common[nz]),
                length = x@length)
          })

setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("Vectors must have the same length.")
            common <- intersect(x@pos, y@pos)
            if (length(common) == 0L) return(0)
            sum(x@value[match(common, x@pos)] * y@value[match(common, y@pos)])
          })

############################################################
## 6. Mean, Norm, Standardize Methods
############################################################

setMethod("mean", "sparse_numeric", function(x, ...) {
  sum(x@value) / x@length
})

setMethod("norm", "sparse_numeric", function(x, type = "2", ...) {
  if (type != "2")
    stop("Only Euclidean norm ('2') supported.")
  sqrt(sum(x@value^2))
})

setMethod("density_ratio", "sparse_numeric", function(x) {
  if (x@length == 0L) return(NA_real_)
  length(x@pos) / x@length
})

setMethod("standardize", "sparse_numeric", function(x, ...) {
  μ <- mean(x)
  n <- x@length
  var <- (sum((x@value - μ)^2) + (n - length(x@pos)) * μ^2) / n
  sd_x <- sqrt(var)

  if (sd_x == 0)
    stop("Standard deviation is zero; cannot standardize.")

  out <- rep((-μ) / sd_x, n)
  if (length(x@pos) > 0)
    out[x@pos] <- (x@value - μ) / sd_x

  nz <- which(out != 0)
  new("sparse_numeric",
      value = out[nz],
      pos = as.integer(nz),
      length = as.integer(n))
})

############################################################
## 7. Operator Overloads
############################################################

setMethod("+", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

setMethod("-", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

setMethod("*", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

############################################################
## 8. Show & Plot Methods
############################################################

setMethod("show", "sparse_numeric", function(object) {
  cat("Formal class 'sparse_numeric'\n")
  cat("Length:", object@length, "\n")
  cat("Nonzero positions:", object@pos, "\n")
  cat("Nonzero values:", object@value, "\n")
})

setMethod("plot", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
  plot(x@pos, x@value, col = "blue", pch = 19,
       xlab = "Index", ylab = "Value", main = "Sparse Vector Comparison", ...)
  points(y@pos, y@value, col = "red", pch = 17)
  common <- intersect(x@pos, y@pos)
  if (length(common) > 0)
    points(common, x@value[match(common, x@pos)], col = "purple", pch = 15)
  legend("topright", legend = c("x", "y", "overlap"),
         col = c("blue", "red", "purple"), pch = c(19, 17, 15))
})

############################################################
## End of File
############################################################




