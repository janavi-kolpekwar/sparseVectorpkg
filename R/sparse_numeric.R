#############################################
# sparse_numeric Class and Methods
#############################################

#' Sparse Numeric Vector Class
#'
#' Represents a sparse numeric vector using only nonzero values and their
#' positions. Designed to support efficient arithmetic without converting
#' to dense representations.
#'
#' @slot value Numeric vector of nonzero values.
#' @slot pos Integer vector of positions of nonzero values.
#' @slot length Integer giving total length of the vector.
#'
#' @export
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

#' @keywords internal
setValidity("sparse_numeric", function(object) {
  if (length(object@value) != length(object@pos)) {
    return("Length of 'value' and 'pos' must match")
  }
  if (any(object@pos > object@length) || any(object@pos < 1)) {
    return("'pos' values must be within [1, length]")
  }
  if (any(duplicated(object@pos))) {
    return("'pos' values must be unique")
  }
  TRUE
})

setOldClass("numeric")

#############################################
# Coercion
#############################################

#' Convert Numeric Vector to Sparse Numeric
#'
#' Converts a numeric vector into a \code{sparse_numeric} representation.
#'
#' @param from A numeric vector.
#'
#' @return A \code{sparse_numeric} object.
#'
#' @export
#' @name as_sparse_numeric
#' @rdname as_sparse_numeric
setAs("numeric", "sparse_numeric", function(from) {
  pos <- which(from != 0)
  value <- from[pos]
  new("sparse_numeric", value = value, pos = as.integer(pos), length = as.integer(length(from)))
})

#' Convert Sparse Numeric to Full Numeric
#'
#' Converts a \code{sparse_numeric} object back into the full numeric vector.
#'
#' @param from A \code{sparse_numeric} object.
#'
#' @return A full numeric vector.
#'
#' @export
#' @name as_sparse_numeric
#' @rdname as_sparse_numeric
setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  out[from@pos] = from@value
  out
})

#############################################
# Helper
#############################################

#' @keywords internal
.check_same_length <- function(x, y) {
  if (x@length != y@length) stop("Vectors must be of the same length")
}

#############################################
# Generics
#############################################

#' Add Two Sparse Numeric Vectors
#'
#' @param x A \code{sparse_numeric} object.
#' @param y A \code{sparse_numeric} object.
#'
#' @return A \code{sparse_numeric} representing \code{x + y}.
#'
#' @export
#' @name sparse_add
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

#' Subtract Sparse Numeric Vectors
#'
#' @param x A \code{sparse_numeric} object.
#' @param y A \code{sparse_numeric} object.
#'
#' @return A \code{sparse_numeric} representing \code{x - y}.
#'
#' @export
#' @name sparse_sub
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))

#' Elementwise Multiply Sparse Numeric Vectors
#'
#' @param x A \code{sparse_numeric}.
#' @param y A \code{sparse_numeric}.
#'
#' @return A \code{sparse_numeric} representing \code{x * y}.
#'
#' @export
#' @name sparse_mult
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' Sparse Crossproduct (Dot Product)
#'
#' @param x A \code{sparse_numeric}.
#' @param y A \code{sparse_numeric}.
#'
#' @return A numeric scalar.
#'
#' @export
#' @name sparse_crossprod
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

#############################################
# Methods
#############################################

#' @export
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  .check_same_length(x, y)
  all_pos <- c(x@pos, y@pos)
  all_val <- c(x@value, y@value)
  combined <- tapply(all_val, all_pos, sum)
  pos <- as.integer(names(combined))
  value <- as.numeric(combined)
  keep <- which(value != 0)

  new("sparse_numeric",
      value = value[keep],
      pos = pos[keep],
      length = x@length)
})

#' @export
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  .check_same_length(x, y)
  all_pos <- union(x@pos, y@pos)
  vals <- sapply(all_pos, function(p) {
    xv <- if (p %in% x@pos) x@value[which(x@pos == p)] else 0
    yv <- if (p %in% y@pos) y@value[which(y@pos == p)] else 0
    xv - yv
  })
  keep <- which(vals != 0)

  new("sparse_numeric",
      value = vals[keep],
      pos = as.integer(all_pos[keep]),
      length = x@length)
})

#' @export
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  .check_same_length(x, y)
  common <- intersect(x@pos, y@pos)
  vals <- x@value[match(common, x@pos)] * y@value[match(common, y@pos)]
  keep <- which(vals != 0)

  new("sparse_numeric",
      value = vals[keep],
      pos = as.integer(common[keep]),
      length = x@length)
})

#' @export
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  .check_same_length(x, y)
  common <- intersect(x@pos, y@pos)
  sum(x@value[match(common, x@pos)] * y@value[match(common, y@pos)])
})

#############################################
# Operator Overloads
#############################################

#' @export
setMethod("+", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_add(e1, e2))

#' @export
setMethod("-", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_sub(e1, e2))

#' @export
setMethod("*", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_mult(e1, e2))

#############################################
# Display
#############################################

#' @export
setMethod("show", "sparse_numeric", function(object) {
  cat("Sparse numeric vector of length", object@length, "\n")
  cat("Non-zero positions:", object@pos, "\n")
  cat("Values:", object@value, "\n")
})

#############################################
# Plotting
#############################################

#' Plot Sparse Numeric Vectors
#'
#' Plots two sparse numeric vectors on the same coordinate axes.
#'
#' @param x A \code{sparse_numeric} object.
#' @param y A \code{sparse_numeric} object.
#'
#' @export
setMethod("plot", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
  plot(x@pos, x@value, col = "blue", pch = 16,
       xlim = c(1, max(x@length, y@length)),
       ylim = range(c(x@value, y@value, 0)),
       xlab = "Index", ylab = "Value", main = "Sparse Numeric Comparison", ...)
  points(y@pos, y@value, col = "red", pch = 17)
  legend("topright", legend = c("x", "y"), col = c("blue", "red"), pch = c(16, 17))
})

#############################################
# Norms
#############################################

#' Squared L2 Norm of a Sparse Vector
#'
#' @param x A \code{sparse_numeric} object.
#'
#' @return Numeric scalar.
#'
#' @export
#' @name sparse_norm2
setGeneric("sparse_norm2", function(x) standardGeneric("sparse_norm2"))

#' @export
setMethod("sparse_norm2", "sparse_numeric", function(x) sum(x@value^2))

#' @export
setGeneric("norm")

#' L2 Norm of a Sparse Vector
#'
#' @param x A \code{sparse_numeric}.
#' @param type Norm type. Only `"2"` supported.
#'
#' @return Numeric scalar.
#'
#' @export
#' @name norm_sparse_numeric
setMethod("norm", "sparse_numeric", function(x, type = "2", ...) {
  if (type != "2") stop("Only Euclidean norm ('2') is supported")
  sqrt(sum(x@value^2))
})

#############################################
# Standardization
#############################################

#' Standardize a Sparse Numeric Vector
#'
#' Centers and scales a sparse numeric vector without constructing the dense
#' version. Zeros are accounted for analytically.
#'
#' @param x A \code{sparse_numeric}.
#'
#' @return A standardized \code{sparse_numeric}.
#'
#' @export
#' @name standardize
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

#' @export
setMethod("standardize", "sparse_numeric", function(x) {

  n <- x@length
  k <- length(x@pos)
  m <- mean(x)

  ss <- sum(x@value^2)
  var_num <- ss + (n - k) * m^2 - 2 * m * sum(x@value) + k * m^2
  sd_x <- sqrt(var_num / (n - 1))

  if (sd_x == 0) stop("Cannot standardize: standard deviation is zero")

  new_vals <- (x@value - m) / sd_x
  zero_val <- (-m) / sd_x

  adjust <- new_vals - zero_val
  keep <- which(adjust != 0)

  new("sparse_numeric",
      value = adjust[keep],
      pos = x@pos[keep],
      length = x@length)
})





