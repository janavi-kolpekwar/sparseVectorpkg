#' Sparse numeric vector class
#'
#' An S4 class to represent numeric vectors in sparse form.
#'
#' @slot value Numeric vector of non-zero values.
#' @slot pos Integer vector of positions (1-based) of non-zero values.
#' @slot length Integer giving the total length of the vector (including zeros).
#'
#' @name sparse_numeric-class
#' @rdname sparse_numeric-class
#' @importFrom methods setClass setValidity setGeneric setMethod new show
#' @importFrom graphics plot points legend
#' @importFrom stats setNames
#' @exportClass sparse_numeric
setClass(
  Class = "sparse_numeric",
  slots = c(
    value  = "numeric",
    pos    = "integer",
    length = "integer"
  )
)

#' @keywords internal
#' @noRd
setValidity("sparse_numeric", function(object) {

  if (length(object@value) != length(object@pos))
    return("Length of 'value' and 'pos' must match.")

  if (any(is.na(object@pos)))
    return("'pos' contains NA values.")

  if (any(object@pos < 1 | object@pos > object@length))
    return("'pos' values must be within vector length range.")

  TRUE
})

## coercion methods -----------------------------------------------------------

#' @keywords internal
#' @noRd
setAs("numeric", "sparse_numeric", function(from) {
  nz <- from != 0
  new("sparse_numeric",
      value  = from[nz],
      pos    = as.integer(which(nz)),
      length = as.integer(length(from)))
})

#' @keywords internal
#' @noRd
setAs("sparse_numeric", "numeric", function(from) {
  x <- numeric(from@length)
  x[from@pos] <- from@value
  x
})

## helper ---------------------------------------------------------------------

#' Internal helper to extract value at a position
#'
#' @param named_vec A named numeric vector.
#' @param p Position index.
#' @keywords internal
#' @noRd
get_val <- function(named_vec, p) {
  val <- named_vec[as.character(p)]
  ifelse(!is.na(val), val, 0)
}

## generics & methods for sparse operations -----------------------------------

#' Elementwise addition of sparse_numeric vectors
#'
#' @param x,y Objects of class \code{sparse_numeric}.
#' @param ... Further arguments passed to methods (currently ignored).
#' @return A \code{sparse_numeric} vector containing elementwise sums.
#' @name sparse_add
#' @export
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

#' @rdname sparse_add
#' @export
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  if (x@length != y@length)
    stop("Vectors must be of same length")

  all_pos <- sort(union(x@pos, y@pos))
  x_vals <- setNames(x@value, x@pos)
  y_vals <- setNames(y@value, y@pos)

  summed <- vapply(all_pos, function(p) get_val(x_vals, p) + get_val(y_vals, p), numeric(1))
  non_zero <- summed[summed != 0]

  new("sparse_numeric",
      value  = as.numeric(non_zero),
      pos    = as.integer(all_pos[summed != 0]),
      length = x@length)
})

#' Elementwise subtraction of sparse_numeric vectors
#'
#' @inheritParams sparse_add
#' @return A \code{sparse_numeric} vector containing elementwise differences.
#' @name sparse_sub
#' @export
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))

#' @rdname sparse_sub
#' @export
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  if (x@length != y@length)
    stop("Vectors must be of same length")

  # consider all positions where either x or y is non-zero
  all_pos <- sort(union(x@pos, y@pos))
  x_vals <- setNames(x@value, x@pos)
  y_vals <- setNames(y@value, y@pos)

  diff_vals <- vapply(
    all_pos,
    function(p) get_val(x_vals, p) - get_val(y_vals, p),
    numeric(1)
  )

  keep <- diff_vals != 0

  new("sparse_numeric",
      value  = as.numeric(diff_vals[keep]),
      pos    = as.integer(all_pos[keep]),
      length = x@length)
})


#' Elementwise multiplication of sparse_numeric vectors
#'
#' @inheritParams sparse_add
#' @return A \code{sparse_numeric} vector containing elementwise products.
#' @name sparse_mult
#' @export
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' @rdname sparse_mult
#' @export
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  if (x@length != y@length)
    stop("Vectors must be of same length")

  common_pos <- intersect(x@pos, y@pos)
  prod_vals <- x@value[match(common_pos, x@pos)] * y@value[match(common_pos, y@pos)]

  new("sparse_numeric",
      value  = prod_vals,
      pos    = as.integer(common_pos),
      length = x@length)
})

#' Sparse crossproduct (dot product)
#'
#' @inheritParams sparse_add
#' @return Numeric scalar, the dot product of \code{x} and \code{y}.
#' @name sparse_crossprod
#' @export
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

#' @rdname sparse_crossprod
#' @export
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  if (x@length != y@length)
    stop("Vectors must be same length")

  common_pos <- intersect(x@pos, y@pos)
  sum(x@value[match(common_pos, x@pos)] * y@value[match(common_pos, y@pos)])
})

## operator overloading -------------------------------------------------------

#' Arithmetic operators for sparse_numeric vectors
#'
#' These methods allow you to use the standard arithmetic operators
#' \code{+}, \code{-}, and \code{*} with \code{sparse_numeric} vectors.
#' They dispatch to \code{sparse_add()}, \code{sparse_sub()}, and
#' \code{sparse_mult()} respectively.
#'
#' @param e1,e2 Objects of class \code{sparse_numeric}.
#'
#' @name sparse_numeric-ops
NULL

#' @rdname sparse_numeric-ops
#' @aliases \S4method{+}{sparse_numeric,sparse_numeric}
#' @export
setMethod("+", c("sparse_numeric","sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

#' @rdname sparse_numeric-ops
#' @aliases \S4method{-}{sparse_numeric,sparse_numeric}
#' @export
setMethod("-", c("sparse_numeric","sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

#' @rdname sparse_numeric-ops
#' @aliases \S4method{*}{sparse_numeric,sparse_numeric}
#' @export
setMethod("*", c("sparse_numeric","sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))



## display methods ------------------------------------------------------------

#' @keywords internal
#' @noRd
setMethod("show", "sparse_numeric", function(object) {
  cat("Sparse numeric vector of length", object@length, "\n")
  cat("Non-zero elements:\n")
  print(data.frame(pos = object@pos, value = object@value))
})


#' @keywords internal
#' @noRd
setMethod("plot", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  plot(x@pos, x@value, col = "blue", pch = 16,
       xlab = "Position", ylab = "Value",
       main = "Sparse Vector Comparison")
  points(y@pos, y@value, col = "red", pch = 17)
  legend("topright", legend = c("x", "y"),
         col = c("blue", "red"), pch = c(16, 17))
})

## norms & summaries ----------------------------------------------------------

#' L2 norm for sparse_numeric vectors (sparse_norm)
#'
#' @param x A \code{sparse_numeric} vector.
#' @return Numeric scalar, the Euclidean norm of \code{x}.
#' @name sparse_norm
#' @export
setGeneric("sparse_norm", function(x) standardGeneric("sparse_norm"))

#' @rdname sparse_norm
#' @export
setMethod("sparse_norm", "sparse_numeric", function(x) sqrt(sum(x@value^2)))

#' Euclidean norm for sparse_numeric via norm()
#'
#' @inheritParams sparse_norm
#' @param ... Additional arguments passed to methods (currently ignored).
#' @return Numeric scalar, the Euclidean (L2) norm of \code{x}.
#' @name norm
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))

#' @rdname norm
#' @export
setMethod("norm", "sparse_numeric", function(x, ...) sqrt(sum(x@value^2)))

#' @keywords internal
#' @noRd
setMethod("mean", "sparse_numeric", function(x, ...) {
  if (x@length == 0L) return(NA_real_)
  sum(x@value) / x@length
})

#' Standardize a sparse_numeric vector
#'
#' Standardizes each element of a \code{sparse_numeric} vector by subtracting
#' the mean and dividing by the standard deviation (as in \code{sd()}).
#'
#' @param x A \code{sparse_numeric} vector.
#' @param ... Ignored.
#' @return A standardized \code{sparse_numeric} vector.
#' @name standardize
#' @export
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

#' @rdname standardize
#' @export
setMethod("standardize", "sparse_numeric", function(x, ...) {

  n <- x@length
  n_int <- as.integer(n)

  # Handle length 0 and 1 specially
  if (n_int <= 0L) {
    return(new("sparse_numeric",
               value  = numeric(0),
               pos    = integer(0),
               length = 0L))
  }

  if (n_int == 1L) {
    return(new("sparse_numeric",
               value  = NA_real_,
               pos    = 1L,
               length = 1L))
  }

  mu <- sum(x@value) / n

  # sum of squared deviations, including zeros
  k <- length(x@value)
  sse_nonzero <- sum((x@value - mu)^2)
  sse_zero    <- (n - k) * mu^2
  sse         <- sse_nonzero + sse_zero

  sd_x <- sqrt(sse / (n - 1L))

  # If sd is zero (all values equal) or NA, everything standardizes to NA
  if (sd_x == 0 || is.na(sd_x)) {
    values <- rep(NA_real_, n_int)
  } else {
    # Start with standardized zeros
    values <- rep(-mu / sd_x, n_int)
    # Overwrite positions that were non-zero in original
    if (k > 0L) {
      values[x@pos] <- (x@value - mu) / sd_x
    }
  }

  new("sparse_numeric",
      value  = values,
      pos    = as.integer(seq_len(n_int)),
      length = n_int)
})





