#' Function to do large scale linear regression
#' 
#' Function to do multiple linear regressions, XB = Y,
#' where X has a large number of columns that do not change
#' and a small number of columns that change and/or Y changes. 
#' 
#' @param xl
#' A matrix with the columns of x that do not change. If the
#' intercept term is needed, there must be a columns of 1's
#' in the matrix.
#' @param xr
#' A matrix with the values for the columns that have values
#' changing. All possible values for xr need to be concatenated
#' into one matrix.
#' @param xrcol
#' The number of columns in each xr matrix. The number of columns
#' in the xr matrix passed to the function must be a multiple
#' of xrcol
#' @param y
#' A matrix with all the possible values of y. Each column
#' contains one possible value for y.
#' @return
#' A matrix with the beta values for each model. The first n
#' columns will be for the first value of xr where n is the
#' number of columns in x.
#' @export
LSLinReg <- function(xl, xr, xrcol = 1L, y) {
  if(missing(xl)) {
    print("No value for xl provided")
    return (list())
  }
  if (is.matrix(xl) == FALSE) {
    print("xl is not a matrix")
    return (list())
  }
  if (length(xrcol) != 1) {
    print("xrcol is not a single value")
    return (list())
  }
  print(xrcol)
  if (is.integer(xrcol) == FALSE) {
    if (as.integer(xrcol) != xrcol) {
      print("Value of xrcol must be an integer")
      return (list())
    }
    xrcol = as.integer(xrcol)
    if (xrcol < 0) {
      print("Value for xrcol must be a postive integer")
      return(list())
    }
  }
  if (missing(xr)) {
    if (xrcol != 0 && xrcol != 1) {
      print("Value provided for xrcol but no xr was passed")
      return (list())
    }
    xrcol <- 0
  } else {
    if (is.matrix(xr) == FALSE) {
      print("xr is not a matrix")
      return (list())
    }
    numxr <- as.integer(ncol(xr) / as.numeric(xrcol))
    if (ncol(xr) / as.numeric(xrcol) != numxr) {
      print("Number of columns in xr is not a multiple of xrcol")
      return (list())
    }
    if (nrow(xl) != nrow(xr)) {
      print("Number of rows in xl and xr must be the same")
      return (list())
    }
  }
  if (missing(y)) {
    print("No value of y provided")
    return (list())
  }
  numy <- ncol(y)
  if (nrow(y) != nrow(xl)) {
    print("Number of rows in xl and y must be the same")
    return (list())
  }

  ql <- matrix(0., nrow(xl), ncol(xl))
  rtl <- matrix(0., ncol(xl), ncol(xl))
  return (LargeScaleLinReg:::XLQR(ql, rtl, xl))
}
  