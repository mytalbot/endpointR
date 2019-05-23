#' Animal Selection for endpointR Analysis
#'
#' Using the testdata output from \code{ep_load}, the \code{ep-select} function subsets the testdata by a
#' user-defined unique animal index. If the input is numeric, e.g. n=1 the first animal in testdata will be chosen. If the
#' input is a character string, e.g. "ZW21", this specific animal from the testdata will be chosen. Animal ids should not be numeric.
#'
#' @param testdata data.frame with test data as provided by \code{ep_load}
#' @param n animal index (numeric or character (animal id))
#'
#' @return vector of a single animal for distinct time points.
#'
#' @export
#'
#' @examples
#' ep_select(gliodat, n = 1)
#'
ep_select      <- function(testdata, n = 1){

  if(is.numeric(n) == TRUE){
    test       <- testdata[n,3:length(testdata)]
    orgw       <- as.numeric(test)
    orgw       <- orgw[!is.na(orgw)]
  }else{
    test       <- testdata[testdata[,1] == n,]
    orgw       <- as.numeric(test)
    orgw       <- orgw[!is.na(orgw)]
    n          <- which(testdata[,1] == n)
  }

  print( data.frame(testdata[n, c(1,2) ], datapoints = length(orgw)) )

  return(orgw)
}
