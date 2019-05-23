#' Animal selection for endpointR analysis
#'
#' @return vector of a single animal for distinct time points.
#'
#' @export
#'
#' @examples
#'
#'
#'
ep_select    <- function(testdata, n=1  ){

  test       <- testdata[n,3:length(testdata)]
  orgw       <- as.numeric(test)
  orgw       <- orgw[!is.na(orgw)]

  return(orgw)
}
