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

  if(is.numeric(n)==TRUE){
    test       <- testdata[n,3:length(testdata)]
    orgw       <- as.numeric(test)
    orgw       <- orgw[!is.na(orgw)]
  }else{
    test       <- testdata[testdata[,1] == n,]
    orgw       <- as.numeric(test)
    orgw       <- orgw[!is.na(orgw)]
    n          <- which(testdata[,1] == n)
  }

  print( data.frame( testdata[n, c(1,2) ], datapoints=length(orgw)) )

  return(orgw)
}
