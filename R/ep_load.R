#' Loads data into the endpointeR
#'
#' @return data.frame
#'
#' @export
#'
#' @examples
#'
#'
#'
ep_load      <- function(file  ){

  testdata   <- data.frame(read.table(file, header =TRUE, sep ="\t") )

  return(testdata)
}
