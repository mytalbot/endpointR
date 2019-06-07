#' Loads Data into the endpointeR
#'
#' Data from a *.txt file will be read into the function so that it can be fed to the endpointR.
#' Longitudinal data need to be in the wide format. They must have an index column with animal id,
#' a descriptive column such as treatment. The following columns contain values for consecutive time points. There should be
#' no missing values until the last measured data point.
#'
#' @param file path and filename as a string (e.g. C:/testfolder/testdata.txt)
#'
#' @importFrom utils read.table
#'
#' @return data.frame
#'
#' @export
#'
ep_load      <- function(file){

  testdata   <- data.frame(read.table(file, header = TRUE, sep = "\t") )

  animals    <- length(unique(testdata[,1]))

  # print(paste("Success! Data loaded with n = ", animals, sep=""))

  return(testdata)
}
