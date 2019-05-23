#' Endpointer function
#'
#' The \code{epR} function is a tool for the identification of humane endpoints using single outcome variables
#' from laboratory animal experiments. Originally, it was developed for using body weight but as values are normalized
#' other continous variables are suited as well. The algorithm is highly functional in identifying strong deviations from
#' a windowed normality. The hypothesis behind this is: larger deviations always point to severity.
#'
#' @param td testdata data.frame with n unique rows and p subsequent time points (e.g. days)
#' @param org boolean (TRUE/FALSE) for using original values. If FALSE, data are normalized.
#' @param wl SD window length (default is 6)
#' @param SDwdth width of the standard deviation around the moving average (default is 2.5)
#' @param mad boolean - use mean absolute deviation as quasi-clinical scoring constraint (default FALSE)
#' @param ltype line type in shown plot
#' @param dotcolor color of the shown dots (default "black")
#' @param cex point size
#' @param cex.axis axis tick size
#' @param cex.lab label size
#' @param xlim range of x-axis (if set to NULL (default), plot will adapt automatically to given range - may not be nice!)
#' @param ylim range of <-axis (if set to NULL (default), plot will adapt automatically to given range - may not be nice!)
#' @param pch R-specific plot symbol for shown dots (default is 19)
#' @param blind boolean (TRUE/FALSE) - if set to TRUE, no plot will be shown (default is FALSE)
#' @param ignupr boolean (TRUE/FALSE) - ignore upper threshold violations (default is FALSE)
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param main title
#'
#' @importFrom stats sd complete.cases
#' @import graphics
#'
#' @return data.frame with enpointeR results (n=number of data points, timepoint=index of marked endpoint,
#' where=upper or lower boundary)
#'
#' @export
#'
#' @examples
#' epR(as.numeric(gliodat[1,3:length(gliodat[1,])]), blind=TRUE )
#'
epR <- function(td = td, org=FALSE, wl = 6, SDwdth = 2.5, mad=TRUE, ltype = "b", dotcolor = "black", cex = 1,
                cex.axis = 1, cex.lab = 1, xlim = NULL, ylim = NULL, pch = 19, blind = FALSE, ignupr = FALSE,
                xlab = "time", ylab = "Moving average (%)", main = NULL){

  # essentials --------------------------------------------------------------
  pointlength    <- length(td)
  xachse         <- seq(1,pointlength)

  if(org == TRUE){
    W            <- td
  }else{
    epdat        <- (td / td[1])*100

  # calculate windowed moving average ---------------------------------------
    W <- c()
    S <- c()
    for (i in wl:pointlength ){
      floeating.mean <- mean(epdat[i:(i-(wl-1))])
      w              <- (epdat[i] / floeating.mean) * 100
      W[i]           <- w
    }
    W[1:wl]<- rep(100,wl)
  }

  # calculate backwards windowed SD and mean --------------------------------
  mysd   <- c()
  mymean <- c()
  for (i in pointlength:wl){
    mySD     <- sd(W[(i-1):(i-wl)],   na.rm=TRUE)
    mysd[i]  <- mySD
    myMean   <- mean(W[(i-1):(i-wl)], na.rm=TRUE)
    mymean[i]<- myMean
  }
  mysd[mysd==0] <- NA # remove 0, otherwise first point gets X-ed
  mysd       <-  mysd*SDwdth

  # calculate MAD -----------------------------------------------------------
  if(mad==TRUE){
  myMAD <- c()
  for (i in wl:pointlength ){
    myMAD[i] <- mean(W[(i-1):((i-1)-(wl-1))]) - W[i]
  }
  myMAD      <- round(myMAD,2)

  # constrain SD by MAD -----------------------------------------------------
  madscore   <- ceiling(abs(myMAD))
  madscore[madscore == 0] <-1
  mysd       <- mysd / madscore
  }else{}

  # plotting ----------------------------------------------------------------
  if(blind==FALSE){
    plot(xachse,
         W,
         type     = ltype,
         pch      = pch,
         col      = dotcolor,
         xlim     = xlim,
         ylim     = ylim,
         cex.lab  = cex.lab,
         cex.axis = cex.axis,
         cex      = cex,
         xlab     = xlab,
         ylab     = ylab,
         main     = main)

    lines(xachse, mymean+mysd, col="blue", lty=2, cex=.5, lwd=2.5)
    lines(xachse, mymean-mysd, col="blue", lty=2, cex=.5, lwd=2.5)

    abline(h=100, col="black",  lwd=2, lty=3)
    abline(v=wl , col="gray40", lwd=1, lty=3)

    points(which(W< mymean-mysd), W[which(W< mymean-mysd)], col="magenta",   pch=4, cex=3, lwd=3)
    points(which(W> mymean+mysd), W[which(W> mymean+mysd)], col="darkgreen", pch=4, cex=3, lwd=3)

  }else{}

  # diagnostics -------------------------------------------------------------
  result <- NULL
  if(ignupr==TRUE){
    lower            <- which(W < mymean - mysd)
    lower.time.idx   <- if(length(lower) == 0) NA else lower

    result           <- data.frame(n = length(W), timepoint = lower.time.idx)
    result$where     <- rep("lower", length(lower.time.idx))
  }else{
    lower            <- which(W < mymean - mysd)
    lower.time.idx   <- if(length(lower) == 0) NA else lower

    result           <- data.frame(n = length(W), timepoint = lower.time.idx)
    result           <- result[order(result$timepoint, decreasing = F),]

    upr              <- which(W > mymean + mysd)
    upr.time.idx     <- if(length(upr) == 0) NA else upr

    result           <- rbind(result, data.frame(n = length(W), timepoint = upr.time.idx))
    result$where     <- append(rep("lower", length(lower.time.idx)), rep("upper", length(upr.time.idx)) )
  }

  if(all(is.na(result$timepoint)) == TRUE){
    result           <- data.frame(n = length(W), timepoint = NA, where = NA)
  }else{
    result           <- result[complete.cases(result), ]
  }

  return(result)
}
