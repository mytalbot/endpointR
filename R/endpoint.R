#' endpointR
#'
#' @return vector of a single animal for distinct time points.
#'
#' @export
#'
endpointR        <- function(td = td, org=FALSE, wl = 6, SDfaktor = 2, mad=TRUE, ltype = "b", dotcolor = "black", cex = 1,
                             cex.axis = 1, cex.lab = 1, xlim = NULL, ylim = NULL, pch = 19,
                             blind = F, ignupr =F, xlab = "time", ylab = "Moving average (%)", main = NULL, ... ){

  # essentials --------------------------------------------------------------
  pointlength    <- length(td)
  xachse         <- seq(1,pointlength)

  if(org==TRUE){
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


  # Calculate backwards windowed SD and mean --------------------------------
  mysd   <- c()
  mymean <- c()
  for (i in pointlength:wl){
    mySD     <- sd(W[(i-1):(i-wl)],   na.rm=TRUE)
    mysd[i]  <- mySD
    myMean   <- mean(W[(i-1):(i-wl)], na.rm=TRUE)
    mymean[i]<- myMean
  }
  mysd[mysd==0] <- NA # remove 0, otherwise first point gets X-ed
  mysd       <-  mysd*SDfaktor

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
    lower            <- which(W< mymean-mysd)
    lower.time.idx   <- if(length(lower)==0) NA else lower

    result           <- data.frame(n = length(W), timepoint = lower.time.idx)
    result$where     <- rep("lower", length(lower.time.idx))
  }else{
    lower            <- which(W< mymean-mysd)
    lower.time.idx   <- if(length(lower)==0) NA else lower

    result           <- data.frame(n = length(W), timepoint = lower.time.idx)
    result           <- result[order(result$timepoint, decreasing = F),]

    upr              <- which(W> mymean+mysd)
    upr.time.idx     <- if(length(upr)==0) NA else upr

    result           <- rbind(result, data.frame(n = length(W), timepoint = upr.time.idx))
    result$where     <- append(rep("lower", length(lower.time.idx)), rep("upper", length(upr.time.idx))  )
  }

  if(all(is.na(result$timepoint))==TRUE){
    result           <- data.frame(n = length(W), timepoint = NA, where = NA)
  }else{
    result           <- result[complete.cases(result), ]
  }

  return(result)
}
