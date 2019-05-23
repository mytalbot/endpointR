#' endpointR
#'
#' @return vector of a single animal for distinct time points.
#'
#' @export
#'
#'
#'
#'
endpointR        <- function(df=orgw, dnorm=TRUE, wl=6, SDfaktor =2,
                             ltype="b", dotcolor="black", mycex=1.4, showOrgBW=1 ){

  # essentials --------------------------------------------------------------
  pointlength    <- length(df)
  xachse         <- seq(1,pointlength)

  # decision: normalize bw or not -------------------------------------------
  if(dnorm!=TRUE){
    epdat        <- orgw
  }else{
    epdat        <- (orgw / orgw[1])*100
  }

  # calculate windowed moving average ---------------------------------------
  W <- c()
  S <- c()
  for (i in wl:pointlength ){
    floeating.mean <- mean(epdat[i:(i-(wl-1))])
    w              <- (epdat[i] / floeating.mean) * 100
    W[i]           <- w
  }
  W[1:wl]<- rep(100,wl)

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
  if(mad==1){
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






}
