
library(endpointR)

tp         <- c()
fp         <- c()
no_fa      <- c()
SP         <- NULL
for(i in 1:63){
  SP       <- NULL
  SP       <- epR(as.numeric(ep_select(gliodat, i)), SDwdth = 1, blind = TRUE )

  # if default... -----------------------------------------------------------
  default    <- ifelse((length(SP) == 0) == TRUE,1,0)
  if(default == 1){
    tp[i]    <- NA
    fp[i]    <- NA
    no_fa[i] <- NA
  }else{
    tp[i]    <- ifelse(sum(SP$timepoint       == unique(SP$n), na.rm = TRUE) == 1, 1, 0)
    fp[i]    <- ifelse( (length(SP$timepoint) != unique(SP$n)) == TRUE, 1, 0)
    no_fa[i] <- sum(SP$timepoint              != unique(SP$n), na.rm = TRUE)
  }
}
ep_perf      <- data.frame(tp=tp, fp=fp, fa=no_fa)
ep_perf

data.frame(mean_tp=mean(ep_perf$tp), mean_fa = mean(ep_perf$fa), mean_nofa= mean(ep_perf$fa))




