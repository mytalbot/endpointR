

library(endpointR)
library(RELSA)

# Load Laura Act Data -----------------------------------------------------
raw       <- relsa_load("C:/MHH Bleich/Papers/PostOPPaper/Data/svenja_dss_mod.txt" )
raw       <- raw[, c(1, 2, 4, 5)]

# Chose animal ------------------------------------------------------------
a         <- 31
dat       <- raw[raw$id==unique(raw$id)[a],  c(1,3,4)]
td        <- reshape(dat, idvar = "id", timevar = "day", direction = "wide")
td        <- as.numeric(td[2:length(td)])

result    <- epR(td        = td,
                 org       = T,
                 wl        = 2,
                 SDwdth    = 2,
                 mad       = T,
                 cex       = 1.4,
                 cex.lab   = 1.2,
                 blind     = F,
                 ignupr    = TRUE,
                 ylim      = c(70,120), #c(300,350),
                 xlim      = c(0,20),
                 xlab      = "day")
print(result)


td        = td
org       = T
wl        = 2
SDwdth    = 2
mad       = F
cex       = 1.4
cex.lab   = 1.2
blind     = F
ignupr    = TRUE
ylim      = c(70,120)
xlim      = c(0,20)
xlab      = "day"

