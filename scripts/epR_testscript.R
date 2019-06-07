
library(endpointR)

file      <- "C:/MHH Bleich/Papers/Schwabe App/Data/test/Validation_data.txt"
testdata  <- ep_load(file )

a         <- 1 #50
td        <- ep_select(testdata, a) #td        <- ep_select(testdata, "WZ12")

result    <- epR(td        = td,
                 org       = F,
                 wl        = 6,
                 SDwdth    = 2,
                 mad       = F,
                 cex       = 1.4,
                 cex.lab   = 1.2,
                 blind     = F,
                 ignupr    = FALSE,
                 ylim      = c(90,105), #c(300,350),
                 xlim      = c(0,28),
                 xlab      = "day")
result




i =15
td        = as.numeric(ep_select(gliodat, i))
org       = F
wl        = 6
SDwdth    = 2
mad       = F
cex       = 1.4
cex.lab   = 1.2
blind     = F
ignupr    = FALSE
ylim      = c(90,120) #c(300,350),
xlim      = c(0,28)
xlab      = "day"
ylab      = "MA"
cex.axis  = 1
cex.lab   = 1
ltype     = "b"
pch       = 19
dotcolor  = "black"
main      ="test"
uprcol    = "darkgreen"
lwrcol    = "magenta"


