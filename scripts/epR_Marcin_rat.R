
library(readxl)
library(endpointR)
library(RELSA)

# Load Marcin Rat Data ----------------------------------------------------
setwd("C:/Users/talbotst/Desktop/Test")
filename      <- "C:/MHH Bleich/Aktuelles/Vienna/data/severity_assessment_project__test_data_from_LBI/DB_FORm_Vienna_rat HS.xlsx"
marcin_rat    <-  data.frame(read_excel(filename , sheet = 4))


# filter ------------------------------------------------------------------
tiere         <- unique(marcin_rat$animal_id)

for(i in 1:length(tiere)){
n             <- i
td            <- marcin_rat[marcin_rat$animal_id==tiere[n], ]$bw
  if(length(td)<6){

  }else{
    f_name       <- paste("marcin_rat ID ", tiere[i],".tiff",sep="")
    tiff(file = f_name, width = 1000, height = 1000, units = "px", pointsize = 7, res = 300, compression = "lzw")

    result    <- epR(td        = td,
                     org       = F,
                     wl        = 2,
                     SDwdth    = 1,
                     mad       = F,
                     cex       = 1.4,
                     cex.lab   = 1.2,
                     blind     = F,
                     ignupr    = T,
                     ylim      = c(90,115), #c(300,350),
                     xlim      = c(0,16),
                     xlab      = "day")
    dev.off()
    }
}



# RELSA -------------------------------------------------------------------
library(readxl)
library(endpointR)
library(RELSA)

setwd("C:/Users/talbotst/Desktop/Test")

# Baseline RELSA ----------------------------------------------------------
raw          <- relsa_load("C:/MHH Bleich/Papers/PostOPPaper/Data/post-op laura.txt", treatment = "Transmitter" )
vars         <- c("bwc", "bur2h","burON","hr","hrv", "temp", "act", "mgs")
turnvars     <- c("hr", "mgs", "temp" )
org          <- cbind(raw[,1:4], raw[,vars])
pre          <- relsa_norm(org,   normthese=c("bur2h","burON","hr","hrv", "temp", "act", "mgs" ), ontime=1)
bsl          <- relsa_baselines(dataset=pre, bslday=-1, variables=vars, turnvars=turnvars)

levels       <- relsa_levels(pre, mypath="C:/MHH Bleich/Papers/PostOPPaper/Relsa package/paper figures/Clusters/", bsl,
                             filename="Burrowing levels", drops=c("bw","score"), turns=c("hr","mgs","temp"), relsaNA=NA, k=4,
                             showScree="no", customCol= c("red","green","blue","magenta"), seed=123, myYlim=c(0,1.4),
                             saveTiff="yes")


# Marcin Rat Testdata ------------------------------------------------------
testraw      <- relsa_load("C:/MHH Bleich/Aktuelles/Vienna/data/severity_assessment_project__test_data_from_LBI/DB_FORm_Vienna_rat HS.txt" )
vars         <- names(testraw[,-c(1:4)])
pre_test     <- cbind(testraw[,1:4], testraw[,vars])
testset      <- relsa_norm(pre_test, normthese=c("bwc","temp"), ontime=1)

animal       <- unique(testset$id)

for(i in 1:length(animal)){
   if( dim(testset[testset$id==animal[i], ])[1] < 2 ) {

   }else{
     RELSA        <- relsa(testset, bsl, a=i, drop=NULL, turnvars="temp", relsaNA=NA)

     label        <- as.character(testset[testset$id==animal[i], ]$treatment[1])

     f_name       <- paste("C:/Users/talbotst/Desktop/Test/",  animal[i],".tiff",sep="")
     tiff(file = f_name, width = 1000, height = 1000, units = "px", pointsize = 6, res = 400, compression = "lzw")

     plotset      <- relsa_plot(testset, RELSA, levels=levels, animal=animal, plotvar=NULL,
                                plotRELSA=TRUE, myylim=c(0,130), myYlim=c(0,1.5), myXlab="day")
     title( paste("ID ", animal[i]," / ", label, sep="") )


     dev.off()
   }
}





