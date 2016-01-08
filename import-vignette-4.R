# plot the cellular stimulus reponse.
# for each protein a new pdf is created which contains the reposnd to each stimulus.

require(devtools)
require(stringr)
require(readxl)
require(plyr)
require(ggplot2)
require(dplyr)

source('~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput/R/plotAll2.R')  # updated plot
source('~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput/R/parseImageJ2.R') # updated import

# setwd("~/Documents/workspace/svn2/sysbiomed/trunk/macroInput/")
setwd("~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput")
document(".")
load_all(".")


# setup import
channel = "YFP-CFP-ratio"

# get directories (they contain experment sets)
fretDir = c("inst/dataFrom20140515/", "inst/dataFrom20140605/", "inst/dataFrom20140618/", "inst/dataFrom20140629/","inst/dataFrom20140727/")

# get sensor filenames for each experiment set
lFiles = sapply(fretDir, function(x) grep(".csv", list.files(x), value=TRUE)) 

# get the sensor names in the filenames (pattern: [experiment]_[sensor].csv)
lFilesSensors = lapply(lFiles, function(x) str_replace(x, "^.*_(.*)\\.csv$", "\\1"))

lookup = read_excel("fretLookup.xlsx") # file for mapping sensors to readouts

modelNames = filter(lookup, `Map All`==1) %>% select(`FRET name`)
modelNames = modelNames$`FRET name`
allData = vector("list", length(modelNames))
names(allData) = modelNames

# import, parse, pick time-frame, and plot the data

for(a in 1:length(modelNames)) {
  
  print(modelNames[a])
  # check that there is any data for this sensor or else skip the sensor
  if(!any(unlist(lFilesSensors)==modelNames[a])) {
    print(paste("No data for sensor:", modelNames[a]))
    next
  }
  
  # read in sensor file from directories
  dirCount=0 # flag
  for(k in 1:length(fretDir)) {
    if(any(lFilesSensors[[k]]==modelNames[a])) { # check file for sensor is in directory k
      dat1 = read.csv(paste(names(lFiles)[k],lFiles[[k]][which(lFilesSensors[[k]]==modelNames[a])],sep=""))
      dat1 = parseImageJ2(dat1, whichChannel=channel, stimFrame=10) # parse data and pick channel
      dirCount = dirCount+1
      
      if(dirCount==1) {
        dat2 = dat1
      } else { 
        dat2 = rbind(dat2, dat1); dirCount=dirCount+1
      }
    }
  }
  
  # there can be a number of experiments in each set
  expers = unique(dat2$stimulus)
  allData[[a]] = vector("list", length(expers))
  names(allData[[a]]) = expers
  
  for(b in 1:length(allData[[a]])) {
    
    dat3 = subset(dat2, stimulus==expers[b])
    # TODO check position again with dima
    dat3 = removeNoise(dat3) # remove noise
    
    # add cluster information
    # do this before 'timeFixFRET' as baseline information is needed
    # dat2 = clusterFRET(dat2, endTime=10000, centroidDist=0.4)
  
    # pick time
    dat3 = timeFixFRET(dat3, endTime=17820)
    dat3$readout = names(allData)[a]
  
    if(any(is.nan(dat3$normValue))) {
      print(paste(names(allData)[a]," has ",length(dat3$normValue[is.nan(dat3$normValue)])," NaN values!", sep=""))
    }
  
    allData[[a]][[b]] = dat3
#     plotDir='./inst/plots/'
#     p <- plotAll2(dat3, title=paste(names(allData)[a],": ",expers[b],sep=""), baseSize=20)
#     pdf(paste(plotDir, names(allData)[a], "_", expers[b],".pdf", sep=""), width=8, height=6)
#     print(p)
#     dev.off()
  }
}

# summary
lapply(allData, summary)

# make midas
readoutsForMIDAS = names(allData) # everything
# readoutsForMIDAS = c("akt","rac_cdc42","egfr","erk12","jnk","ras","p70s6_1")

# USER EDIT ---------------------------------------------------------------
allExpers = unique(unlist(lapply(allData, names)))
inhibIx = 5:7 # in allExpers, which are inhibitors?
expersToInclude = 1:length(allExpers)
# /USER EDIT/ -------------------------------------------------------------

treatmentGroups = list("egf"=c(1:7,10:11), "pi3ki"=c(5:7), "igf"=c(8:11))
allMidas = writeFRETmidas(allData, readoutsForMIDAS, allExpers, inhibIx, expersToInclude, treatmentGroups, FALSE, "MIDAS/fret3/test.csv")

