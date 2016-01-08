# plot population avareged (median) cell responses.
require(devtools)
require(stringr)
require(readxl)
require(plyr)
require(ggplot2)
require(dplyr)

source('~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput/R/plotAll2.R')  # updated plot
source('~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput/R/parseImageJ2.R') # updated import
source('~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput/R/multiplot.R') # updated import
source('~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput/R/meanFRET2.R')

setwd("~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput")
document(".")
load_all(".")

load("vignette-4_results.RData")

plot.index = 0;
## plot all cell response
for(a in 1:length(allData)){
  p=list();
  plot.index = 0
  for(b in 1:length(allData[[a]])){
    plot.index = plot.index + 1;
    mOutput <- meanFRET2(fretInput = allData[[a]][[b]],metric = "median")
    #p[[plot.index]] <- ggplot(mOutput) + geom_line(aes(x=time, y=median)) +xlab("") + ylab("") + ggtitle(names(allData[[a]])[[b]]) #
    #p[[plot.index]] <- ggplot(mOutput,aes(x=time, y=median)) + geom_errorbar(aes(ymin=median-se, ymax=median+se), width=.1) + geom_line() + geom_point() +xlab("") + ylab("") + ggtitle(names(allData[[a]])[[b]]) 
    p[[plot.index]] <- ggplot(mOutput,aes(x=time/60, y=median, ymin=median-se, ymax=median+se)) + 
                        geom_line() + 
                        geom_ribbon(alpha=0.5) +
                        xlab("") + ylab("") + ggtitle(names(allData[[a]])[[b]]) 
  }
  plotDir='./inst/plots/'
  pdf(file = paste(plotDir, names(allData)[a],"pop_median",".pdf", sep=""), width=8, height=6)
  #print(p)
  
  multiplot(plotlist = p,cols=3)
  dev.off()
}
