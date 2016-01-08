require(devtools)
require(stringr)
require(readxl)
require(plyr)
require(ggplot2)
require(dplyr)

source('~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput/R/plotAll2.R')  # updated plot
source('~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput/R/parseImageJ2.R') # updated import
source('~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput/R/multiplot.R') # updated import

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
    p[[plot.index]] <- ggplot(allData[[a]][[b]]) + geom_line(size=.2,aes(x=time, y=normValue, group=traceID)) +xlab("") + ylab("")+ ylim(0,2) + ggtitle(names(allData[[a]])[[b]]) 
    
  }
  plotDir='./inst/plots/'
  pdf(file = paste(plotDir, names(allData)[a],".pdf", sep=""), width=8, height=6)
  #print(p)
  
   multiplot(plotlist = p,cols=3)
  dev.off()
}


