# compute the distribution of the cell response data along time
# we dont use it any more, we compute before the plotting to easily adjust the resolution of the bining.
require(devtools)
require(stringr)
require(readxl)
require(plyr)
require(ggplot2)
require(dplyr)



setwd("~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput")
document(".")
load_all(".")

load("vignette-4_results.RData")

resp_distr = vector("list", length(modelNames))
names(resp_distr) = modelNames

for(irep in 1:length(allData)){

    resp_distr[[irep]] = vector("list", length(expers))
    names(resp_distr[[irep]]) = expers
    
    for(jstim in 1:length(allData[[irep]])){
        
        Time  = unique(allData[[irep]][[jstim]]$time)
        resp_distr[[irep]][[jstim]]$time = Time
        bins = seq(from = 0.0,to = 3, by = 0.01)
        resp_distr[[irep]][[jstim]]$bins = bins
        
        resp_distr[[irep]][[jstim]]$counts = matrix(nrow = length(Time),ncol = length(bins)-1)
        resp_distr[[irep]][[jstim]]$breaks = matrix(nrow = length(Time),ncol = length(bins))
        resp_distr[[irep]][[jstim]]$mids = matrix(nrow = length(Time),ncol = length(bins)-1)
        
        for(t in 1:length(Time)){
            
            vals <- subset(allData[[irep]][[jstim]], time==Time[t]  ,select = normValue)  
            
            # report outliars
            if(any(vals$normValue > 3.0,na.rm = TRUE)) print("value greater than 3 detected")
            
            
            # we use equidistant bins to determine the empirical distribution (counts)
            h = hist(vals$normValue[vals$normValue <= max(bins)],breaks=bins,plot=FALSE,right=TRUE,include.lowest = TRUE)  
            resp_distr[[irep]][[jstim]]$counts[t,] = h$counts
            resp_distr[[irep]][[jstim]]$breaks[t,] = h$breaks
            resp_distr[[irep]][[jstim]]$mids[t,] = h$mids
            }
    }
}
save(list = c("resp_distr","bins"),file="response_distribution.RData")
