# plot population avareged (median) cell responses.
require(devtools)
require(stringr)
require(readxl)
require(plyr)
require(ggplot2)
require(dplyr)
require(plot3D)

source('~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput/R/plotAll2.R')  # updated plot
source('~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput/R/parseImageJ2.R') # updated import
source('~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput/R/multiplot.R') # updated import
source('~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput/R/meanFRET2.R')

setwd("~/Documents/SaezGrp/LocalGitRepo/SC_PoLeMo/")
document(".")
load_all(".")

load("~/Documents/SaezGrp/LocalGitRepo/SC_PoLeMo/vignette-4_results.RData")
#load("response_distribution.RData")
plotDir='./inst/plots/'

plot.index = 0;
## plot all cell response
for(irep in 1:length(allData)){


 p=list();
  plot.index = 0

  
 pdf(file = paste(plotDir, names(allData)[irep],"_contours",".pdf", sep=""), width=8, height=6)
  
for(jstim in 1:length(allData[[irep]])){

    plot.index = plot.index + 1;
    Time  = unique(allData[[irep]][[jstim]]$time)
    bins = seq(from = 0.5,to = 2.0, by = 0.02) 
    
    resp_distr[[irep]][[jstim]]$counts = matrix(nrow = length(Time),ncol = length(bins)-1)
    resp_distr[[irep]][[jstim]]$breaks = matrix(nrow = length(Time),ncol = length(bins))
    resp_distr[[irep]][[jstim]]$mids = matrix(nrow = length(Time),ncol = length(bins)-1)
    
    for(t in 1:length(Time)){
        
        vals <- subset(allData[[irep]][[jstim]], time==Time[t]  ,select = normValue)  
        
        # report outliars
        if(any(vals$normValue > 3.0,na.rm = TRUE)) print("value greater than 3 detected")
        
        
        # we use equidistant bins to determine the empirical distribution (counts)
        h = hist(vals$normValue[min(bins)<=vals$normValue & vals$normValue <= max(bins)],breaks=bins,plot=FALSE,right=TRUE,include.lowest = TRUE)  
        resp_distr[[irep]][[jstim]]$counts[t,] = h$counts
        resp_distr[[irep]][[jstim]]$breaks[t,] = h$breaks
        resp_distr[[irep]][[jstim]]$mids[t,] = h$mids
    }
    
    # remove the first time point since the data was normalized by that (all data are 1)
    C = resp_distr[[irep]][[jstim]]$counts[2:91 ,]   
    M = resp_distr[[irep]][[jstim]]$mids[2:91,]
    T = matrix(Time[2:91],nrow=nrow(C),ncol=ncol(C))/60    # scale to minutes\
    
    #OPTION 1: 
    ## 3D Interactive (rotatable) plot
#     require(rgl)
#     open3d()
#     colorlut <- cm.colors(max(C,na.rm = TRUE)-min(C,na.rm = TRUE), alpha = 0.50)
#     col <- colorlut[ C - min(C,na.rm = TRUE) + 1 ] 
#     x = as.vector(M[,1])
#     y = as.vector(T[1,])
#     z = C
#     surface3d(x,y , z,color=col,alpha=0.5)
#     aspect3d(x=1, y = 1, z = 2)
#     axes3d(edges = "bbox", labels = TRUE, tick = TRUE, nticks = 5,
#            box = TRUE, expand = 1.13)
    
    #OPTION 2:
    ## 3D perspective plot with contour
#  require(plot3D)    
#    persp3D(z=C, x=M, y=T, 
#             clim = c(0.5,max(C)), 
#             ticktype = "detailed",shade = 0.1,
#             xlab="FRET ratio",ylab="time (min)",zlab="counts",
#             xlim=c(0.5,2.0),zlim=c(-15,max(C)), phi = 15, theta = -40, alpha = 0.5)
#   contour3D(z=-14,x=M[1,], y=T[,1], colvar = t(C), add = TRUE,clim =c(0.5,max(C)),colkey = FALSE )
#     
    #OPTION 3:
   ## Filled contour is also nice:
    #contour2D(z = t(C), x=M[1,], y=T[,1], add = FALSE,clim =c(0.5,max(C)),colkey = TRUE, xlab="FRET ratio",ylab="time [min]",resfac = 1) 
    filled.contour(z = t(C), x=M[1,], y=T[,1], xlab="FRET ratio",ylab="time [min]",zlim = c(1,max(C)) ,levels = pretty(c(1,max(C)), max(C)-1), nlevels = max(C)-1,color = heat.colors)
    title( paste(names(allData)[irep],names(allData[[irep]])[jstim],sep="_") )
  
    }
  
 
  
  #multiplot(plotlist = p,cols=3)
  dev.off()
}
