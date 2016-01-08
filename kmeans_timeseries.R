# K-means clustering of timeseries


setwd("~/Documents/SaezGrp/LocalGitRepo/SC_PoLeMo")
source("handleMissingData.R")
load("allData.RData")

plotDir='./inst/plots/'

for(a in 1:length(allData)) {
    
    plot.index = 0
    pdf(file = paste(plotDir, names(allData)[a],"_clustered_reponse",".pdf", sep=""), width=8, height=6)
    
    for(b in 1:length(allData[[a]])) {
        p=list();
        plot.index = plot.index +1 
        datasheet = allData[[a]][[b]]

        time = unique(datasheet$time)
        #ggplot(data = datasheet[,c("time","normValue","traceID")], aes(time, normValue, group=traceID, color=traceID )) + geom_line() 

        mydata <- acast(datasheet, time ~ traceID, value.var = "normValue")
        #mydata <- cbind(time,mydata)
        mydata <- handleMissingData(mydata)
       
        # transpose for clustering
        mydata = t(mydata)

        # Determine number of clusters
        wss = c()
        for (i in 1:5) wss[i] <- sum(kmeans(mydata, 
                                             centers=i,nstart = 20,iter.max = 100)$withinss)
        p[[1]] <- ggplot(data = data.frame(x=1:5,y=wss),aes(x=x,y=y )) + geom_line() + geom_point() +
            xlab("Number of Clusters") + ylab("Within groups sum of squares")

        # K-Means Cluster Analysis of data 
        fit <- kmeans(mydata, 2,iter.max = 5000, nstart = 200, algorithm = "Hartigan-Wong" ) # 2 cluster solutio n    "Hartigan-Wong"

        # melt the data
        mydata = melt(mydata)
        names(mydata) = c("traceID","Time","normValue")
            
        clusters = data.frame(traceID = names(fit$cluster),cluster = fit$cluster)
        clustered.data = join(mydata,clusters, by="traceID")

    
        p[[2]] <-  ggplot(data = clustered.data, aes( colour = as.factor(cluster)) ) + geom_line(aes(x = Time, y = normValue, group=traceID)) + theme(legend.position="none") 
        multiplot(plotlist = p,cols=2)
}

    
    

    
    dev.off()
}