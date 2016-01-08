# K-means clustering of timeseries


setwd("~/Documents/SaezGrp/LocalGitRepo/SC_PoLeMo")
source("handleMissingData.R")
load("allData.RData")

plotDir='./inst/plots/'

for(a in 1:length(allData)) {
    p=list();
    plot.index = 0
    
    for(b in 1:length(allData[[a]])) {
        plot.index = plot.index +1 
        datasheet = allData[[a]][[b]]

        utraceID =  unique(datasheet$traceID)

        time = unique(datasheet$time)
        #ggplot(data = datasheet[,c("time","normValue","traceID")], aes(time, normValue, group=traceID, color=traceID )) + geom_line() 

        mydata= subset(x=datasheet,subset=traceID==utraceID[1],select="normValue")
        usedtrace = utraceID[[1]]

        for(iID in 2:length(utraceID)){   #
            mydata= cbind(mydata,subset(x=datasheet,subset=traceID==utraceID[iID],select="normValue"))
            usedtrace = cbind(usedtrace,utraceID[[iID]])
        }
    names(mydata)<- levels(utraceID)[usedtrace]

    
    mydata <- handleMissingData(mydata)
# mydata.na <- na.omit(mydata) # listwise deletion of missing
#mydata <- scale(mydata) # standardize variables

# transpose for clustering
mydata = t(mydata)

# Determine number of clusters
# wss <- (ncol(mydata)-1)*sum(apply(mydata,1,var))
# for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
#                                      centers=i,nstart = 20,iter.max = 100)$withinss)
# plot(1:15, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 2,iter.max = 5000, nstart = 200, algorithm = "Hartigan-Wong" ) # 2 cluster solutio n    "Hartigan-Wong"
 # get cluster means 
#aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
#mydata.clustered <- data.frame(mydata, fit$cluster)


#clustered.data = data.frame(subset(datasheet,subset = is.element(datasheet$traceID, levels(utraceID)[usedtrace])),rep(fit$cluster,each = length(time)))

clustered.data = melt(mydata )
t = names(clustered.data)
t[[12]] = "clusterID"
names(clustered.data) = t

p[[plot.index]] <-  ggplot(data = clustered.data, aes( colour = as.factor(clusterID)) ) + geom_line(aes(x = time, y = normValue, group=traceID)) + theme(legend.position="none")
}

    
    pdf(file = paste(plotDir, names(allData)[a],"_clustered_reponse",".pdf", sep=""), width=8, height=6)

    multiplot(plotlist = p,cols=3)
    dev.off()
}