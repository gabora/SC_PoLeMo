# manual clustering
# do the blue and red curves have different dynamics?

setwd("~/Documents/SaezGrp/LocalGitRepo/SC_PoLeMo")
source("handleMissingData.R")
load("allData.RData")

plotDir='./inst/plots/'

a=1
b=1 

        datasheet = allData[[a]][[b]]
        
        time = unique(datasheet$time)

        mydata <- acast(datasheet, time ~ traceID, value.var = "normValue")
        mydata <- handleMissingData(mydata)
        
        mycluster = mydata[5,]<1.15
       
        mydata = t(mydata)
        
        # Determine number of clusters
        # wss <- (ncol(mydata)-1)*sum(apply(mydata,1,var))
        # for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
        #                                      centers=i,nstart = 20,iter.max = 100)$withinss)
        # plot(1:15, wss, type="b", xlab="Number of Clusters",
        #     ylab="Within groups sum of squares")
        
        # K-Means Cluster Analysis of data 
        fit <- kmeans(mydata, 2,iter.max = 5000, nstart = 200, algorithm = "Hartigan-Wong" ) # 2 cluster solutio n    "Hartigan-Wong"
        
        
        #clustered.data = data.frame(subset(datasheet,subset = is.element(datasheet$traceID, levels(utraceID)[usedtrace])),rep(fit$cluster,each = length(time)))
        # melt the data
        mydata = melt(mydata)
        names(mydata) = c("traceID","Time","normValue")
        
        clusters = data.frame(traceID = names(fit$cluster),cluster = fit$cluster)
        clustered.data = join(mydata,clusters, by="traceID")
        
        clusters2 = data.frame(traceID = names(mycluster),cluster2 = mycluster)
        clustered.data = join(clustered.data,clusters2, by="traceID")
        
        #p[[plot.index]] <-  
        ggplot(data = clustered.data, aes( colour = as.factor(cluster2)) ) + geom_line(aes(x = Time, y = normValue, group=traceID)) + theme(legend.position="none") 
    