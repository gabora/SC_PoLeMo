# K-means clustering of timeseries


setwd("~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput")

load("allData.RData")


datasheet = allData[["Cdc42"]][["IGF"]]

utraceID =  unique(datasheet$traceID)

time = unique(datasheet$time)
#ggplot(data = datasheet[,c("time","normValue","traceID")], aes(time, normValue, group=traceID, color=traceID )) + geom_line() 



mydata= subset(x=datasheet,subset=traceID==utraceID[1],select="normValue")
usedtrace = utraceID[[1]]

for(iID in 2:length(utraceID)){   # 
    tmp = subset(x=datasheet,subset=traceID==utraceID[iID],select="normValue")
    
     if (any(is.na(tmp ))) { next}
    
    mydata= cbind(mydata,tmp)
    usedtrace = cbind(usedtrace,utraceID[[iID]])
}
names(mydata)<- levels(utraceID)[usedtrace]

# mydata.na <- na.omit(mydata) # listwise deletion of missing
#mydata <- scale(mydata) # standardize variables



# FFT columnwise
f = mvfft(as.matrix(mydata-1))
f2 = data.frame(f)
f3 = data.frame(melt(f2),rep(1:91,ncol(mydata)))
names(f3)= c("id","value","freq")

#ggplot() + geom_line(data = f3,aes(x=freq,y=abs(value),group=as.factor(id),color=id)) + xlim(0,10)


#change the data to fourier coefficients
mydata = t(abs(f))

# Determine number of clusters
wss <- (ncol(mydata)-1)*sum(apply(mydata,1,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i,nstart = 20,iter.max = 100)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 2,iter.max = 5000, nstart = 200, algorithm = "Hartigan-Wong" ) # 2 cluster solutio n    "Hartigan-Wong"
 # get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata.clustered <- data.frame(mydata, fit$cluster)


clustered.data = data.frame(subset(datasheet,subset = is.element(datasheet$traceID, levels(utraceID)[usedtrace])),rep(fit$cluster,each = length(time)))

t = names(clustered.data)
t[[12]] = "clusterID"
names(clustered.data) = t

ggplot(data = clustered.data, aes( colour = as.factor(clusterID)) ) + geom_line(aes(x = time, y = normValue, group=traceID)) 
