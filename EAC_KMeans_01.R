# data about the mcdonalds menu comes from: http://blog.echen.me/2012/03/20/infinite-mixture-models-with-nonparametric-bayes-and-the-dirichlet-process/
# synthetic data comes from: http://cs.joensuu.fi/sipu/datasets/ 

#-------------------------------
# load required libraries
#-------------------------------
require(ggplot2)
require(reshape)
require(cluster)

#-------------------------------
# define methods for EAC
#-------------------------------
createCoAssocMatrix <- function (Iter, rangeK, dataSet) {
  nV <- dim(dataSet)[1]
  CoAssoc <- matrix(rep(0,nV*nV), nrow = nV)
  k_iter <- numeric(Iter)
  
  for(j in 1:Iter){
    jK <- sample(c(rangeK[1]:rangeK[2]),1, replace = FALSE)
    cat("Iteration j=",j," k choosen ",jK,"\n")
    jSpecCl <- kmeans(x = dataSet, centers = jK)$cluster
    CoAssoc_j <- matrix(rep(0,nV*nV), nrow = nV)
    for(i in unique(jSpecCl)){
      indVenues <- which(jSpecCl == i)
      CoAssoc_j[indVenues,indVenues] <- CoAssoc_j[indVenues,indVenues] + (1/Iter)
    }
    CoAssoc <- CoAssoc + CoAssoc_j
  }
  return(CoAssoc)
}

eac <- function(Iter, rangeK, dataset, hcMethod = "single"){
  CoAssocSim <- createCoAssocMatrix(Iter, rangeK, dataset)
  
  # determine the cut
  CoAssocDist <- 1 - CoAssocSim
  hclustM <- hclust(as.dist(CoAssocDist), method = hcMethod)
  cutValue <- hclustM$height[which.max(diff(hclustM$height))]
  return(cutree(hclustM, h = cutValue))
}

#-------------------------------
# load and prepare data
#-------------------------------
setwd("C:/Users/rroesler/R_Workspace/R_BLOG/")

# Dataset 1 - Aggregation dataset
dataAggregatione <- read.csv("DATA/Aggregation.txt", sep="\t", header=FALSE)
dataAggregationeScaled <- scale(dataAggregatione[,-3]) # normalize data
dataAggregatione <- data.frame(dataAggregationeScaled, name = as.character(c(1:nrow(dataAggregationeScaled))))
rownames(dataAggregatione) <- dataAggregatione$name
ggplot(dataAggregatione, aes(V1, V2)) + geom_point()

# Dataset 2 - Spiral dataset
dataSpiral <- read.csv("DATA/spiral.txt", sep="\t", header=FALSE)
dataSpiralScaled <- scale(dataSpiral[,-3]) # normalize data
dataSpiral <- data.frame(dataSpiralScaled, name = as.character(c(1:nrow(dataSpiralScaled))))
rownames(dataSpiral) <- dataSpiral$name
ggplot(dataSpiral, aes(V1, V2)) + geom_point()

# Dataset 3 - the mcdonalds menu (see blogpost 
# http://blog.echen.me/2012/03/20/infinite-mixture-models-with-nonparametric-bayes-and-the-dirichlet-process
# for another clustering
dataMC <- read.csv("DATA/mcdonalds-normalized-data.tsv")
dataMC$total_fat <- as.numeric(dataMC$total_fat)
numericAttr <- c("total_fat","cholesterol", "sodium", "dietary_fiber", "sugars",
                 "protein", "vitamin_a_dv", "vitamin_c_dv", "calcium_dv",
                 "iron_dv", "calories_from_fat", "saturated_fat", "trans_fat",
                 "carbohydrates")
dataMC <- na.omit(dataMC) # drop NAs
dataMCScaled <- scale(dataMC[,-ncol(dataMC)]) # normalize data
dataMC <- data.frame(dataMCScaled, name = dataMC$name)

#-------------------------------
# Try K-means with some "optimized" k
#-------------------------------
require(clValid)
set.seed(1234)
internAggregation <- clValid(obj = dataAggregatione[, c(1,2)], nClust =c(2:50), maxitems = 1000, clMethods=c("kmeans"), 
                  validation="internal", verbose=TRUE)
summary(internAggregation)
kmeansResultsAggreation <- kmeans(x = dataAggregatione[,c(1,2)], centers = 4)$cluster
dataAggregatione$clusterSimpleKmeans  <- as.character(kmeansResultsAggreation)
ggplot(dataAggregatione, aes(V1, V2)) + geom_point(aes(colour = clusterSimpleKmeans))

set.seed(1234)
internSpiral <- clValid(obj = dataSpiral[, c(1,2)], nClust =c(2:50), maxitems = 1000, clMethods=c("kmeans"), 
                             validation="internal", verbose=TRUE)
summary(internSpiral)
kmeansResultsSpiral <- kmeans(x = dataSpiral[,c(1,2)], centers = 38)$cluster
dataSpiral$clusterSimpleKmeans  <- as.character(kmeansResultsSpiral)
ggplot(dataSpiral, aes(V1, V2)) + geom_point(aes(colour = clusterSimpleKmeans))


#-------------------------------
# Try with EAC
#-------------------------------

# DS1
set.seed(1234)
EACResults_Aggregatione <- eac(Iter = 200, rangeK = c(2,50), dataset = dataAggregatione[, c(1,2)], hcMethod = "single")
table(EACResults_Aggregatione)
dataAggregatione$clusterEAC  <- as.character(EACResults_Aggregatione)
ggplot(dataAggregatione, aes(V1, V2)) + geom_point(aes(colour = clusterEAC))

# DS2
set.seed(1234)
EACResults_Spiral <- eac(Iter = 200, rangeK = c(2,50), dataset = dataSpiral[, c(1,2)], hcMethod = "single")
table(EACResults_Spiral)
dataSpiral$clusterEAC  <- as.character(EACResults_Spiral)
ggplot(dataSpiral, aes(V1, V2)) + geom_point(aes(colour = clusterEAC))


# DS3
set.seed(1234)
EACResults_MC <- eac(Iter = 1000, rangeK = c(2,50), dataset = dataMC[, numericAttr], hcMethod = "single")
table(EACResults_MC)
dataMC$cluster  <- as.character(EACResults_MC)

#  the following code snippet is taken from http://blog.echen.me/2012/03/20/infinite-mixture-models-with-nonparametric-bayes-and-the-dirichlet-process/ 
x = ddply(dataMC, .(name), function(df) head(df, 1)) # ignore duplicate food items.
x = ddply(x, .(cluster), function(df) head(df, 5)) # for each cluster, take at most 5 items (to avoid the plot being dominated by large clusters).
x$name = factor(x$name, levels = x$name[order(x$cluster)], ordered = T) # Reorder names by cluster (so we can get a plot where all points in a cluster are together).
m = melt(x, id = c("name", "cluster")) # Turn this into a tall-thin matrix.

m$name <- paste("(",m$cluster,") ",m$name,sep="")
qplot(variable, weight = value, data = m, 
      color = cluster, fill = cluster,
      geom = "bar", 
      xlab = "Nutritional variable", ylab = "z-scaled value", main = "McDonald's Food Clusters") + 
  facet_wrap(~ name, ncol = 7) + coord_flip() + scale_colour_hue("cluster") +
  opts(axis.text.y = theme_text(size = 5), axis.text.x = theme_text(size = 5)) + scale_fill_hue("cluster")
ggsave(file = paste("BLOG_KMEANS_EAC_CLUSTERING_",format(Sys.time(), "%Y-%m-%d"),".png", sep=""), width=25, height=15)