# Warum? Wo wird es benötigt?
# - CLuster Validation
# - Consensus Clustering
# - Visualisierung in Karten

# Literatur
# Ensembles of Partitions via Data Resampling
# Two Algorithms for Maximum and Minimum Weighted Bipartite Matching
# Stability-Based Validation of Clustering Solutions

# https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles

minWeightBipartiteMatching <- function(clusteringA,clusteringB){
  require(clue)
  idsA <- unique(clusteringA) # distinct cluster ids in a
  idsB <- unique(clusteringB) # distinct cluster ids in b
  nA <- length(clusteringA) # number of instances in a
  nB <- length(clusteringB) # number of instances in b
  if(length(idsA) != length(idsB) || nA != nB){
    stop("number of cluster or number of instances do not match")
  }
  
  nC <- length(idsA)
  tupel <- c(1:nA)
  
  # calculation of the distance matrix
  assignmentMatrix <- matrix(rep(-1,nC*nC), nrow=nC)
  for(i in 1:nC){
    tupelClusterI <- tupel[clusteringA == i]
    sol.row.i <- sapply(1:nC, function(i,clusterIDsB,tupelA_I){
                           nA_I <- length(tupelA_I) # number of elements in cluster I
                           tupelB_I <- tupel[clusterIDsB == i]
                           nB_I <- length(tupelB_I)
                           nTupelIntersect <- length(intersect(tupelA_I, tupelB_I))
                           return( (nA_I - nTupelIntersect) + (nB_I - nTupelIntersect) )},
                         clusteringB, tupelClusterI)  
    assignmentMatrix[i,] <- sol.row.i
  }
  
  # Bestimmen der Zuordnung
  result <- solve_LSAP(assignmentMatrix,maximum=FALSE)
  attr(result, "assignmentMatrix") <- assignmentMatrix
  return(result)
}
  
#tmp <- minWeightBipartiteMatching(c(1,1,2,3,3,4,4,4,2),c(2,2,3,1,1,4,4,4,3))


#     [,1] [,2] [,3] [,4]
# [1,]    4    0    4    5
# [2,]    4    4    0    5
# [3,]    0    4    4    5
# [4,]    5    5    5    0

###################################
require(spdep)
require(rgdal) # requires sp, will use proj.4 if installed
require(maptools)
require(ggplot2)
require(plyr)
library(grid); vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
gpclibPermit() # required for fortify method

bh <- readShapePoly(system.file("etc/shapes/bhicv.shp",
                                package="spdep")[1])
bh@data$id = rownames(bh@data)
bh.points = fortify(bh, region="id")
bh.df = join(bh.points, bh@data, by="id")

# clustering after stanardization of data
dpad <- data.frame(scale(bh@data[,5:8]))
set.seed(1234); res1 <- kmeans(dpad, centers=10)
set.seed(9999); res2 <- kmeans(dpad, centers=10)

# add cluster id to polygon layer
bh.df.cl = merge(bh.df, data.frame(id = bh@data$id, CL1 = res1$cluster, CL2 = res2$cluster), by="id")

p1 <- ggplot(bh.df.cl) + 
  aes(long,lat, group=group, fill=as.factor(CL1)) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() + scale_fill_brewer(palette="Set3")
p2 <- ggplot(bh.df.cl) + 
  aes(long,lat, group=group, fill=as.factor(CL2)) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() + scale_fill_brewer(palette="Set3")
grid.newpage(); pushViewport(viewport(layout = grid.layout(1, 2)))
print(p1, vp = vplayout(1, 1))
print(p2, vp = vplayout(1, 2))

# matching
matching <- minWeightBipartiteMatching(res1$cluster, res2$cluster)
clusterA <- res1$cluster
tmp <- sapply(1:length(matching), function(i){
  clusterA[which(res1$cluster == i)] <<- matching[i]
})
#cbind(res1$cluster, clusterA)

clusterB <- res2$cluster
bh.df.cl.mwbm = merge(bh.df, data.frame(id = bh@data$id, CL1 = clusterA, CL2 = clusterB), by="id")

p1 <- ggplot(bh.df.cl.mwbm) + 
  aes(long,lat, group=group, fill=as.factor(CL1)) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() + scale_fill_brewer(palette="Set3")
p2 <- ggplot(bh.df.cl.mwbm) + 
  aes(long,lat, group=group, fill=as.factor(CL2)) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() + scale_fill_brewer(palette="Set3")
grid.newpage(); pushViewport(viewport(layout = grid.layout(1, 2)))
print(p1, vp = vplayout(1, 1))
print(p2, vp = vplayout(1, 2))

# results
# - better match
# - instability of kmean results