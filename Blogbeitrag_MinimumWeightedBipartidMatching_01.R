Title
========================================================

Some time ago I stumbled about a problem arising from labeling of clustering's. 
Here the partition an instance belongs to is labeled mostly through an integer ranging from 1 to K, 
where k is the number of clusters. The task here was to plot a map of the results from clustering of 
spatial polygons where every label is represented by some colour. 
But after rerunning the clustering algorithm (kmeans in this case) the 
assignment between the clusters and the labeling changed completely. 
This is because there is no unique connection between a partition and a specific label. 
So graphical representations of two clustering's (which only have some slight differences) look 
like they are completely different. The following R code depict some simple example of this matter:

```{r}
require(spdep)
require(rgdal) # requires sp, will use proj.4 if installed
require(maptools)
require(ggplot2)
require(plyr)
library(grid)

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y) # panel function for the plots

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
```

You can also embed plots, for example:

```{r fig.width=7, fig.height=6}
plot(cars)
```

