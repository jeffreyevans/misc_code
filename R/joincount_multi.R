# Create multi-class joincount based on nearest neighbor or distance
#  can be used to explore the spatial structure of binomial data
library(spdep)
options(warn = -1)
  data(meuse)
  sp::coordinates(meuse) <- ~x+y  

# binom <- as.factor(rbinom(dim(meuse)[1], 1, 0.5)) # simulated
binom <- cut(meuse@data$zinc, breaks=c(112,326,1840), labels=c("0","1"))
  names(binom) <- rownames(meuse@data)

# based on k-nearest neighbour
#COL.nb <- knn2nb(knearneigh(coordinates(meuse), k=4), row.names=rownames(meuse@data)) 
#plot(COL.nb, coords=coordinates(meuse))  

# Based on (all linked) distance
k.nn <- knn2nb(knearneigh(coordinates(meuse)))
  all.linked <- max(unlist(nbdists(k.nn, coordinates(meuse))))
  COL.nb <- dnearneigh(coordinates(meuse), d1=0, d2=all.linked,  
                       row.names = rownames(meuse@data)) 					
  lw <- nb2listw(COL.nb, style="B")

plot(COL.nb, coords=coordinates(meuse)) 
  
# giving the counts of joins by type, dropping double-counting in the 
# summation. listw2star() is used in localmoran.sad() and elsewhere to 
# generate observation-wise weights objects. This isn't an implementation of 
# LICD in that the subregion for each i is simply its set of neighbours, not 
# a moving window, but at least provides a basis for classification.

# global
spdep::joincount.multi(binom, lw)

# local
Vis <- lapply(1:length(binom), function(i) spdep::listw2star(lw, i, "B",
              n=length(binom), D=NULL, a=NULL))
			  
# Matrix of frequency of join counts by row
#   (e.g., row 1 has 3 joins for 0:0 and 1 join for 1:0)) 			  
ljcs <- t(sapply(Vis, function(x) { res <- spdep::joincount.multi(binom, x,
          zero.policy=TRUE); res[-nrow(res),1]}))
		  
# There are warnings because inference fails (as it should),  
#   but the tally is the same, with:
apply(ljcs, 2, sum)/2
