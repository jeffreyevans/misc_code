#### N-th order Polygon contingency example
library(spdep)
library(rgdal)
library(sf)
library(sp)

columbus <- readOGR(system.file("shapes/columbus.shp", package="spData")[1])

# First-order neighbors (those touching source polygon)
nb.1st <- nb2listw(poly2nb(columbus), style = "B", zero.policy = TRUE)
  W1 <- as(nb.1st, "CsparseMatrix")
  
# Second-order neighbors (those touching the polygons touching the source polygon)  
nb.2nd = nblag(poly2nb(columbus), 2)
  W2 <- as(nb2listw(nb.2nd[[2]], style = "B", zero.policy = TRUE), "CsparseMatrix")

# Third-order neighbors (those touching the polygons touching the source polygon)  
nb.3rd = nblag(poly2nb(columbus), 3)
  W3 <- as(nb2listw(nb.3rd[[3]], style = "B", zero.policy = TRUE), "CsparseMatrix")
  
i = 20  # polygon index to check neighbors  
nb.1st.idx <- colnames(W1)[which(W1[i,] > 0 )]
  nb.1st.idx <- which(row.names(columbus) %in% nb.1st.idx)

nb.2nd.idx <- colnames(W2)[which(W2[i,] > 0 )] 
  nb.2nd.idx <- which(row.names(columbus) %in% nb.2nd.idx)
  
nb.3rd.idx <- colnames(W3)[which(W3[i,] > 0 )] 
  nb.3rd.idx <- which(row.names(columbus) %in% nb.3rd.idx)  
  
plot(columbus, border="grey")
  plot(columbus[nb.3rd.idx,], col="green", add=TRUE)
    plot(columbus[nb.2nd.idx,], col="cyan", add=TRUE)
      plot(columbus[nb.1st.idx,], col="blue", add=TRUE)
        plot(columbus[i,], col="red", add=TRUE)
          box()
    title("First, Second and Third Order Neighbors")   	  
    legend("topleft", legend=c("source polygon","1st order","2nd order", "3rd order"),
           fill=c("red","blue","cyan","green"))  

##########################################################
nc <- sf::st_read(system.file("shapes/sids.shp", 
         package="maptools")[1])
  #nc <- as(st_cast(nc, "POLYGON"), "Spatial")		 

# Queens - contiguity defines a neighbor when at least one point on the boundary of  
#          one polygon is shared with at least one point of its neighbor 
#          (common border or corner)
( nb.FQ = poly2nb(nc, queen=TRUE, row.names=nc$FIPSNO) )

# Rooks -  contiguity does not include corners, only borders, thus comprising only  
#          polygons sharing more than one boundary point
( nb.RK = poly2nb(nc, queen=FALSE, row.names=nc$FIPSNO) )

# Higher order neighbors - HON are useful when looking at the effect of lags on 
#                          spatial autocorrelation and in spatial autoregressive 
#                          models like SAR with a more global spatial autocorrelation
# second order rook contiguity
( nb.SRC = nblag(nb.RK, 2) ) 
