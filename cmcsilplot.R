library(cluster)
# this part calculates cluster significance and the best choice of number of clusters
cmcsilplot <- function (my.cluster, my.dist){
    sample.no <- length(my.cluster$order)
    my.opt <- numeric((sample.no - 2))
    # calculate silhouette coefficient
    for(i in 1:length(my.opt)){
        my.tree <- cutree(my.cluster, k=i+1)
        my.sil <- silhouette(my.tree, dist=my.dist)
        my.opt[i] <- mean(my.sil[, "sil_width"])
    }
    plot(2:sample.no, c(my.opt, 0), type="o", lwd=2, las=2, ylab="silhouette", xlab="number of clusters")
    abline(v=seq(from=0, to=sample.no, by=2), lty=2)
    y <- c(my.opt, 0)
    max_val <- which.max(y) + 1
    title(paste("The optimal number of cluster is", max_val))
    
}


