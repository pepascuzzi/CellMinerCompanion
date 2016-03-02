library(cluster)
# this part calculates cluster significance and the best choice of number of clusters
cmcsilplot <- function (cluster.clust) {
    
    # #	# calculate cluster correlation	
    # 	if (dist.choice == "Pearson correlation distance") {
    # 		cluster.cor <- cor(zscore.mat, use="pairwise.complete.obs", method="pearson")
    # 		cluster.dist <- as.dist(1 - cluster.cor)
    # 	} else if (dist.choice == "Euclidean") {
    # 		cluster.dist <- daisy(t(zscore.mat), metric="euclidean", stand=F)
    # 	} else {
    # 		cluster.dist <- daisy(t(zscore.mat), metric="manhattan", stand=F)
    # 	}
    # 	
    # 	cluster.clust <- agnes(cluster.dist, diss=T, method="average")
    sample.no <- length(cluster.clust$order)
    my.opt <- numeric((sample.no - 2))
    # calculate silhouette coefficient
    for(i in 1:length(my.opt)){
        my.tree <- cutree(cluster.clust, k=i+1)
        my.sil <- silhouette(my.tree, dist=cluster.clust$diss)
        my.opt[i] <- mean(my.sil[, "sil_width"])
    }
    plot(2:sample.no, c(my.opt, 0), type="o", lwd=2, las=2, ylab="silhouette", xlab="number of clusters")
    abline(v=seq(from=0, to=sample.no, by=2), lty=2)
    y <- c(my.opt, 0)
    max_val <- which.max(y) + 1
    title(paste("The optimal number of cluster is", max_val))
    
}


