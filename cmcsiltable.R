library(cluster)
# this part calculates cluster significance and the best choice of number of clusters
cmcsiltable <- function (cluster.clust, cluster.no) {
	
# 	# calculate cluster correlation	
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
	
    my.tree <- cutree(cluster.clust, k=cluster.no)
    my.sil <- silhouette(my.tree, dist=cluster.clust$diss)
    sil.table <- cbind(cluster.clust$order.lab, as.data.frame(my.sil[cluster.clust$order, ]))	
	sil.table$mean.cluster.sil <- sapply(sil.table[, "cluster"], function(x)(mean(sil.table[sil.table[, "cluster"]==x, "sil_width"])))
	colnames(sil.table) <- c("sample","cluster","neighbor","sample.silhouette","cluster.silhouette")
	return(sil.table)
}


