library(cluster)
# this part calculates cluster significance and the best choice of number of clusters
cmcsiltable <- function (my.cluster, my.dist, my.no) {
    my.tree <- cutree(my.cluster, k=my.no)
    my.sil <- silhouette(my.tree, dist=my.dist)
    sil.table <- cbind(my.cluster$order.lab, as.data.frame(my.sil[my.cluster$order, ]))	
	sil.table$mean.cluster.sil <- sapply(sil.table[, "cluster"], function(x)(mean(sil.table[sil.table[, "cluster"]==x, "sil_width"])))
	colnames(sil.table) <- c("sample","cluster","neighbor","sample.silhouette","cluster.silhouette")
	return(sil.table)
}


