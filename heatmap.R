require(cluster)
require(RColorBrewer)
source("cut2.R")


heatmap <- function(cluster.clust, cluster.no,zscore.mat,maporder,type) {

if (maporder == "use hierarchical cluster order") {
	my.cuts <- cutree(cluster.clust, cluster.no)
	sample.count <- table(my.cuts)
	#reorder to be consistent with hclust
	my.cuts <- my.cuts[cluster.clust$order]
	sample.count <- sample.count[unique(my.cuts)]
	sample.count <- sample.count[length(sample.count):1]
	ordered.mat <- zscore.mat[,cluster.clust$order]
} else {
	ordered.mat <- zscore.mat
}

##This code needed to produce horizontal bars across heat map
##that group the cell lines by tissue.
cell.tags <- c("BR:", "CNS:", "CO:", "LE:", "ME:", "LC:", "OV:", "PR:", "RE:")
cell.count <- vector(mode="integer", length=length(cell.tags))
for(i in 1:length(cell.tags)){
    cell.count[i] <- length(grep(cell.tags[i], rownames(zscore.mat)))
}

mat.rows <- nrow(ordered.mat)
mat.cols <- ncol(ordered.mat)

image.list<-list(x=1:mat.cols,y=1:mat.rows,z=t(ordered.mat[mat.rows:1,]))

if (type == "deciles") {
	break.length <- 10
	my.breaks <- cut2(ordered.mat, g=break.length, onlycuts=T)
} else {
	constant <- c(-3.30, -2.57, -1.96, -1.65, 0, 1.65, 1.96, 2.57, 3.30)
	mini <- min(ordered.mat,na.rm=T)
	maxi <- max(ordered.mat,na.rm=T)
	if (maxi > 5) {
		my.breaks.maxi <- maxi
	} else {
		my.breaks.maxi <- 5
	}

	if (mini < -5){
		my.breaks.mini <- mini
	} else {
		my.breaks.mini <- -5
	}

	my.breaks <- c(my.breaks.mini,constant,my.breaks.maxi)
}

n <- length(my.breaks)

my.breaks[1] <- floor(my.breaks[1])
my.breaks[n] <- ceiling(my.breaks[n])
color.no <- n-1
my.colors <- brewer.pal(color.no, "RdBu")[color.no:1]


key.list <- list(x=1:n, y=1, z=as.matrix(my.breaks[2:n], ncol=1))

layout(matrix(c(1,0,1,2,1,0), ncol=3), width=c(1/3, 1/3, 1/3), heights=c(0.9, 0.1))

par(las=1, mar=c(1,8,9,2)+0.1, cex=1)

image(image.list,col=my.colors,breaks=my.breaks,yaxt="n",xaxt="n",ylab="",main=NULL)
box(lwd=2)
mtext(rownames(ordered.mat)[1:mat.rows],side=2,at=ncol(image.list$z):1,adj=1,line=1,cex=1,family="mono")
mtext(colnames(ordered.mat),side=3,at=1:nrow(image.list$z),line=0.5,cex=1.2, las=2,family="mono")
abline(h=cumsum(cell.count[length(cell.count):1]) + 0.5, lwd=2)
if (maporder == "use hierarchical cluster order") {
	abline(v=cumsum(sample.count[length(sample.count):1]) + 0.5
, lwd=2)
}

par(mar=c(4,1,0.5,1))
image(key.list, breaks=my.breaks,yaxt="n",xaxt="n",ylab="",main=NULL, col=my.colors)
mtext("z-score",side=2,adj=1,line=1,cex=1.2)
axis(1, at=seq(from=1, to=n, by=1), labels=signif(my.breaks[1:n], digits=2), las=2)
}