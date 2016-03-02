require(xlsx)
# this part is reading data from our own template excel file 
cmcreadtemplate <- function(inFile){

mat <- read.xlsx(inFile$datapath[1],sheetIndex =1,header=T,row.names=1)

#get the cell lines name
cell.lines <- rownames(mat)
##Get the compound names from the first row
label <- colnames(mat)

activity.mat <- matrix(nrow=dim(mat)[1], ncol=dim(mat)[2])
rownames(activity.mat) <- cell.lines
colnames(activity.mat) <- label

##Get the activities
for(i in 1:dim(mat)[2]){
    activity.mat[,i] <- as.numeric(as.character(mat[,i]))
}

cm.probe.count <- vector(mode="integer", length=dim(mat)[2])
my.out <- list(matrix=activity.mat,counts=cm.probe.count)
return(my.out)
}


