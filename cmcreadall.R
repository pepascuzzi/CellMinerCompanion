require(xlsx)
# this part is reading multiple input excel files and will return a matrix with combining all data and the number of experiments involved for each input data

cmcreadall <- function(inFile){

# get number of files and create an empty vector
cm.files <- c(inFile$name)
cm.probe.count <- vector(mode="integer", length=length(cm.files))

# get detail information for input files to check if they are drug, gene or micRNA data
indicator <- read.xlsx(inFile$datapath[1], sheetIndex=1, rowIndex=2, colIndex=2, colClasses="character", header=F)[,1]
if (indicator == "Drug activity z score") {
	for(i in 1:length(cm.files)){
    	cm.probe.count[i] <- read.xlsx(inFile$datapath[i], sheetIndex=1, rowIndex=12, colIndex=2, colClasses="integer", header=F)[,1]
	}
	cell.lines <- as.character(read.xlsx2(inFile$datapath[1], sheetIndex=1, startRow=16, endRow=75, colIndex=1, colClasses="character", header=F,stringAsFactors=F,as.is=T)[,1])
}else if (indicator == "Gene transcript level z score") {
	for(i in 1:length(cm.files)){
    	cm.probe.count[i] <- read.xlsx(inFile$datapath[i], sheetIndex=1, rowIndex=8, colIndex=2, colClasses="integer", header=F)[,1]
	}
	cell.lines <- as.character(read.xlsx2(inFile$datapath[1], sheetIndex=1, startRow=13, endRow=72, colIndex=1, colClasses="character", header=F,stringAsFactors=F,as.is=T)[,1])
}else{
	cell.lines <- as.character(read.xlsx2(inFile$datapath[1], sheetIndex=1, startRow=13, endRow=72, colIndex=1, colClasses="character", header=F,stringAsFactors=F,as.is=T)[,1])
}

##Get the compound tags from the file names
label <- unname(sapply(c(inFile$name), function(x)(strsplit(x, split="_")[[1]][1])))

# create a matrix with rownames corresponding to cancer cell lines and column names are each input file
activity.mat <- matrix(nrow=60, ncol=length(cm.files))
rownames(activity.mat) <- cell.lines
colnames(activity.mat) <- label

##Get the activities data from each input file
if (indicator == "Drug activity z score") {
	for(i in 1:length(cm.files)){
    	activity.mat[,i] <- read.xlsx(inFile$datapath[i], sheetIndex=1, rowIndex=84:143, colIndex=cm.probe.count[i] + 3, colClasses="numeric", header=F)[,1]
	}
}else if (indicator == "Gene transcript level z score"){
	for(i in 1:length(cm.files)){
    	activity.mat[,i] <- read.xlsx(inFile$datapath[i], sheetIndex=1, rowIndex=89:148, colIndex=cm.probe.count[i] + 3, colClasses="numeric", header=F)[,1]
	}
}else{
	for(i in 1:length(cm.files)){
    	activity.mat[,i] <- read.xlsx(inFile$datapath[i], sheetIndex=1, rowIndex=90:149, colIndex=2, colClasses="numeric", header=F)[,1]
	}
}

my.out <- list(matrix=activity.mat,counts=cm.probe.count)
return(my.out)
}


