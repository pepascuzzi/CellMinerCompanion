# this part does two jobs, first, drop any columns or rows that user do not want to include in analysis,;second job is normalize the data, which will procude the zscore matrix
cmcnormalization <- function(check,exp,repval,missingval,celllines,file,score) {
	
	# drop any columns that do not meet user-defined threshold
	file <- file[,which(as.numeric(check[1, ]) >= exp)]
	check <- check[,which(as.numeric(check[1, ]) >= exp)]
	file <- file[,which(as.numeric(check[4, ]) <= repval)]
	check <- check[,which(as.numeric(check[4, ]) <= repval)]
	file <- file[,which(as.numeric(check[5, ]) <= missingval)]
		
	# drop any rows that users do not want to include in analysis	
	if (is.null(celllines)){
		file
	} else {
		row <- as.numeric(celllines)
		file <- file[-c(row),]
	}
	
	# normalized data either by column or by matrix
	if (score == "by column"){
		activity.mean <- apply(file, 2,mean, na.rm=T)
		activity.sd <- apply(file, 2,sd,na.rm=T)
		zscore.mat <- sweep(file, 2, activity.mean, "-")
		zscore.mat <- sweep(zscore.mat, 2, activity.sd, "/")
	} else if (score == "by matrix") {
		activity.mean <- mean(file, na.rm=T)
		activity.sd <- sd(file, na.rm=T)
		zscore.mat <- sweep(file, 1, activity.mean, "-")
		zscore.mat <- sweep(zscore.mat, 1, activity.sd, "/")
	}
	return(zscore.mat)
}

		
		
