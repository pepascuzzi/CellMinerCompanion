library(shiny)
require(xlsx)
require(cluster)
require(RColorBrewer)
source("readall.R")
source("readtemplate.R")
source("heatmap.R")
source("qualitycheck.R")

shinyServer(function(input, output, session){
	 
	# read in files
	datasetInput <- reactive({
		inFile <- input$file
		if (is.null(inFile))
			return(NULL)
		if (dim(inFile)[1] != 1) {
			readall(inFile)
		} else {
			readtemplate(inFile)
		}
	})
	  
	# this is quality check
	datasetInput2 <- reactive({
		if (is.null(datasetInput()))
			return(NULL)
		dataset <- datasetInput()$matrix
		counts <- datasetInput()$counts
		qualitycheck(dataset,counts)
	})        
	  
	# this is z-score
	datasetInput3 <- reactive({
		if (is.null(datasetInput()))
			return(NULL)
		file <- datasetInput()$matrix
		# drop any columns that do not meet user-defined threshold
		check <- datasetInput2()
		exp <- as.numeric(input$exp)
		repval <- as.numeric(input$repvalue)
		missingval <- as.numeric(input$missingvalue)
		file <- file[,which(as.numeric(check[1, ]) >= exp)]
		check <- check[,which(as.numeric(check[1, ]) >= exp)]
		file <- file[,which(as.numeric(check[4, ]) <= repval)]
		check <- check[,which(as.numeric(check[4, ]) <= repval)]
		file <- file[,which(as.numeric(check[5, ]) <= missingval)]
		
		# drop any rows that users do not want to include in analysis	
		if (is.null(input$group)){
			file
		} else {
			row <- as.numeric(input$group)
			file <- file[-c(row),]
		}
		
		if (input$score == "by column"){
			activity.mean <- apply(file, 2,mean, na.rm=T)
			activity.sd <- apply(file, 2,sd,na.rm=T)
			zscore.mat <- sweep(file, 2, activity.mean, "-")
			zscore.mat <- sweep(zscore.mat, 2, activity.sd, "/")
		} else if (input$score == "by matrix") {
			activity.mean <- mean(file, na.rm=T)
			activity.sd <- sd(file, na.rm=T)
			zscore.mat <- sweep(file, 1, activity.mean, "-")
			zscore.mat <- sweep(zscore.mat, 1, activity.sd, "/")
		}
	})	    

	# this is distance cluster
	datasetInput4 <- reactive({
		if (is.null(datasetInput()))
			return(NULL)
		zscore.mat <- datasetInput3()
			
		if (input$distance == "Pearson correlation distance") {
			cluster.cor <- cor(zscore.mat, use="pairwise.complete.obs", method="pearson")
			cluster.dist <- as.dist(1 - cluster.cor)
		} else if (input$distance == "Euclidean") {
			cluster.dist <- daisy(t(zscore.mat), metric="euclidean", stand=F)
		} else {
			cluster.dist <- daisy(t(zscore.mat), metric="manhattan", stand=F)
		}
		
		cluster.clust <- agnes(cluster.dist, diss=T, method="average")
	})
	
	# this is ordered zscore matrix
	datasetInput5 <- reactive({
		if (is.null(datasetInput()))
			return(NULL)
		zscore.mat <- datasetInput3()
		cluster.clust <- datasetInput4()
		if (input$maporder == "use hierarchical cluster order") {
			ordered.mat <- zscore.mat[,cluster.clust$order]
		} else {
			ordered.mat <- zscore.mat
		}
	})

	# output
	# output quality check matrix
	output$check <- renderTable({
		dataset <- datasetInput2()
	})	
	# output original matrix
	output$activity <- renderTable({
		dataset <- datasetInput()$matrix
	})	
	# output normalized zscore matrix
	output$zscore <- renderTable({
		dataset <- datasetInput5()
	})	
	
	plotWidth <- reactive({
		if (input$gw =="standard")
			return (800)
		else
			return (2000)
	})
	# output heatmap
	output$plot1 <- renderPlot({
		if (is.null(datasetInput()))
			return(NULL) 
		cluster.clust <- datasetInput4()
		cluster.no <- input$cluster
		zscore.mat <- datasetInput3()
		maporder <- input$maporder
		type <- input$maptype
		heatmap(cluster.clust,cluster.no,zscore.mat,maporder,type)
	}, width=plotWidth
	)	
    # output dendrogram 
	output$plot2 <- renderPlot({
		if (is.null(datasetInput()))
			return(NULL) 
		cluster.clust <- datasetInput4()
		zscore.mat <- datasetInput3()[, cluster.clust$order]
		n <- ncol(zscore.mat)
		par(las=1, mar=c(8, 4, 3, 1) + 0.1, cex=1.5)
		plot(as.dendrogram(cluster.clust),leaflab="none",cex.lab=2)
		mtext(colnames(zscore.mat), side=1, at=1:n, line=0.5, cex=1.2, las=2, family="mono")
	}, width=plotWidth
	)		
	
	# download table and plot
	# download template table
	output$downloadTemplate <- downloadHandler(
		filename = function() { 
			paste('template.xlsx')},
		content = function(file) {
			template <- matrix(0,ncol=5,nrow=60)
			rownames(template) <- name
			colnames(template) <- c("drug/gene1","drug/gene2","drug/gene3","drug/gene4","drug/gene5")
			write.xlsx(template,col.names=T,row.names=T,sheetName="sheet1",showNA=T, file)
		}  			
	)
    # download orignial matrix
	output$downloadTable <- downloadHandler(
		filename = function() { 
			paste('OriginalMatrix.xlsx')},
		content = function(file) {
			write.xlsx(datasetInput()$matrix,col.names=T,row.names=T,sheetName="sheet1",showNA=T, file)
		}  			
	)
	# download zscore matrix
	output$downloadTable2 <- downloadHandler(
		filename = function() { 
			paste(input$score,input$distance,'cluster #',input$cluster,'ZscoreMatrix.xlsx',sep='.')},
		content = function(file) {
			write.xlsx(datasetInput5(),col.names=T,row.names=T,sheetName="sheet1",showNA=T, file)
		}
	)
	# download quality check matrix
	output$downloadTable3 <- downloadHandler(
		filename = function() { 
			paste('QualityCheck.xlsx')},
		content = function(file) {
			write.xlsx(datasetInput2(),col.names=T,row.names=T,sheetName="sheet1",showNA=T, file)
		}
	)
    # download heatmap  
	output$downloadFigure1 <- downloadHandler(
		filename = function() { 
			paste(input$score,input$distance,input$maptype,'cluster',input$cluster,'Heatmap',input$type, sep='.')},
		content = function(file) {
			if ((input$type == "pdf") & (input$gw =="standard")) {
				pdf(file,height=12,width=12)
			} else if ((input$type == "png") & (input$gw =="standard")) {
				png(file,units="in",width = 12, height = 12,res=300)
			} else if ((input$type == "pdf") & (input$gw =="large")){
				pdf(file,height=12,width=24)	
			} else if ((input$type == "png") & (input$gw =="large")){
				png(file,units="in",width = 24, height = 12,res=300)
		}
    	
    		if (is.null(datasetInput()))
				return(NULL)		
			cluster.clust <- datasetInput4()
			cluster.no <- input$cluster
			zscore.mat <- datasetInput3()
			maporder <- input$maporder
			type <- input$maptype
			heatmap(cluster.clust,cluster.no,zscore.mat,maporder,type)
		dev.off()
		}
	) 
    # download dendrogram    
	output$downloadFigure2 <- downloadHandler(
		filename = function() { 
			paste(input$score,input$distance,'cluster #',input$cluster,'Dendrogram',input$type, sep='.')},
		content = function(file) {
			if ((input$type == "pdf") & (input$gw =="standard")) {
				pdf(file,height=12,width=12)
			} else if ((input$type == "png") & (input$gw =="standard")) {
				png(file,units="in",width = 12, height = 12,res=300)
			} else if ((input$type == "pdf") & (input$gw =="large")){
				pdf(file,height=12,width=24)	
			} else if ((input$type == "png") & (input$gw =="large")){
				png(file,units="in",width = 24, height = 12,res=300)
			}
			if (is.null(datasetInput()))
				return(NULL) 
			cluster.clust <- datasetInput4()
			zscore.mat <- datasetInput3()[,cluster.clust$order]
			n <- ncol(zscore.mat)
			par(las=1, mar=c(7,3,2,1)+0.1, cex=1.5)
			plot(as.dendrogram(cluster.clust),leaflab="none",cex.lab=2)
			mtext(colnames(zscore.mat),side=1,at=1:n,line=0.5,cex=1, las=2,family="mono") 
		dev.off()
		}
	)

    
	#Names for each tab       
	output$title <- renderText({
		if (is.null(datasetInput()))
			return(NULL) 
		paste("quality check")
	})
    
	output$title1 <- renderText({
		if (is.null(datasetInput()))
			return(NULL) 
		paste("original matrix")
	})
    
	output$title2 <- renderText({
		if (is.null(datasetInput()))
			return(NULL) 
		z.choice <- input$score
		paste (z.choice,'z-score matrix',sep=" ")
	})
    
	output$title3 <- renderText({
		if (is.null(datasetInput()))
			return(NULL)
		z.choice <- input$score
		paste (z.choice, 'heatmap',sep=" ")
	})
    
	output$title4 <- renderText({
		if (is.null(datasetInput()))
			return(NULL)
		z.choice <- input$score
		paste (z.choice,'dendrogram',sep=" ")
	})

})