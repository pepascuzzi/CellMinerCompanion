library(shiny)
require(xlsx)
require(cluster)
require(RColorBrewer)
source("cmcreadall.R")
source("cmcreadtemplate.R")
source("cmcheatmap.R")
source("cmcqualitycheck.R")
source("cmcnormalization.R")
source("cmcdistance.R")
source("cmcsilplot.R")
source("cmcsiltable.R")

shinyServer(function(input, output, session){
    
    # read in files, either multiple excel files or our own template excel file
    datasetInput <- reactive({
        inFile <- input$file
        if (is.null(inFile))
            return(NULL)
        # read multiple files by our defined function (cmcreadall) or read from template (cmcreadtemplate)
        if (dim(inFile)[1] != 1) {
            cmcreadall(inFile)
        } else {
            cmcreadtemplate(inFile)
        }
    })
    
    # check the input file using our own function (cmcqualitycheck) and return a table with quality report
    datasetInput2 <- reactive({
        if (is.null(datasetInput()))
            return(NULL)
        dataset <- datasetInput()$matrix
        counts <- datasetInput()$counts
        cmcqualitycheck(dataset,counts)
    })        
    
    # normalized input data
    datasetInput3 <- reactive({
        if (is.null(datasetInput()))
            return(NULL)
        # get variables from users choice
        file <- datasetInput()$matrix
        check <- datasetInput2()
        exp <- as.numeric(input$exp)
        repval <- as.numeric(input$repvalue)
        missingval <- as.numeric(input$missingvalue)
        celllines <- input$group
        score <- input$score
        # normalized input data by our own function(cmcnormalization)
        zscore.mat <- cmcnormalization(check,exp,repval,missingval,celllines,file,score)
    })	    
    
    # calculate distance cluster based on normalized data
    datasetInput4 <- reactive({
        if (is.null(datasetInput()))
            return(NULL)
        zscore.mat <- datasetInput3()
        dist.choice <- input$distance
        cluster.dist <- cmcdistance(dist.choice, zscore.mat)
    })
    
    datasetInput5 <- reactive({
        if (is.null(datasetInput()))
            return(NULL)
        cluster.dist <- datasetInput4()
        cluster.clust <- agnes(cluster.dist, diss=T, method="average")
    })
    
    # order zscore matrix based on cluster order
    datasetInput6 <- reactive({
        if (is.null(datasetInput()))
            return(NULL)
        zscore.mat <- datasetInput3()
        cluster.clust <- datasetInput5()
        if (input$maporder == "use hierarchical cluster order") {
            ordered.mat <- zscore.mat[, cluster.clust$order]
        } else {
            ordered.mat <- zscore.mat
        }
    })
    
    # output
    # output quality check table
    output$check <- renderTable({
        dataset <- datasetInput2()
    })	
    # output original matrix
    output$activity <- renderTable({
        dataset <- datasetInput()$matrix
    })	
    # output normalized zscore matrix
    output$zscore <- renderTable({
        dataset <- datasetInput6()
    })	
    
    # define plot width
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
        # get data and heatmap order and type
        cluster.clust <- datasetInput5()
        cluster.no <- input$cluster
        zscore.mat <- datasetInput3()
        maporder <- input$maporder
        type <- input$maptype
        # plot heatmap
        cmcheatmap(cluster.clust, cluster.no, zscore.mat, maporder, type)
    }, width=plotWidth
    )	
    # output dendrogram 
    output$plot2 <- renderPlot({
        if (is.null(datasetInput()))
            return(NULL) 
        cluster.clust <- datasetInput5()
        zscore.mat <- datasetInput3()[, cluster.clust$order]
        n <- ncol(zscore.mat)
        par(las=1, mar=c(8, 4, 3, 1) + 0.1, cex=1.5)
        # plot dendrogram and add labels
        plot(as.dendrogram(cluster.clust),leaflab="none",cex.lab=2)
        mtext(colnames(zscore.mat), side=1, at=1:n, line=0.5, cex=1.2, las=2, family="mono")
    }, width=plotWidth
    )	
    # output silhouette plot
    output$plot3 <- renderPlot({
        if (is.null(datasetInput()))
            return(NULL) 
        cluster.dist <- datasetInput4()
        cluster.clust <- datasetInput5()
        # plot number of cluster and silhouette coefficient
        cmcsilplot(my.cluster=cluster.clust, my.dist=cluster.dist)		
    }, width=plotWidth
    )	
    # output silhouette table
    output$siltable <- renderTable({
        if (is.null(datasetInput()))
            return(NULL) 
        cluster.dist <- datasetInput4()
        cluster.clust <- datasetInput5()
        cluster.no <- input$cluster
        # plot number of cluster and silhouette coefficient
        cmcsiltable(my.cluster=cluster.clust, my.dist=cluster.dist, my.no=cluster.no)		
    })			
    
    # download table and plot
    # download template excel file
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
    output$downloadTable1 <- downloadHandler(
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
            } else if ((input$type == "pdf") & (input$gw =="wide")){
                pdf(file,height=12,width=24)	
            } else if ((input$type == "png") & (input$gw =="wide")){
                png(file,units="in",width = 24, height = 12,res=300)
            }
            
            if (is.null(datasetInput()))
                return(NULL)		
            cluster.clust <- datasetInput4()
            cluster.no <- input$cluster
            zscore.mat <- datasetInput3()
            maporder <- input$maporder
            type <- input$maptype
            cmcheatmap(cluster.clust,cluster.no,zscore.mat,maporder,type)
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
            } else if ((input$type == "pdf") & (input$gw =="wide")){
                pdf(file,height=12,width=24)	
            } else if ((input$type == "png") & (input$gw =="wide")){
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
    output$title1 <- renderText({
        if (is.null(datasetInput()))
            return(NULL) 
        paste("quality check")
    })
    
    output$title2 <- renderText({
        if (is.null(datasetInput()))
            return(NULL) 
        paste("original matrix")
    })
    
    output$title3 <- renderText({
        if (is.null(datasetInput()))
            return(NULL) 
        z.choice <- input$score
        paste (z.choice,'z-score matrix',sep=" ")
    })
    
    output$title4 <- renderText({
        if (is.null(datasetInput()))
            return(NULL)
        z.choice <- input$score
        paste (z.choice, 'heatmap',sep=" ")
    })
    
    output$title5 <- renderText({
        if (is.null(datasetInput()))
            return(NULL)
        z.choice <- input$score
        paste (z.choice,'dendrogram',sep=" ")
    })
    
    output$title6 <- renderText({
        if (is.null(datasetInput()))
            return(NULL)
        z.choice <- input$score
        paste (z.choice,'silhouette plot',sep=" ")
    })
    
    output$title7 <- renderText({
        if (is.null(datasetInput()))
            return(NULL)
        z.choice <- input$score
        cluster.no <- input$cluster
        paste (z.choice,'silhouette table',sep=" ")
    })
    
})