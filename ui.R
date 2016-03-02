library(shiny)

# Define UI for application
shinyUI(fluidPage(
	titlePanel("CellMiner Companion"),
	p("CellMiner Companion is a web application designed to facilitate the exploration and visualization of NCI-60 data retrieved from queries to ", a("CellMiner",href="http://discover.nci.nih.gov/cellminer", style="color:red"),". You can upload multiple files for gene expression, drug activity, or microRNA expression, and CellMiner Companion will parse the data from these files and assemble a single data matrix.  CellMiner Companion will also check individual datasets for potential issues, allowing users to discard problematic datasets.  The uploaded data will be z-score normalized and visualized as a heatmap.  Hierarchical clustering can be performed columnwise to help users to detect patterns in the data.  Cell lines are arranged in rows and the order is fixed.  Multiple parameters can be adjusted, and the output graphs and table will be rapidly updated.  This makes CellMiner Companion an invaluable tool for exploring CellMiner NCI-60 data. If you have any questions or suggestions, please contact Pete E. Pascuzzi, ppascuzz@purdue.edu",style="color:darkblue"),
	sidebarLayout(
		sidebarPanel(
    		fluidRow(
    			p(h4("Step 1: Parameter Setting")),
    			selectInput("score", label=("Z-score Choice"),list("by column","by matrix")),
    			checkboxInput("detail1", "check here for details"),
    			conditionalPanel(
  					condition = "input.detail1 == true",
  					helpText("Z-score Choice: This determines how the z-scores are calculated, using either the mean and standard deviation for each drug/gene/miRNA or a single mean and standard deviation from the entire data matrix. Use by column if you want to compare gene expression or drug sensitivity trends across the cell lines. Use by matrix is you want to compare the absolute gene expression or drug sensitivity values.",style="color:darkblue")
  				),
  				
    			selectInput("distance", label=h5(strong("Distance Choice")),list("Pearson correlation distance","Euclidean","Manhattan")),
    			checkboxInput("detail2", "check here for details"),
    			conditionalPanel(
  					condition = "input.detail2 == true",
   					helpText("Distance:choice: Use the Pearson correlation distance if you want to compare trends across genes or drugs. Use the Euclidean or Manhattan distances if you want to compare absolute gene expression or drug sensitivity values.",style="color:darkblue")
				),
    			selectInput("maptype", label=h5(strong("Heatmap Type")),list("p-value estimate", "deciles")),
    			checkboxInput("detail3", "check here for details"),
    			conditionalPanel(
  					condition = "input.detail3 == true",
   					helpText("Heatmap Type: This sets the break points for the heatmap. For p-value estimate, the break points for the bins are set to z-scores of 0, +/- 1.65, +/- 1.96, +/- 2.57, +/- 3.30 and +/- 5 (or the minimum/maximum z-score), equivalent to p-values of > 0.1, <= 0.1, <= 0.05, <= 0.01 and <= 0.001. The method emphasizes the extreme values in the dataset. For deciles, the z-scores are grouped into ten bins each with an equal number of observations. e.g. the bottom 10% of observations will be the darkest blue while the top 10% will be dark red. ",style="color:darkblue")
  				),
  				selectInput("maporder", label=h5(strong("Heatmap Order")),list("use hierarchical cluster order","use alphabetical order")),
    			checkboxInput("detail4", "check here for details"),
    			conditionalPanel(
  					condition = "input.detail4 == true",
  					helpText("Heatmap Order: This will add vertical gridlines to the heatmap to visualize clusters from the hierarchical clustering. Use the dendrogram and the silhouette analysis to help determine an optimal number of clusters. You cannot add gridlines if you order your columns alphabetically.",style="color:darkblue")
				),
				numericInput("cluster", label = h5(strong("Number of Clusters")), value =3),
    			checkboxInput("detail5", "check here for details"),
    			conditionalPanel(
  					condition = "input.detail5 == true",
  					helpText("Number of clusters: Examine the dendrogram for the clustering and the silhouette plot to choose an optimal number of clusters. Optimal clustering is achieved when the mean silhouette is maximized. Use the dendrogram and silhouette table to identify tight clusters and possible outliers in the data.",style="color:darkblue")
				),
			    selectInput("gw", label=("Graph Width"),list("standard","wide")),
    			checkboxInput("detail6", "check here for details"),
    			conditionalPanel(
  					condition = "input.detail6 == true",
  					helpText("Graph Width: this has two options for the width of the heatmap. When you input more than 60 files, you may choose wide option for heatmap",style="color:darkblue")
				),
				p(h4("Step 2: Upload Your File(s)")),
    			fileInput("file", label = NULL,multiple=T),
    			helpText("You can upload multiple Excel files returned by Cell Miner or a single Excel file with data for multiple genes or drugs. In the latter case, please download our template.",style="color:darkblue"),
    			downloadButton('downloadTemplate', 'Template'),
                p(h4("Step 3: Quality Check Datasets")),
    			checkboxInput("check", "check here for details"),
				conditionalPanel(
  					condition = "input.check == true",
  					helpText("Quality Check: The datasets are checked for three issues: 1) the number of assays for each gene or drug, 2) the number of repeated values, 3) the number of missing values and 4) the range of the data. Datasets with multiple assays are likely more accurate than datasets with a single assay. Datasets with many repeated or missing values should be used cautiously because they can generate errors at multiple points in the analysis. For drug sensitivity, the range of the data can help you to infer the units for the drug concentration. Many use the molar concentration but not all. Use caution when combining drug sensitivity data with different units! Cutoffs can be set for experiment number and repeated or missing values.",style="color:darkblue")
				),
				sliderInput("exp", label = h5(strong("Number of Experiments")), min=0,max=10,value =0),
    			helpText("Set a cutoff value for number of experiments to drop datasets.",style="color:darkblue"),
				sliderInput("repvalue", label = h5(strong("Number of Repeated Values")), min=0,max=60,value = 60),
    			helpText("Set a cutoff value for repeated values to drop datasets.",style="color:darkblue"),
    			sliderInput("missingvalue", label = h5(strong("Number of Missing Values")), min=0,max=60,value = 60),
    			helpText("Set a cutoff value for repeated values to drop datasets.",style="color:darkblue"),
    			checkboxInput("celllines", "check here to EXCLUDE specific cell Lines from analysis"),
				conditionalPanel(
  					condition = "input.celllines == true",
  					helpText("You can choose to exclude specific cell lines from your analysis, e.g. too many missing values or issues with the cell line. Check the cell line metadata at", a("CellMiner",href="http://discover.nci.nih.gov/cellminer", style="color:red"),".",style="color:darkblue"),
  					checkboxGroupInput("group", label=h5(strong("Cell Lines EXCLUDE from Analysis")),choices = list("BR:MCF7"=1,"BR:MDA_MB_231"=2,"BR:HS578T"=3,"BR:BT_549"=4,"BR:T47D"=5,"CNS:SF_268"=6,"CNS:SF_295"=7,"CNS:SF_539"=8,"CNS:SNB_19"=9,"CNS:SNB_75"=10,"CNS:U251"=11,"CO:COLO205"=12,"CO:HCC_2998"=13,"CO:HCT_116"=14,"CO:HCT_15"=15,"CO:HT29"=16,"CO:KM12"=17,"CO:SW_620"=18,"LE:CCRF_CEM"=19,"LE:HL_60"=20,"LE:K_562"=21,"LE:MOLT_4"=22,"LE:RPMI_8226"=23,"LE:SR"=24,"ME:LOXIMVI"=25,"ME:MALME_3M"=26,"ME:M14"=27,"ME:SK_MEL_2"=28,"ME:SK_MEL_28"=29,"ME:SK_MEL_5"=30,"ME:UACC_257"=31,"ME:UACC_62"=32,"ME:MDA_MB_435"=33,"ME:MDA_N"=34,"LC:A549"=35,"LC:EKVX"=36,"LC:HOP_62"=37,"LC:HOP_92"=38,"LC:NCI_H226"=39,"LC:NCI_H23"=40,"LC:NCI_H322M"=41,"LC:NCI_H460"=42,"LC:NCI_H522"=43,"OV:IGROV1"=44,"OV:OVCAR_3"=45,"OV:OVCAR_4"=46,"OV:OVCAR_5"=47,"OV:OVCAR_8"=48,"OV:SK_OV_3"=49,"OV:NCI_ADR_RES"=50,"PR:PC_3"=51,"PR:DU_145"=52,"RE:786_0"=53,"RE:A498"=54,"RE:ACHN"=55,"RE:CAKI_1"=56,"RE:RXF_393"=57,"RE:SN12C"=58,"RE:TK_10"=59,"RE:UO_31"=60),inline=F)
                ),
    			p(h4("Step 4: Download")),
    			checkboxInput("download", "check here for details"),
				conditionalPanel(
  					condition = "input.download == true",
  					helpText("You can download multiple tables and plots from your analysis. Please note that the z-score matrix is reordered based on your clustering and heatmap settings. The original data matrix and the quality check table is NEVER reordered.",style="color:darkblue")
				),
    			downloadButton('downloadTable1', 'Input Data Matrix'),
    			downloadButton('downloadTable2', 'Z-score Matrix'),
    			downloadButton('downloadTable3', 'Quality Check'),
    			br(),
    			br(),
    			radioButtons('type','Select figure format',choices=list("pdf","png"),inline=T),
    			downloadButton('downloadFigure1', 'Heatmap'),
    			downloadButton('downloadFigure2', 'Dendrogram')
    		)
     	),
    	mainPanel(
    		tabsetPanel(
    			tabPanel(h4(textOutput("title1")), tableOutput("check")),
    			tabPanel(h4(textOutput("title2")), tableOutput("activity")),
    			tabPanel(h4(textOutput("title3")), tableOutput("zscore")),
    			tabPanel(h4(textOutput("title4")), plotOutput("plot1",height=1000)),
    			tabPanel(h4(textOutput("title5")), plotOutput("plot2",height=600)),
    			tabPanel(h4(textOutput("title6")), plotOutput("plot3",height=600)),  					tabPanel(h4(textOutput("title7")), tableOutput("siltable"))
    		)
       	)
   	)
))
