#------------------------------------------------------------------------------
# README.txt,version 1.1 2015/12/03
 
# Read this before use the program: CellMiner Companion
CellMiner Companion is a web application designed to facilitate the exploration and visualization of NCI-60 data retrieved from a CellMiner query, http://discover.nci.nih.gov/cellminer.  You can upload multiple files for gene transcript, drug activity, microRNA and protein assays, and CellMiner Companion will parse the data from these files and assemble a single data matrix.  CellMiner Companion will also check individual datasets for potential issues, allowing users to discard problematic datasets.  The uploaded data will be z-score normalized and visualized as a heatmap.  Hierarchical clustering will also be performed to help users to detect patterns across the data.  Multiple parameters can be adjusted, and the appliation with react by updating the output.  This makes CellMiner Companion an invaluable tool for exploring CellMiner NCI-60 data.

To cite CellMiner Companion: 
Wang et al 2015 CellMiner Companion, bioinformatics (under revision) 

For questions, please contact: 
Pete Pascuzzi: ppascuzz@purdue.edu
#------------------------------------------------------------------------------

# Availability

The source code for CellMiner Companion are available at 
https://github.com/pepascuzzi/CellMinerCompanion.git

#------------------------------------------------------------------------------
# Required packages
CellMiner Companion is implemented in Shiny, a R-based web frame language.
Packages that are used in CellMiner Companion are:

# xlsx 
Adrian A. Dragulescu (2014). xlsx: Read, write, format Excel 2007 and Excel
97/2000/XP/2003 files.R package version 0.5.7.http://CRAN.R-project.org/package=xlsx 

# cluster
Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M., Hornik, K.(2015). cluster:
Cluster Analysis Basics and Extensions. R package version 2.0.1.

# RColorBrewer (Neuwirth, 2014)
Erich Neuwirth (2014). RColorBrewer: ColorBrewer Palettes. R package version 1.1-2.
http://CRAN.R-project.org/package=RColorBrewer

# Hmisc
Frank E Harrell Jr, with contributions from Charles Dupont and many others. (2015). 
Hmisc: Harrell Miscellaneous. R package version 3.16-0. 
http://CRAN.R-project.org/package=Hmisc
# We mainly used cut2 function from Hmisc package. 

#-------------------------------------------------------------------------------
# explanation of parameters used in CellMiner Companion

1. Z-score choice 
This determines how the z-scores are calculated, using either the mean and standard deviation for each gene/drug or a single mean and standard deviation from the entire data matrix. Use by column if you want to compare gene expression or drug sensitivity trends across the cell lines. Use by matrix is you want to compare the absolute gene expression or drug sensitivity values.

2. Distance Choice 
The Pearson correlation distance is best if you want to compare trends across genes or drugs. The Euclidean or Manhattan distances may be better if you want to compare absolute gene expression or drug sensitivity values.

3. Heatmap Type 
This sets the break points for the heatmap. For deciles, the z-scores are grouped into ten bins each with an equal number of observations. e.g. the bottom 10% of observations will be the darkest blue while the top 10% will be dark red. For p-value estimate, the break points for the bins are set to z-scores of 0, +/- 1.65, +/- 1.96, +/- 2.57, +/- 3.30 and +/- 5 (or the minimum/maximum z-score), equivalent to p-values of > 0.1, <= 0.1, <= 0.05, <= 0.01 and <= 0.001. The method emphasizes the extreme values in the dataset.

4. Heatmap Order 
This determines whether the columns of the heatmap will be reordered based on the results of the hierarchical clustering or alphabetical by file name. If you want to force a specific column order, you can add leading text to your file names and order alphabetically.

5. Number of Clusters
This will add vertical gridlines to the heatmap to visualize clusters from the hierarchical clustering. You cannot add gridlines if you order your columns alphabetically.

6. Graph Width
This sets the width of the heatmap.

#-------------------------------------------------------------------------------
# explanation of quality check in CellMiner Companion
CellMiner Companion checks the datasets for three issues: 1) the number of assays for each gene or drug, 2) the number of repeated values, 3) the number of missing values and 4) the range of the data. Datasets with multiple assays are likely more accurate than datasets with a single assay. Datasets with many repeated or missing values should be used cautiously, and can generate errors at multiple points. For drug sensitivity, the range of the data can help you to infer the units for the drug concentration. Many use the molar concentration but not all. Use caution when combining drug sensitivity data with different units! Cutoffs can be set for experiment number and repeated or missing values.

#-------------------------------------------------------------------------------
# end of README.txt
#-------------------------------------------------------------------------------
