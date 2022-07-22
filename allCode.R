library(devtools)
#create_package()
#use_git()
library(tximport)
library(tidyverse)
library(DESeq2)
s_sheet <- read_csv("data/samplesheet_corrected.csv")
quantOut <- "data/quantOut"
tx2gene <- read_tsv("data/references/tx2gene.tsv")

use_r('importSampleTxiAndSaveRds')

load_all()

# commit function
#  R CMD check
check()

# edit description
# ctri + .

# add licence
use_mit_license()

###################################################################################
# importSampleTxiAndSaveRds
# add documentation
# open function for documentation
use_r('importSampleTxiAndSaveRds')
# place cursor in somewhere function defamation
# Then code -> insert Roxygen skeleton
# then add required information
# then
document()

txi <- importSampleTxiAndSaveRds(s_sheet = s_sheet,
                                 quantOut = 'data/quantOut',
                                 tx2gene = tx2gene)
###################################################################################
# transformCounts
rawCounts <- txi$counts
mode(rawCounts) <- 'integer'

use_r('transformCounts')

# add documentation using roxygen2
document()

# load functions
load_all()
trnCounts <- transformCounts(rawCounts = rawCounts, countsCutOff = 10, FUN = vst)
###################################################################################


###################################################################################
# PCA plos
use_r('getPcaPlot')
getPcaPlot(countsDat = trnCounts, s_sheet = s_sheet,pcaColFactor = 'SampleGroup')
###################################################################################

###################################################################################
# function argument checking functions
use_r('checkArguments')
###################################################################################

###################################################################################
# assign colors
use_r('assignColors')
assignColors(s_sheet = s_sheet, colorByCol = 'SampleName')
###################################################################################

###################################################################################
# hierarchical clustering plot
use_r('hierarchicalClustPlot')
hierarchicalClustPlot(countsDat=trnCounts, s_sheet = s_sheet, colorByCol = 'SampleGroup', title = 'RNAse exp' )
###################################################################################


###################################################################################
# how to add package name to DESCRIPTION file?
usethis::use_package('ggplot2')
###################################################################################


###################################################################################
# get total reads from salmon output
use_r("getReadCountsFromSalmonLogs")

readsPerSample <- getReadCountsFromSalmonLogs(s_sheet = s_sheet, quantOut = quantOut)
###################################################################################

###################################################################################
# reads per sample barplot
readCounts <- readsPerSample
use_r('readsPerSampleBarPlot')

readsPerSampleBarPlot(readCounts = readsPerSample)
###################################################################################

###################################################################################
# reads / sample group boxplot
use_r('readsPerGroupBoxplot')
readsPerGroupBoxplot(s_sheet = s_sheet, readCounts = readsPerSample)

###################################################################################


###################################################################################
#
###################################################################################
