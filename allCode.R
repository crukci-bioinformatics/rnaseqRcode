library(devtools)
#create_package()
#use_git()
library(tximport)
library(tidyverse)
library(DESeq2)
s_sheet <- read_csv("data/samplesheet_corrected.csv") %>%
  arrange(SampleGroup,Replicate)
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
# corrillationPlot
use_r("correlationPlot")
correlationPlot(countsMat = trnCounts, s_sheet = s_sheet)
###################################################################################


###################################################################################
# Fraction of ribosomal genes
gtf <- loadGTF(gtfFile = 'data/references/mmu.GRCm38.gtf')
use_r('riboFractionPlot')
riboFractionPlot(countsData = rawCounts, gtf = gtf )
###################################################################################


###################################################################################
# counts density plot
use_r('countsDensityPlot')
countsDensityPlot(s_sheet = s_sheet, countsData = rawCounts)
###################################################################################


###################################################################################
# DESeq2
use_r("createDdsAndDESeq")
design <- checkAndCreateCorrectDesignString(s_sheet = s_sheet, design = 'SampleGroup')
dds <- createDdsAndDESeq(txi=txi, s_sheet = s_sheet, design = design )
###################################################################################


###################################################################################
# plot dispersion Esimates
plotDispEsts(dds)
###################################################################################

###################################################################################
use_r('normFactorsBoxplot')
normFactorsBoxplot(dds=dds)
###################################################################################

###################################################################################
# save counts after adding gene symbols
use_r('saveCounts')
countsDir <- 'data/counts'
saveCounts(dds = dds, txi = txi, countsDir = countsDir, gtf = gtf )
###################################################################################


###################################################################################
# write DE results
contrastFile = 'contrasts.csv'
factorName <- 'SampleGroup'
numerator <- 'Infected_d33'
denominator <- 'Uninfected_d33'
use_r("writeAndPlotDEResults")

res <- writeAndPlotDEResults(dds = dds,
                      factorName = factorName,
                      numerator=numerator,
                      denominator=denominator,
                      gtf = gtf,
                      DeOutDir = DeOutDir,
                      pValCutoff = 0.05
                      )
###################################################################################

###################################################################################
# MA plot
use_r('getMAplot')

getMAplot(res = res, numerator = numerator,
          denominator=denominator,
          topN=20, genesToShow = c('ESR1'))
###################################################################################


###################################################################################
# pvalue distribution plot
use_r("getPvalDistriPlot")

getPvalDistriPlot(res = res,
                  numerator = numerator,
                  denominator=denominator,
                  pValCutoff = pValCutoff)

###################################################################################


###################################################################################
# volcano plot
use_r("getVolcanoPlot")
getVolcanoPlot(res=res, numerator = numerator,
               denominator = denominator,
               topN = 100, genesToShow = 'ESR1',
               pValCutoff = 0.05)
###################################################################################


###################################################################################
# hearmap
use_r('getHeatmap')

getHeatmap(dds=dds, topN=500, annoGroup = c('SampleGroup'))
###################################################################################

###################################################################################
###################################################################################
# I am not sure why? But load_all() loads all functions including helper functions.
# but with install() these functions are not accessible
# therefore decided to create separate functions

use_r('is_validMetaData')

use_r('is_validTx2gene')

use_r('is_validColor')

use_r('is_validSampleColors')

use_r("is_fileExists")

use_r("checkAndCreateCorrectDesignString")

use_r("loadGTF")

use_r("addGeneInfoFromGtfToResTab")

###################################################################################
###################################################################################




###################################################################################
use_r('plotGeneBiotypes')
plotGeneBiotypes(countsData = rawCounts, gtf=gtf)
###################################################################################

###################################################################################
# add library complexity plot
use_r('getLibraryComplexityPlot')
getLibraryComplexityPlot(countsData = rawCounts, s_sheet = s_sheet)
###################################################################################


###################################################################################
# Gene detection plot
use_r('getGeneDetctionPlot')

getGeneDetctionPlot(countsData = rawCounts, s_sheet = s_sheet)

###################################################################################


###################################################################################
# Counts distribution plots on random genes
use_r("getGeneCountsPlot")
getGeneCountsPlot(dds=dds,
                  numerator = numerator, denominator = denominator,
                  factorName = factorName,
                  gtf=gtf,
                  pValCutoff=pValCutoff,
                  topN=10, genesToShow=genesToShow)
###################################################################################


###################################################################################
# get PCA loading plots
use_r('getPcaLoadingsPlot')
getPcaLoadingsPlot(countsData=trnCounts, s_sheet = s_sheet,
                   genesToShow = NULL, gtf = gtf, topN = 10)

###################################################################################
###################################################################################
# # karyogram plot
use_r("getKaryogramPlot")
getKaryogramPlot(dds=dds,
                 numerator = numerator,
                 denominator = denominator,
                 factorName = factorName,gtf = gtf,
                 pValCutoff = pValCutoff,
                 genome = genome

                  )
###################################################################################


###################################################################################
# install package
install()
###################################################################################
