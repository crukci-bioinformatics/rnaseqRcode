library(devtools)
#create_package()
#use_git()
library(tximport)
library(tidyverse)
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

# add documentation
# open function for documentation
use_r('importSampleTxiAndSaveRds')
# place cursor in somewhere function defanation
# Then code -> insert Roxygen skeleton
# then add required information
# then
document()


