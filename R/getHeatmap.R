# check arguments
checkArg_getHeatmap <- function( dds, topN, annoGroup){
  assert_that(is(dds, 'DESeqDataSet'))
  assert_that(is.numeric(topN))
  assert_that( all(annoGroup %in% names(colData(dds) )))
}
#' Gives heat-map
#'
#' @param dds DESeqDataSet object
#' @param topN integer; top N highly variable genes to use in heat-map
#' @param annoGroup a character vector; one more meta data column names for annotating heat-map. Default is SampleGroup
#'
#' @return
#' @export getHeatmap
#'
#' @examples
#' @importFrom DESeq2 counts vst
#' @importFrom circlize colorRamp2
#' @importFrom ComplexHeatmap HeatmapAnnotation Heatmap
#' @importFrom matrixStats rowVars
#'
getHeatmap <- function(dds, topN=500, annoGroup = 'SampleGroup'){

  checkArg_getHeatmap(dds=dds, topN = topN, annoGroup = annoGroup)

  rawCounts <- counts(dds, normalized=FALSE)
  trnsCounts <- vst(rawCounts)

  vars <- rowVars(trnsCounts)
  topVriableMat <- trnsCounts[head(order(vars, decreasing=TRUE), n=topN ), ]
  zMatrix <- t(scale(t(topVriableMat), center=TRUE, scale=TRUE))

  absZval <- round(max(abs(range(zMatrix)) ) )
  selColors <- c("blue", "white", "red")
  colRamp <- colorRamp2(c(-absZval, 0, absZval), selColors)

  set.seed(1)
  hmAnno <- HeatmapAnnotation(df = as.data.frame(colData(dds)[,annoGroup, drop=FALSE]))
  Heatmap(zMatrix, name = "z-score",
          col = colRamp,
          show_row_names = FALSE,
          top_annotation =hmAnno
  )
}
