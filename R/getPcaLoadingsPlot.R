# check arguments
checkArg_getPcaLoadingsPlot <- function( countsData, s_sheet, gtf,
                                         topN, genesToShow, columnsToCheck){
  assert_that(is.numeric(topN))
  assert_that(is.character(genesToShow) | is.null(genesToShow))
  assert_that(is.matrix(countsData))
  assert_that(is_validMetaData(s_sheet = s_sheet, columnsToCheck = columnsToCheck))
  assert_that(is(gtf, 'GRanges'))
}
#' Gives PCA loadings rank plot
#'
#' @param countsData a counts matrix
#' @param s_sheet a data frame; sample sheet used for plot aesthetics
#' @param gtf a GRanges object; typical output from \code{import.gff}
#' @param topN integer; number of top gene names to show on plot; default top 10
#' @param genesToShow a string; list genes to show
#'
#' @return An object created by \code{ggplot}
#' @export getPcaLoadingsPlot
#'
#' @examples
#'
#' @import ggplot2
#' @importFrom dplyr filter mutate if_else select left_join pull
#' @importFrom stringr str_c str_split
#' @importFrom magrittr %>%
#' @importFrom assertthat is.string
#' @importFrom tibble rownames_to_column
#' @importFrom purrr map_chr
#' @importFrom ggrepel geom_text_repel
#'
getPcaLoadingsPlot <- function(countsData, s_sheet, gtf, topN= 10, genesToShow=NULL){

  checkArg_getPcaLoadingsPlot(countsData=countsData,
                              s_sheet=s_sheet,
                              gtf=gtf,
                              topN=topN,
                              genesToShow=genesToShow,
                              columnsToCheck = 'SampleName'
                              )

  pcaData <- prcomp(t(countsData), center = TRUE, scale. = TRUE )

  loadings <- pcaData$rotation %>%
    as.data.frame() %>%
    rownames_to_column(var='gene_id')

  geneTab <- gtf %>%
    as.data.frame() %>%
    filter(type == 'gene' ) %>%
    select(gene_id, gene_name) %>%
    mutate(gene_name = toupper(gene_name)) %>%
    filter(!duplicated(gene_id))

  plotData <- left_join(loadings, geneTab, by='gene_id') %>%
    mutate(PC_rank = rank(PC1))

  topNfeatures <- plotData %>%
    filter( rank(PC1) <= topN | rank(-PC1) <= topN) %>%
    pull(gene_name)

  if(!is.null(genesToShow)){
    genesToShow <- genesToShow %>%
      str_split(.,',', simplify = T) %>%
      as.vector() %>%
      map_chr(str_remove_all, ' ')
    topNfeatures <- c(topNfeatures, genesToShow)
  }

  texData <- plotData %>%
    filter(gene_name %in% topNfeatures)

  p <- ggplot(plotData, aes(x=PC_rank, y=PC1, label = gene_name)) +
    geom_point( alpha=0.2, shape=21, size=3, fill='grey') +
    ggrepel::geom_text_repel( data=texData, max.overlaps=30, size=3, color='black', fontface='bold') +
    labs(
      x="Rank (PC1 Loadings)",
      y='PC1',
      title = "PC1's largest contributing genes"
    ) +
    theme(
      panel.background = element_blank(),
      axis.text = element_text( color = 'blue'),
      axis.text.x = element_text(angle=90),
      plot.title = element_text(color='brown', hjust = 0.5, size=15, face='bold')
    )

  return(p)

}


