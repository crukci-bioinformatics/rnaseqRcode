# check arguments
checkArg_plotGeneBiotypes <- function(countsData, gtf, s_sheet){
  assert_that(is.matrix(countsData))
  assert_that(is(gtf, 'GRanges'))
  assert_that(is_validMetaData(s_sheet, columnsToCheck = c('SampleName')))
}

#' gives Gene biotypes read share
#'
#' @param countsData counts matrix
#' @param gtf GRanges object
#' @param s_sheet a data frame; sample metadata
#'
#' @return An object created by \code{ggplot}
#' @export plotGeneBiotypes
#'
#' @examples
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom stringr str_replace_all str_detect
#' @importFrom magrittr %>%
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#'

plotGeneBiotypes <- function(countsData, gtf, s_sheet){

  checkArg_plotGeneBiotypes(countsData = countsData, gtf=gtf, s_sheet=s_sheet)


  s_sheet <- s_sheet %>%
    arrange(SampleGroup)

  otherType <- c('TEC', 'sense overlapping', 'antisense',
                 'sense intronic', 'processed transcript',
                 'misc RNA', 'ribozyme')
  smallRNA <- c( 'miRNA', 'scRNA', 'snRNA', 'snoRNA', 'sRNA', 'scaRNA')
  mtRNA <- c( 'Mt rRNA', 'Mt tRNA')

  gtfTab <- gtf %>%
    as.data.frame() %>%
    filter(type=='gene') %>%
    dplyr::select(gene_id, gene_biotype) %>%
    mutate(gene_biotype = str_replace_all(gene_biotype, '_', ' ')) %>%
    mutate(gene_biotype = if_else(str_detect(gene_biotype, 'pseudogene'), 'pseudogene', gene_biotype)) %>%
    mutate(gene_biotype = if_else(str_detect(gene_biotype, regex('^TR|IG.*gene$')), 'IG/TR gene', gene_biotype)) %>%
    mutate(gene_biotype = if_else(str_detect(gene_biotype,regex('l*ncRNA')), 'lncRNA', gene_biotype)) %>%
    mutate(gene_biotype = if_else(gene_biotype %in% otherType, 'Other', gene_biotype)) %>%
    mutate(gene_biotype = if_else(gene_biotype %in% smallRNA, 'small RNA', gene_biotype)) %>%
    mutate(gene_biotype = if_else(gene_biotype %in% mtRNA, 'Mt RNA', gene_biotype))


  countsData <- countsData%>%
    as.data.frame() %>%
    mutate_if(is.numeric, as.integer) %>%
    rownames_to_column(var='gene_id') %>%
    left_join(gtfTab, by='gene_id') %>%
    dplyr::select(gene_id, gene_biotype, everything()) %>%
    pivot_longer(cols = -gene_id:-gene_biotype, names_to = 'SampleName', values_to = 'counts') %>%
    drop_na() %>%
    group_by(SampleName, gene_biotype) %>%
    summarise(counts= sum(counts))


  totCountsTab <- countsData %>%
    ungroup() %>%
    group_by(SampleName) %>%
    summarise(totalCounts = sum(counts) )

  combTab <- left_join(countsData, totCountsTab, by='SampleName') %>%
    mutate(countsPecent = round((counts/totalCounts) * 100, digits = 2)) %>%
    mutate( SampleName = factor(SampleName, levels = s_sheet$SampleName))

  p <- ggplot(combTab, aes(x=SampleName, y=countsPecent, group=gene_biotype, color=gene_biotype)) +
    geom_point() +
    geom_line() +
    labs(
      x = 'Sample Name',
      y = '% reads',
      title='Gene biotypes read share',
      color='Gene biotype'
    ) +
    theme_classic() +
    theme(
      panel.background = element_blank(),
      #axis.text = element_text(color='blue'),
      plot.title = element_text(hjust = 0.5, color='black', size=15, face='bold'),
      axis.text.x = element_text(angle = 90)
    )

  return(p)

}
