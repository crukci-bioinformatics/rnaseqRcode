argCheck_riboFractionPlot <- function(countsData, gtf, s_sheet){
  assert_that(is.matrix(countsData))
  assert_that(is(gtf, 'GRanges') )
  is_validMetaData(s_sheet = s_sheet, columnsToCheck = 'SampleName')
}

#' Get Ribosomal and Nonribosomal counts barplot.
#'
#' @param countsData a matrix; counts matrix preferably raw counts matrix.
#' @param gtf a GRanges object; gft
#' @param s_sheet  a data frame; sample meta data sheet
#'
#' @return An object created by \code{ggplot}
#' @export riboFractionPlot
#'
#' @examples
#' @import ggplot2
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_relevel
#' @importFrom tibble rownames_to_column


riboFractionPlot <- function(countsData, gtf, s_sheet){

  argCheck_riboFractionPlot(countsData = countsData,
                            gtf = gtf,
                            s_sheet = s_sheet)

  s_sheet <- s_sheet %>%
    arrange(SampleGroup)

  gtf <- gtf %>%
    .[.$type == 'gene'] %>%
    .[!str_detect(.$gene_biotype, 'pseudogene' )] %>%
    .[str_detect(.$gene_name, regex('^RP[LS]', ignore_case = TRUE) ) | .$gene_biotype == 'rRNA' ]

  countsData <- countsData[ rowSums(countsData) > 0, ] %>%
    as.data.frame() %>%
    rownames_to_column(var='gene_id') %>%
    mutate(Source = if_else(gene_id %in% gtf$gene_id, 'Ribosomal', 'Nonribosomal'))

  sumTab <- countsData %>%
    group_by(Source) %>%
    summarise_if( is.numeric, sum) %>%
    pivot_longer(cols = -Source, names_to = 'SampleName', values_to = 'counts') %>%
    ungroup()

  totCounts <- sumTab %>%
    group_by(SampleName) %>%
    summarise(totalCounts = sum(counts))

  sumTab <- inner_join(sumTab, totCounts, by='SampleName') %>%
    mutate(percentCounts = (counts / totalCounts) * 100 ) %>%
    mutate(Source = fct_relevel(Source, c('Ribosomal', 'Nonribosomal'))) %>%
    mutate(SampleName = factor(SampleName, levels = as.vector(s_sheet$SampleName)))


  p <- ggplot(data=sumTab, mapping = aes(x=SampleName, y=percentCounts, fill=Source)) +
    geom_bar( stat = 'identity') +
    labs(
      x='Sample Name',
      y='% Counts',
      title = 'Ribosomal and Nonribosomal read fraction'
    ) +
    theme(
      panel.background = element_blank(),
      axis.text.x = element_text(angle=90),
      legend.position = 'bottom',
      plot.title = element_text(size=15, face='bold', color='black', hjust = 0.5)
    )

  return(p)
}
