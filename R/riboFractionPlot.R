argCheck_riboFractionPlot <- function(countsData, gtfFile){
  assert_that(is.matrix(countsData))
  assert_that(file.exists(gtfFile))
}

#' Get Ribosomal and Nonribosomal counts barplot.
#'
#' @param countsData a matrix; counts matrix preferably raw counts matrix.
#' @param gtfFile a string; gft file name.
#'
#' @return An object created by \code{ggplot}
#' @export riboFractionPlot
#'
#' @examples
#' @import ggplot2
#' @importFrom rtracklayer import.gff
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_relevel
#' @importFrom tibble rownames_to_column


riboFractionPlot <- function(countsData, gtfFile){

  argCheck_riboFractionPlot(countsData = countsData, gtfFile = gtfFile)


  gtf <- tryCatch(import.gff( gtfFile, format='gtf') %>%
                    .[.$type == 'gene'] %>%
                    .[!str_detect(.$gene_biotype, 'pseudogene' )] %>%
                    .[str_detect(.$gene_name, regex('^RP[LS]', ignore_case = T) ) | .$gene_biotype == 'rRNA' ],
                  error = function(cond){
                    message(cond)
                    stop('Provide correct GTF file')
                    }
  )

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
    mutate(Source = fct_relevel(Source, c('Ribosomal', 'Nonribosomal')))

  p <- ggplot(data=sumTab, mapping = aes(x=SampleName, y=percentCounts, fill=Source)) +
    geom_bar( stat = 'identity') +
    labs(
      x='Sample Name',
      y='% Counts',
      title = 'Ribosomal and Nonribosomal read fraction'
    ) +
    scale_fill_manual(values = c('violet', 'tan') ) +
    theme(
      panel.background = element_blank(),
      axis.text.x = element_text(angle=90),
      axis.ticks.x = element_blank(),
      axis.text  = element_text(color='blue', face='bold'),
      legend.position = 'bottom',
      plot.title = element_text(size=15, face='bold', color='brown', hjust = 0.5)
    )

  return(p)
}
