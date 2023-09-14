#' Add gene information from GTF to given data frame
#'
#' @param res a data frame
#' @param gtf a GRanges object
#'
#' @return a data frame
#' @export addGeneInfoFromGtfToResTab
#'
#' @examples
#' @importFrom dplyr filter select mutate left_join arrange everything
#' @importFrom stringr str_replace_all
#' @importFrom tibble rownames_to_column
#'
addGeneInfoFromGtfToResTab <- function(res,gtf){

  gtfTab <- gtf %>%
    as.data.frame() %>%
    filter(type=='gene') %>%
    dplyr::select(gene_id, gene_name, chr=seqnames, start, end, strand, gene_biotype) %>%
    #mutate(gene_name=toupper(gene_name)) %>%
    mutate(gene_biotype = str_replace_all(gene_biotype, '_', ' '))

  res <- res %>%
    rownames_to_column(var='gene_id') %>%
    left_join(gtfTab, by='gene_id') %>%
    dplyr::select(gene_id,gene_name, everything()) %>%
    arrange(padj)

  return(res)

}
