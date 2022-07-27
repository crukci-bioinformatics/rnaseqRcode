checkArg_saveCounts <- function(dds, txi, countsDir, gtf){
  assert_that(is(dds, 'DESeqDataSet'))
  assert_that(!is_empty(txi) & is.list(txi))
  assert_that(dir.exists(countsDir))
  assert_that(is(gtf, 'GRanges'))

}

#' write counts data
#'
#' @param dds DESeqDataSet object; dds after applying estimateSizeFactors
#' @param txi a list; typical output of \code{tximport}
#' @param countsDir a string; directory to write counts files
#' @param gtf a GRanges object; typical output from \code{import.gff}
#'
#' @return NULL
#' @export saveCounts
#'
#' @examples
#'
#' @importFrom dplyr filter select mutate left_join
#' @importFrom tibble rownames_to_column
#' @importFrom readr write_csv
#' @importFrom stringr str_c
#' @importFrom rlang is_empty
#'
saveCounts <- function(dds, txi, countsDir, gtf ){

  checkArg_saveCounts(dds=dds, txi=txi, countsDir=countsDir, gtf=gtf)

  gtfTab <- gtf %>%
    as.data.frame() %>%
    filter(type=='gene') %>%
    dplyr::select(gene_id, gene_name, chr=seqnames, start, end, strand, gene_biotype) %>%
    mutate(gene_name=toupper(gene_name))

  rawCounts <- txi$counts
  mode(rawCounts) <- 'integer'

  rawCounts %>%
    as.data.frame() %>%
    rownames_to_column(var='gene_id') %>%
    left_join(gtfTab, by='gene_id') %>%
    dplyr::select(gene_id, gene_name, chr, start, end, strand, gene_biotype, everything()) %>%
    write_csv(file = str_c(countsDir, 'allRawCounts.csv', sep='/'))

  rawCounts %>%
    .[rowSums(.) > 0, ] %>%
    as.data.frame() %>%
    rownames_to_column(var='gene_id') %>%
    left_join(gtfTab, by='gene_id') %>%
    dplyr::select(gene_id, gene_name, chr, start, end, strand, gene_biotype, everything()) %>%
    write_csv(file = str_c(countsDir, 'filteredRawCounts.csv', sep='/'))

  counts(dds, normalized=TRUE) %>%
    as.data.frame() %>%
    rownames_to_column(var='gene_id') %>%
    left_join(gtfTab, by='gene_id') %>%
    dplyr::select(gene_id, gene_name, chr, start, end, strand, gene_biotype, everything()) %>%
    write_csv(file = str_c(countsDir, 'normalizedCounts.csv', sep='/'))

}
