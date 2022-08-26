# check arguments
checkArg_get3dPCAplot <- function(countsDat, s_sheet, columnsToCheck) {
  assert_that(is.matrix(countsDat))
  assert_that(is_validMetaData(s_sheet = s_sheet, columnsToCheck = columnsToCheck))
}

#' Gives 3d PCA plot
#'
#' @param countsDat a counts matrix
#' @param s_sheet a data frame; sample sheet used for plot aesthetics
#' @param pcaColFactor a character vector with one value; column name in a sample sheet for PCA color aesthetic
#'
#' @return a plot object created by plot_ly
#' @export get3dPCAplot
#'
#' @examples
#' @importFrom stringr str_c
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr inner_join
#' @importFrom plotly plot_ly add_markers layout
#' @importFrom magrittr %>%

get3dPCAplot <- function(countsDat, s_sheet, pcaColFactor="SampleGroup"){

  checkArg_get3dPCAplot(countsDat=countsDat,
                        s_sheet=s_sheet,
                        columnsToCheck=pcaColFactor
                        )

  pcaData <- prcomp(t(countsDat), center = TRUE, scale. = TRUE, rank. = 3 )

  expVar <- round(((pcaData$sdev)^2 / sum((pcaData$sdev)^2 ) ) * 100, digits = 0 ) %>%
    str_c(.,'%', sep='')
  pc1Var <- str_c('PC1 (', expVar[1], ')', sep='' )
  pc2Var <- str_c('PC2 (', expVar[2], ')', sep='' )
  pc3Var <- str_c('PC3 (', expVar[3], ')', sep='' )

  components <- pcaData[["x"]] %>%
    as.data.frame() %>%
    rownames_to_column('SampleName')

  components <- inner_join(components, s_sheet, by='SampleName')

  fig <- plot_ly(data=components,
                 x = ~PC1,
                 y = ~PC2,
                 z = ~PC3,
                 color = ~SampleGroup
                 ) %>%
    add_markers()

  fig <- fig %>%
    layout(
      title = 'title',
      scene = list(bgcolor = "white",
                   xaxis=list(title=list(text=pc1Var)),
                   yaxis=list(title=list(text=pc2Var)),
                   zaxis=list(title=list(text=pc3Var))
                   )
    )

  return(fig)
}
