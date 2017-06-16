#' Get the metadata of the indicators
#'
#' Data frame with series code, name, description, and source for the govdata360
#' @param nameSearch search indicator by name
#' @param sourceSearch search indicator by source
#' @return Data frame with id, name, dataset, valueType, datasetID,
#' defaultViz, doNotUseVix, rank, definition, units, subIndicatorType, timeFrames, byProduct, byPartner, periodicity and dateRange
#' @examples
#' gov360msearch()
#' @export
gov360msearch <- function(nameSearch="", sourceSearch=""){
  
  
  url <- gov360IndURL()
  indURL <- "indicators/"
  indicatorsGov360 <<- govdata360get.raw(paste0(url,indURL)) 
  
  if(nameSearch!="")
  {
    indicators <- subset(indicatorsGov360, grepl(nameSearch, name,ignore.case=TRUE))
  }
  
  if(sourceSearch!="")
  {
    indicators <- subset(indicatorsGov360, grepl(sourceSearch, dataset,ignore.case=TRUE))
  }
  
  if(nameSearch=="" && sourceSearch=="")
  {
    indicators <- indicatorsGov360
  }
  
  indicators

}

