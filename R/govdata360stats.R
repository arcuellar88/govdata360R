#' Pull the data for some indicators and countries. 
#' 
#' Data frame with the data of the specified indicators and countries
#' All countries and All indicators cannot be requested
#' When All indicators data is requested, data can be requested for a maximum of 4 countries
#' When All country data is requested, data can be requested for a maximum of 10 indicators (see iadbstats.list)
#' 
#' @param country Character string. ISO3 code of the countries. E.g COL for Colombia or 'ALL' for all the countries
#' @param dateRange Character string. Range of years of the indicators 
#' @param indicatorcode codes of the indicators separated by comma. 'All' for all the indicators
#' @return Data frame with  CountryCode, CountryTableName, IndicatorCode, IndicatorName, TopicName, SubTopicName, Year, Quarter,
#' AggregationLevel, AggregatedValue, UOM 
#' @export
#' @examples
#' gov360stats(country="COL",dateRange="2010",indicatorcode="27712")
#' gov360stats(country="ARG,COL",dateRange="2010,2011",indicatorcode="27712")
gov360stats <- function(country="ALL",dateRange="2010",indicatorcode="ALL"){

  if(country=="ALL" && indicatorcode=="ALL") stop("All countries and All indicators cannot be requested")
  
  if(indicatorcode=="ALL"&& stringr::str_count(country, ',')>3) stop("When All indicators data is requested, data can be requested for a maximum of 4 countries")
  
  if(country=="ALL"&& stringr::str_count(indicatorcode, ',')>9) stop("When All country data is requested, data can be requested for a maximum of 10 indicators")
  
  urlmeta <- "data/?"
  url <- ""
  if(country=='ALL')
  {
    searchTimeFrames <- paste0("timeframes=",dateRange)
    searchIndicator <- paste0("&indicators=",indicatorcode)
    
    urlData <- gov360IndURL()
    
    url <- paste0(urlData,urlmeta,searchTimeFrames,searchIndicator)
  }
  else
  {
    searchCountry <- paste0("countries=",country)
    searchTimeFrames <- paste0("&timeframes=",dateRange)
    searchIndicator <- paste0("&indicators=",indicatorcode)
    
    urlData <- gov360IndURL()
    
    #REST API URL  
    url <- paste0(urlData,urlmeta,searchCountry,searchTimeFrames,searchIndicator)
  }
 
  govdata360get.rawInd(url)  
}

#' Pull the data for a vector of indicators and ALL the countries. 
#' 
#' Data frame with the data of the specified indicators and countries
#' 
#' @param dateRange Character string. Range of years of the indicators 
#' @param indicatorCodes vector of indicator codes c("27712","27870")
#' @return Data frame with  CountryCode, CountryTableName, IndicatorCode, IndicatorName, TopicName, SubTopicName, Year, Quarter, AggregationLevel, AggregatedValue, UOM 
#' @export
#' @examples
#' codes=c("27712","27870")
#' gov360stats.list(indicatorCodes=codes)
gov360stats.list <- function(dateRange="2010",indicatorCodes){

  scountry="ALL"
 
  #split the codes into groups of maximum 10 indicator
  df = split(indicatorCodes, ceiling(seq_along(indicatorCodes)/10))
  
  indicator_list = list()
  
  for(i in 1:length(df))
  {
    #Pull the data for a subset of 10 indicators
    ind = paste(as.character(df[[1]]),collapse=",")
    print(ind)
    indicator_list[[i]] = gov360stats(country=scountry,dateRange=dateRange,indicatorcode=ind)
  }  
  
  #Combine results into one data frame
  full_data = dplyr::bind_rows(indicator_list) 
  
  full_data
}


#' Get the metadata of the indicators
#
#' @param dsId The id of the dataset
gov360DataSet <- function(dsId)
{
  library(stringr)
  URL <- "http://govdata360-backend.worldbank.org/api/v1/datasets/?/dump.csv"
  URL <- gsub("\\?",dsId,URL)
  
  destfile <- "dataset?.csv"
  destfile <- gsub("\\?",dsId,destfile)
  
  download.file(URL, destfile)
  
  df<-read.csv(destfile, encoding = 'UTF-8')
  
  df
}
