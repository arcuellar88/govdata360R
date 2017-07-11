#' Call the Data Catalog API
#'
#' Helper function for the data catalog call for Indicators metadata
#' JSON
#'
#' @param url A charcter string. A formatted url string
#' @note This call is seperate because the data catalog is actaully a different
#'  API and therefore has a different return structure.
#' @return A data frame
govdata360get.rawInd <- function(url) {

  return_get <- httr::GET(url)
  return_json <- httr::content(return_get, as = "text")
  return_list <- jsonlite::fromJSON(return_json,  flatten = TRUE)

  if(length(return_list$data)>0)
  {

    numCountry <- NROW(return_list$countries)

    countryData <- list()

    for (c in 1:numCountry){

      numInd <- NROW(return_list$data$indicators[[c]])

      country=rep(return_list$data$id[c],numInd)

      countryData[[c]] = cbind(country,return_list$data$indicators[[c]][-2])

      #Transforma data
      countryData[[c]] = countryData[[c]] %>% gather('year','value',3:length(countryData[[c]]))

      countryData[[c]] <-  countryData[[c]][!is.na( countryData[[c]]$value),] #remove rows where value=NA
    }


    #Merge all countries into one data frame
    result = dplyr::bind_rows(countryData)

    #format year
    result =as.data.frame( apply(result,2, function(x) gsub("values.","",x)))

     result

  }
  else
    print("No Data")


}

#' Call the Data Catalog API
#'
#' Helper function for the data catalog call
#'
#' @param url A charcter string. A formatted url string
#' @note This call is seperate because the data catalog is actaully a different
#'  API and therefore has a different return structure.
#' @return A data frame
govdata360get.raw <- function(url) {

  return_get <- httr::GET(url)
  return_json <- httr::content(return_get, as = "text")
  return_list <- jsonlite::fromJSON(return_json,  flatten = TRUE)

  return_list
}

#' Internal function to get the urls of the API
#'
#' List with 2 entries. The first one with the base_url and the second one with the utils_url#'
#'
#' @return List with 2 entries. The first one with the base_url and the second one with the utils_url#'
gov360IndURL <- function() {

  base_url <- "http://govdata360-backend.worldbank.org/api/v1/"

  base_url
}
