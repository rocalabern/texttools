# roxygen2::roxygenise()

#' @title google.full_address_df
#' @usage google.full_address_df(dfInput, removeFlat = TRUE)
#' @export google.full_address_df
google.full_address_df <- function(dfInput, removeFlat = TRUE) {
  google.full_address(dfInput$address, dfInput$city, dfInput$province, dfInput$zipcode, removeFlat)
}

#' @title google.full_address
#' @usage google.full_address(address, city, province, zipcode, removeFlat = TRUE)
#' @export google.full_address
google.full_address <- function(address, city, province, zipcode, removeFlat = TRUE) {
  if (removeFlat) {
    listInputAddressWithoutNumber = gsub("[[:digit:]]", "", address)
    listInputAddressFirstNumber = text.get_first_number(address)

    output = paste0(
      listInputAddressWithoutNumber, ", ",
      listInputAddressFirstNumber, ", ",
      zipcode, " ",
      city, ", ",
      province)
  } else {
    output = paste0(
      address, ", ",
      zipcode, " ",
      city, ", ",
      province)
  }
  return (output)
}

#' @title google.normalize_df
#' @usage google.normalize_df(dfInput)
#' @export google.normalize_df
google.normalize_df <- function(dfInput) {
  google.normalize(dfInput$address, dfInput$city, dfInput$province, dfInput$zipcode)
}

#' @title google.normalize
#' @usage google.normalize(address, city, province, zipcode)
#' @export google.normalize
google.normalize <- function(address, city, province, zipcode) {
  full_address = google.full_address(address, city, province, zipcode, TRUE)
  normalized_address = full_address
  for (i in 1:length(full_address)) {
    dfGoogle = google.geoCode(full_address[i])
    if (!is.null(dfGoogle)) {
      if (dfGoogle$location_type[1] == "ROOFTOP") {
        normalized_address[i] = dfGoogle$formatted_address[1]
      }
    }
  }
  return(normalized_address)
}

#' @title google.geoCode
#' @usage google.geoCode(address, return.call = "json", sensor = "false", verbose = FALSE)
#' @export google.geoCode
google.geoCode <- function(address, return.call = "json", sensor = "false", verbose = FALSE) {
  if(verbose) message(address)
  root <- "http://maps.google.com/maps/api/geocode/"
  strURL <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  if(verbose) message(strURL)
  u <- URLencode(strURL)
  doc <- RCurl::getURL(u)
  x <- RJSONIO::fromJSON(doc, simplify = FALSE)
  if(verbose) message(paste0("STATUS = ", x$status))
  if(x$status=="OK") {
    nResults = length(x$results)
    if(verbose) cat(paste0("Results : ",nResults,"\n"))
    output = data.frame(lat=numeric(nResults), lng=numeric(nResults), location_type=character(nResults), formatted_address=character(nResults), stringsAsFactors = FALSE)
    for (i in 1:nResults) {
      lat <- x$results[[i]]$geometry$location$lat
      lng <- x$results[[i]]$geometry$location$lng
      location_type  <- x$results[[i]]$geometry$location_type
      formatted_address  <- x$results[[i]]$formatted_address
      output$lat[i] = lat
      output$lng[i] = lng
      output$location_type[i] = location_type
      output$formatted_address[i] = formatted_address
    }
    return(output)
  } else {
    return(NULL)
  }
}
