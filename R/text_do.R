# roxygen2::roxygenise()

#' @title text.DigOrig.BasicProcess
#' @usage text.DigOrig.BasicProcess(x)
#' @export text.DigOrig.BasicProcess
text.DigOrig.BasicProcess <- function (x) {
  x = toupper(x)
  x = text.trim(x)
  # x = text.remove_extra_characters_readable(x)
  x = text.remove_extra_spaces(x)
  x = text.remove_links(x)
  x = text.remove_tabs(x)
  x = text.remove_newlines(x)
  x = text.toASCII(x, TRUE, FALSE, listGMNonASCII, listGMASCII)
  x = text.remove_punctuation(x, " ")
  x = text.remove_extra_spaces(x)
  x = text.trim(x)
  return (x)
}

  # street = chartr("ÑÇ", "NC", street)
  # strCURL = paste0("curl -H \"Accept: application/json\" -H \"Content-type: application/json\" -X POST -d \"{\\\"zipCode\\\" : \\\"",zipcode,"\\\", \\\"street\\\":\\\"",street,"\\\"}\" http://",ip_PuntuaMiZona,"/get_json/ -u ",user_PuntuaMiZona,":",pass_PuntuaMiZona)
  # try_code <- try(
  #   {
  #     message(strCURL)
  #     str_json = system(strCURL, intern=TRUE)
  #     str_json = str_json[length(str_json)]
  #     pos_ini = gregexpr("{", str_json, fixed=TRUE)[[1]]
  #     pos_end = nchar(str_json)
  #     str_json = substr(str_json, pos_ini, pos_end)
  #   }, silent = TRUE )

#' @title text.DigOrig.PuntuaMiZona
#' @usage text.DigOrig.PuntuaMiZona(
#'  street,
#'  zipcode,
#'  ip_PuntuaMiZona = connData$puntuamizona_ip,
#'  user_PuntuaMiZona = connData$puntuamizona_user,
#'  pass_PuntuaMiZona = connData$puntuamizona_pass,
#'  throw_error = texttools:::param.default.text.PuntuaMiZona.ThrowError)
#' @export text.DigOrig.PuntuaMiZona
text.DigOrig.PuntuaMiZona <- function(
  street,
  zipcode,
  ip_PuntuaMiZona = connData$puntuamizona_ip,
  user_PuntuaMiZona = connData$puntuamizona_user,
  pass_PuntuaMiZona = connData$puntuamizona_pass,
  throw_error = texttools:::param.default.text.PuntuaMiZona.ThrowError
) {

  if (throw_error) {
      req <- httr::POST(
        url=paste0("http://",ip_PuntuaMiZona,"/get_json/"),
        httr::authenticate(user_PuntuaMiZona, pass_PuntuaMiZona, type = "basic"),
        httr::add_headers("Content-Type" = "application/json"),
        body = list(zipCode=zipcode, street=street),
        encode="json")

      httr::stop_for_status(req)
      str_json <- httr::content(req, "text", encoding="UTF-8")
      return(jsonlite::fromJSON(str_json))
  } else {
    try_code <- try(
    {
      req <- httr::POST(
        url=paste0("http://",ip_PuntuaMiZona,"/get_json/"),
        httr::authenticate(user_PuntuaMiZona, pass_PuntuaMiZona, type = "basic"),
        httr::add_headers("Content-Type" = "application/json"),
        body = list(zipCode=zipcode, street=street),
        encode="json")

      httr::stop_for_status(req)
      str_json <- httr::content(req, "text", encoding="UTF-8")
      # validate(json)
    }, silent = TRUE )
    if( 'try-error' %in% class(try_code) ) {
      return(NULL)
    } else {
      return(jsonlite::fromJSON(str_json))
    }
  }
}

#' @title text.DigOrig.AddressNormalizer
#' @usage text.DigOrig.AddressNormalizer(
#'  input_address,
#'  input_city,
#'  input_zipcode,
#'  ip_PuntuaMiZona = connData$puntuamizona_ip,
#'  user_PuntuaMiZona = connData$puntuamizona_user,
#'  pass_PuntuaMiZona = connData$puntuamizona_pass)
#' @export text.DigOrig.AddressNormalizer
text.DigOrig.AddressNormalizer <- function (
  input_address,
  input_city,
  input_zipcode,
  ip_PuntuaMiZona = connData$puntuamizona_ip,
  user_PuntuaMiZona = connData$puntuamizona_user,
  pass_PuntuaMiZona = connData$puntuamizona_pass
) {
  dfParsed = text.address.parser(input_address, dict_fixed, dict_regexp, dict_regexp_number)
  final_bsc_street = dfParsed$street
  final_bsc_number = dfParsed$number
  final_bsc_city = input_city
  final_bsc_zipcode = input_zipcode

  listPuntuaMiZona = text.DigOrig.PuntuaMiZona(final_bsc_street, input_zipcode, ip_PuntuaMiZona, user_PuntuaMiZona, pass_PuntuaMiZona)
  final_pmz_street = listPuntuaMiZona$street
  final_pmz_number = dfParsed$number
  final_pmz_city = listPuntuaMiZona$city
  final_pmz_zipcode = listPuntuaMiZona$zipCode

  final_pmz_street = text.DigOrig.BasicProcess(final_pmz_street)
  final_pmz_street = text.address.add_street(final_pmz_street)

  final_bsc_city = text.DigOrig.BasicProcess(final_bsc_city)
  final_pmz_city = text.DigOrig.BasicProcess(final_pmz_city)

  final_bsc_zipcode = text.DigOrig.BasicProcess(final_bsc_zipcode)
  final_pmz_zipcode = text.DigOrig.BasicProcess(final_pmz_zipcode)

  ind_zipcode_ini = as.numeric(substr(listPuntuaMiZona$zipCode, 1, 2) == substr(input_zipcode, 1, 2))
  ind_zipcode_end = (
    as.numeric(substr(listPuntuaMiZona$zipCode, 3, 3)==substr(input_zipcode, 3, 3)) +
      as.numeric(substr(listPuntuaMiZona$zipCode, 4, 4)==substr(input_zipcode, 4, 4)) +
      as.numeric(substr(listPuntuaMiZona$zipCode, 5, 5)==substr(input_zipcode, 5, 5))
  )

  if (ind_zipcode_ini==1 && ind_zipcode_end>=2) {
    final_street = final_pmz_street
    final_number = final_pmz_number
    final_city = final_pmz_city
    final_zipcode = final_pmz_zipcode
  } else {
    final_street = final_bsc_street
    final_number = final_bsc_number
    final_city = final_bsc_city
    final_zipcode = final_bsc_zipcode
  }

  if (is.na(final_city) || final_city == "") {
    if (!is.na(final_pmz_city) && final_pmz_city != "") {
      final_city = final_pmz_city
    }
    if (!is.na(final_bsc_city) && final_bsc_city != "") {
      final_city = final_bsc_city
    }
  }

  if (is.na(final_zipcode) || final_zipcode == "") {
    if (!is.na(final_pmz_zipcode) && final_pmz_zipcode != "") {
      final_zipcode = final_pmz_zipcode
    }
    if (!is.na(final_bsc_zipcode) && final_bsc_zipcode != "") {
      final_zipcode = final_bsc_zipcode
    }
  }

  return(list(
    final_street = final_street,
    final_number = final_number,
    final_city = final_city,
    final_zipcode = final_zipcode,
    final_bsc_street = final_bsc_street,
    final_bsc_number = final_bsc_number,
    final_bsc_city = final_bsc_city,
    final_bsc_zipcode = final_bsc_zipcode,
    final_pmz_street = final_pmz_street,
    final_pmz_number = final_pmz_number,
    final_pmz_city = final_pmz_city,
    final_pmz_zipcode = final_pmz_zipcode
  ))
}

#' @title text.DigOrig.AddressNormalizer.Debug
#' @usage text.DigOrig.AddressNormalizer.Debug(
#'  input_address,
#'  input_city,
#'  input_zipcode,
#'  ip_PuntuaMiZona = connData$puntuamizona_ip,
#'  user_PuntuaMiZona = connData$puntuamizona_user,
#'  pass_PuntuaMiZona = connData$puntuamizona_pass)
#' @export text.DigOrig.AddressNormalizer.Debug
text.DigOrig.AddressNormalizer.Debug <- function (
  input_address,
  input_city,
  input_zipcode,
  ip_PuntuaMiZona = connData$puntuamizona_ip,
  user_PuntuaMiZona = connData$puntuamizona_user,
  pass_PuntuaMiZona = connData$puntuamizona_pass
) {
  output = text.DigOrig.AddressNormalizer(
    input_address,
    input_city,
    input_zipcode,
    ip_PuntuaMiZona, user_PuntuaMiZona, pass_PuntuaMiZona)

  message(paste(output$final_bsc_street, output$final_bsc_number, output$final_bsc_city, output$final_bsc_zipcode, sep=" | "))
  message(paste(output$final_pmz_street, output$final_pmz_number, output$final_pmz_city, output$final_pmz_zipcode, sep=" | "))
  message(paste(output$final_street, output$final_number, output$final_city, output$final_zipcode, sep=" | "))

  invisible(output)
}
