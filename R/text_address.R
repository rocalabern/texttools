# roxygen2::roxygenise()

#' @title text.address.parser
#' @export
text.address.parser <- function(
  address,
  # city, province, zipcode,
  dict_fixed = NULL,
  dict_regexp = NULL,
  dict_regexp_number = NULL,
  verbose=FALSE
) {

  ############################## DELETE \ DEBUG
  if (FALSE) {
    detach(df)
    attach(df)

    address = df$address
    city = df$city
    province = df$province
    zipcode = df$zipcode

    dict_fixed = fread("dictionaries/dict_fixed.txt", colClasses=c("character", "character"), strip.white=FALSE, encoding="UTF-8")
    dict_regexp = fread("dictionaries/dict_regexp.txt", colClasses=c("character", "character"), strip.white=FALSE, encoding="UTF-8")

    verbose = TRUE
  }

  # PREPROCESS STRING
  mod_address = address
  mod_address = toupper(mod_address)
  mod_address = text.trim(mod_address)
  mod_address = text.remove_extra_characters_readable(mod_address)
  mod_address = text.remove_extra_spaces(mod_address)
  mod_address = text.remove_links(mod_address)
  mod_address = text.remove_tabs(mod_address)
  mod_address = text.remove_newlines(mod_address)
  mod_address = text.toASCII(mod_address, TRUE, FALSE, listGMNonASCII, listGMASCII)
  if (!is.null(dict_fixed)) {
    for (i_row in 1:nrow(dict_fixed)) {
      if (nchar(dict_fixed$pattern[i_row])>0) {
        mod_address = gsub(dict_fixed$pattern[i_row], dict_fixed$replacement[i_row], mod_address, fixed = TRUE)
        mod_address = text.remove_extra_spaces(mod_address)
      }
    }
  }
  if (!is.null(dict_regexp)) {
    for (i_row in 1:nrow(dict_regexp)) {
      if (nchar(dict_regexp$pattern[i_row])>0) {
        ############################## DELETE \ DEBUG
        # address[ind1[7]]
        # ind1[7] 68
        # ind1[38] 446
        # ind1[79] 971
        # ind1[54] 28591
        # 7             Musico Anton Roch,1-Blqu.5-6,Ptal. 2,1-A
        # 38                                   calle paular 3.1a
        # 79                calle de la ciudad encantada 9 a 2.2
        # 54 c/. Urbanizacin Colonia de San Lamberto n 8 - B
        debug = FALSE
        id_debug = 68
        if (debug) {
          id_debug = 68
          address_prev = mod_address[id_debug]
        }

        mod_address = gsub(dict_regexp$pattern[i_row], dict_regexp$replacement[i_row], mod_address)
        mod_address = text.remove_extra_spaces(mod_address)

        if (debug) {
          address_post = mod_address[id_debug]

          if (address_prev != address_post) {
            print("")
            print(dict_regexp$pattern[i_row])
            print(address_prev)
            print(address_post)
            rm(address_prev)
            rm(address_post)
          }
        }
      }
    }
  }

  mod_address = text.remove_extra_spaces(mod_address)
  mod_address = text.trim(mod_address)

  # PARSER
  MIN_THRESHOLD = 4 + text.address.threshold_street(mod_address)

  first_comma = unlist(lapply(gregexpr(",", mod_address), min))
  first_NUM = unlist(lapply(gregexpr("NUM", mod_address), min))
  first_DIGIT = unlist(lapply(gregexpr("\\d+", mod_address), min))

  MAX_POS = 10^6+10*max(c(first_comma, first_NUM, first_DIGIT))
  first_comma = ifelse(first_comma<MIN_THRESHOLD, MAX_POS, first_comma)
  first_NUM   = ifelse(first_NUM<MIN_THRESHOLD, MAX_POS, first_NUM)
  first_DIGIT = ifelse(first_DIGIT<MIN_THRESHOLD, MAX_POS, first_DIGIT)

  mod_address_street = mod_address
  mod_address_number = ""
  pos_split = pmin(first_comma, first_NUM, first_DIGIT)
  for (el in 1:length(mod_address)) {
    if (pos_split[el]<MAX_POS && pos_split[el]<=mod_address[el]) {
      mod_address_street[el] = substr(mod_address[el], 1, pos_split[el]-1)
      mod_address_number[el] = substr(mod_address[el], pos_split[el], nchar(mod_address[el]))
    }
  }
  ############################## DELETE \ DEBUG
  if (FALSE) {
    ind = c(2025, 2029, 2031, 9890)
    ind = sample(1:length(address), 1000)

    data.frame(A=substr(address[ind],1,50),B=substr(mod_address[ind],1,50))

    data.frame(A=substr(address[ind],1,50),B=substr(mod_address_street[ind],1,50))

    data.frame(A=substr(mod_address[ind],1,50),B=substr(mod_address_street[ind],1,50))

    data.frame(A=substr(mod_address_street[ind],1,50),B=substr(mod_address_number[ind],1,50))

    data.frame(A=substr(address[ind],1,50),B=substr(mod_address_street[ind],1,50),C=substr(mod_address_number[ind],1,50))
  }

  # FINAL REMOVES EXTRA CHARACTERS
  mod_address = text.remove_punctuation(mod_address, " ")
  mod_address = text.remove_extra_spaces(mod_address)
  mod_address = text.trim(mod_address)
  mod_address = text.address.add_street(mod_address)

  mod_address_street = text.remove_punctuation(mod_address_street, " ")
  mod_address_street = text.remove_extra_spaces(mod_address_street)
  mod_address_street = text.trim(mod_address_street)
  mod_address_street = text.address.add_street(mod_address_street)

  mod_address_number = text.remove_punctuation(mod_address_number, " ")
  if (!is.null(dict_regexp_number)) {
    for (i_row in 1:nrow(dict_regexp_number)) {
      if (nchar(dict_regexp_number$pattern[i_row])>0) {
        mod_address_number = gsub(dict_regexp_number$pattern[i_row], dict_regexp_number$replacement[i_row], mod_address_number)
        mod_address_number = text.remove_extra_spaces(mod_address_number)
      }
    }
  }
  mod_address_number = text.remove_extra_spaces(mod_address_number)
  mod_address_number = text.trim(mod_address_number)

  ############################## DELETE \ DEBUG
  if (FALSE) {
    ind = sample(1:length(address), 1000)
    data.frame(A=substr(address[ind],1,50),B=substr(mod_address[ind],1,50))

    ind = sample(1:length(address), 1000)
    data.frame(A=substr(address[ind],1,50),B=substr(mod_address_street[ind],1,50), B=substr(mod_address_number[ind],1,50))

    ind1 = which(stringr::str_detect(address, "\\."))
    for (i in 1:round(length(ind1)/100)) {
      i_min = 1 + (i-1)*100
      i_max = min(length(ind1), i_min + 99)
      ind = ind1[i_min:i_max]
      print(data.frame(A=substr(address[ind],1,50),B=substr(mod_address[ind],1,50)))
      readline()
    }

    rev(address[ind][order(abs(nchar(address[ind]) -  nchar(mod_address[ind])))[1:100]])

    df[ind, c("address", "city", "zipcode"), with=FALSE]

    ind = grep("ç", address, fixed=TRUE)
    data.frame(A=substr(address[ind],1,50),B=substr(mod_address[ind],1,50))
  }

  # PREPARE OUTPUT FORMAT
  output = data.frame(
    street = mod_address_street,
    number = mod_address_number,
    stringsAsFactors = FALSE)

  return(output)
}

#' @title text.address.normalize
#' @export
text.address.normalize <- function(address, city, province, zipcode) {
  listInputAddressWithoutNumber = gsub("[[:digit:]]", "", address)
  listInputAddressFirstNumber = text.get_first_number(address)

  output = paste0(
    listInputAddressWithoutNumber, ", ",
    mod_address_number, ", ",
    zipcode, " ",
    city, ", ",
    province)

  return(output)
}

#' @title text.address.threshold_street
#' @export
text.address.threshold_street <- function (input_address) {
  output <- sapply(1:length(input_address),
         function(el) {
            var = unlist(strsplit(input_address[el], " ", fixed=TRUE))
            if (
              var[1]=="AVENIDA"
               || var[1]=="CALLE"
               || var[1]=="TRAVESIA"
               || var[1]=="PASEO"
               || var[1]=="PASAGE"
               || var[1]=="RAMBLA"
               || var[1]=="URBANIZACION"
               || var[1]=="CARRETERA"
               || var[1]=="PLAZA"
               || var[1]=="BARRIO"
               || var[1]=="BARRIADA"
               || var[1]=="POLIGONO"
              ) {
               var = nchar(var[1])+1
              } else {
               var = 0
              }
            return(var)
         }
  )
  return(output)
}

#' @title text.address.add_street
#' @export
text.address.add_street <- function (input_address) {
  output <- sapply(1:length(input_address),
         function(el) {
           var = unlist(strsplit(input_address[el], " ", fixed=TRUE))
           if (
             var[1]!="AVENIDA"
             && var[1]!="CALLE"
             && var[1]!="TRAVESIA"
             && var[1]!="PASEO"
             && var[1]!="PASAGE"
             && var[1]!="RAMBLA"
             && var[1]!="URBANIZACION"
             && var[1]!="CARRETERA"
             && var[1]!="PLAZA"
             && var[1]!="BARRIO"
             && var[1]!="BARRIADA"
             && var[1]!="POLIGONO"
           ) {
             var = paste0("CALLE ", paste(var, collapse=" "))
           } else {
             var = paste(var, collapse=" ")
           }
           return(var)
         }
  )
  return(output)
}
