# roxygen2::roxygenise()

param.default.text.filter.message = TRUE
param.default.text.filter.batch = 50

param.default.text.PuntuaMiZona.ThrowError = FALSE

setVar <- function (var, value) {
  strValue = paste(capture.output(dump("value", file="")), collapse = "")
  if (substring(strValue, 1, 9)=="value <- ") {
    strValue = substring(strValue, 10)
  } else if (substring(strValue, 1, 8)=="value<- ") {
    strValue = substring(strValue, 9)
  } else if (substring(strValue, 1, 8)=="value <-") {
    strValue = substring(strValue, 9)
  } else if (substring(strValue, 1, 7)=="value<-") {
    strValue = substring(strValue, 8)
  } else if (substring(strValue, 1, 8)=="value = ") {
    strValue = substring(strValue, 9)
  } else if (substring(strValue, 1, 7)=="value= ") {
    strValue = substring(strValue, 8)
  } else if (substring(strValue, 1, 7)=="value =") {
    strValue = substring(strValue, 8)
  } else if (substring(strValue, 1, 6)=="value=") {
    strValue = substring(strValue, 7)
  }
  unlockBinding(var, env = asNamespace('texttools'))
  eval(parse(text=paste0(var," <- ",strValue)), envir = asNamespace('texttools'))
  lockBinding(var, env = asNamespace('texttools'))
}

#' @title text.set.default.text.PuntuaMiZona.ThrowError
#' @usage text.set.default.text.PuntuaMiZona.ThrowError(value)
#' @export text.set.default.text.PuntuaMiZona.ThrowError
text.set.default.text.PuntuaMiZona.ThrowError <- function (value) {
  setVar("param.default.text.PuntuaMiZona.ThrowError", value)
}

#' @title text.set.default.text.filter.message
#' @usage text.set.default.text.filter.message(value)
#' @export text.set.default.text.filter.message
text.set.default.text.filter.message <- function (value) {
  setVar("param.default.text.filter.message", value)
}

#' @title text.set.default.text.filter.batch
#' @usage text.set.default.text.filter.batch(value)
#' @export text.set.default.text.filter.batch
text.set.default.text.filter.batch <- function (value) {
  setVar("param.default.text.filter.batch", value)
}
