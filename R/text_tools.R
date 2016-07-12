# roxygen2::roxygenise()

#' @title text.get_cases
#' @usage text.get_cases(x, pattern, ...)
#' @export text.get_cases
text.get_cases <- function(x, pattern, ...) {
  all_finds = gregexpr(pattern, x, ...)

  ini_pos = as.numeric(all_finds[[1]])
  end_pos = attr(all_finds[[1]], "match.length")

  if (ini_pos[1]==-1) {
    output = NULL
  } else {
    output = sapply(1:length(ini_pos), function(i) {substr(x, ini_pos[i], ini_pos[i]+end_pos[i]-1)})
  }
  return(output)
}

#' @title text.get_number
#' @usage text.get_number(x)
#' @export text.get_number
text.get_number <- function(x) {
  all_finds = gregexpr("\\d+", x)

  ini_pos = as.numeric(all_finds[[1]])
  end_pos = attr(all_finds[[1]], "match.length")

  if (ini_pos[1]==-1) {
    output = NULL
  } else {
    output = sapply(1:length(ini_pos), function(i) {substr(x, ini_pos[i], ini_pos[i]+end_pos[i]-1)})
  }
  return(output)
}

#' @title text.get_first_number
#' @usage text.get_first_number(x)
#' @export text.get_first_number
text.get_first_number <- function(x) {
  all_finds = gregexpr("\\d+", x)

  output = lapply(1:length(x),
                  function(el) {
                    ini_pos = as.numeric(all_finds[[el]])
                    end_pos = attr(all_finds[[el]], "match.length")

                    if (ini_pos[1]==-1) {
                      output = ""
                    } else {
                      output = substr(x[el], ini_pos[1], ini_pos[1]+end_pos[1]-1)
                    }
                    return(output)
                  }
  )

  output = unlist(output)

  return(output)
}

#' @title text.filter
#' @usage text.filter(
#'   x,
#'   pattern,
#'   ignore.case = FALSE,
#'   perl = FALSE,
#'   fixed = FALSE,
#'   message = texttools:::param.default.text.filter.message,
#'   batch = texttools:::param.default.text.filter.batch)
#' @export text.filter
text.filter <- function(
  x,
  pattern,
  ignore.case = FALSE,
  perl = FALSE,
  fixed = FALSE,
  message = texttools:::param.default.text.filter.message,
  batch = texttools:::param.default.text.filter.batch
) {
  indFinds = which(grepl(pattern, x, ignore.case=ignore.case, perl=perl, fixed=fixed))

  if (message) {
    for (i in 1:ceiling(length(indFinds)/batch)) {
      message(paste0(x[indFinds[(1+(i-1)*batch):min(length(indFinds), i*batch)]], collapse="\n"))
    }
  }

  invisible(indFinds)
}
