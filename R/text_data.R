# roxygen2::roxygenise()

x.global.assign <- function(var, val) {
  if (!exists(var, envir=globalenv(), inherits=FALSE)) {
    assign(var, val, envir=globalenv(), inherits=FALSE)
  } else {
    message(paste0("Variable ",var," already exist at global environment. It will not be created."))
  }
}

x.global.rm <- function(var) {
  if (!exists(var, envir=globalenv(), inherits=FALSE)) {
    message(paste0("Variable ",var," doest not exist at global environment. It will not be deleted."))
  } else {
    rm(list=var, envir=globalenv(), inherits=FALSE)
  }
}

# if (FALSE) {
#   # http://superuser.com/questions/311937/how-do-i-create-separate-zip-files-for-each-selected-file-directory-in-7zip
#   # https://info.nrao.edu/computing/guide/file-access-and-archiving/7zip/7z-7za-command-line-guide#section-10
#   str7z = "C:/\"Program Files\"/7-Zip/7z.exe"
#   strFolder = paste0(getwd(), "/", "inst/data/callejero/")
#   listFiles = list.files(strFolder)
#   for (strFile in listFiles) {
#     strPathFile = paste0(strFolder, strFile)
#     strCommand = paste0(str7z, " a -tzip ",strPathFile,".zip ",strPathFile," -mx=9")
#     system(command=strCommand)
#   }
# }

# dict_replacements = data.table::fread("resources/replacements/dict_replacements.txt", colClasses=c("character", "character"), strip.white=FALSE, encoding="UTF-8")
# listReplacements = list()
# listReplacements[dict_replacements$Key] = dict_replacements$Value
#
# listGMNonASCII  = listReplacements$listGMNonASCII
# listGMASCII     = listReplacements$listGMASCII
#
# listNonASCII  = listReplacements$listNonASCII
# listASCII     = listReplacements$listASCII
#
# dict_fixed = data.table::fread("dictionaries/dict_fixed.txt", colClasses=c("character", "character"), strip.white=FALSE, encoding="UTF-8")
# dict_regexp = data.table::fread("dictionaries/dict_regexp.txt", colClasses=c("character", "character"), strip.white=FALSE, encoding="UTF-8")
# dict_regexp_number = data.table::fread("dictionaries/dict_regexp_number.txt", colClasses=c("character", "character"), strip.white=FALSE, encoding="UTF-8")

#' @title text.load_data
#' @usage text.load_data()
#' @export text.load_data
text.load_data <- function () {
  strPackageFolder = find.package("texttools")
  # strPackageFolder = paste0(.libPaths()[1], "/texttools")

  strFolderReplacements = paste0(strPackageFolder, "/data/replacements/")
  strFolderAddressDictionaries = paste0(strPackageFolder, "/data/address/")

  dict_replacements = data.table::fread(paste0(strFolderReplacements, "dict_replacements.txt"), colClasses=c("character", "character"), strip.white=FALSE, encoding="UTF-8")
  listReplacements = list()
  listReplacements[dict_replacements$Key] = dict_replacements$Value

  listGMNonASCII  = listReplacements$listGMNonASCII
  listGMASCII     = listReplacements$listGMASCII

  listNonASCII  = listReplacements$listNonASCII
  listASCII     = listReplacements$listASCII

  dict_fixed = data.table::fread(paste0(strFolderAddressDictionaries, "dict_fixed.txt"),
                                 colClasses=c("character", "character"), strip.white=FALSE, encoding="UTF-8")
  dict_regexp = data.table::fread(paste0(strFolderAddressDictionaries, "dict_regexp.txt"),
                                  colClasses=c("character", "character"), strip.white=FALSE, encoding="UTF-8")
  dict_regexp_number = data.table::fread(paste0(strFolderAddressDictionaries, "dict_regexp_number.txt"),
                                         colClasses=c("character", "character"), strip.white=FALSE, encoding="UTF-8")

  x.global.assign("dict_replacements", dict_replacements)

  x.global.assign("listReplacements", listReplacements)

  x.global.assign("listGMNonASCII", listGMNonASCII)
  x.global.assign("listGMASCII", listGMASCII)
  x.global.assign("listNonASCII", listNonASCII)
  x.global.assign("listASCII", listASCII)

  x.global.assign("dict_fixed", dict_fixed)
  x.global.assign("dict_regexp", dict_regexp)
  x.global.assign("dict_regexp_number", dict_regexp_number)
}

#' @title text.unload_data
#' @usage text.unload_data()
#' @export text.unload_data
text.unload_data <- function () {
  x.global.rm("dict_replacements")

  x.global.rm("listReplacements")

  x.global.rm("listGMNonASCII")
  x.global.rm("listGMASCII")
  x.global.rm("listNonASCII")
  x.global.rm("listASCII")

  x.global.rm("dict_fixed")
  x.global.rm("dict_regexp")
  x.global.rm("dict_regexp_number")
}
