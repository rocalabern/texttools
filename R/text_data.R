# roxygen2::roxygenise()

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

