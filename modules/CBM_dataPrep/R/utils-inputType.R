
# Helper functions: Detect input type

isFile <- function(x){
  length(x) == 1 && is.character(x) &&
    tryCatch(file.exists(x), error = function(e) FALSE)
}

isURL <- function(x){
  length(x) == 1 && is.character(x) &&
    any(sapply(c("^https:", "^http:", "^www\\."), grepl, x, ignore.case = TRUE))
}

isValue <- function(x){
  length(x) == 1 && (is.vector(x) | is.factor(x)) &&
    !isFile(x) && !isURL(x) && !isCBMsource(x)
}

isCBMsource <- function(x){
  length(x) == 1 && is.character(x) &&
    x %in% CBMutils::CBMsources$sourceID
}

