## Capture the objects behind LandR#215: predict.merMod() failing with
## "X %*% fixef(object) : non-conformable arguments" inside
## LandR::makeAndCleanInitialCohortData(), for a specific ELF.
##
## Four hypotheses have already been refuted by synthetic reproduction (see the issue), so
## this stops guessing and takes the real objects. It wraps lme4's predict.merMod so that
## a failing call saves the fitted model and the newdata, then re-raises. Nothing else
## about the run changes, and no interactive attention is needed -- though the objects are
## also left in .GlobalEnv so the session can be inspected by hand.
##
## Run from the project root with .ELFind set, e.g.
##   .ELFind <- "11.3"; .rep <- 1; source("R/diagnoseAgeImputation.R")

.diagOut <- normalizePath("~/ageImputationFailure.rds", mustWork = FALSE)

local({
  ns <- asNamespace("lme4")
  orig <- get("predict.merMod", envir = ns)
  if (isTRUE(attr(orig, ".diagWrapped"))) return(invisible())

  wrapped <- function(object, newdata = NULL, ...) {
    out <- tryCatch(orig(object, newdata = newdata, ...), error = function(e) e)
    if (inherits(out, "error")) {
      info <- tryCatch({
        X <- stats::model.matrix(stats::delete.response(stats::terms(object)), newdata)
        list(
          message      = conditionMessage(out),
          nFixef       = length(lme4::fixef(object)),
          fixefNames   = names(lme4::fixef(object)),
          nMatrixCols  = ncol(X),
          matrixCols   = colnames(X),
          ## the actual answer: which columns predict() builds that the model lacks
          inMatrixNotFixef = setdiff(colnames(X), names(lme4::fixef(object))),
          inFixefNotMatrix = setdiff(names(lme4::fixef(object)), colnames(X)),
          fitSpecies   = tryCatch(levels(droplevels(stats::model.frame(object)[["speciesCode"]])),
                                  error = function(e) NULL),
          newSpecies   = tryCatch(sort(unique(as.character(newdata[["speciesCode"]]))),
                                  error = function(e) NULL),
          newIsFactor  = tryCatch(is.factor(newdata[["speciesCode"]]), error = function(e) NA),
          newLevels    = tryCatch(levels(newdata[["speciesCode"]]), error = function(e) NULL),
          formula      = tryCatch(deparse(stats::formula(object)), error = function(e) NULL)
        )
      }, error = function(e) list(message = conditionMessage(out),
                                  introspectionFailed = conditionMessage(e)))

      saveRDS(list(info = info, mod = object, newdata = newdata,
                   fitData = tryCatch(stats::model.frame(object), error = function(e) NULL),
                   when = Sys.time(), elf = if (exists(".ELFind")) get(".ELFind") else NA),
              .diagOut)
      assign(".ageFailure", list(info = info, mod = object, newdata = newdata),
             envir = .GlobalEnv)

      message("\n", strrep("=", 70))
      message("AGE IMPUTATION FAILURE CAPTURED -> ", .diagOut)
      message("  ", info$message)
      if (!is.null(info$nFixef))
        message("  fixef(): ", info$nFixef, "   predict() model matrix: ", info$nMatrixCols)
      if (length(info$inMatrixNotFixef))
        message("  columns predict() built that the model lacks: ",
                paste(info$inMatrixNotFixef, collapse = ", "))
      if (length(info$inFixefNotMatrix))
        message("  coefficients with no matching column: ",
                paste(info$inFixefNotMatrix, collapse = ", "))
      message("  species in fit: ", paste(info$fitSpecies, collapse = ", "))
      message("  species in newdata: ", paste(info$newSpecies, collapse = ", "))
      message("  also left in .GlobalEnv as `.ageFailure`")
      message(strrep("=", 70), "\n")
      stop(out)
    }
    out
  }
  attr(wrapped, ".diagWrapped") <- TRUE
  utils::assignInNamespace("predict.merMod", wrapped, ns = "lme4")
  message("predict.merMod wrapped; failures will be saved to ", .diagOut)
})

if (!exists(".ELFind")) stop("set .ELFind before sourcing this, e.g. .ELFind <- \"11.3\"")
message("running global.R for ELF ", .ELFind, " with age-imputation capture armed")
source("global.R")
