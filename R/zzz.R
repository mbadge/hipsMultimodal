# ---- FS ----

# Absolute Path ----

#' Configured map of absolute project dir paths by system.
#'
#' This serves a similar ad hoc functionality to the aaa.R .onLoad() routine in ProjUtilsRads.
#'
#' @export
#' @examples
#' FS_hipsDir()
FS_hipsDir <- function() {
    hips_dir <- switch(
        MyUtils::whoami(),
        "Home-Desk-Win"="P:/hips",
        "Home-Desk-Linux"="/media/marcus/Projects/hips/",
        "Home-Sys76-Linux"="/home/marcus/Projects/hips/")
    stopifnot(dir.exists(hips_dir))
    hips_dir
}



# Relative Paths ----
# Defs
#' @export
fp_ml_subdir <- file.path("analysis", "ml")

# Accessors ----

#' Access ML dir contents
#'
#' @export
#' @examples
#' Fp_ml_dir() %>% dir.exists()
#' Fp_ml_dir("foo.txt")
#' Fp_ml_dir("models", "foo.txt")
Fp_ml_dir <- function(...) {file.path(FS_hipsDir(), fp_ml_subdir, ...)}



# fs info accessors ----
#' Convenience fxn to access pkg configuration info in inst/info
#'
#' hipsInfo_ does SE, and hipsInfo is a NSE wrapper.
#'
#' @param filestem chr(1)
#' @return rds file contents, generally a data.frame
#' @export
#'
#' @examples
#' hipsInfo_()
#' hipsInfo_("targets")
hipsInfo_ <- function(filestem) {
    if (missing(filestem)) {
        info_dir <- system.file('info', package = "hips", mustWork = TRUE)
        avail = paste0("available info sets:\n",
                       paste(list.files(info_dir, pattern=".rds") %>% fp_stem, collapse="\n"))
        cat(avail)
        return(invisible())
    }

    fp <- system.file('info', paste0(filestem, ".rds"), package = "hips", mustWork = TRUE)
    readRDS(fp)
}


#' @export
#' @rdname hipsInfo_
#' @examples
#' hipsInfo()
#' hipsInfo(targets)
hipsInfo <- function(filestem) {
    if (missing(filestem)) {
        info_dir <- system.file('info', package = "hips", mustWork = TRUE)
        avail = paste0("available info sets:\n",
                       paste(list.files(info_dir, pattern=".rds") %>% fp_stem, collapse="\n"))
        cat(avail)
        return(invisible())
    }

    hipsInfo_(deparse(substitute(filestem)))
}


# Utility Map ----
predictorSet2Predictors <- function(predictorSet) {
    # Precondition
    stopifnot(is.character(predictorSet), length(predictorSet) == 1)
    stopifnot(predictorSet %in% hipsOpt(predictors))

    switch(predictorSet,
           "image" = kIMG_VARS,
           "ehr" = EHR,
           "multimodal" = c(EHR, kIMG_VARS)
    )
}


