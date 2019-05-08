# Store pkg-wide configuration variables here, and they'll be instantiated into
# options when the hips package is loaded.
#
# Package option schema:
# [pkg name] DOT [snake_case_option_name]
# hips.option_one
.onLoad <- function(libname, pkgname) {
    # FS locations ----
    DIR_IMG_ORIG <- "/media/marcus/Vulcan/radiology/msh_hip/00_parsed_dcms/"
    DIR_IMG_PROC <- "/media/marcus/Vulcan/radiology/msh_hip/npy_imgs/"

    # Cohort Table column parsing ----
    ID_VARS <- c("img", "pt")
    DEM_VARS <- c("age", "sex")
    PT_VARS <- c("age", "sex", "bmi", "fall_lgl", "pain_lgl")
    HP_VARS <- c("order_date", "order_wday", "order_time",
                "dept", "projection_set", "order_priority", "name_operator",
                "name_interpreter", "device_brand", "device_model",
                "procedure_radiation",
                "dT.ordered_to_exam", "dT.exam_to_prelim", "dT.exam_to_final")
    ineligible_hp_vars <- c("name_interpreter", "dT.exam_to_prelim", "dT.exam_to_final")  # Exclude these as explanatory variables
    HP_PRED <- setdiff(HP_VARS, ineligible_hp_vars)
    COVARS <- c(PT_VARS, HP_VARS)
    TARGETS <- c("fx", COVARS)

    # Modeling ----
    COHORTS <- c("crossSectional", "caseControl_matchNone", "caseControl_matchDem", "caseControl_matchPt", "caseControl_matchAll")
    PREDICTOR_SETS <- c("img", "pt", "hp", "imgPt", "imgPtHp")
    BTLNCKS <- c("rnd", "pre")

    # NSE FXN
    #' @examples
    #' Show(PREDICTORS)
    Show <- function(FLAG) {
        paste0(MyUtils::str_case_title(deparse(substitute(FLAG))), ": ",
               paste(FLAG, collapse=", "), "\n\n"
        )
    }

    if (interactive()) {
        packageStartupMessage("Setting Hip Analysis Global Variables:\n\n",
                              Show(DIR_IMG_ORIG),
                              Show(DIR_IMG_PROC),
                              Show(ID_VARS),
                              Show(DEM_VARS),
                              Show(PT_VARS),
                              Show(HP_VARS),
                              Show(HP_PRED),
                              Show(COVARS),
                              Show(TARGETS),
                              Show(COHORTS),
                              Show(PREDICTOR_SETS),
                              Show(BTLNCKS),
                              "call e.g. `hipsOpt(ehr_vars)` to fetch")
    }

    opt <- options()
    opt_hips <- list(
        hips.dir_img_orig = DIR_IMG_ORIG,
        hips.dir_img_proc = DIR_IMG_PROC,
        hips.id_vars = ID_VARS,
        hips.dem_vars = DEM_VARS,
        hips.pt_vars = PT_VARS,
        hips.hp_vars = HP_VARS,
        hips.hp_pred = HP_PRED,
        hips.covars = COVARS,
        hips.targets = TARGETS,
        hips.cohorts = COHORTS,
        hips.predictor_sets = PREDICTOR_SETS,
        hips.btlncks = BTLNCKS
    )

    to_set <- !(names(opt_hips) %in% names(opt))
    if(any(to_set)) options(opt_hips[to_set])
    invisible()
}




# ---- Option accessors ----

#' Convenience fxn to fetch an option set by the hips package.
#'
#' hipsOpt uses NSE to save typing, use hipOpt_ for SE
#'
#' Package option schema:
#' [pkg name] DOT [snake_case_option_name]
#' eg: hips.option_one
#'
#' @param opt_substr chr(1) specific hips option
#' @param ... optional default if variable isn't set \code{\link[base]{options}}
#'
#' @return set option or ...
#' @export
#'
#' @examples
#' hipsOpt_('ehr_vars')
hipsOpt_ <- function(opt_substr, ...) {
    # Precondition
    stopifnot(is.character(opt_substr), length(opt_substr) == 1)

    getOption(paste("hips", opt_substr, sep = "."), ...)
}

#' @export
#' @rdname hipsOpt_
#' @examples
#' hipsOpt()
#' hipsOpt(ehr_vars)
hipsOpt <- function(opt_substr, ...) {
    if (missing(opt_substr)) {
        avail = paste0("hips pkg options:\n",
                       paste(stringr::str_subset(names(options()), "hips"), collapse="\n"))
        cat(avail)
        return(invisible())
    }

    hipsOpt_(deparse(substitute(opt_substr)))
}
