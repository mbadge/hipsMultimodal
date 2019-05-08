# Cohorts and Cohort Manipulations


# ---- Cohort accessors ----

#' Load a cohort table
#'
#' @param mutating chr(1) describing the mutated version of the cohort frame
#' @param filtering chr(1) describing the set of patients to include
#'
#' @return img-major data.frame with scalars and btlncks
#' @export
#'
#' @family cohorts
#' @examples
#' hipsCohort_()
#' hipsCohort_(mutating = "binary")
#' hipsCohort_(mutating = "binary", filtering = "caseControl_matchDem")
hipsCohort_ <- function(mutating = "none", filtering = "crossSectional") {
    stopifnot(mutating %in% c("none", "binary", "complete"))
    if (filtering %ni% hipsOpt(cohorts))
        stop("Invalid filtering: '", filtering, "'. Try one of ", str_x(hipsOpt(cohorts)), call. = FALSE)

    # By default, use the full scalars table
    if (mutating == "none") {
        data(scalars, package = "hips", envir = environment())
    } else {
        fp <- file.path(FS_hipsDir(), "analysis", "cohorts",
                        paste0(mutating, "Scalars.rds"))
        scalars <- readRDS(fp)
    }

    fp <- system.file('bulky_data', 'btlnck_pcs.rds', package = "hips", mustWork = TRUE)
    btlncks <- readRDS(fp)

    bt <- dplyr::inner_join(scalars, btlncks, by="img")

    if (filtering != "crossSectional") {
        data("caseControlCohorts", package="hips")
        CaseControlSlot <- switch(filtering,
                                  caseControl_matchNone = "rnd",
                                  caseControl_matchDem = "balDem",
                                  caseControl_matchAll = "balAll")
        imgs2keep <- caseControlCohorts[[CaseControlSlot]]
        bt %<>%
            filter(img %in% imgs2keep)
    }

    class(bt) <- c("cohort_df", class(bt))
    attr(bt, "mutation") <- mutating
    attr(bt, "cohort") <- filtering
    bt
}

#' @export
#' @rdname hipsCohort_
#' @examples
#' hipsCohort()
#' hipsCohort(mutating = binary)
#' hipsCohort(mutating = complete)
#' hipsCohort(mutating = complete, filtering = caseControl_matchDem)
hipsCohort <- function(mutating, filtering) {
    Mutating <- if(missing(mutating)) "none" else deparse(substitute(mutating))
    Filtering <- if(missing(filtering)) "crossSectional" else deparse(substitute(filtering))

    hipsCohort_(mutating = Mutating,
                filtering = Filtering)
}

#' Fetch Adelaide Test Samples split into differet subsampling cohorts
#'
#' @param binarized lgl(1) whether to binarize all features (eg, for OR computation)
#'
#' @family cohorts
#' @return list of cohort scalar tables
#' @export
#' @examples
#' adlCohort()
#' adlCohort(binarized=TRUE)
adlCohort <- function(binarized=FALSE) {
    scalars_df <- if (binarized) {
        FP <- file.path(FS_hipsDir(), "analysis", "cohorts", "binaryAdlTest.rds")
        stopifnot(file.exists(FP))
        readRDS(FP)
    } else {
        data("adl_test", package="hips")
        adl_test
    }

    cohorts <- scalars_df %>%
        replicate(n=3, simplify=FALSE)
    names(cohorts) <- c("caseControl_matchDem", "caseControl_matchAll", "caseControl_matchNone")
    cohorts <- purrr::imap(cohorts, ~.x[!is.na(.x[, .y]), ])
    cohorts <- purrr::prepend(cohorts, list("crossSectional" = scalars_df))
    purrr::map(cohorts, select, fx:order_wday)
}


# Display methods ----

#' @rdname hipsCohort_
#' @examples
#' hipsCohort() %>% print
#' hipsCohort(binary) %>% print
#' hipsCohort() %>% select(-img)
print.cohort_df <- function(x, ...) {
    out <- tryCatch(ComplexSummary(x, ...),
                    error=function(cond) {
                        cat("Can't compute `ComplexSummary()`:\nUsing `NextMethod()`\n")
                        return(NULL)
                    })
    if (is.null(out)) out <- NextMethod(x)
    out
}


#' Augment cohort table with a stratified train/test partition
#'
#' this fxn wraps \code{\link[caret]{createDataPartition}}
#'
#' @param cohort_df data.frame
#' @param grp_col chr(1)
#' @param target_col chr(1)
#'
#' @return data.frame with same number of rows and an additional column `partition` with values train and test
#' @export
#'
#' @examples
#' StratifiedPartition(mtcars, grp_col="am", target_col="mpg") %>% s
#' StratifiedPartition(mtcars, grp_col="am", target_col="mpg") %>% select(am, partition) %>% table_()
#' StratifiedPartition(hipsCohort(), grp_col = "device_model") %>% select(device_model, partition) %>% table_()
#' StratifiedPartition(hipsCohort()) %>% select(pt, partition) %>% table_()
#' try(StratifiedPartition(hipsCohort() %>% sample_n(500)) %>% select(pt, partition) %>% table_())
#' StratifiedPartition(hipsCohort()) %>% attributes() %>% s
StratifiedPartition <- function(cohort_df, grp_col="pt", target_col="fx") {
    # preconditions
    stopifnot(is.character(grp_col), length(grp_col) == 1, is.character(target_col), length(target_col) == 1)
    check_y(y = cohort_df[[target_col]])

    # Ensure all groups have data
    local(expr = {  # raises or invisibly returns NULL
        xtab <- table(cohort_df[[grp_col]])
        if (any(xtab == 0)) {
            xtab_msg <- paste("'", names(xtab)[xtab == 0], "'", collapse = ", ", sep = "")
            stop(paste("One or more factor levels in the grouping variable has no data:", xtab_msg), call. = FALSE)
        }
    })

    # Check and remove missingness in group and target cols
    #! This routine removes attributes from cohort_df, so first save attributes of interest
    A_mutation <- attr(cohort_df, "mutation", exact = TRUE)
    A_cohort <- attr(cohort_df, "cohort", exact = TRUE)
    cohort_df <- local(expr = {
        key_cohort_df_cols <- select(cohort_df, !!grp_col, !!target_col)

        if (any(is.na(key_cohort_df_cols))) {
            if(EBImage:::interactiveMode())  # interactive and NOT knitting
                DF.assess_missingness(key_cohort_df_cols)
            warning("Model frame contains missing values in group and/or target columns. ",
                    "Removing any incomplete record")
        }

        cohort_df <- tidyr::drop_na(cohort_df, !!grp_col, !!target_col)
        cohort_df %<>% droplevels()
    })

    pts <- cohort_df %>% tidyr::nest(-!!grp_col)

    # Stratified 70/30 train/test split
    train_idx <- sample(x = 1:nrow(pts), size = floor(nrow(pts) * 0.75), replace = FALSE)
    train_df <- pts[train_idx, ]
    test_df <- pts[-train_idx, ]

    stratified_cohort <- bind_rows(train = train_df, test = test_df, .id = "partition") %>%
        tidyr::unnest()

    # decorate return object
    class(stratified_cohort) <- c("StratifiedPartition", class(cohort_df))
    attr(stratified_cohort, "grp") <- grp_col
    attr(stratified_cohort, "target_col") <- target_col
    attr(stratified_cohort, "mutation") <- A_mutation
    attr(stratified_cohort, "cohort") <- A_cohort

    # postconditions
    stopifnot(all(sort(unique(stratified_cohort$partition)) == c("test", "train")))
    stopifnot(all(  # Each group appears in exactly 1 partition
        stratified_cohort %>%
            split(.[[grp_col]]) %>%
            purrr::map("partition") %>%
            purrr::map_int(n_distinct) %>%
            magrittr::equals(1)
    ))

    invisible(stratified_cohort)
}


