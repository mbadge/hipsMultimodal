# Functions for hypothesis tests on individual or pairs of ROC curves

#!!! extreme duplication and divergence from AnalysisToolkit code

#' Classifier Abstract Base Class.
#'
#' @name Classifier
#'
#' @slot Y logical or binary integer. the true labels.
#' @slot id character. Image IDs.
#'
#' @import methods
#' @export
#' @family classifiers
setClass("Classifier",  # Abstract Base Class for a Continuous or Discrete Binary Classifier
         slots = list(Y = "logical",
                      id = "character"))

setClass("ClassifierCurve",
         contains = "Classifier",
         slots = list(pY = "numeric"))

#' View summary stats for a classifier point.
#'
#' @param x a \code{\link{Classifier}}
#' @param ... dots, needed to stay matched to the generic
#'
#' @import broom
#' @export
#' @family tidy, glance
#'
#' @examples
#' data(classifiers, package="AnalysisToolkit")
#' glance(classifiers$classifier_points[[1]])
glance.ClassifierCurve <- function(x, ...) {
    # Compute operating-point independent stats: auc, auprc ----
    roc <- cC2roc(x)  # pROC::roc object

    # AUC with delong CIs
    auc_ci_df <- pROC::ci.auc(roc, ...) %>%
        as.data.frame() %>%
        t() %>%
        as.data.frame() %>%
        set_names(., c("lower", "auc", "upper"))

    # Check if auc ci was calculabe (sufficient data points? #24)
    if (any(is.na(auc_ci_df))) {
        warning("auc confidence interval not calculable")
        auc_ci_df <- data.frame("auc" = pROC::auc(roc) %>% as.numeric)
    } else {
        # Add significance indicator text if CI calculated
        auc_ci_df %<>%
            mutate(is_sig = map_chr(lower, ~ifelse(.x > 0.5, "*", "")))
        auc_ci_df <- auc_ci_df[c("auc", "lower", "upper", "is_sig")]
        auc_ci_df <- rename(auc_ci_df, auc_lower = "lower", auc_upper = "upper")
    }

    auprc_s <- auprc(x)
    c_stat_df <- tibble::add_column(auc_ci_df, auprc = auprc_s)

    # Find Operating Point dependent stats ----
    op_stats <- c("threshold", "specificity", "sensitivity", "accuracy", "npv", "ppv", "tn", "tp", "fn", "fp")

    op_stat_df <- pROC::coords(roc, x="best", best.method="youden", ret = op_stats) %>%
        t() %>%
        as.data.frame()
    # Edge case: when there are 2 equivalent points, 2 rows are returned
    # Keep the higher sensitivity point (1st point).  see projR/hips#52
    #! Duplication with R/roc_prc.R gg_data_roc
    if (nrow(op_stat_df) > 1)
    {
        op_stat_df <- op_stat_df[1, ]
    }

    stat_df <- cbind(c_stat_df, op_stat_df)

    stat_df[] %<>% map_if(.p = partial(str_detect, pattern = "[0-9]+"),
                          .f = as.numeric)

    stat_df
}


#' Compare 2 ROC curves for AUC equivalence
#'
#' @params cC_list = list of my ClassifierCurve objects
#' @return h.test object
#'
#' @export
compare_cCs <- function(cC_lst = list(), compare_test = "delong", pilot = FALSE) {
    # Precondition ----
    stopifnot(all(map_chr(cC_lst, class) == "ClassifierCurve"))
    stopifnot(compare_test %in% c("delong", "bootstrap"))

    # Handle sample naming ----
    # Default names if input list is unnamed
    if (!assertive::has_names(cC_lst)) {
        names(cC_lst) <- stringr::str_c("cC", 1:length(cC_lst))
    }
    # Collapse names to snake case for combo handling
    names(cC_lst) %<>% MyUtils::str_case_snake()

    # Data crafting ----
    # Enlist all couples
    cC_pairs <- combn(names(cC_lst), 2, simplify=FALSE)
    names(cC_pairs) <- map_chr(cC_pairs, stringr::str_c, collapse = "-")

    # Wrapper customization ----
    if (pilot) {bootN = 100} else {bootN=2000}
    CompareRocs <- partial(pROC:::roc.test.roc,
                           boot.n = bootN,
                           method = compare_test)

    # Engine ----
    cC_pairs %>%
        map(~cC_lst[.x]) %>%
        map(~map(.x, cC2roc)) %>%
        map(~CompareRocs(roc1=.x[[1]], roc2=.x[[2]]))
}

