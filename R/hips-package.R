#' Hips.
#'
#' Project package to analyze hip radiographs with statistical learning models.
#'
#' @section Cohort Constructors:
#'
#' Constructor functions for loading cohorts
#' \code{\link{hipsCohort_}}
#' \code{\link{adlCohort}}
#'
#' @section Model Training:
#'
#' Partition cohort table
#' \code{\link{StratifiedPartition}}
#'
#' Train model
#' \code{\link{trainGlm}}
#'
#' @section Model Evaluation:
#'
#' \code{\link{predict_pY}}
#'
#' @section Views:
#'
#' Tables: \code{\link{ComplexSummary}}
#' Figures: \code{\link{gg_pc}}
#'
#' @section Stats:
#'
#' Odds Ratios: \code{\link{OddsRatios}}
#'
#' @name hips
#' @docType package
"_PACKAGE"


#' Adelaide test cohort scalars and inference results
#'
#' data.frame with 4,568 examples and 11 variables
#'
#' @family cohorts
#' @seealso \code{\link{adlCohort}}
#' @docType data
"adl_test"


#' MSH cohort data
#'
#' data.frame with 23,589 examples and 21 variables
#'
#' @family cohorts
#' @seealso \code{\link{hipsCohort}}
#' @docType data
"scalars"



#' matched cohort enrollment lists
#'
#' List of 3 chr(1566) containing ids for participants in each of 3 case-control cohorts
#'
#' @family cohorts
#' @docType data
"caseControlCohorts"
