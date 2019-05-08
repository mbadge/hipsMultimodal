# Complex table build-up
#   1) define BaseSummaries which compute a single statistic
#   2) make all Individual Base Summary Values pretty as strings
#   3) group compound stats


#' Compute hip-cohort-table-specific individual statistics
#'
#' Default stats hardcoded into \code{BaseSummary}, additional stats
#' can be passed to \code{\link[dplyr]{summarise}} via \code{...}
#'
#' @param x data.frame, hip cohort table
#' @param grp x_chr, names of grouping columns
#' @param ... additional stats to compute
#'
#' @import dplyr
#' @export
#' @seealso ComplexSummary
#'
#' @examples
#' grp_lst <- list(character(0), "sex", "fx", c("sex", "dept"))
#' map(grp_lst, BaseSummary, x=hips::scalars)
#' map(grp_lst, BaseSummary, x=hips::scalars, bmi_mean = mean_(bmi))
#' BaseSummary(hips::scalars %>% filter(fx), grp = c("sex", "device_model"))
BaseSummary <- function(x, grp = character(0), ...) {
    # Compute grouped base metrics
    x %<>%
        mutate(sex = map_lgl(sex, `==`, "f")) %>%   # recode gender
        group_by_(.dots=grp) %>%
        summarise(img_n = n_distinct(img),
                  pt_n = n_distinct(pt),
                  age_mean = mean_(age),
                  age_sd = sd_(age),
                  f_sum = sum_(sex),
                  f_prev = mean_(sex) * 100,
                  bmi_mean = mean_(bmi),
                  bmi_sd = sd_(bmi),
                  fx_sum = sum_(fx),
                  fx_prev = mean_(fx) * 100,
                  dev_mod_n = n_distinct(device_model),
                  dev_brand_n = n_distinct(device_brand),
                  fall_prev = mean_(fall_lgl) * 100,
                  fall_sum = sum_(fall_lgl),
                  pain_prev = mean_(pain_lgl) * 100,
                  pain_sum = sum_(pain_lgl),
                  ...) %>%
        arrange(desc(img_n))
    attr(x, "sum_nms") <- c("No. radiographs",
                            "No. patients",
                            "No. scanners",
                            "No. scanner manufacturers",
                            "Age, mean (SD), years",
                            "Female frequency, No. (%)",
                            "Fracture frequency, No. (%)",
                            "BMI, mean (SD)",
                            "Fall frequency, No. (%)",
                            "Pain frequency, No. (%)"
                            )
    attr(x, "grp") <- grp
    x
}


#' Beautify individual table statistics
#'
#' @param x data.frame, hip cohort table
#' @param ... additional args to pass to \code{\link[base]{format}}
#'
#' @seealso ComplexSummary
#' @export
#' @examples
#' grp_lst <- list(character(0), "sex", "fx", c("sex", "dept"))
#' map(grp_lst, compose(PrettyEach, BaseSummary), x = scalars)
#' map(grp_lst, compose(partial(PrettyEach, trim=TRUE), BaseSummary), x=scalars)
PrettyEach <- function(x, ...) {
    x[] %<>% map_if(.p = is.numeric, .f=format, big.mark=",", digits=1, ...)
    x
}


#' Join compound table statistic columns.
#'
#' @param x data.frame with individual statistics to join
#'
#' @seealso ComplexSummary
#'
#' @export
#' @examples
#' map(grp_lst, compose(PrettyEach, BaseSummary), x=scalars)
#' map(grp_lst, compose(UniteCols, PrettyEach, BaseSummary), x=scalars)
#' map(grp_lst, compose(UniteCols, partial(PrettyEach, trim=TRUE), BaseSummary), x=scalars)
UniteCols <- function(x) {
    res <- x %>%
        mutate(age_cmpd = map2_chr(age_mean, age_sd, ~glue("{.x} ({.y})"))) %>%
        select(-age_mean, -age_sd) %>%
        mutate(f_cmpd = map2_chr(f_sum, f_prev, ~glue("{.x} ({.y})"))) %>%
        select(-f_sum, -f_prev) %>%
        mutate(fx_cmpd = map2_chr(fx_sum, fx_prev, ~glue("{.x} ({.y})"))) %>%
        select(-fx_sum, -fx_prev) %>%
        mutate(bmi_cmpd = map2_chr(bmi_mean, bmi_sd, ~glue("{.x} ({.y})"))) %>%
        select(-bmi_mean, -bmi_sd) %>%
        mutate(fall_cmpd = map2_chr(fall_sum, fall_prev, ~glue("{.x} ({.y})"))) %>%
        select(-fall_sum, -fall_prev) %>%
        mutate(pain_cmpd = map2_chr(pain_sum, pain_prev, ~glue("{.x} ({.y})"))) %>%
        select(-pain_sum, -pain_prev)
    names(res) <- c(attr(x, "grp"), attr(x, "sum_nms"))
    res
}

#' Wrapper for fxns that generates combined table statistics by grouping
#'
#' @export
#'
#' @family viewTable
#' @export
#' @examples
#' ComplexSummary(hipsCohort())
#' ComplexSummary(hipsCohort(), grp = "sex")
ComplexSummary <- purrr::compose(UniteCols, PrettyEach, BaseSummary)

#' Kable S3 generic for kable printing with nice defaults
#'
#' @param x object to render in a report
#'
#' @family viewTable
#' @export
Kable <- function(x, ...) UseMethod("Kable")

#' @rdname Kable
#' @examples
#' Kable(hipsCohort())
#' Kable(x = hipsCohort(), grp = "sex")
#' Kable(x = hipsCohort(), grp = c("sex", "dept"))
Kable.cohort_df <- function(x, grp = character(0), ...) {
    # ComplexSummary currently produces 4 compound numeric variables that should be R justified
    # other grouping variables should be center justified
    alignment <- str_c(str_c(rep_len("c", length(grp)), collapse=""),
                       "rrrr", collapse="")
    caption <- if (length(grp) == 0) {
        "Full Cohort"
    } else {
        paste("By ", str_x(AES_TARGET_LABELS[grp]))
    }

    sum_df <- ComplexSummary(x = x, grp = grp)

    # Pretty group names with AES_TARGET_LABELS
    sum_df %<>% mapnames(from = names(AES_TARGET_LABELS),
                         to = unname(AES_TARGET_LABELS))

    knitr::kable(sum_df, align = alignment, caption = caption)
}
