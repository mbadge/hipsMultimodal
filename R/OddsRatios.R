# ---- Compute Pair-wise Odds Ratios ----
# Univariate Discrete ORs
# Compute from 2x2 frequency table
# Measure the association between exposure covars and fracture using the odds ratio
# 2-sided Fisher's exact test for count data


#' Only keep variables with two unique non-missing levels
#'
#' Used for pair-wise Odds Ratio computation
#'
#' @examples
#' hipsCohort() %>% keep_binary()
#' hipsCohort() %>% split(.$device_model) %>% map(keep_binary)
keep_binary <- function(df) {
    df %>%
        purrr::keep(.p = ~n_distinct(.x, na.rm=TRUE) == 2)
}


#' Compute pair-wise odds ratios between binary variables and a target
#'
#' @return a covariate-major data.frame with S3 class 'OddsRatios'
#'
#' @family OddsRatios
#' @export
#' @examples
#' ors <- OddsRatios(hipsCohort(binary), "fx")
#' ors2 <- hipsCohort(binary) %>% split(.$device_model) %>% OddsRatios(target_chr = "fx", grp_chr = "device_model")
OddsRatios <- function(x, ...) UseMethod("OddsRatios")

#' @export
#' @rdname OddsRatios
OddsRatios.data.frame <- function(x, target_chr = "fx") {
    # precondition
    stopifnot(is.character(target_chr), target_chr %in% names(x))

    orDf <- x %>%
        keep_binary() %>%
        select(-one_of(target_chr)) %>%
        map(table, x[[target_chr]]) %>%
        map(fisher.test) %>%
        map(`[`, c("p.value", "conf.int", "estimate")) %>%
        map(unlist) %>%
        (lift_dl(bind_rows, .id="target")) %>%
        mapnames(from = c("conf.int1", "conf.int2", "estimate.odds ratio"),
                 to   = c("ymin", "ymax", "y"))

    class(orDf) <- c("OddsRatios", class(orDf))
    orDf
}

#' @export
#' @rdname OddsRatios
OddsRatios.list <- function(x, target_chr = "fx", grp_chr = "device_model") {
    x %>%
        map(OddsRatios, target_chr = target_chr) %>%
        lift_dl(bind_rows, .id = grp_chr)()
}


# ---- View Multi-Associations Odds Ratios ----

#' @rdname ggOddsRatios
#' @examples
#' trans_sig(1/rnorm(100))
trans_sig <- function(x) {
    ifelse(x < (10^-25),
           "***",
           ifelse(x < (10^-10),
                  "**",
                  ifelse(x < (0.05), "*", "")))
}


#' View odds ratios of pair-wise target associations.
#'
#' @param x data.frame with odds ratios to view
#' @param sig lgl(1) should asterixis be added to designate significance?
#'
#' @family OddsRatios
#' @importFrom magrittr "%<>%"
#' @export
#' @examples
#' ggOddsRatios(hipsCohort(binary))
#' ggOddsRatios(OddsRatios(hipsCohort(binary)))
#' ggOddsRatios(hipsCohort(binary), sig = FALSE)
#' ggOddsRatios(OddsRatios(hipsCohort(binary)), sig = FALSE)
#' ggOddsRatios(OddsRatios(hipsCohort(binary) %>% split(.$device_model)), sig = TRUE) + aes_device_col()
ggOddsRatios <- function(x, ...) UseMethod("ggOddsRatios")

#' @export
#' @rdname ggOddsRatios
ggOddsRatios.OddsRatios <- function(x, sig = TRUE) {
    gg <- ggplot(x, aes(x = target, y=y, ymin=ymin, ymax=ymax)) +
        geom_linerange(position = Pd(width = 0.5)) +
        geom_point(position = Pd(width = 0.5)) +
        geom_hline(yintercept = 1, alpha = 0.5, linetype = 1) +
        geom_hline(yintercept = .1, alpha = 0.25, linetype = 1) +
        geom_hline(yintercept = 10, alpha = 0.25, linetype = 1) +
        theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
        scale_x_target() +
        scale_y_log10(breaks = c(0.1, 1, 10), labels = c(0.1, 1, 10)) +
        ylab(expression(paste("Odds Ratio ", over("P(fx|v+)", "P(fx|v-)"))))

    if (sig) {
        gg %<>% `+`(geom_text(aes(label = trans_sig(p.value)), nudge_x = 0.3, show.legend = FALSE))
    }

    gg %<>% `+`(coord_flip())
    gg
}

#' @export
#' @rdname ggOddsRatios
ggOddsRatios.cohort_df <- function(x, target_chr = 'fx', sig=TRUE) {
    orDf <- OddsRatios(x, target_chr = target_chr)
    ggOddsRatios(orDf, sig = sig)
}
