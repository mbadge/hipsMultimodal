#' View a PC plot with indicated color/alpha
#'
#' @family viewFig
#' @export
#' @examples
#' df <- data.frame(PC1 = rnorm(100), PC2 = rnorm(100), FX = rep(c(FALSE, TRUE), 100/2))
#' gg_pc(df, FX) + hips::scale_color_fx()
gg_pc <- function(DF, COL) {
    ggplot(DF, aes_(x=~PC1, y=~PC2, col=substitute(COL), alpha=substitute(COL))) +
        geom_point() +
        scale_x_continuous(breaks=NULL) +
        scale_y_continuous(breaks=NULL) +
        coord_equal()
}


#' View data.frame missing data proportion for each variable with missingness
#'
#' @family viewFig
#' @export
#' @examples
#' forcats::gss_cat %>% ggMissing()
#' hipsCohort() %>% ggMissing
#' datasets::mtcars %>% ggMissing()
ggMissing <- function(x) {
    # precondition
    stopifnot(is.data.frame(x))

    # edge case - no missing data
    # exit early b/c fct_reorder will raise otherwise
    if (sum(is.na(x)) == 0) {
        message("No missing data!")
        return(NULL)
    }

    gg <- x %>%
        compose(colSums, is.na)() %>% keep(.p = ~.x > 1) %>%
        tibble::enframe(name = "variable", value = "n_missing") %>%
        ggplot(., aes(x=fct_reorder(variable, n_missing), y=n_missing)) +
            geom_col(fill="#E7298A") +
            geom_hline(yintercept = nrow(x)) +
            gg_x_target()
    gg
}
