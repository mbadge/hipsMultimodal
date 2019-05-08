#' Confirm that a target vector is suitable for a classification model.
#'
#' copied from \code{\link[caret]{train}}.
#'
#' @param y x_fct or x_chr to with true labels
#' @param modelType chr(1)
#'
#' @return silent; raises if ill-suited model target
#'
#' @importFrom caret trainControl
#' @export
#' @family checks
#'
#' @examples
#' check_y(iris[, 5])
#' suppressMessages(check_y(iris[, 5]))
#' try(check_y(fct_expand(iris[, 5], "new_lvl")))
#' try(check_y(fct_recode(iris[, 5], "3x" = "setosa")))
check_y <- function(y) {
    modelType <- caret:::get_model_type(y)

    if(modelType == "Classification") {
        ## We should get and save the class labels to ensure that predictions are coerced
        ## to factors that have the same levels as the original data. This is especially
        ## important with multiclass systems where one or more classes have low sample sizes
        ## relative to the others
        classLevels <- levels(y)
        attributes(classLevels) <- list(ordered = is.ordered(y))

        xtab <- table(y)
        if(any(xtab == 0)) {
            xtab_msg <- paste("'", names(xtab)[xtab == 0], "'", collapse = ", ", sep = "")
            stop(paste("One or more factor levels in the outcome has no data:", xtab_msg), call. = FALSE)
        } else {
            message("All outcome levels are represented")
        }

        if(any(classLevels != make.names(classLevels))) {
            stop(paste("At least one of the class levels is not a valid R variable name;",
                       "This will cause errors when class probabilities are generated because",
                       "the variables names will be converted to ",
                       paste(make.names(classLevels), collapse = ", "),
                       ". Please use factor levels that can be used as valid R variable names",
                       " (see ?make.names for help)."), call. = FALSE)
        } else {
            message("All outcome levels are valid variable names")
        }
    }

    invisible(NULL)
}



#' Get a list of levels for each factor in a data.frame
#'
#' A list is created that contains a x_chr for each factor variable
#' containing all unique factor levels
#'
#' @param df a data.frame with 1+ factor variables
#'
#' @return A list with length equal to the number of factor variables in df.
#'
#' @export
#' @examples
#' get_df_fct_lvls(iris, xtabs=TRUE)
#' get_df_fct_lvls(forcats::gss_cat, xtabs=TRUE)
get_df_fct_lvls <- function(df, xtabs=FALSE) {
    fct_df <- df %>%
        Filter(f=is.factor) %>%
        droplevels()

    if (xtabs) {fct_df %>% purrr::map(table) %>% print()}

    fct_df %>% purrr::map(levels)
}




#' Verify that all factor levels in df2 are available in df1
#'
#' Used to ensure a model trained on df2 can be applied to all examples in df2
#'
#' @param df1 data.frame with 1+ factor variables
#' @param df2 data.frame with 1+ factor variables
#'
#' @return invisible.  stops runtime if new inference levels are found in df2
#' @export
#'
#' @family checks
#' @examples
#' df2 = gss_cat
#' df2$race <- as.character(df2$race)
#' df3 = gss_cat %>% filter(race %in% c("Black", "White"))
#' check_dfs_fct_match(gss_cat, gss_cat)
#' try(check_dfs_fct_match(gss_cat, df2))
#' try(check_dfs_fct_match(gss_cat, df3))
check_dfs_fct_match <- function(df1, df2, .silent=FALSE) {
    # confirm both dfs have the same factor variables
    fct_var_lsts <- map(list(df1, df2), get_df_fct_lvls)

    are_fcts_shared <- if (length(fct_var_lsts[[1]]) != length(fct_var_lsts[[2]])) {
        FALSE
    } else {  # `==` comparison only works with equal length operands
        fct_var_lsts %>% map(names) %>% map(sort) %>% lift_dl(`==`)() %>% all()
    }
    if (!are_fcts_shared) {
        #browser()
        only_df1 <- fct_var_lsts %>% map(names) %>% lift_dl(setdiff)()
        only_df2 <- fct_var_lsts %>% map(names) %>% rev() %>% lift_dl(setdiff)()

        msg <- "Not all factors found in both data.frames:\n"
        if (!purrr::is_empty(only_df1)) {
            msg <- paste(msg, str_x(only_df1), "only found in", deparse(substitute(df1)), "\n")
        }
        if (!purrr::is_empty(only_df2)) {
            msg <- paste(msg, str_x(only_df2), "only found in", deparse(substitute(df2)))
        }
        stop(msg)
    }

    # confirm levels of all factors are shared
    # similarly sort fct dfs
    sorted_fcts_chr <- fct_var_lsts[[1]] %>% names %>% sort
    fct_pair_lsts <- fct_var_lsts %>%
        purrr::map(`[`, sorted_fcts_chr) %>%
        transpose
    are_fct_lvls_equal <- fct_pair_lsts %>%
        purrr::map_lgl(
            purrr::lift_dl(setequal)
        )
    if (!all(are_fct_lvls_equal)) {
        stop("Factor levels differ for variable(s): ",
             str_x(names(fct_pair_lsts)[!are_fct_lvls_equal]))
    }

    # Else success
    if (!.silent) {
        cat("both dataframes have the same factor levels for factor variables:\n",
            get_df_fct_lvls(df1) %>% names() %>% MyUtils::str_x())
    }

    invisible(NULL)
}

