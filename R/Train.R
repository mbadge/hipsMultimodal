#' Train a glm model
#'
#' This function wraps \code{\link[caret]{train}} to train a glm model with my prefered settings and a
#' x_chr interface instead of a formula interface
#'
#' @param train_df a radiograph-major data.frame with training examples.  See \code{StratifiedPartition} to partition data.
#' @param target chr(1) names of target to train
#' @param predictors x_chr names of predictor columns
#' @param btlnck
#'
#' @return a caret S3 train object
#'
#' @export
#' @examples
#' train_glm(hipsCohort() %>% unnest(pre), target = "fx", predictors = str_c("PC", 1:10))
train_glm <- function(train_df, target = "fx", predictors, id_col = "img") {
    # Preconditions
    stopifnot(id_col %in% names(train_df), target %in% names(train_df), all(predictors %in% names(train_df)))

    # Craft target variable
    train_df[[target]] %<>% as.factor() %>% forcats::fct_relabel(make.names)
    check_y(train_df[[target]])

    # Extract modeling data
    X = train_df %>%
        dplyr::select(dplyr::one_of(predictors))
    rownames(X) <- train_df[[id_col]]  # for later reference via rownames(model$trainingData)
    Y = train_df[[target]]

    # Precondition
    if (any(is.na(X))) {stop("Missing Values")}

    trCtrl <- caret::trainControl(method = "none", summaryFunction=caret::twoClassSummary, classProbs = TRUE)

    glm_mod <- caret::train(X, Y,
                 method="glm",
                 preProcess = c("center", "scale"),
                 trControl = trCtrl,
                 metric = "ROC")

    glm_mod
}


#' @rdname train_glm
#' @export
#' @examples
#' trainGlm(hipsCohort(), predictor_set="image")
trainGlm <- function(train_df, target = "fx", predictor_set, id_col = "img") {
    if (stringr::str_detect(predictor_set, "img") && "pre" %in% names(train_df)) {
        warning("Using pretrained image features.", call. = FALSE)
        train_df %<>% tidyr::unnest_(unnest_cols = "pre")
    }

    predictors <- hipsInfo_("predictor_sets")[[predictor_set]]

    train_glm(train_df, target = target, predictors = predictors, id_col = id_col)
}


#' @rdname train_glm
#' @export
#' @examples
#' trainTestGlm(hipsCohort(), predictor_set = "image")
trainTestGlm <- function(cohort_df, target = "fx", predictor_set, grp_col = "pt", img_id = "img") {
    # precondition
    stopifnot(predictor_set %in% hipsOpt(predictor_sets), target %in% names(cohort_df), img_id %in% names(cohort_df))

    # Stratify cohort_df
    strat_df <- StratifiedPartition(cohort_df, grp_col = grp_col, target_col = target)
    strat_df %<>% split(.$partition)

    # Train
    mod <- trainGlm(strat_df$train, target = target, predictor_set = predictor_set)

    # Collect Test group
    test_df <- if (stringr::str_detect(predictor_set, "img") && "pre" %in% names(strat_df$test)) {
        strat_df$test %<>% unnest(pre)
    } else {
        strat_df$test
    }

    pY <- predict_pY(mod, newdata = test_df)
    Y <- test_df[[target]]

    cC <- AnalysisToolkit::ClassifierCurve(pY = pY, Y = Y, id = test_df[[img_id]])

    perf_tbl <- glance(cC)

    return(list("CC"=cC, "df"=strat_df))
}



#' Base Linear Regression Model Function
#'
#' @param train_df df with at least target and predictors cols
#' @param target chr(1)
#' @param predictors chr(n)
#'
#' @return caret train object
#'
#' @export
#' @examples
#' train_lm(hipsCohort(), target = "age", predictors = "device_model")
train_lm <- function(train_df, target, predictors) {
    # preconditions
    stopifnot(is.data.frame(train_df), is.character(target), is.character(predictors))
    stopifnot(target %in% names(train_df), all(predictors %in% names(train_df)))

    X <- train_df %>%
      select(one_of(predictors))
    Y <- train_df[[target]]
    caret::train(X, Y, method = "lm")
}


#' @rdname train_lm
#' @export
#' @examples
#' trainLm(hipsCohort(), target = "age", predictor_set = "image")
trainLm <- function(train_df, target, predictor_set) {
    if (stringr::str_detect(predictor_set, "img") && "pre" %in% names(train_df)) {
        warning("Using pretrained image features.", call. = FALSE)
        train_df %<>% tidyr::unnest_(unnest_cols = "pre")
    }

    predictors <- hipsInfo_("predictor_sets")[[predictor_set]]

    train_lm(train_df, target, predictors)
}


#' @rdname train_lm
#' @export
#' @examples
#' trainTestLm(hipsCohort(), target = "age", predictor_set = "image")
trainTestLm <- function(cohort_df, target, predictor_set, grp_col="pt") {
    strat_df <- StratifiedPartition(cohort_df, grp_col = grp_col, target_col = target)
    strat_df %<>% split(.$partition)

    mod <- trainLm(strat_df$train, target = target, predictor_set = predictor_set)

    if (stringr::str_detect(predictor_set, "img") && "pre" %in% names(strat_df$test)) {
        warning("Using pretrained image features.", call. = FALSE)
        strat_df$test %<>% tidyr::unnest_(unnest_cols = "pre")
    }

    test_inf <- strat_df$test %>%
        tibble::add_column(Y = .[[target]],
                           Y_ = predict(mod, newdata = .))

    gg <- ggplot(test_inf, aes(x=Y_, y=Y)) +
        geom_point() +
        geom_smooth(se = FALSE) +
        labs(x = paste("Predicted", str_case_title(target)),
             y = paste("Actual", str_case_title(target)))
    print(gg)

    return(test_inf)
}
