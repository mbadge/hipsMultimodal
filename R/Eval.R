# Methods to extract and evaluation trained models (caret::train)

#' Predict probas for a classification models positive class
#'
#' Positive class identity inferred from model$levels
#'
#' @param model caret classification model
#' @param ... possibly a new data frame with argname newdata
#'
#' @return numeric vector with length equal to nrow in newdata df or the training df
#'
#' @family evalTrain
#' @export
predict_pY <- function(model, ...) {
    stopifnot(class(model) == "train")
    stopifnot(model$modelType == "Classification")

    pos_class <- model$levels %>% `[`(2)

    partial(predict, type="prob")(model, ...) %>%
    `[[`(pos_class)
}


# Model Info Extractors ----

#' Extract predictor terms for a caret model
#'
#' @param x caret model
#'
#' @return x_chr of predictor terms. categoricals will be broken out to individual contrast levels
#'
#' @family evalTrain
#' @export
train_terms <- function(x) {
    stopifnot(class(x) == "train")

    x$finalModel$coefficients %>%
        names()
}


#' Extract total and limiting number of training examples from a caret model
#'
#' @param x caret model
#'
#' @return nmd_int(2) with n_limiting and n_train example counts
#'
#' @family evalTrain
#' @export
train_n_eg <- function(x) {
    stopifnot(class(x) == "train")

    x$trainingData$.outcome %>%
        each(n_limiting = compose(min, table),
             n_train = compose(sum, `!`, is.na))()
}


#' Extract vector of training examples used for a model
#'
#' @param x caret model
#'
#' @return x_chr with identifiers for training data
#'
#' @family evalTrain
#' @export
train_eg_ids <- function(x) {
    stopifnot(class(x) == "train")

    rownames(x$trainingData)
}
