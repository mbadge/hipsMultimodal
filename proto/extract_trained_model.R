# Explore how to extract key data from trained caret model lists
library(hips)

InFp <- function(fn) {
    par_dir <- Fp_ml_dir("multimodal")
    file.path(par_dir, fn)
}
models <- readRDS(file = InFp("trained_models.rds"))
load(InFp("test_cohort.Rdata"))

exp_narrative <- read_lines(InFp("experiment.txt"))


exp_narrative

# View training terms ----
models %>%
    map("finalModel") %>%
    map("coefficients") %>%
    map(names)

#' @examples
#' train_terms(models[[1]])
#' map(models, train_terms)
train_terms <- function(x) {
    stopifnot(class(x) == "train")

    x$finalModel$coefficients %>%
        names()
}


# extract sample sizes ----
n_training_df <- map(models, c("trainingData", ".outcome")) %>%
    map(., each(n_limiting = compose(min, table),
                n_train = compose(sum, `!`, is.na))) %>%
    lift_dl(rbind)() %>%
    as.df() %>%
    tibble::rownames_to_column(var = "predictors")

#' @examples
#' train_n_eg(models[[1]])
#' map(models, train_n_eg)
train_n_eg <- function(x) {
    stopifnot(class(x) == "train")

    x$trainingData$.outcome %>%
        each(n_limiting = compose(min, table),
             n_train = compose(sum, `!`, is.na))()
}


# extract training example ids
models[[1]]$trainingData %>% rownames()
