library(caret)
library(AnalysisToolkit)
devtools::load_all()


# FLAGS ----
SaveFp <- function(file_stem) {
    par_dir <- Fp_ml_dir("by_cohort")
    file.path(par_dir, file_stem)
}


cohorts <- load(file = "analysis/ml/cohorts.Rdata")
data("caseControlCohorts")
cc_train_dfs <- map(
    caseControlCohorts, .f = ~filter(cohort_dfs$train, img %in% .x)
)
train_dfs <- c(list("full"=cohort_dfs$train), cc_train_dfs)

# Train ----
glms <- train_dfs %>%
    map(trainGlm, predictor_set = "img")


# save test cohorts ----
cc_test_dfs <-  map(
    caseControlCohorts, .f = ~filter(cohort_dfs$test, img %in% .x)
)
test_cohorts <- c(list("full"=cohort_dfs$test), cc_test_dfs)

# Save ----
saveRDS(glms, file = SaveFp("trained_models.rds"))
save(test_cohorts, file = SaveFp("test_cohorts.Rdata"))
