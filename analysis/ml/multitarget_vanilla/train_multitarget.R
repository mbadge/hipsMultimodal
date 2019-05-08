EXPERIMENT <- glue::glue("
Train binary classifiers for each primary and secondary target.
Model: glm
Predictors: imagenet pretrained CNN embedding 10 PCs
Targets: binarized targets
* continuous variables are bins by over/under median
* categorical variables have the 2 most frequent levels retained and other levels dropped
")


devtools::load_all()
library(caret)
library(AnalysisToolkit)

# FLAGS ----
SaveFp <- function(file_stem) {
    par_dir <- Fp_ml_dir("multitarget_vanilla")
    file.path(par_dir, file_stem %>%
        MyUtils::append_date_time_stamp())
}

# Pkg globals ----
kTARGETS <- hipsOpt(targets)
kPREDICTOR_SETS <- hipsOpt(predictor_sets)
kCOVARS <- hipsOpt(covars)


load("analysis/ml/cohorts.Rdata")
# MAIN ----
binaryBT <- hipsCohort(mutating = binary)  # Ys

train_df <- binaryBT %>%
    filter(img %in% cohort_dfs$train$img)
test_df <- binaryBT %>%
    filter(img %in% cohort_dfs$test$img)

# Only do image models for this round
# TRAIN ----
TRAIN <- partial(trainGlm, train_df = train_df, predictor_set="img")
vanilla_models <- map(kTARGETS, TRAIN)

names(vanilla_models) <- kTARGETS


# SAVE ----
saveRDS(vanilla_models, file = SaveFp("trained_models.rds"))
save(test_df, file = SaveFp("test_cohort.Rdata"))
write_lines(EXPERIMENT, path = SaveFp("experiment.txt"))
