EXPERIMENT <- glue::glue("
Train patient-stratified cross-sectional multimodal models
Cohort: complete - median imputation or explicit '(Missing)'
Model: glm
Predictors: image, pt, hp, multimodal
Targets: fracture
")


library(caret)
library(AnalysisToolkit)
devtools::load_all()


# FLAGS ----
SaveDir <- Fp_ml_dir("multimodal")

# Precondition
if(!dir.exists(SaveDir)) {stop("Output Directory not found: ", SaveDir)}


# Pkg data ----
# cohort_df <- hipsCohort(complete)

# Craft cohorts
(a <- trainTestGlm(cohort_df, target = "fx", predictor_set = "img"))
a$df$train$rnd <- NULL
a$df$train %<>% unnest(pre)
a$df$test$rnd <- NULL
cohort_dfs <- a$df
save(cohort_dfs, file="analysis/ml/cohorts.Rdata")
load("analysis/ml/cohorts.Rdata")


#

# Helpers ----
SaveFp <- function(file_stem, dir = SaveDir) {
    file.path(dir, file_stem %>%
                  MyUtils::append_date_time_stamp())
}


# MAIN ----
# cohort_df %<>% StratifiedPartition(grp_col = "pt", target_col = "fx")

train_df <- cohort_dfs$train
test_df <- cohort_dfs$test

# Train routine ----
# Train by predictor set ----
img_mod <- trainGlm(train_df, predictor_set = "img")
pt_mod <- trainGlm(train_df, predictor_set = "pt")
hp_mod <- trainGlm(train_df, predictor_set = "hp")
ptHp_mod <- trainGlm(train_df, predictor_set = "ptHp")
imgPt_mod <- trainGlm(train_df, predictor_set = "imgPt")
imgHp_mod <- trainGlm(train_df, predictor_set = "imgHp")
imgPtHp_mod <- trainGlm(train_df, predictor_set = "imgPtHp")

models <- list("img"=img_mod,
               "pt" =pt_mod,
               "hp" =hp_mod,
               "ptHp" = ptHp_mod,
               "imgPt"=imgPt_mod,
               "imgHp"=imgHp_mod,
               "imgPtHp"=imgPtHp_mod)

# postcondition
stopifnot(all(map(models, class) == "train"))

# Save ----
saveRDS(models, file = SaveFp("trained_models.rds"))
save(train_df, test_df, file = SaveFp("cohorts.Rdata"))
write_lines(EXPERIMENT, path = SaveFp("experiment.txt"))
beep_()
