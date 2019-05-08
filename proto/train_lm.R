# Train linear regression models on continuous variables
library(hips)
library(caret)

# FLAGS ----
kCOHORT_MUTATION <- "complete"
kINDICATION <- "age"
kPREDICTORS <- hipsInfo(predictor_sets)$image

# Abstract training df columns
id_col <- "img"
prtn_group_col <- "pt"

# kNUM_VARS <- c("age",
#                "bmi",
#                "order_date",
#                "procedure_radiation",
#                "dT.ordered_to_exam",
#                "dT.exam_to_prelim",
#                "dT.exam_to_final")



# MAIN ----


# Prepare train/test data
# cohort_df <- hipsCohort_(mutating = kCOHORT_MUTATION)
# strat_df <- StratifiedPartition(cohort_df, grp_col = prtn_group_col, target_col = indication)
# partition_dfs <- strat_df %>%
#     split(.$partition) %>%
#     map(unnest_, unnest_cols = "pre") %>%
#     map(tibble::column_to_rownames, var = id_col)
Partitions <- function(cohort, strat, target) {
    cohort_df <- hipsCohort_(mutating = cohort)
    strat_df <- StratifiedPartition(cohort_df, grp_col = strat, target_col = target)
    strat_df %>%
        split(.$partition) %>%
        map(unnest_, unnest_cols = "pre") %>%
        map(tibble::column_to_rownames, var = id_col)
}
partition_dfs <- Partitions(cohort = kCOHORT_MUTATION, strat = prtn_group_col, target = kINDICATION)

# Train model
# X <- partition_dfs$train %>%
#     select(starts_with("PC"))
# Y <- partition_dfs$train[[indication]]
# img_mod <- train(X, Y,
#                  method = "lm",
#                  predictors = "image")

#' @examples
#' Train(Partitions()$train, "age", hipsInfo(predictor_sets)$image)
Train <- function(df, target, predictors) {
    X <- df %>% select(one_of(predictors))
    Y <- df[[target]]
    train(X, Y,
          method = "lm")
}
mod <- Train(partition_dfs$train, target = kINDICATION, predictors = kPREDICTORS)

Inference <- function (model, partitions, target) {
    # Infer train data
    train_inf <- partitions$train %>%
        tibble::add_column(Y = model$trainingData$.outcome,
                           Y_ = predict(model),
                           resid = model %>% resid() %>% abs())
    test_inf <- partitions$test %>%
        tibble::add_column(Y = .[[target]],
                           Y_ = predict(model, newdata = .),
                           resid = abs(Y-Y_))
    bind_rows(train_inf, test_inf)
}


inf_df <- Inference(mod, partition_dfs, target = kINDICATION)


Eval <- function(DATA) {
    ga <- ggplot(DATA, aes(x=Y_, y=Y)) +
        geom_point(alpha = 0.01) +
        geom_smooth(se = FALSE) +
        facet_grid(. ~ partition)

    gb <- ggplot(DATA, aes(x=Y, y=abs(resid))) +  #, col = device_model)) +
        geom_point(alpha = 0.01) +
        stat_smooth(se = FALSE) +
        facet_grid(. ~ partition)

    cowplot::plot_grid(ga, gb, nrow=2)
}
Eval(inf_df)



# LM <- function(COHORT,
#                  TARGET,
#                  PREDICTORS) {
#
#     prtns <- Partitions(cohort = COHORT, strat = "pt", target = TARGET)
#
#     Train(prtns$train, target = TARGET, predictors = PREDICTORS) %>%
#         Inference(partitions = prtns, target = TARGET) %>%
#         Eval()
# }

# LM(COHORT = "complete", TARGET = "age", PREDICTORS = hipsInfo(predictor_sets)$image)
# LM(COHORT = "complete", TARGET = "order_date", PREDICTORS = hipsInfo(predictor_sets)$image)
# LM(COHORT = "none", TARGET = "order_date", PREDICTORS = hipsInfo(predictor_sets)$image)
#
# cohorts <- c("none", "complete")
# res <- map(cohorts, LM,
#     TARGET = "age",
#     PREDICTORS = hipsInfo(predictor_sets)$image)
#
# res <- map(kNUM_VARS, ~LM(TARGET=.x, COHORT = "none", PREDICTORS = hipsInfo(predictor_sets)$image))


# train_params <- expand.grid(cohorts, kNUM_VARS)
# res <- mapply(LM, COHORT=train_params$Var1, TARGET=train_params$Var2,
#               MoreArgs = list(PREDICTORS = hipsInfo(predictor_sets)$image))

# # Infer train data
# train_inf <- partition_dfs$train %>%
#     tibble::add_column(Y = img_mod$trainingData$.outcome,
#                        Y_ = predict(img_mod),
#                        resid = img_mod %>% resid() %>% abs())
# test_inf <- partition_dfs$test %>%
#     tibble::add_column(Y = .[[indication]],
#                        Y_ = predict(img_mod, newdata = .),
#                        resid = abs(Y-Y_))
#
#
# ggplot(train_inf, aes(x=Y_, y=Y)) +  #, col = device_model)) +
#     geom_point(alpha = 0.01) +
#     geom_smooth(se=FALSE)  #, aes(group = 1))
#
# ggplot(train_inf, aes(x=Y, y=abs(resid))) +  #, col = device_model)) +
#     geom_point(alpha = 0.01) +
#     stat_smooth()
#
# ggplot(test_inf, aes(x=Y_, y=Y)) +  #, col = device_model)) +
#     geom_point(alpha = 0.01) +
#     geom_smooth(se=FALSE)  #, aes(group = 1))
# ggplot(test_inf, aes(x=Y, y=abs(resid))) +  #, col = device_model)) +
#     geom_point(alpha = 0.01) +
#     stat_smooth()
