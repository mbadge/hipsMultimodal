# Train glm models on patient-stratified cross-sectional data
# This exploratory prototyping routine looks at the various component steps involved
# e.g., preprocessing (center and scale)

library(caret)
devtools::load_all()

# Use all EHR variables that would be known at the moment the image is acquired
#! see #71
kEHR_VARS <- hipsOpt(ehr_vars)


# Abstract training df columns
id_col <- "img"
indication <- "fx"
predictors <- kEHR_VARS
prtn_group_col <- "pt"

# Load training data
print("Missing data imputed")
cohort_df <- StratifiedPartition(hipsCohort(mutating=complete))
train_df <- cohort_df %>% filter(partition == "train")
# Select pertient columns
train_df %<>%
    dplyr::select(dplyr::one_of(indication, predictors, prtn_group_col))

# If id_col is specified, save for later
if (!is.na(id_col)) {
    stopifnot(id_col %in% names(train_df),
              is.character(train_df[[id_col]]))
    img_ids <- train_df[[id_col]]
    train_df[id_col] <- NULL
}


# Groom for caret binary classification
# caret train wants Y to be a factor with levels suitable for column names
if ((compose(length, unique)(train_df[[indication]])) != 2) {
    n_unique <- compose(length, unique)(train_df[[indication]])
    stop("expected 2 unique Y values for glm model, found ", n_unique,
         "levels in variable ", indication)
}
train_df[[indication]] %<>% as.factor %>% forcats::fct_relabel(make.names)

X = train_df %>%
    dplyr::select(dplyr::one_of(predictors))
Y = train_df[[indication]]
# Precondition
check_y(Y)  # Target is valid for classification

# check preprocessing
pp_cohort <- preProcess(X, method = c("center", "scale"))
predict(pp_cohort, newdata = cohort_df %>% filter(partition == "test") %>% select(one_of(predictors)))


# Train models ----
trCtrl1 <- trainControl(method = "none", summaryFunction=caret::twoClassSummary, classProbs = TRUE)
glm_mod1 <- caret::train(x=X, y=Y, preProcess = c("center", "scale"),
                         method="glm", metric="ROC",
                         trControl = trCtrl1)
glm_mod1

# Fxnalize ----
Train <- function(train_df, indication, predictors) {
    # Groom for caret binary classification
    # caret train wants Y to be a factor with levels suitable for column names
    if ((compose(length, unique)(train_df[[indication]])) != 2) {
        n_unique <- compose(length, unique)(train_df[[indication]])
        stop("expected 2 unique Y values for glm model, found ", n_unique,
             "levels in variable ", indication)
    }
    train_df[[indication]] %<>% as.factor %>% forcats::fct_relabel(make.names)

    X = train_df %>%
        dplyr::select(dplyr::one_of(predictors))
    Y = train_df[[indication]]

    trCtrl <- trainControl(method = "none", summaryFunction=caret::twoClassSummary, classProbs = TRUE)
    caret::train(X, Y, preProcess = c("center", "scale"), method="glm", metric = "ROC", trControl = trCtrl)
}
TRAIN <- partial(Train, indication = "fx", predictors = kEHR_VARS)
