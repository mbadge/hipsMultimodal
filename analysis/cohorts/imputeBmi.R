# Issue #79
library(hips)
cohort_df <- hipsCohort()
cohort_df %<>% unnest(pre)


# Predictor sets
kPT <- setdiff(hipsOpt(pt_vars), "bmi")
kHP_ALL <- hipsOpt(hp_vars)

incomplete_hp_vars <- cohort_df %>% select(one_of(kHP_ALL)) %>% compose(colSums, is.na)() %>% magrittr::is_greater_than(0) %>% which %>% names
kHP_COMPLETE <- setdiff(kHP_ALL, incomplete_hp_vars)


# Use BMI complete cases
bmi_df <- cohort_df %>%
    drop_na(bmi)


# Cost fxn
rsq <- function(x, y) cor(x, y) ^2
rmse <- function(x, y) sqrt(mean((x-y)^2))

# Median imputation
bmi_df$bmi_imp <- median(bmi_df$bmi)

ggplot(bmi_df, aes(x = bmi_imp, y = bmi)) +
    geom_point() +
    geom_smooth()

# image only
img_mod <- train_lm(bmi_df, target = "bmi", predictors = str_c("PC", 1:10))
bmi_df$bmi_imp_img <- predict(img_mod, newdata = bmi_df)

rsq(bmi_df$bmi, bmi_df$bmi_imp_img)
ggplot(bmi_df, aes(x = bmi_imp_img, y = bmi)) +
    geom_point() +
    geom_smooth(method = "lm")

# Pt only
pt_mod <- train_lm(bmi_df, target = "bmi", predictors = c(kPT))
bmi_df$bmi_imp_pt <- predict(pt_mod, newdata = bmi_df)

rsq(bmi_df$bmi, bmi_df$bmi_imp_pt)
ggplot(bmi_df, aes(x = bmi_imp_pt, y = bmi)) +
    geom_point() +
    geom_smooth(method = "lm")


# Hp complete only
hp_mod <- train_lm(bmi_df, target = "bmi", predictors = c(kHP_COMPLETE))
bmi_df$bmi_imp_hp <- predict(hp_mod, newdata = bmi_df)

rsq(bmi_df$bmi, bmi_df$bmi_imp_hp)
ggplot(bmi_df, aes(x = bmi_imp_hp, y = bmi)) +
    geom_point() +
    geom_smooth(method = "lm")


# IMG+PT
imgPt_mod <- train_lm(bmi_df, target = "bmi", predictors = c(str_c("PC", 1:10), kPT))
bmi_df$bmi_imp_imgPt <- predict(imgPt_mod, newdata = bmi_df)

rsq(bmi_df$bmi, bmi_df$bmi_imp_imgPt)
ggplot(bmi_df, aes(x = bmi_imp_imgPt, y = bmi)) +
    geom_point() +
    geom_smooth(method = "lm")

# IMG+HP
imgHp_mod <- train_lm(bmi_df, target = "bmi", predictors = c(str_c("PC", 1:10), kPT))
bmi_df$bmi_imp_imgHp <- predict(imgHp_mod, newdata = bmi_df)

rsq(bmi_df$bmi, bmi_df$bmi_imp_imgHp)
ggplot(bmi_df, aes(x = bmi_imp_imgHp, y = bmi)) +
    geom_point() +
    geom_smooth(method = "lm")


# IMG+PT+HP
imgPtHp_mod <- train_lm(bmi_df, target = "bmi", predictors = c(str_c("PC", 1:10), kPT, kHP_COMPLETE))
bmi_df$bmi_imp_imgPtHp <- predict(imgPtHp_mod, newdata = bmi_df)

rsq(bmi_df$bmi, bmi_df$bmi_imp_imgPtHp)
ggplot(bmi_df, aes(x = bmi_imp_imgPtHp, y = bmi)) +
    geom_point() +
    geom_smooth(method = "lm")


# Impute other columns ----
# Impute explicit NA
colSums(is.na(bmi_df)) %>% sort(decreasing = TRUE)
bmi_df$dept %<>% fct_explicit_na()
bmi_df$name_operator %<>% fct_explicit_na()

# Median impute continuous
bmi_df$procedure_radiation[is.na(bmi_df$procedure_radiation)] <- median_(bmi_df$procedure_radiation)
bmi_df$dT.exam_to_prelim[is.na(bmi_df$dT.exam_to_prelim)] <- median_(bmi_df$dT.exam_to_prelim)
bmi_df$dT.exam_to_final[is.na(bmi_df$dT.exam_to_final)] <- median_(bmi_df$dT.exam_to_final)
bmi_df$dT.ordered_to_exam[is.na(bmi_df$dT.ordered_to_exam)] <- median_(bmi_df$dT.ordered_to_exam)


# Hp all only
hpA_mod <- train_lm(bmi_df, target = "bmi", predictors = c(kHP_ALL))
bmi_df$bmi_imp_hp <- predict(hp_mod, newdata = bmi_df)

rsq(bmi_df$bmi, bmi_df$bmi_imp_hp)
ggplot(bmi_df, aes(x = bmi_imp_hp, y = bmi)) +
    geom_point() +
    geom_smooth(method = "lm")

# Hp all + IMG
imgHpA_mod <- train_lm(bmi_df, target = "bmi", predictors = c(str_c("PC", 1:10), kHP_ALL))
bmi_df$bmi_imp_hp <- predict(imgHpA_mod, newdata = bmi_df)

ggplot(bmi_df, aes(x = bmi_imp_hp, y = bmi)) +
    geom_point() +
    geom_smooth(method = "lm")

# IMG+PT+HP_img
imgPtHpImp_mod <- train_lm(bmi_df, target = "bmi", predictors = c(str_c("PC", 1:10), kPT, kHP_ALL))
bmi_df$bmi_imp_imgPtHpImp <- predict(imgPtHpImp_mod, newdata = bmi_df)

rsq(bmi_df$bmi, bmi_df$bmi_imp_imgPtHpImp)
ggplot(bmi_df, aes(x = bmi_imp_imgPtHpImp, y = bmi)) +
    geom_point() +
    geom_smooth(method = "lm")


# Results comparison
models <- list(img_mod, pt_mod, hp_mod, imgPt_mod, imgHp_mod, imgPtHp_mod, hpA_mod, imgHpA_mod, imgPtHpImp_mod)
res_df <- map_dfr(models, "results")
res_df <- res_df[, c("RMSE", "Rsquared", "RMSESD", "RsquaredSD")]
res_df %<>% tibble::add_column(predictor_sets = c("Img", "Pt", "Hp", "ImgPt", "ImgHp", "ImgPtHp", "Hp", "ImgHp", "ImgPtHp"),
                               imputed_hp = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
                               .before="RMSE")
res_df
res_df %>% arrange(RMSE)
write_csv(res_df, "analysis/cohorts/imputeBmiStats.csv")

# Append bmi_w_imp to cohort table
# Impute full cohort missing hp variables
cohort_df <- hipsCohort()

# Impute other columns ----
# Impute explicit NA
colSums(is.na(cohort_df)) %>% sort(decreasing = TRUE)
cohort_df$dept %<>% fct_explicit_na()
cohort_df$name_operator %<>% fct_explicit_na()

# Median impute continuous
cohort_df$procedure_radiation[is.na(cohort_df$procedure_radiation)] <- median_(cohort_df$procedure_radiation)
cohort_df$dT.exam_to_prelim[is.na(cohort_df$dT.exam_to_prelim)] <- median_(cohort_df$dT.exam_to_prelim)
cohort_df$dT.exam_to_final[is.na(cohort_df$dT.exam_to_final)] <- median_(cohort_df$dT.exam_to_final)
cohort_df$dT.ordered_to_exam[is.na(cohort_df$dT.ordered_to_exam)] <- median_(cohort_df$dT.ordered_to_exam)

colSums(is.na(cohort_df)) %>% sort(decreasing = TRUE)

# Impute missing BMIs
cohort_df %<>% unnest(pre)

cohort_df$bmi_imp <- predict(imgPtHpImp_mod, newdata=cohort_df)

cohort_df$bmi_w_imp <- ifelse(is.na(cohort_df$bmi), cohort_df$bmi_imp, cohort_df$bmi)

# Save table of image + bmi_w_imp
bmi_imp_df <- cohort_df[, c("img", "bmi_w_imp")]
colnames(bmi_imp_df) <- c("img", "bmi")
write_csv(bmi_imp_df, "analysis/cohorts/imputedBmi.csv")
