# Coerce pkg scalars table to have no missing values
#
# Categoricals -> explicit NA
# Continuous -> median imputation
devtools::load_all()


data("scalars", package = "hips")

#' @examples
#' saveCompleteScalars(scalars)
saveCompleteScalars <- function(scalars_df) {
    FP = "analysis/cohorts/completeScalars.rds"
    if (file.exists(FP)) stop(paste("There's already a file saved to", FP))

    saveRDS(scalars_df, file = FP)
}


# ------------------------------- Handle missing data -------------------------------
ComplexSummary(scalars)
DF.assess_missingness(scalars)

incomplete_col_idx <- scalars %>% (compose(as_mapper(~.x > 0), colSums, is.na)) %>% which()
scalars[incomplete_col_idx] %>% map_chr(class)

# Use explicit NA for missing values
scalars[incomplete_col_idx] %>%
    keep(.p=is.factor) %>%
    map(table_)
scalars[incomplete_col_idx] %<>% mutate_if(.p = is.factor, .f = fct_explicit_na)

scalars[incomplete_col_idx] %>%
    keep(.p=is.factor) %>%
    map(table_)

# Use median imputation for continuous variables
missing_medians <- scalars[incomplete_col_idx] %>%
    keep(.p = compose(`!`, is.factor)) %>%
    summarise_all(.funs = median_)



g1 <- scalars[incomplete_col_idx] %>%
    keep(.p=compose(`!`, is.factor)) %>%
    gather(key = "var", value = "val") %>%
    ggplot(., aes(x = val)) +
        facet_grid(. ~ var, scale="free_x") +
        geom_histogram()
g1
g1 + scale_x_log10() + scale_y_log10()

scalars[names(missing_medians)] %<>%
    map2(missing_medians, ~ifelse(is.na(.x), .y, .x))


g2 <- scalars[incomplete_col_idx] %>%
    keep(.p=compose(`!`, is.factor)) %>%
    gather(key = "var", value = "val") %>%
    ggplot(., aes(x = val)) +
    facet_grid(. ~ var, scale="free_x") +
    geom_histogram()

cowplot::plot_grid(g1, g2, ncol = 1)
list(g1, g2) %>%
    map(`+`, scale_x_log10()) %>%
    map(`+`, scale_y_log10()) %>%
    cowplot::plot_grid(plotlist = ., ncol = 1)

# Use multimodal BMI imputation
bmi_df <- read_csv("analysis/cohorts/imputedBmi.csv")

df <- inner_join(scalars, bmi_df, by = "img")
scalars$bmi <- df$bmi.y

g2 <- scalars[incomplete_col_idx] %>%
    keep(.p=compose(`!`, is.factor)) %>%
    keep(.p=compose(`!`, is.character)) %>%
    gather(key = "var", value = "val") %>%
    ggplot(., aes(x = val)) +
    facet_grid(. ~ var, scale="free_x") +
    geom_histogram()

cowplot::plot_grid(g1, g2, ncol = 1)
list(g1, g2) %>%
    map(`+`, scale_x_log10()) %>%
    map(`+`, scale_y_log10()) %>%
    cowplot::plot_grid(plotlist = ., ncol = 1)

DF.assess_missingness(scalars)

saveCompleteScalars(scalars)
