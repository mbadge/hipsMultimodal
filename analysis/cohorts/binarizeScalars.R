# Coerce pkg chort tables for MSH and Adelaide into all binary targets

isFactorish <- Or(is.character, is.factor)

# Binary coercion methods ----
# Continuous: Greater than median
GtMedian_ <- purrr::as_mapper(~ .x > median_(.x))
GtMedian <- function(x_dbl) {
    stopifnot(is.numeric(x_dbl))
    GtMedian_(x_dbl)
}

# Factors


# Specials
Date_dayOfWeek_2_weekDayEnd <- function(x) {
    stopifnot(isFactorish(x))
    x %>% as.factor %>% fct_collapse("weekday"=c("Mon", "Tues", "Wed", "Thurs", "Fri"),
                           "weekend"=c("Sun", "Sat"))
}

# ----------------------------- MSH -----------------------------
# Binarize All Potential Targets
data("scalars", package="hips")
saveMshScalars <- function(scalars_df) {
    FP = "analysis/cohorts/binaryScalars.rds"
    if (file.exists(FP)) stop(paste("There's already a file saved to", FP))
    saveRDS(scalars_df, file = FP)
}


# Coerce all columns to a format conducive to binary classification
# Continuous
# Save tabulation of median values used for binarization per reviewer request
medians <- scalars %>%
    summarise_if(.p = is.numeric, .f = median_) %>%
    AnalysisToolkit::t2idf(name = "variable", value = "median") %>%
    mutate(median = round(median, digits = 0))
medians$variable %<>% mapvalues(from = names(hips:::AES_TARGET_LABELS), to = unname(hips:::AES_TARGET_LABELS))
Write(medians, fp = "analysis/cohorts/medians.csv")

scalars[] %<>% map_if(.p = is.numeric, .f = GtMedian)

# Special
scalars$order_wday %<>% Date_dayOfWeek_2_weekDayEnd()

# Factors
scalars$dept %<>%
    fct_recode(NULL="outpt")
scalars$projection_set %<>%
    fct_collapse(lat = c("ap_ll", "ap_lr"))


scalars$name_operator %>% table_
scalars$name_operator %<>%
    fct_collapse(NULL="other_valid_entry",
                 NULL="technologist")
scalars$name_interpreter %<>%
    fct_collapse(darren="darren",
                 NULL = "alex",
                 sridhar="sridhar",
                 NULL="Other")
scalars$device_brand %>% table_
scalars$device_brand %<>% fct_recode(NULL="fujifilm", NULL="philips")
scalars$device_model %>% table
scalars$device_model %<>% fct_lump(n=2, other_level=NA_character_)

#! WTF @ factor level being coded as NA but not equal to NA
scalars$device_model %>% table_
is.na(scalars$device_model) %>% sum
levels(scalars$device_model) %>% `[`(3) %>% is.na()
any(scalars$device_model == "NA")
any(scalars$device_model == "<NA>")
any(is.na(scalars$device_model))
# Patch: convert to character and back
scalars$device_model %<>% as.character() %>% as.factor()
table(scalars$device_brand, scalars$device_model)


# Postcondition
map_int(scalars, compose(length, unique))  # NAs are considered a unique level

# Save ----
saveMshScalars(scalars)


# ----------------------------- Adelaide -----------------------------
data("adl_test", package = "hips")
saveAdlScalars <- function(scalars_df) {
    FP = "analysis/cohorts/binaryAdlTest.rds"
    if (file.exists(FP)) stop(paste("There's already a file saved to", FP))
    saveRDS(scalars_df, file = FP)
}


adl_test$age %<>% GtMedian()
adl_test$order_wday %<>% Date_dayOfWeek_2_weekDayEnd()

# Factors
adl_test$device_model %>% table_()
adl_test$device_model %>% as_factor() %>% fct_lump(n = 2, other_level = NA_character_) %>% table_()
adl_test$device_model %<>% as_factor() %>% fct_lump(n = 2, other_level = NA_character_)

adl_test$name_interpreter %>% table_()
adl_test$name_interpreter %<>% fct_recode(NULL = "UNKNOWN")
adl_test$name_interpreter %<>% as_factor() %>% fct_lump(n=2, other_level = NA_character_)

adl_test$device_brand %>% table_()
adl_test$device_brand %>% as_factor() %>% fct_lump(n=2, other_level = NA_character_) %>% table()
adl_test$device_brand %<>% as_factor() %>% fct_lump(n=2, other_level = NA_character_)

# WTF @ the pseudo explicit NA with these preventing OR computation:
adl_test$sex %<>% fct_recode(NULL = "O")
adl_test$sex %<>% as.character() %>% as.factor()
adl_test$device_brand %<>% as.character() %>% as.factor()
adl_test$device_model %<>% as.character() %>% as.factor()
adl_test$name_interpreter %<>% as.character() %>% as.factor()


# Postcondition
map_int(adl_test, compose(length, unique))  # NAs are considered a unique level

# Save ----
saveAdlScalars(adl_test)
