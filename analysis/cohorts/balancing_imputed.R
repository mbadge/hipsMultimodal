devtools::load_all()
library(e1071)

imp <- hipsCohort(complete)
imp$fx %<>% (compose(as.factor, tolower, as.factor))
imp$fx %<>% mapvalues(from = c("true", "false"), to = c("case", "cont"))

covars <- hipsOpt(covars) %>% set_names()


f_covars <- function(covars) {
    as.formula(paste("fx ~ ", str_c(covars, collapse = " + ")))
}

imp$name_interpreter %<>% as.factor()


# Match by demographics or all metadata ----
matchDem <- matchControls(fx ~ age + sex, data = imp)
matchPt <- matchControls(f_covars(hipsOpt(pt_vars)), data = imp)
matchAll <- matchControls(f_covars(hipsOpt(covars)), data = imp)


imp$cohortDem <- matchDem$factor
imp$cohortPt <- matchPt$factor
imp$cohortAll <- matchAll$factor


balDem <- imp %>%
    filter(!is.na(cohortDem)) %>%
    extract2("img")
balPt <- imp %>%
    filter(!is.na(cohortPt)) %>%
    extract2("img")
balAll <- imp %>%
    filter(!is.na(cohortAll)) %>%
    extract2("img")

rnd_conts <- imp %>%
    filter(fx == "cont") %>%
    sample_n(783) %>%
    extract2("img")
case_ids <- imp %>%
    filter(fx == "case") %>%
    extract2("img")


caseControlCohorts <- list(
    rnd = c(rnd_conts, case_ids),
    balDem = balDem,
    balPt = balPt,
    balAll = balAll
)

caseControlCohorts %>% Venn()
devtools::use_data(caseControlCohorts, overwrite = TRUE)
