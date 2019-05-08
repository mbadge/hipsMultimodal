# Targets
kID_COLS <- c("img", "pt")
kPATH_COLS <- "fx"
kPT_COLS <- c("age", "sex", "bmi", "fall_lgl", "pain_lgl")
kHOSP_COLS <- c("dept", "name_operator", "name_interpreter", "device_brand", "device_model", "dT.exam_to_prelim", "dT.exam_to_final")
kORDER_COLS <- c("order_date", "order_wday", "order_time", "order_priority", "dT.ordered_to_exam")
kTECH_COLS <- c("projection_set", "procedure_radiation")

kTARGETS <- list(kPATH_COLS, kPT_COLS, kHOSP_COLS, kORDER_COLS, kTECH_COLS)
names(kTARGETS) <- c("path", "pt", "hosp", "order", "tech")


target.info <- data.frame(
    target = unlist(kTARGETS),
    target_mode = kTARGETS %>%
        map_int(.f=len) %>%
        rep.int(names(kTARGETS), .)
)
target.info %>% knitr::kable(row.names = FALSE)


# Refactor to higher level groupings
target.info$target_mode %<>% fct_collapse(Disease = "path", Patient = "pt", `Hospital`=c("hosp", "order", "tech"))

saveRDS(object = target.info, file = "inst/info/targets.rds")
