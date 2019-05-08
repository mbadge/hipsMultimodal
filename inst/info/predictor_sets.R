hipsOpt(predictor_sets)

IMG_VARS <- str_c("PC", 1:10)
PT_VARS <- hipsOpt(pt_vars)
HP_PRED <- hipsOpt(hp_pred)

predictor_lst <- list(
    img = IMG_VARS,
    pt = PT_VARS,
    hp = HP_PRED,
    ptHp = c(PT_VARS, HP_PRED),
    imgPt = c(IMG_VARS, PT_VARS),
    imgHp = c(IMG_VARS, HP_PRED),
    imgPtHp = c(IMG_VARS, PT_VARS, HP_PRED)
)

saveRDS(predictor_lst, file = "inst/info/predictor_sets.rds")
