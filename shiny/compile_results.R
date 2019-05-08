kDIR_ANN_IN <- "shiny/res"

data("scalars", package = "hips")

ann_df <- list.files(kDIR_ANN_IN, pattern = "*.csv", full.names = TRUE) %>%
    map(Read) %>%
    bind_rows()
ann_df %<>% mapnames(from = "fx", to = "fx_manual")

DF <- left_join(ann_df, scalars, by = c("img_id" = "img"))
DF %<>% mapnames(from = "fx", to = "fx_nlp")

table(DF$fx_manual, DF$fx_nlp)

# Y
DF$fx_manual %<>% as_factor %>% fct_recode(F = "N", F = "No mention", T = "Y") %>% as.logical()
# Y_
DF$fx_nlp

AnalysisToolkit::ClassifierPoint(Y_ = DF$fx_nlp, Y = DF$fx_manual, id = DF$img_id)

AnalysisToolkit::ClassifierPoint(Y_ = DF$fx_nlp, Y = DF$fx_manual, id = DF$img_id) %>%
    AnalysisToolkit::glance_pretty() %>%
    Write("shiny/nlp_stats.csv")

Write(x = DF, "shiny/ann_df.csv")


ann_df$prior_imaging %>% table_()
