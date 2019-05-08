devtools::load_all()


# ---- Load binarized datasets ----
# MSH ----
binaryBt <- hipsCohort(binary)
binaryBt$procedure_overRadiation <- NULL
binaryAdl <- readRDS("analysis/cohorts/binaryAdlTest.rds")

# ADL ----
adl_cohorts <- adlCohort(binarized=TRUE)

# ---- Full Cross-Sectional Dataset ----
# MSH ----
ors <- OddsRatios(binaryBt)
gg_pool <- ggOddsRatios(ors)

# ADL ----
adl_cohorts$crossSectional %>%
    OddsRatios() %>%
    ggOddsRatios()


# ---- Adl by cohort ----
adl_cohorts %>%
    OddsRatios(target_chr = "fx", grp_chr = "cohort") %>%
    ggOddsRatios() %+%
    aes_cohort_col()


# ---- Cross-Sectional Dataset By device ----
# Get all levels of device_model from full bt --
Bt <- hipsCohort()
# Confirm perfect overlap
list(binaryBt, Bt) %>% map("img") %>% (lift_dl(all.equal)) %>% stopifnot()

Bt$device_model %>% table_
binaryBt$device_model %>% table_
# only keep full scanner model

# DF contains the full device_model listing with all other variables binarized
DF <- bind_cols(Bt["device_model"], binaryBt)

# Include only the most frequently used scanners
DF$device_model %<>% fct_lump(n = 4) %>% fct_collapse(`NULL` = "x0862")
DF[is.na(DF$device_model), "device_model"] <- "Other"

# Split by device model, remove this col
DFs <- DF %>%
    split(f = .$device_model) %>%
    map(select, -device_model)

ors_byDevice <- OddsRatios(DFs)

# Remove crazy extremes
ors_byDevice %<>%
    filter(y != Inf) %>%
    filter(y != 0)

# Complete plot df for proper alignment
ors_byDevice$target %<>% fct_expand("device_model")
ors_byDevice %<>% complete(device_model, target)
class(ors_byDevice) <- c("OddsRatios", class(ors_byDevice))

gg_byDevice <- ggOddsRatios(ors_byDevice) + aes_device_col()


# Collate
target_order <- ors$target %>% fct_reorder(ors$y) %>% levels
ors$target %<>% fct_relevel(target_order)
ors_byDevice$target %<>% fct_relevel(target_order)

gg_byDevice <- ggOddsRatios(ors_byDevice) + aes_device_col()
gg_pool <- ggOddsRatios(ors)

library(cowplot)
gg_byDevice %<>% `+`(theme(axis.text.y = element_blank(),
                      axis.title.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.title.x = element_blank()))
gg_pool %<>% `+`(theme(axis.title.x = element_text(hjust=0)))

GG <- plot_grid(gg_pool, gg_byDevice, ncol = 2, align = 'h', rel_widths = c(1, 1))
GG

#cowplot::save_plot("analysis/figures/oddsRatios/overall_byDevice.png", base_width = 8, base_height = 6, GG)
cowplot::save_plot("analysis/figures/oddsRatios/overall_byDevice.tiff", base_width = 8, base_height = 6, GG)
#cowplot::save_plot("analysis/figures/oddsRatios/overall_byDevice.svg", base_width = 8, base_height = 6, GG)
