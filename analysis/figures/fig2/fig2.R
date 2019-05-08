# Create composite Figure 2 with supervised ML results
library(hips)

kDIR_IN_ML <- Fp_ml_dir()

# Classification / Multitarget ----
kFP_IN_MULTITARGET <- file.path(kDIR_IN_ML, "multitarget_vanilla", "roc_plt_data.rds")
multitarg_roc_df <- readRDS(kFP_IN_MULTITARGET)


gg_roc <- ggplot(multitarg_roc_df,
                 aes(x = fct_reorder(target, auc),
                     y = auc,
                     ymin = auc_lower,
                     ymax = auc_upper)) +
    geom_errorbar(width = 0.25, position = Pd()) +
    geom_point(position = Pd()) #+
    # geom_text(aes(label = is_sig), nudge_x = 0.2)
gg_roc <- gg_roc +
    geom_hline(yintercept = 0.5, alpha = 0.5, linetype = 2) +
    geom_hline(yintercept = 1, alpha = 0.5) +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_x_target() +
    labs(y = "AUROC +/- 95% bootstrap CI") +
    theme(strip.text.y = element_text(angle = 0),
          axis.text.x = element_text(vjust = 0.5)) +
    scale_y_continuous(breaks = c(0.5, 0.75, 1), labels = c(0.5, 0.75, 1)) +
    theme(legend.position = "bottom", legend.direction = "vertical")
GG_ROC <- gg_roc %+%
    coord_flip() +
    theme(plot.title = element_text(hjust = 1))
GG_CLASS <- GG_ROC + aes(col = target_mode) + scale_color_targetMode()


# Regression ----
kFP_IN_REGRESSION <- file.path(kDIR_IN_ML, "lm", "lm_inf_tbl.rds")
regress_inf_df <- readRDS(kFP_IN_REGRESSION)

# Remove dT values > 1d
excess_time_df <- regress_inf_df %>%
    filter(model == "dT.exam_to_final", Y > 24*60)
regress_inf_df <- setdiff(regress_inf_df, excess_time_df)
excess_time_df <- regress_inf_df %>%
    filter(model == "dT.exam_to_prelim", Y > 24*60)
regress_inf_df <- setdiff(regress_inf_df, excess_time_df)
excess_time_df <- regress_inf_df %>%
    filter(model == "dT.ordered_to_exam", Y > 24*60)
regress_inf_df <- setdiff(regress_inf_df, excess_time_df)


# Change units on time measures to hours
regress_inf_df[regress_inf_df$model == "order_time", ] %<>%
    mutate(Y = Y/60/60,
           Y_ = Y_/60/60)
regress_inf_df[regress_inf_df$model == "dT.exam_to_final", ] %<>%
    mutate(Y = Y/60,
           Y_ = Y_/60)
regress_inf_df[regress_inf_df$model == "dT.exam_to_prelim", ] %<>%
    mutate(Y = Y/60,
           Y_ = Y_/60)
regress_inf_df[regress_inf_df$model == "dT.ordered_to_exam", ] %<>%
    mutate(Y = Y/60,
           Y_ = Y_/60)
# Change study date to year
regress_inf_df[regress_inf_df$model == "order_date", ] %<>%
    mutate(Y = Y/365,
           Y_ = Y_/365)


# facet grouping --
AES_TARGET_REG_LBLS <- c(
    "age" = "Age (yr)",
    "bmi" = "BMI (kg/m2)",
    "order_date" = "Study Year",
    "order_time" = "Order Time (hr)",
    "procedure_radiation" = "Radiation\nDose (uAs)",
    "dT.exam_to_final" = "Time to Final\nInterp. (hr)",
    "dT.exam_to_prelim" = "Time to Initial\nInterp. (hr)",
    "dT.ordered_to_exam" = "Imaging\nWait Time (hr)"
)
facet_target <- function(facets = ~ model, scales = "free", as.table = FALSE,
                      labeller = ggplot2::as_labeller(AES_TARGET_REG_LBLS), ...)
{
    ggplot2::facet_wrap(facets = facets, scales = scales, as.table = as.table,
                        labeller = labeller, ...)
}

GG_REG <- ggplot(regress_inf_df, aes(x = Y_, y=Y)) +
    geom_point(alpha = 0.2) +
    geom_smooth(se = FALSE, method = "lm", col = "purple") +
    facet_target(ncol = 2, as.table = TRUE) +
    labs(x = "Predicted", y = "Actual")

# # cowplot grouping --
# reg_view <- function(DAT) {
#     ggplot(DAT, aes(x=Y_, y=Y)) +
#         geom_point() +
#         geom_smooth(se = FALSE, method="lm") +
# #        coord_fixed(ratio = 1, clip = "off") +
#         theme(axis.title = element_blank())
# }
# regress_inf_df %>%
#     split(.$model) %>%
#     map(reg_view) %>%
#     lift_dl(cowplot::plot_grid, ncol = 2, align = 'hv', axis = "b")()


# Multimodal ----
kFP_IN_MULTIMODAL <- file.path(kDIR_IN_ML, "multimodal", "plot_data.Rdata")
load(kFP_IN_MULTIMODAL)

gg_roc_sum <- ggplot(PLT_DAT_ROC_SUM,
                 aes(x = fct_reorder(Classifier, auc),
                     y = auc,
                     ymin = auc_lower,
                     ymax = auc_upper,
                     col = Classifier)) +
    geom_errorbar(width = 0.25, position = Pd()) +
    geom_point(position = Pd()) +
    scale_predictor(color)  # +
    # geom_text(aes(label = is_sig), nudge_x = 0.2)
gg_roc_sum <- gg_roc_sum +
    geom_hline(yintercept = 0.5, alpha = 0.5, linetype = 2) +
    geom_hline(yintercept = 1, alpha = 0.5) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(y = "AUROC +/- 95% bootstrap CI",
         x = "Predictor Set") +
    theme(strip.text.y = element_text(angle = 0),
          axis.text.x = element_text(vjust = 0.5)) +
    scale_y_continuous(breaks = c(0.5, 0.75, 1), labels = c(0.5, 0.75, 1)) +
    theme(legend.position = "bottom", legend.direction = "vertical")
GG_ROC_SUM <- gg_roc_sum %+%
    coord_flip()
GG_ROC_SUM



gg_roc <- ggplot2::ggplot(PLT_DAT_ROC$lines, ggplot2::aes(x=x, y=y, col = Classifier)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = PLT_DAT_ROC$points, shape=10, size = 3) +
    ggplot2::geom_rug(data = PLT_DAT_ROC$points) +
    # ggplot2::geom_text(data = PLT_DAT_ROC$points, aes(label = is_sig), size = 5, nudge_y = 0.03, show.legend=FALSE) +
    AnalysisToolkit::gg_style_roc +
    scale_predictor(color)
gg_roc

gg_prc <- gg_prc <- ggplot2::ggplot(PLT_DAT_PRC$lines, ggplot2::aes(x=x, y=y, col=Classifier)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = PLT_DAT_PRC$points, shape=10, size = 3) +
    ggplot2::geom_rug(data = PLT_DAT_PRC$points) +
    AnalysisToolkit::gg_style_prc +
    scale_predictor(color)
gg_prc


# Agregate ----
# Put classification title shifted to left (under y labels)
GG_CLASS %<>% `+`(theme(axis.title.x = element_text(hjust = 1)))


GG_ROC_SUM %<>% `+`(guides(color = FALSE))
GG_ROC_SUM %<>% `+`(theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()))
GG_ROC_SUM

gg_roc %<>% `+`(theme(plot.title = element_blank(), legend.position = "none"))

gg_prc %<>% `+`(theme(plot.title = element_blank(), legend.position = "bottom", legend.direction = "vertical"))
gg_prc %<>% `+`(guides(color = guide_legend(ncol = 3, title.hjust = 0.5)))
LEG <- cowplot::get_legend(gg_prc)
gg_prc %<>% `+`(theme(legend.position = "none"))

GG_MULTIMODAL <- cowplot::plot_grid(gg_roc, GG_ROC_SUM, gg_prc, LEG, ncol = 1, align = 'h', axis = "lr", rel_heights = c(1, 0.7, 1, 0.5))
GG_MULTIMODAL



##### FULL AGGREGATE ----
`G!G` <- cowplot::plot_grid(GG_CLASS, GG_REG, GG_MULTIMODAL, ncol = 3, labels = letters[1:3], rel_widths = c(1, 1, 0.8))
`G!G`

#cowplot::save_plot(plot = `G!G`, filename = "analysis/figures/fig2/composite2.svg", base_height = 6, base_width = 12)
#cowplot::save_plot(plot = `G!G`, filename = "analysis/figures/fig2/composite2.tiff", base_height = 6, base_width = 12)
#cowplot::save_plot(plot = `G!G`, filename = "analysis/figures/fig2/composite2.eps", base_height = 6, base_width = 12)
cowplot::save_plot(plot = `G!G`, filename = "analysis/figures/fig2/composite2.pdf", base_height = 6, base_width = 12)

`G!G` <- cowplot::plot_grid(GG_CLASS, GG_REG, GG_MULTIMODAL, ncol = 3, labels = letters[1:3], rel_widths = c(1, 1, 1))
`G!G`
# cowplot::save_plot(plot = `G!G`, filename = "analysis/figures/fig2/composite2_10.tiff", base_height = 6, base_width = 10)
#cowplot::save_plot(plot = `G!G`, filename = "analysis/figures/fig2/composite2.png", base_height = 6, base_width = 12)


#! NDM recs figs 100-300kb http://mts-npjdigitalmed.nature.com/cgi-bin/main.plex?form_type=display_auth_instructions
#! The above tiff is 16Mb
# cowplot::save_plot(plot = `G!G`, filename = "analysis/figures/fig2/composite2_10_72.tiff", base_height = 6, base_width = 10, dpi = 72)
# cowplot::save_plot(plot = `G!G`, filename = "analysis/figures/fig2/composite2_72.tiff", base_height = 6, base_width = 12, dpi = 72)
#! ^ these 72 dpi images are just over 1MB, still 2x but much closer
