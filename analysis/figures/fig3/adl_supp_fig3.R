# Create composite Figure 3 with Matched Cohort Analysis
library(hips)

# Load Data ----
load(file.path(Fp_ml_dir(), "by_cohort", "adl_plot_data.Rdata"))  # Perf tables


# ROC Summary ----
gg_roc_sum <- ggplot(PLT_DAT_ADL_ROC_SUM,
                     aes(x = cohort,
                         y = auc,
                         ymin = auc_lower,
                         ymax = auc_upper,
                         col = cohort)) +
    geom_errorbar(width = 0.25, position = Pd()) +
    geom_point(position = Pd()) +
    geom_text(aes(label = is_sig), nudge_x = 0.2) +
    aes_cohort_col() +
    scale_x_cohort()
gg_roc_sum <- gg_roc_sum +
    geom_hline(yintercept = 0.5, alpha = 0.5, linetype = 2) +
    geom_hline(yintercept = 1, alpha = 0.5) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(y = "AUROC +/- \n95% bootstrap CI") +
    theme(axis.text.x = element_text(vjust = 0.5)) +
    scale_y_continuous(breaks = c(0.5, 0.75, 1), labels = c(0.5, 0.75, 1)) +
    theme(legend.position = "bottom", legend.direction = "vertical")
GG_ROC_SUM <- gg_roc_sum %+%
    coord_flip()
GG_ROC_SUM


# ROC ----
gg_roc <- ggplot2::ggplot(PLT_DAT_ADL_ROC$lines, ggplot2::aes(x=x, y=y, col = cohort)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = PLT_DAT_ADL_ROC$points, shape=10, size = 3) +
    ggplot2::geom_rug(data = PLT_DAT_ADL_ROC$points) +
    ggplot2::geom_text(data = PLT_DAT_ADL_ROC$points, aes(label = is_sig), size = 5, nudge_y = 0.03, show.legend=FALSE) +
    AnalysisToolkit::gg_style_roc +
    aes_cohort_col()
GG_ROC <- gg_roc

# PRC ----
gg_prc <- gg_prc <- ggplot2::ggplot(PLT_DAT_ADL_PRC$lines, ggplot2::aes(x=x, y=y, col=cohort)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = PLT_DAT_ADL_PRC$points, shape=10, size = 3) +
    ggplot2::geom_rug(data = PLT_DAT_ADL_PRC$points) +
    AnalysisToolkit::gg_style_prc +
    aes_cohort_col()
GG_PRC <- gg_prc

# ORs ----
GG_OR <- ADL_OR_DAT %>%
    ggOddsRatios() %+%
    aes_cohort_col()

# Composite ----
GG_ROC %<>% `+`(theme(legend.position = "bottom", legend.title.align = 0.5, legend.direction = "vertical"))
GG_ROC %<>% `+`(guides(col = guide_legend(reverse = TRUE, ncol = 2)))
GG_LEG <- cowplot::get_legend(GG_ROC)

GG_ROC_SUM %<>% `+`(theme(legend.position = "none"))
GG_ROC_SUM2 <- GG_ROC_SUM + theme(legend.position = "none", axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank(), axis.title.y = element_blank())
GG_ROC %<>% `+`(theme(legend.position = "none", plot.title = element_blank()))
GG_PRC %<>% `+`(theme(legend.position = "none", plot.title = element_blank()))
GG_OR %<>% `+`(theme(legend.position = "none"))


#GG_PERFS <- cowplot::plot_grid(GG_ROC, GG_ROC_SUM, GG_PRC, ncol = 1, align = 'v')
#GG_PERFS <- cowplot::plot_grid(GG_ROC, GG_ROC_SUM2, GG_PRC, GG_LEG, ncol = 1, rel_heights = c(1, 0.6, 1, 0.4))
GG_PERFS <- cowplot::plot_grid(GG_ROC, GG_ROC_SUM2, GG_PRC, align = 'v', ncol = 1, rel_heights = c(1, 1, 1), labels = letters[2:4])
GG_VIEWS <- cowplot::plot_grid(GG_OR, GG_PERFS, ncol = 2, labels = c("a", NULL), rel_widths = c(1, 0.6))
GG_LEG_PAN <- cowplot::plot_grid(NULL, GG_LEG, NULL, ncol = 3, rel_widths = c(2, 0.1, 2))
GG <- cowplot::plot_grid(GG_VIEWS, GG_LEG_PAN, ncol = 1, rel_heights = c(1, 0.2))
GG

ggsave(plot = GG, filename = "analysis/figures/fig3/adl_supp_fig3.png", width = 6, height = 6)
ggsave(plot = GG, filename = "analysis/figures/fig3/adl_supp_fig3.svg", width = 6, height = 6)
