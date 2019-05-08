# Create composite Figure 3 with Matched Cohort Analysis
library(hips)

# Load Data ----
load(file.path(Fp_ml_dir(), "by_cohort", "plot_data.Rdata"))  # Perf tables
load(file.path(Fp_ml_dir(), "by_cohort", "by_test_cohorts.Rdata"))  # Test cohort tables (for OR computation)


# ROC Summary ----
gg_roc_sum <- ggplot(PLT_DAT_ROC_SUM,
                     aes(x = cohort,
                         y = auc,
                         ymin = auc_lower,
                         ymax = auc_upper,
                         col = cohort)) +
    geom_errorbar(width = 0.25, position = Pd()) +
    geom_point(position = Pd()) +
    geom_text(aes(label = is_sig), nudge_x = 0.2) +
    aes_cohort_col() +
    scale_x_cohort() +
    geom_hline(yintercept = 0.5, alpha = 0.5, linetype = 2) +
    geom_hline(yintercept = 1, alpha = 0.5) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(y = "AUROC") +
    theme(axis.text.x = element_text(vjust = 0.5)) +
    scale_y_continuous(breaks = c(0.5, 0.75, 1), labels = c(0.5, 0.75, 1)) +
    theme(legend.position = "bottom", legend.direction = "vertical") +
    coord_flip()
GG_ROC_SUM <- gg_roc_sum


# ROC ----
PLT_DAT_ROC$lines$cohort %<>% as.factor %>% fct_relevel(hipsOpt(cohorts)) %>% fct_rev()
gg_roc <- ggplot2::ggplot(PLT_DAT_ROC$lines, ggplot2::aes(x=x, y=y, col = cohort)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = PLT_DAT_ROC$points, shape=10, size = 3) +
    ggplot2::geom_rug(data = PLT_DAT_ROC$points) +
    ggplot2::geom_text(data = PLT_DAT_ROC$points, aes(label = is_sig), size = 5, nudge_y = 0.03, show.legend=FALSE) +
    AnalysisToolkit::gg_style_roc +
    aes_cohort_col()
GG_ROC <- gg_roc

# PRC ----
gg_prc <- gg_prc <- ggplot2::ggplot(PLT_DAT_PRC$lines, ggplot2::aes(x=x, y=y, col=cohort)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = PLT_DAT_PRC$points, shape=10, size = 3) +
    ggplot2::geom_rug(data = PLT_DAT_PRC$points) +
    AnalysisToolkit::gg_style_prc +
    aes_cohort_col()
GG_PRC <- gg_prc

# ORs ----
cohort_imgs <- map(test_cohorts, "img")
mshBinary <- hipsCohort(mutating = binary)
mshBinCohorts <- map(cohort_imgs, ~filter(mshBinary, img %in% .x))


OR_DATA <- mshBinCohorts %>%
    OddsRatios(grp_chr = "cohort")

#!#! Artificially cap extremes in the data so that ggplot will incorrectly display them the way I want
OR_DATA$y %>% max()
OR_DATA$ymax %<>% map_dbl(~min(., 50))

OR_DATA$y %>% min()
OR_DATA$ymin %>% map_dbl(~max(., 0.1))
OR_DATA$ymin %<>% map_dbl(~max(., 0.1))

# Order factors
OR_DATA$cohort %<>% as.factor %>% fct_relevel(hipsOpt(cohorts)) %>% fct_rev()
ordered_targets <- OR_DATA %>%
    group_by(target) %>%
    summarise(or = mean(y)) %>%
    arrange(desc(or)) %$%
    target
OR_DATA$target %<>% as.factor() %>% fct_relevel(ordered_targets) %>% fct_rev()


GG_OR <- OR_DATA %>%
    ggOddsRatios() %+%
    aes_cohort_col()

# Tabulate N Sig ----
OR_DATA %>%
    mutate(is_sig = map_lgl(p.value, ~ .x < 0.05)) %>%
    group_by(cohort) %>%
    filter(is_sig) %>%
    summarise(n_sig = n())

# Composite ----
GG_ROC %<>% `+`(theme(legend.position = "bottom", legend.title.align = 0.5, legend.direction = "vertical"))
GG_ROC %<>% `+`(guides(col = guide_legend(reverse = TRUE, ncol = 2)))
GG_LEG <- cowplot::get_legend(GG_ROC)

GG_ROC_SUM %<>% `+`(theme(legend.position = "none"))
GG_ROC_SUM2 <- GG_ROC_SUM + theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank())
GG_ROC_SUM3 <- GG_ROC_SUM2 + theme(axis.title.y = element_blank())
GG_ROC %<>% `+`(theme(legend.position = "none", plot.title = element_blank()))
GG_PRC %<>% `+`(theme(legend.position = "none", plot.title = element_blank()))
GG_OR %<>% `+`(theme(legend.position = "none"))


#GG_PERFS <- cowplot::plot_grid(GG_ROC, GG_ROC_SUM, GG_PRC, ncol = 1, align = 'v')
#GG_PERFS <- cowplot::plot_grid(GG_ROC, GG_ROC_SUM2, GG_PRC, GG_LEG, ncol = 1, rel_heights = c(1, 0.6, 1, 0.4))
GG_PERFS <- cowplot::plot_grid(GG_ROC, GG_ROC_SUM3, GG_PRC, align = 'v', ncol = 1, rel_heights = c(2, 0.7, 1), labels = letters[2:4])
# GG_VIEWS <- cowplot::plot_grid(GG_OR, GG_PERFS, ncol = 2, labels = c("a", NULL), rel_widths = c(1, 0.6))
GG_LEG_PAN <- cowplot::plot_grid(NULL, GG_LEG, NULL, ncol = 3, rel_widths = c(2, 0.1, 1))
# GG <- cowplot::plot_grid(GG_VIEWS, GG_LEG_PAN, ncol = 1, rel_heights = c(1, 0.2))
# GG

GG_LEFT <- cowplot::plot_grid(GG_OR, GG_LEG_PAN, ncol = 1, rel_heights = c(1, 0.15), labels = c("a", NULL))
GG <- cowplot::plot_grid(GG_LEFT, GG_PERFS, ncol = 2, rel_widths = c(1, 0.8))
GG

# ggsave(plot = GG, filename = "analysis/figures/fig3/fig3.png", width = 8, height = 8)
#ggsave(plot = GG, filename = "analysis/figures/fig3/fig3_2.tiff", width = 8, height = 8)
ggsave(plot = GG, filename = "analysis/figures/fig3/fig3_2.pdf", width = 8, height = 8)
# ggsave(plot = GG, filename = "analysis/figures/fig3/fig3.svg", width = 8, height = 8)


#! Per NDM rec, make lower res
#! http://mts-npjdigitalmed.nature.com/cgi-bin/main.plex?form_type=display_auth_instructions
#ggsave(plot = GG, filename = "analysis/figures/fig3/fig3_2_72.tiff", width = 8, height = 8, dpi = 72)
