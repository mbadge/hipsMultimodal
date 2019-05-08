# Compute the principal components of bottleneck features, saving the output, views, and fit objects


library(AnalysisToolkit)
library(tibble)
devtools::load_all()

# FLAGS ----
# Input
btlncks_byCnn <- readRDS(file="inst/bulky_data/btlncks_byCnn.rds")
# Output
PRCOMP_PTH <- function(cnn) {file.path("inst", "bulky_data", str_c(cnn, "_pca.rda"))}  # store pc rotation by cnn
PC_PTH <- file.path("inst", "bulky_data", "btlnck_pcs.rds")
GG_PTH <- file.path("inst", "bulky_data", "btlnck_pc_cve_scat.svg")
# Fit params
kN_FIT <- 1000  # How many samples to fit model on
kFIT_PREPROC_SCALE <- TRUE
kFIT_RANK <- 10

# Functions ----
FIT_PCA <- partial(AnalysisToolkit::fit_pca, n_samples = kN_FIT,
                   scale.=kFIT_PREPROC_SCALE, rank=kFIT_RANK, id_coln="original_id")
PREDICT_PCA <- partial(AnalysisToolkit::predict_pca, id_coln="original_id")


# MAIN ----
pc_fit_lst <- btlncks_byCnn %>%
    split(.$cnn) %>%
    map(FIT_PCA)

pcs_byCnn <- btlncks_byCnn %>%
    split(.$cnn) %>%
    map(select, -cnn) %>%
    map2_dfr(pc_fit_lst, ., PREDICT_PCA, .id="cnn")

# Views
gg_cve <- map_dfr(pc_fit_lst, gg_data_pcaCve, .id = "cnn") %>%
    ggplot(., aes(x=x, y=y, col=fct_reorder2(cnn, x, y))) +
        geom_point() +
        geom_line() +
        gg_style_pcaCve +
        scale_color_cnn() +
        theme(legend.position="bottom", legend.direction = "vertical") +
        labs(x = "Number of\nPrincipal Components")

gg_scat <- map_dfr(pc_fit_lst, gg_data_pcaScatter, .id = "cnn") %>%
    mutate(cnn = as.factor(cnn)) %>%
    group_by(cnn) %>%
    mutate_at(.vars = vars(x, y), ~ (.x - min(.x)) / (max(.x) - min(.x))) %>%
    ungroup() %>%
    mutate(cnn = as.factor(cnn) %>% fct_relevel("rnd", "pre")) %>%
    {
        ggplot(., aes(x=x, y=y, col=cnn)) +
            facet_wrap(~ cnn, nrow=1, labeller = as_labeller(AES_CNN_LABELS)) +
            gg_style_pcaScatter +
            scale_color_cnn() +
            coord_cartesian(xlim=c(0, 1), ylim=c(0, 1), expand=FALSE) +
            geom_point(alpha=0.1) +
            theme(legend.position="none")
    }

gg <- cowplot::plot_grid(gg_cve, gg_scat, nrow = 2, rel_heights = c(0.6, 0.4))

# Save
iwalk(pc_fit_lst, ~saveRDS(object = .x, file = PRCOMP_PTH(.y)))
saveRDS(object = pcs_byCnn, PC_PTH)
cowplot::ggsave(gg, filename = GG_PTH, width = 3.51, height = 5.5, units = "in")



########################### Retroactive figure gen ####################################
prePC <- readRDS("analysis/pca/pre_pca.rda")
rndPC <- readRDS("analysis/pca/rnd_pca.rda")
pc_fit_lst <- list("rnd"=rndPC, "pre"=prePC)

gg_cve <- map_dfr(pc_fit_lst, gg_data_pcaCve, .id = "cnn") %>%
    ggplot(., aes(x=x, y=y, col=fct_reorder2(cnn, x, y))) +
    geom_point() +
    geom_line() +
    gg_style_pcaCve +
    scale_color_cnn() +
    theme(legend.position="bottom", legend.direction = "vertical") +
    labs(x = "Number of\nPrincipal Components")

gg_scat <- map_dfr(pc_fit_lst, gg_data_pcaScatter, .id = "cnn") %>%
    mutate(cnn = as.factor(cnn)) %>%
    group_by(cnn) %>%
    mutate_at(.vars = vars(x, y), ~ (.x - min(.x)) / (max(.x) - min(.x))) %>%
    ungroup() %>%
    mutate(cnn = as.factor(cnn) %>% fct_relevel("rnd", "pre")) %>%
    {
        ggplot(., aes(x=x, y=y, col=cnn)) +
            facet_wrap(~ cnn, nrow=1, labeller = as_labeller(AES_CNN_LABELS)) +
            gg_style_pcaScatter +
            scale_color_cnn() +
            coord_cartesian(xlim=c(0, 1), ylim=c(0, 1), expand=FALSE) +
            geom_point(alpha=0.1) +
            theme(legend.position="none")
    }

gg <- cowplot::plot_grid(gg_cve, gg_scat, nrow = 2, rel_heights = c(0.6, 0.4))
cowplot::ggsave(gg, filename = "analysis/pca/btlnck_pc_cve_scat.png", width = 3.51, height = 5.5, units = "in")
