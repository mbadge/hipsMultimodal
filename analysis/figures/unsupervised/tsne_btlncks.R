# # View PC further dimensionality reductions colored by covariates
# library(Rtsne)
# library(vizR)
devtools::load_all()
#
# # FLAGS ----
# FP_BTLNCKS_IN <- system.file("bulky_data", "btlncks_byCnn.rds", package = "hips", mustWork = TRUE)
# kPILOT <- FALSE
kSUBSET_COMMON_DEVICES <- TRUE  # Only show devices with >500 image examples
#

# Constants ----
# Aesthetics
AES_FX_ALPHA <- c("TRUE" = 0.5,
                  "FALSE" = 0.05)
if(interactive()) {pal_alpha(AES_FX_ALPHA)}



# MAIN ----
bt <- hipsCohort()  # Scalars
btlncks_byCnn <- readRDS(FP_BTLNCKS_IN)  # All features

# Only use examples from the most common devices
if (kSUBSET_COMMON_DEVICES) {
    bt <- local({
        # Which devices have >500 samples?
        devices_w_500_egs <- bt %>%
            group_by(device_model) %>%
            summarise(n_per_device = n()) %>%
            filter(n_per_device > 500) %>%
            use_series(device_model)

        bt %>%
            filter(device_model %in% devices_w_500_egs)
    })
}

# if (kPILOT) {
#     bt %<>% sample_n(500)
# }
#
#
# Unravel and reshape image-major df into an image-btlnck major df
AES_DEVICE_ALPHA <- bt %>%
    droplevels() %>%
    gather(key = "cnn", value = "PCs", one_of(hipsOpt(btlncks))) %>%
    unnest(PCs) %>%
    group_by(device_model) %>%
    summarise(n = n()) %>%
    tibble::deframe()
AES_DEVICE_ALPHA <- 1 - (AES_DEVICE_ALPHA / sum(AES_DEVICE_ALPHA))
AES_DEVICE_ALPHA <- AES_DEVICE_ALPHA - min(AES_DEVICE_ALPHA)/2
vizR::pal_alpha(AES_DEVICE_ALPHA)
#
#
# # Join tables before computing tsne to filter to only relevant images
# btlnck_ids <- btlncks_byCnn$original_id %>% unique
# bt_ids <- bt$img %>% unique
# lift_ld(Venn)(btlnck_ids, bt_ids)
#
# btlncks_byCnn %<>% filter(original_id %in% bt_ids)
# btlncks_byCnn$cnn %<>% fct_drop()
# btlncks_byCnn$cnn %<>% as.factor() %>% fct_relevel("rnd", after = 0L)
#
# # Check for duplicated rows
# # Dupd rows will cause tsne optimization to fail
# mtxs <- btlncks_byCnn %>%
#     split(.$cnn) %>%
#     map(select, starts_with("btlnck"))
#
# is_dupd <- map(mtxs, duplicated)
# # Remove duplicated btlnck entries from separated mtx and identifiers
# mtxs <- map2(mtxs, is_dupd, ~.x[!.y, ])
# ids <- btlncks_byCnn %>% split(.$cnn) %>% map("original_id") %>% map2(is_dupd, ~.x[!.y])
#
# # tsne ----
# tsne_byCnn <- map(mtxs, Rtsne) %>% map("Y")
# tsne_byCnn %<>% map2(ids, `rownames<-`)
#
# tsne_dfs <- tsne_byCnn %>%
#     map(as.df) %>%
#     map(tibble::rownames_to_column, var="id")
# tsne_df <- lift_dl(bind_rows, .id = "cnn")(tsne_dfs)
# names(tsne_df) <- c("cnn", "id", "tsne1", "tsne2")
# tsne_df$cnn %<>% as.factor() %>% fct_relevel("rnd", after = 0L)
#
#
# dat2 <- inner_join(tsne_df, bt %>% keep(.p = is_atomic), by=c("id"="img"))

# save(dat2, file = "analysis/figures/unsupervised/tsne_data.Rdata")
load(file = "analysis/figures/unsupervised/tsne_data.Rdata")

gg_tsne <- function(COL, DATA = dat2) {
    ggplot(DATA, aes_(x=~tsne1, y=~tsne2, col=substitute(COL))) +
        geom_point(size=1) +
        scale_x_continuous(breaks=NULL) +
        scale_y_continuous(breaks=NULL) +
        coord_equal() +
        facet_cnn(scales = "fixed")
}

ga <- gg_tsne(device_model) +
    aes(alpha = device_model) +
    scale_alpha_manual(values = AES_DEVICE_ALPHA * 0.2, guide=FALSE) +
    scale_color_device()
gb <- gg_tsne(fx) +
    aes(alpha = fx) +
    scale_alpha_manual(values = AES_FX_ALPHA, guide=FALSE) +
    scale_color_fx()
gc <- gg_tsne(projection_set, DATA = dat2 %>% filter(!is.na(projection_set))) +
    scale_color_view() +
    aes(alpha = projection_set) +
    scale_alpha_manual(values = rep(0.1, 3), guide=FALSE)


# Remove tsne axis titles
ga %<>% `+`(theme(axis.title = element_blank(), legend.justification = "center"))
gb %<>% `+`(theme(axis.title = element_blank(), legend.justification = "center"))
gc %<>% `+`(theme(axis.title = element_blank(), legend.justification = "center"))
# Remove the facet header from all but row 1
gbs <- gb %+% theme(strip.text = element_blank())
gcs <- gc %+% theme(strip.text = element_blank())

GG_TSNE <- cowplot::plot_grid(ga, gcs, gbs, nrow=3,
                              align = "hv", axis="lr")
GG_TSNE

ggsave(filename = "analysis/figures/unsupervised/tsne_device_fx2.png", plot = GG_TSNE, width = 7.3, height = 5)
