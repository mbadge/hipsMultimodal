# Create view of tsne from pixel and CNN featurizers and various colorings
#
# See #68

#! Highly duplicated with tsne_btlncks.R and tsne_refinedImg.R

library(Rtsne)
library(vizR)
devtools::load_all()

# FLAGS ----
kPILOT <- TRUE
kPILOT_N <- 500
kSUBSET_COMMON_DEVICES <- TRUE  # Only show devices with >500 image examples

# Input Data
FP_IN_BTLNCKS <- system.file("bulky_data", "btlncks_byCnn.rds", package = "hips", mustWork = TRUE)
DIR_IN_IMGS <- hipsOpt(dir_img_proc)

# MAIN ----
# Define Cohort ----
# Use the scalars table to select pilot img_ids
bt <- hipsCohort()  # Scalars

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
if (kPILOT) {
    bt %<>% sample_n(kPILOT_N)
    pilot_img_ids <- bt$img
}

# Load Raw Images ----
#FPS_IMG <- list.files(DIR_IN_IMGS, full.names = TRUE)
#! Check whether corresponding image files are available
if (kPILOT) {
    img_lst <- map(pilot_img_ids, readImg)
    img_lst %<>% map(~.x[, , 1])  # Shear images to one of the 3 dupd rgb layers
} else {stop("Not Implemented")}

img_lst <- map(bt$img, ~ readImg(.x)[, , 1])

pixel_lst <- map(img_lst, as.vector)  # Flatten images to 1D vector
pixel_mtx <- lift_dl(rbind)(pixel_lst)


# Load Btlnck Feats ----
btlncks_byCnn <- readRDS(FP_IN_BTLNCKS)
if (kPILOT) {
    btlncks_byCnn %<>%
        filter(original_id %in% pilot_img_ids)
} else {stop("Not Implemented")}

btlnck_df_lst <- btlncks_byCnn %>%
    split(.$cnn) %>%
    map(select, starts_with("btlnck"))
btlnck_imgIds <- btlncks_byCnn %>%
    split(.$cnn) %>%
    map("original_id")

# Validation ----
stopifnot(!any(duplicated(pixel_mtx)))
stopifnot(!any(duplicated(btlnck_df_lst$rnd)))

# T-SNE ----
pixel_tsne <- Rtsne(pixel_mtx)
pixel_tsne %<>% `$`(Y)
rownames(pixel_tsne) <- pilot_img_ids

btlnck_tsnes <- map(btlnck_df_lst, Rtsne)
# What should I cache?  The original object doesn't add value since I'm just using default params
btlnck_tsnes %<>% map("Y")
map2(btlnck_tsnes, btlnck_imgIds, `rownames<-`)

#! Cache
save(pixel_tsne, btlnck_tsnes, file = "cache/tsne_composite.tsnes.Rdata")


# Aggregate plot data ----

# Craft individual tsne feature tables
FEAT_COL_NMS <- c("id", "tsne1", "tsne2")
pixel_tsne %<>% as.df()
pixel_tsne %<>% tibble::add_column(id = pilot_img_ids, .before = 1)
pixel_tsne %<>% `names<-`(FEAT_COL_NMS)

btlnck_tsnes %<>% map(as.df)
btlnck_tsnes %<>%
    map2(btlnck_imgIds, `rownames<-`) %>%
    map(tibble::rownames_to_column, var = "id")
btlnck_tsnes %<>% map(`names<-`, FEAT_COL_NMS)

# Aggregate tsne features
tsnes <- c(list("pixel"=pixel_tsne), btlnck_tsnes)
tsne_df <- lift_dl(bind_rows, .id = "featurizer")(tsnes)

# Add scalar features for coloring
COL_VARS <- c("device_model", "projection_set", "fx")
bt %<>% select(img, one_of(COL_VARS))

plt_df <- tsne_df %>% left_join(bt, by=c("id"="img"))



# Aesthetics ----
plt_df$featurizer %<>% as.factor() %>% fct_relevel("pixel", "rnd", "pre")

AES_FX_ALPHA <- c("TRUE" = 0.5,
                  "FALSE" = 0.2)
if(interactive()) {pal_alpha(AES_FX_ALPHA)}

kALPHA_DEV_SCALE_FACTOR <- 4  # higher values -> less transparent
kALPHA_PROJ_SCALE_FACTOR <- .6

AES_DEVICE_ALPHA <- local({
    freq_df <- bt %>%
        droplevels() %>%
        group_by(device_model) %>%
        summarise(n = n()) %>%
        tibble::deframe()
    AES_DEVICE_ALPHA <- 1 - (freq_df / sum(freq_df))
    AES_DEVICE_ALPHA <- AES_DEVICE_ALPHA - min(AES_DEVICE_ALPHA)/kALPHA_DEV_SCALE_FACTOR
})
if(interactive()) {vizR::pal_alpha(AES_DEVICE_ALPHA)}

AES_FEATURIZER_LABELS <- c("pixel" = "Image Pixels",
                           "rnd" = "CNN Features\nRandomly Initialized",
                           "pre" = "CNN Features\nPre-Trained")


# View
gg_tsne <- function(DATA, COL) {
    ggplot(DATA, aes_(x=~tsne1, y=~tsne2, col=substitute(COL))) +
        geom_point(size=1) +
        scale_x_continuous(breaks=NULL) +
        scale_y_continuous(breaks=NULL) +
        coord_equal() +
        facet_wrap(facets = ~featurizer, ncol=1, scales = "fixed", labeller=ggplot2::as_labeller(AES_FEATURIZER_LABELS), strip.position = "left") +
        theme(axis.title = element_blank(), legend.justification = "center", legend.position = "bottom", legend.direction = "vertical")
}

ga <- gg_tsne(plt_df, device_model) +
    aes(alpha = device_model) +
    scale_alpha_manual(values = AES_DEVICE_ALPHA * 0.3, guide=FALSE) +
    scale_color_device() +
    theme(strip.text.y = element_text(angle=180))
gb <- gg_tsne(projection_set, DATA = plt_df %>% filter(!is.na(projection_set))) +
    scale_color_view() +
    aes(alpha = projection_set) +
    scale_alpha_manual(values = rep(kALPHA_PROJ_SCALE_FACTOR, 3), guide=FALSE)
gc <- gg_tsne(plt_df, fx) +
    aes(alpha = fx) +
    scale_alpha_manual(values = AES_FX_ALPHA, guide=FALSE) +
    scale_color_fx()

# Remove the facet header from all but row 1
gbs <- gb %+% theme(strip.text = element_blank())
gcs <- gc %+% theme(strip.text = element_blank())

GG_TSNE <- cowplot::plot_grid(ga, gbs, gcs,
                              nrow=1, rel_widths = c(2, 1, 1),
                              axis="lr", align = "h")
GG_TSNE


ggsave(filename = "analysis/figures/unsupervised/tsne_composite.svg", plot = GG_TSNE, width = 7.3, height = 5)
