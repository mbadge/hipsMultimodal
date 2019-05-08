library(hips)
library(Rtsne)

DIR_IMGS <- "/media/marcus/Vulcan/radiology/msh_hip/npy_imgs"

FPS_IMG <- list.files(DIR_IMGS, full.names = TRUE)

# View Single Image
# FP_IMG <- FPS_IMG[1]
# npy <- readNpy(FP_IMG)
# vizR::Viz(npy)

# TSNE 5 images
PILOT_FPS_IMG <- sample(FPS_IMG, size = 91, replace = FALSE)

npy_lst <- map(PILOT_FPS_IMG, readNpy)
img_ids <- PILOT_FPS_IMG %>% basename %>% str_sub(end = -5)

# # Check if all 3 layers of the first image are the same
# img1r <- npy_lst[[1]][, , 1] %>% as.vector()
# img1g <- npy_lst[[1]][, , 2] %>% as.vector()
# img1b <- npy_lst[[1]][, , 3] %>% as.vector()
#
# all(img1r == img1g)
# all(img1r == img1b)
# # - YES!

# So just use the red layer of each image for tsne
npyr_lst <- map(npy_lst, ~.x[, , 1])
pixel_lst <- map(npyr_lst, as.vector)

pixel_mtx <- lift_dl(rbind)(pixel_lst)

stopifnot(!any(duplicated(pixel_mtx)))

tsne <- Rtsne(pixel_mtx)
tsne <- tsne$Y

rownames(tsne) <- img_ids
tsne_df <- tsne %>% as.df %>% tibble::rownames_to_column(var = "img")

names(tsne_df) <- c("img", "tsne1", "tsne2")

# Get metadata
bt <- hipsCohort()
# Set transparency to inverse frequency
AES_DEVICE_ALPHA <- bt %>%
    group_by(device_model) %>%
    summarise(n = n()) %>%
    tibble::deframe()
AES_DEVICE_ALPHA <- 1 - (AES_DEVICE_ALPHA / sum(AES_DEVICE_ALPHA))
AES_DEVICE_ALPHA <- AES_DEVICE_ALPHA - min(AES_DEVICE_ALPHA)/2
vizR::pal_alpha(AES_DEVICE_ALPHA)



dat <- inner_join(tsne_df, bt %>% keep(.p = is_atomic), by=c("img"))

gg_tsne <- function(COL, DATA = dat) {
    ggplot(DATA, aes_(x=~tsne1, y=~tsne2, col=substitute(COL))) +
        geom_point(size=1) +
        scale_x_continuous(breaks=NULL) +
        scale_y_continuous(breaks=NULL) +
        coord_equal()
}

ga <- gg_tsne(device_model)
gb <- gg_tsne(fx)
gc <- gg_tsne(projection_set)


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

#ggsave(filename = "analysis/figures/tsne_device_fx.png", plot = GG_TSNE, width = 7.3, height = 5)
