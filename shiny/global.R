library(shiny)
library(hips)
library(MyUtils)


data("scalars", package = "hips")


# Env
options(shiny.reactlog=TRUE)  # allows reactivity inspection in browser with Ctrl-F3


# PARAMS ----
kDIR_RES_OUT <- FS_hipsDir() %>% file.path("shiny/res")


# Main ----
set.seed(66)

sample_df <- scalars %>%
    # Since report text is the same for each image from the same patient, only take the first image entry
    group_by(pt) %>%
    mutate(img_rank = rank(img)) %>%
    filter(img_rank == 1) %>%
    ungroup() %>%
    # Then sample collect 50 fractures and 50 non-fractures
    group_by(fx) %>%
    sample_n(size = 50) %>%
    ungroup() %>%
    # Filter columns
    select(img, `Report Text`)

rm(scalars)
