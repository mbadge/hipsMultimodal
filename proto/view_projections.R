# View images from cases with different billing codes


data(scalars, package = "hips")
scalars$projection_set %>% table_()

# Select 3 studies with each billing code
eg_pts <- scalars %>%
    group_by(projection_set) %>%
    sample_n(3) %>%
    ungroup() %$%
    pt

eg_df <- scalars %>%
    filter(pt %in% eg_pts)
eg_df$pt %<>% as.factor()
eg_df %<>% droplevels()

eg_df %<>% select(img, pt, fx, projection_set, `Report Text`)
eg_df %<>% arrange(pt)

# ap_ll: ap L leg and frog leg
eg_df %>%
    filter(pt == "2671") %$%
    img %>%
    map(showImg)
# ap_ll: 1 frog L leg
eg_df %>%
    filter(pt == "7906") %$%
    img %>%
    map(showImg)
# ap_ll: 2 ap L leg
eg_df %>%
    filter(pt == "3148") %$%
    img %>%
    map(showImg)

# BL: all AP, 1 bl, 3 each leg (1 straight, 2 frog)
eg_df %>%
    filter(pt == "4608") %$%
    img %>%
    map(showImg)
# BL: all AP, 2 each leg (1 straight, 1 frog)
eg_df %>%
    filter(pt == "5387") %$%
    img %>%
    map(showImg)
# BL: all AP, 2 each leg (1 straight, 1 frog)
eg_df %>%
    filter(pt == "8112") %$%
    img %>%
    map(showImg)

# ap_lr: AP R straight and frog
eg_df %>%
    filter(pt == "0482") %$%
    img %>%
    map(showImg)
# ap_lr: AP R straight and frog
eg_df %>%
    filter(pt == "0719") %$%
    img %>%
    map(showImg)

# ap_ll x 6 + ap_lr x2
eg_df %>%
    filter(pt == "6986") %$%
    img %>%
    map(showImg)
