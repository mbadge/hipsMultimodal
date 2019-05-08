# adlCohort()
adl_pt <- c("age", "sex")
adl_hp <- c("device_model", "device_brand", "name_interpreter", "order_wday")

# hips <- hipsCohort()
msh_dem <- hipsOpt(dem_vars)
msh_pt <- hipsOpt(pt_vars)
msh_hp <- c(adl_hp, c("dept", "projection_set", "order_date", "order_time", "name_operator", "procedure_radiation", "dT.ordered_to_exam", "dT.exam_to_prelim", "dT.exam_to_final"))

matching_var_lst <- list("adl_pt"=adl_pt, "adl_pthp"=c(adl_pt, adl_hp),
                         "msh_dem" = msh_dem, "msh_pt"=msh_pt, "msh_pthp"=c(msh_pt, msh_hp))

tbl <- map(matching_var_lst, mapvalues, from = names(AES_TARGET_LABELS),
    to = unname(AES_TARGET_LABELS)) %>%
    map_chr(str_x) %>%
    enframe() %>%
    separate(name, into=c("dataset", "matching"), sep="_")
tbl %<>% tibble::add_row(dataset="adl", matching="Random", value=NA_character_)
tbl %<>% tibble::add_row(dataset="msh", matching="Random", value=NA_character_)

tbl$matching %<>% as_factor() %>% fct_relevel("Random", "dem", "pt", "pthp")

tbl %<>%
    arrange(dataset, matching) %>%
    mutate(dataset = mapvalues(dataset, from = c("adl", "msh"), to = c("Adelaide", "Mount Sinai"))) %>%
    mutate(matching = mapvalues(matching, from = c("pt", "pthp"), to = c("PT", "PT+HP")))

names(tbl) <- c("Dataset", "Matching", "Matched Variables")

saveRDS(object = tbl, file = "inst/info/caseControl_matchingVars.rds")
