# Simple helper to pre-train and save ml inference results
# Mainly used for composite figure generation
library(hips)

# Pkg Data
scalars_base <- hipsCohort()

# Continuous Vars
vars_cont <- scalars_base %>% map_lgl(is.numeric) %>% which %>% names %>% set_names

# Train models
inf_tbl_lst <- map(vars_cont, ~trainTestLm(scalars_base, target = .x, predictor_set = "image"))


# Craft model-img-major inference table
lm_inf_tbl <- inf_tbl_lst %>%
    map(`[`, c("Y", "Y_")) %>%
    lift_dl(bind_rows, .id = "model")()

saveRDS(lm_inf_tbl, file = Fp_ml_dir() %>% file.path("lm", "lm_inf_tbl.rds"))
