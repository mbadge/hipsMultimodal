library(ComplexHeatmap)
library(RColorBrewer)
library(viridis)


bt <- hipsCohort()

# Pilot
ckpt_bt <- bt
bt <- bt_common_devices %>%
    group_by(fx) %>%
    sample_n(721) %>%
    gather(key = "cnn", value = "PCs", rnd, pre)
bt %<>% unnest(PCs)
bt$ft <- NULL

bt %<>%
    group_by(cnn) %>%
    mutate_at(.vars = vars(starts_with("PC")), .funs = scale) %>%
    ungroup()


mtx_lst <- bt %>%
    select(starts_with("PC")) %>%
    split(bt$cnn) %>%
    map(tibble::remove_rownames) %>%
    map(as.matrix)
mtx_lst %<>% map(`colnames<-`, NULL)
heatmap_base_lst <- imap(mtx_lst,
                         ~Heatmap(matrix = .x, column_title = AES_CNN_LABELS[.y],
                                  col = viridis(500),
                                  row_title = "Radiograph",
                                  name = "PC activation",
                                  show_row_dend = FALSE,
                                  show_column_dend = FALSE)
)

# Base
reduce(heatmap_base_lst, `+`)

names(paired) <- bt$device_model %>% unique()
names(set3) <- bt$device_model %>% unique()
pal_col(set3)
pal_col(AES_DEVICE_ALPHA)

# Annotate labels
#' draw_anno(bt, "cnn", aes=list(cnn=AES_CNN_COLORS))
draw_anno <- function(anno_df, var, aes) {
    anno_cols <- HeatmapAnnotation(df = anno_df[var], col = aes)
    draw(anno_cols, 1:nrow(anno_df))
}

df_lst <- bt %>%
    split(.$cnn) %>%
    map(., select, fx, device_model, projection_set, age, sex) %>%
    map(as.data.frame)



lbl_anno_lst <- map2(df_lst,
                     bt %>%
                         split(.$cnn),
                     ~rowAnnotation(df = .x,
                                    col = list(fx = AES_FX_COLORS[.x$fx %>% as.character()],
                                               device_model = AES_DEVICE_COLORS[.x$device_model %>% as.character()],
                                               #projection_set = AES_VIEW_COLORS[.x$projection_set %>% as.character()],
                                               age = circlize::colorRamp2(c(0, 100), c("white", "black")),
                                               sex = AES_SEX_COLORS[.x$sex %>% as.character()])))
#barplot1 = row_anno_barplot(.y$order_date, axis=TRUE)))

#points = row_anno_points(.y$age, axis=TRUE)))

HEAT_MPS <- map2(heatmap_base_lst, lbl_anno_lst, `+`)

draw(heatmap_base_lst$pre + heatmap_base_lst$rnd)
draw(HEAT_MPS$pre + HEAT_MPS$rnd)
draw(HEAT_MPS$rnd + HEAT_MPS$pre)
reduce(HEAT_MPS, `+`)
reduce(rev(HEAT_MPS), `+`)

# ACK by default the second heatmap has rows aligned to the first

# Rebuild dfs so cnn tables are matched
lbl_anno_lst$pre

draw(heatmap_base_lst$rnd + lbl_anno_lst$pre + heatmap_base_lst$pre)
