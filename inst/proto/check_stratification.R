df <- hipsCohort()
df %<>% discard(is.list)


df$pt %>% levels() %>% length()

# df %>%
#     ComplexSummary("dept")
# 2421+3676+1720+1686

df %>%
    group_by(dept) %>%
    summarise(n_pts = n_distinct(pt)) %>%
    ungroup() %>%
    summarise(sum(n_pts))

# For each patient, how many non-zero department entries are there?
df %>%
    select(pt, dept) %>%
    table_() %>%
    `!=`(0) %>%
    rowSums() %>%
    table()
#! WTF @ 441 pts w/ 2 departments, and 19 pts w/ 3 departments

# Save these odd frequency tables
n_depts_per_pt <- df %>%
    select(pt, dept) %>%
    table_() %>%
    `!=`(0) %>%
    rowSums() %>%
    table() %>%
    as.data.frame() %>%
    `names<-`(c("n_depts", "n_pt"))
n_dev_per_pt <- df %>%
    select(pt, device_model) %>%
    table_() %>%
    `!=`(0) %>%
    rowSums() %>%
    table() %>%
    as.data.frame() %>%
    `names<-`(c("n_devices", "n_pt"))

write_csv(n_depts_per_pt, "inst/proto/n_depts_per_pt.csv")
write_csv(n_dev_per_pt, "inst/proto/n_devices_per_pt.csv")

multi_dept_pts <- df %>%
    select(pt, dept) %>%
    table_() %>%
    `!=`(0) %>%
    rowSums() %>%
    keep(as_mapper(~ .x > 1)) %>%
    names()


df %>%
    filter(pt %in% multi_dept_pts) %>%
    arrange(pt)
