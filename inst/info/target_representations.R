# Tabulate the original and binarized representations of targets
#
#!!! Final table semi-manually curated:
#!!! https://docs.google.com/spreadsheets/d/1O5S9r17s2_Ya9LdFNl0-7zztg8bL4WSibEqviFbK0mY/edit#gid=0
#
# Reviewers request the form (e.g., nominal, ordinal, continuous)
# for factors, the levels
# for continuous, the binarization thresholds (median)
#
# Output
# Variable | Original Representation | Binarized Representation
# age    |  numeric (mean, sd)   |  age >= {median}
# device_model   |  nominal (20 levels)  |


# Helper fxns ----
pretty_label_targets <- function(x) {
    stopifnot(is.character(x))

    x %<>% mapvalues(from = names(hips:::AES_TARGET_LABELS),
                     to = unname(hips:::AES_TARGET_LABELS))

    return(x)
}

pretty_label_col <- function(x, target_colnm = "name") {
    stopifnot(is.data.frame(x))

    x[[target_colnm]] %<>% pretty_label_targets()

    return(x)
}


# Main ----
# Scalar data tables
df <- hipsCohort()
b_df <- hipsCohort(mutating = binary)

# Filter to just variables
df %<>% discard(is.list)
df$name_interpreter %<>% as.factor()
tbl <- df %>%
    select(one_of(names(hips:::AES_TARGET_LABELS))) %>%
    map_chr(class) %>%
    tibble::enframe() %>%
    pretty_label_col() %>%
    `names<-`(c("var", "orig_rep"))

df %>%
    select(one_of(names(hips:::AES_TARGET_LABELS))) %>%
    keep(is.factor) %>%
    map(levels) %>%
    mapnames(from = names(hips:::AES_TARGET_LABELS),
             to = unname(hips:::AES_TARGET_LABELS))

# LGL <- function(x, pos = "positive") {
#     perct <- mean(x) * 100
#     glue("{round(perct, 1)}% {pos}")
# }
# NUM <- function(x) {
#     mean <- mean(x)
#     sd <- sd(x)
#     glue("mean {round(mean, 0)}, s.d. {round(sd, 0)}")
# }
# LGL(df$fx)
# NUM(df$age)



# Check types
b_df %>%
    select(one_of(names(hips:::AES_TARGET_LABELS))) %>%
    map_chr(class) %>%
    tibble::enframe() %>%
    pretty_label_col() %>%
    `names<-`(c("var", "bin_rep"))

# Check levels
b_df %>%
    select(one_of(names(hips:::AES_TARGET_LABELS))) %>%
    keep(is.factor) %>%
    map(levels) %>%
    mapnames(from = names(hips:::AES_TARGET_LABELS),
             to = unname(hips:::AES_TARGET_LABELS))
