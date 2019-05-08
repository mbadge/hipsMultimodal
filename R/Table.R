#' @include zzz.R
NULL

kDIR_OUT_TBL <- file.path(FS_hipsDir(), "out")

FpOutTbl <- function(fn) {
    DirOut <- file.path(kDIR_OUT_TBL, MyUtils::fp_stem(fn))
    if (!dir.exists(DirOut)) {dir.create(DirOut)}

    fp <- file.path(DirOut, fn)
    return(fp)
}


tbl_html <- function(x, bn) UseMethod("tbl_html")

tbl_html.data.frame <- function(x, bn) {
    x %>%
        knitr::kable(format = "html") %>%
        kableExtra::kable_styling(bootstrap_options = c("condensed"), full_width = FALSE) %>%
        # TODO add default extension addition
        kableExtra::save_kable(., file = FpOutTbl(paste(bn, "html", sep=".")))
}

tbl_html.knitr_kable <- function(x, bn) {
    x %>%
        # TODO add default extension addition
        kableExtra::save_kable(., file = FpOutTbl(paste(bn, "html", sep=".")))
}


tbl_latex <- function(x, bn) {
    x %>%
        knitr::kable(format = "latex") %>%
        base::cat(., file = FpOutTbl(paste(bn, "txt", sep=".")))
}

tbl_xlsx <- function(x, bn) {
    x %>%
        as.data.frame() %>%
        xlsx::write.xlsx(., file = FpOutTbl(paste(bn, "xlsx", sep=".")), row.names = FALSE)
}


#' Save table file(s) for hips project
#'
#' @param x data.frame or kable table to be saved
#' @param bn chr(1) target file basename. defaults to name of `x`
#' @param tbl_type chr(n) types of table files to generate
#'
#' @return None. called for side effect
#'
#' @export
#'
#' @examples
#' Tbl(mtcars)
Tbl <- function(x, bn="", tbl_type = c("html", "latex", "xlsx")) {
    # precondition
    supported_types <- c("html", "latex", "xlsx")
    stopifnot(all(tbl_type %in% supported_types))

    # defaults to name of x
    if (bn == "") {bn = deparse(substitute(x))}

    # method invocation loop
    for (typ in tbl_type) {
        do.call(paste0("tbl_", typ), list(x=x, bn=bn))
    }

    return(NULL)
}

