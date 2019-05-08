# ---------------- Explanatory/Predictor Variable Sets ----------------
AES_PREDICTOR_LABELS <- c(
    "img" = "IMG",
    "pt" = "PT",
    "hp" = "HP",
    "imgPt" = "IMG + PT",
    "imgHp" = "IMG + HP",
    "ptHp" = "PT + HP",
    "imgPtHp" = "IMG + PT + HP"
)

# AES_PREDICTOR_PALETTE <- c(
#     "img" = "#E6AB02",  # Yellow
#     "pt" = "#E7298A",  # Pink
#     "hp" = "#1B9E77",  # Teal
#     "imgPt" = "#D95F02",  # Orange
#     "imgHp" = "#66A61E",  # Green
#     "ptHp" = "#7570B3",  # Purple
#     "imgPtHp" = "#666666"  # Grey
# )

AES_PREDICTOR_PALETTE <- c(
    "img" = "#00AAAA",  # Cyan
    "pt" = "#AA00AA",  # Magenta
    "hp" = "#AAAA00",  # Yellow
    "imgPt" = "#0000aa",  # Blue
    "imgHp" = "#00AA00",  # Green
    "ptHp" = "#AA0000",  # Red
    "imgPtHp" = "#666666"  # Grey
)




#' Scales for predictor sets.
#'
#' @family gg_aes
#' @importFrom purrr compose
#' @export
#' @examples
#' vizR::pal_col(AES_PREDICTOR_PALETTE)
#' predictor_df <- data.frame(x = seq_along(AES_PREDICTOR_LABELS),
#'   data_value = names(AES_PREDICTOR_LABELS),
#'   pretty_value = unname(AES_PREDICTOR_LABELS))
#' ggplot(predictor_df, aes(x=x, fill=data_value)) +
#'   geom_bar() +
#'   scale_predictor_("fill")
#' ggplot(predictor_df, aes(x=x, col=data_value)) +
#'   geom_bar(fill="white") +
#'   scale_predictor_("color")
scale_predictor_ <- function(aesthetic,
                            title = "Explanatory Variables",
                            values = AES_PREDICTOR_PALETTE,
                            labels = AES_PREDICTOR_LABELS)
{
    if(purrr::compose(`!`, `%in%`)(aesthetic, c("color", "fill"))) {
        warning("Only tested for color and fill, good luck with ", aesthetic)
    }
    FUNC <- match.fun(paste0("scale_", aesthetic, "_manual"))
    FUNC(title, values = values, labels = labels)
}
#' @export
#' @rdname scale_predictor_
#' @examples
#' ggplot(predictor_df, aes(x=x, fill=data_value)) +
#'   geom_bar() +
#'   scale_predictor(fill)
#' ggplot(predictor_df, aes(x=x, col=data_value)) +
#'   geom_bar(fill="white") +
#'   scale_predictor(color)
scale_predictor <- function(aesthetic, ...) {
    scale_predictor_(deparse(substitute(aesthetic)), ...)
}

aes_predictor_col <- function() {
    list(ggplot2::aes(col = predictor), scale_predictor(color))
}
aes_predictor_fill <- function() {
    list(ggplot2::aes(fill = predictor), scale_predictor(fill))
}


#' Beautify predictor labels
#'
#' @family gg_aes
#' @export
#' @examples
#' PrettyPredictors("pt")
PrettyPredictors <- function(x) {
    mapvalues(x,
              from = names(AES_PREDICTOR_LABELS),
              to = unname(AES_PREDICTOR_LABELS))
}


# ---------------- CNN / Btlnck Set ----------------
AES_CNN_LABELS <- c(
    "rnd" = "Randomly Initialized CNN",
    "pre" = "Pre-Trained CNN"
)
AES_CNN_COLORS <- c(
    "rnd" = "#666666",
    "pre" = "#A6761D"
)

#' Scales for CNNs
#'
#' @family gg_aes
#' @export
#' @examples
#' vizR::pal_col(AES_CNN_COLORS)
#' ggplot(data.frame(x = 1:2, cnn = hips::hipsOpt(btlncks)), aes(x=x, fill=cnn)) +
#'   geom_bar() + scale_fill_cnn()
scale_color_cnn <- function(title="Encoding CNN", values=AES_CNN_COLORS, labels=AES_CNN_LABELS) {
    ggplot2::scale_color_manual(title, values=values, labels=labels)
}

#' @rdname scale_color_cnn
#' @export
aes_cnn_col <- function() {
    list(aes(col = btlnck), scale_color_cnn())
}

#' @export
#' @rdname scale_color_cnn
scale_fill_cnn <- function(title="Encoding CNN", values=AES_CNN_COLORS, labels=AES_CNN_LABELS) {
    ggplot2::scale_fill_manual(title, values=values, labels=labels)
}

#' Facet by CNN
#'
#' @family gg_aes
#' @export
#' @examples
#' data.frame(cnn = rep(c("rnd", "pre"), 5), n=1:10) %>% ggplot(., aes(x=n)) + geom_histogram() + facet_cnn()
facet_cnn <- function(facets = ~ cnn, scales = "free", as.table = FALSE,
                      labeller = ggplot2::as_labeller(AES_CNN_LABELS), ...)
{
    ggplot2::facet_wrap(facets = facets, scales = scales, as.table = as.table,
               labeller = labeller, ...)
}

# ---------------- Target ----------------
AES_TARGET_LABELS <- c(
    "age" = "Age",
    "bmi" = "BMI",
    "dept" = "Department",
    "device_brand" = "Scanner Manufacturer",
    "device_model" = "Scanner Model",
    "fx" = "Fracture",
    "name_interpreter" = "Radiologist",
    "name_operator" = "Technician",
    "order_date" = "Order Date",
    "order_priority" = "Order Priority",
    "order_time" = "Order Time",
    "order_wday" = "Order Weekday",
    "procedure_radiation" = "Radiation Dose",
    "projection_set" = "Laterality",
    "sex" = "Gender",
    "dT.exam_to_final" = "Time to Final Interp.",
    "dT.exam_to_prelim" = "Time to Initial Interp.",
    "dT.ordered_to_exam" = "Imaging Wait Time",
    "pain_lgl" = "Pain",
    "fall_lgl" = "Fall"
)

#' Make a ggplot scale to color by target
#'
#' @family gg_aes
#' @export
#' @examples
#' scale_color_target()
scale_color_target <- function(
    title="Classification Target",
    labels=AES_TARGET_LABELS, ...)
{
    ggplot2::scale_color_manual(title, labels=labels, ...)
}


#' ggplot scale to map targets to the x-axis
#'
#' @family gg_aes
#' @export
#' @examples
#' scale_x_target() %>% s
scale_x_target <- function(
    title="Classification Target",
    labels=AES_TARGET_LABELS, ...)
{
    ggplot2::scale_x_discrete(title, labels=labels, ...)
}
#' @rdname scale_x_target
#' @export
scale_y_target <- function(
    title="Classification Target",
    labels=AES_TARGET_LABELS, ...)
{
    ggplot2::scale_y_discrete(title, labels=labels, ...)
}

#' ggplot wrapper to make an x scale with angled labels
#'
#' @family gg_aes
#' @export
#' @examples
#' gg_x_target() %>% s
gg_x_target <- function(...) {
    list(scale_x_target(),
         ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust=1)))
}


#' Beautify target labels
#'
#' @family gg_aes
#' @export
#' @exampels
#' PrettyTargets("name_operator")
PrettyTargets <- function(x) {
    mapvalues(x,
              from = names(AES_TARGET_LABELS),
              to = unname(AES_TARGET_LABELS))
}

# ---------------- Target Mode ----------------
AES_TARGETMODE_LABELS <- c(
    "Patient" = "PT",
    "Disease" = "Disease",
    "Hospital" = "HP"
)

AES_TARGETMODE_COLORS <- c(
    "Patient" = "#E7298A",
    "Disease" = "#A6761D",
    "Hospital" = "#1B9E77"
)

#' Make a ggplot scale to color by target
#'
#' @family gg_aes
#' @export
#' @examples
#' scale_color_target()
scale_color_targetMode <- function(
    title="Target Type",
    labels=AES_TARGETMODE_LABELS,
    values = AES_TARGETMODE_COLORS)
{
    ggplot2::scale_color_manual(title, labels=labels, values=values)
}




# ---------------- Fracture ----------------
AES_FX_LABELS <- c("TRUE" = "True", "FALSE" = "False")
AES_FX_COLORS <- c("TRUE" = "#D95F02", "FALSE" = "#666666")

#' Make a ggplot scale to color by fracture
#'
#' @family gg_aes
#' @export
#' @examples
#' df <- data.frame(fx = rep(c("TRUE", "FALSE"), 50), PC1=rnorm(100), PC2=rnorm(100))
#' ggplot(df, aes(x=PC1, y=PC2, col=fx)) + geom_point() + scale_color_fx()
scale_color_fx <- function(title="Fracture", values=AES_FX_COLORS, labels=AES_FX_LABELS)
{
  ggplot2::scale_color_manual(title, values=values, labels=labels)
}



# ---------------- Sex ----------------
AES_SEX_LABELS <- c("f" = "Female", "m" = "Male")
AES_SEX_COLORS <- c("f" = "#F781BF", "m" = "#377EB8")

#' ggplot scale fill by gender
#'
#' @family gg_aes
#' @export
#' @examples
#' pal_col(AES_SEX_COLORS)
scale_fill_sex <- function(title = "Gender", values = AES_SEX_COLORS, labels = AES_SEX_LABELS)
{
    ggplot2::scale_fill_manual(title, values=values, labels=labels)
}


# ---------------- Cohort ----------------
AES_COHORT_LABELS <- c("crossSectional" = "Cross Sectional",
                       "caseControl_matchNone" = "Case Control, no matching",
                       "caseControl_matchDem" = "Case Control, matched Age, Gender",
                       "caseControl_matchPt" = "Case Control, matched PT",
                       "caseControl_matchAll" = "Case Control, matched PT + HP")

# RColorBrewer::brewer.pal(9, 'Accent') %>% pal_col
AES_COHORT_COLORS <- c("crossSectional" = "#A6761D",
                       "caseControl_matchNone" = "#666666",
                       "caseControl_matchDem" = "#D95F02",
                       "caseControl_matchPt" = "#E7298A",
                       "caseControl_matchAll" = "#7570B3")

#' ggplot scale color by cohort
#'
#' @family gg_aes
#' @export
#' @examples
#' pal_col(AES_COHORT_COLORS)
scale_color_cohort <- function(title = "Test Cohort", values = AES_COHORT_COLORS, labels = AES_COHORT_LABELS)
{
    ggplot2::scale_color_manual(title, values=values, labels=labels)
}

#' @export
#' @rdname scale_color_cohort
aes_cohort_col <- function() {
    list(ggplot2::aes(col = cohort), scale_color_cohort())
}

#' ggplot scale to map targets to the x-axis
#'
#' @family gg_aes
#' @export
#' @examples
#' scale_x_cohort() %>% s
scale_x_cohort <- function(
    title="Cohort Population",
    labels=AES_COHORT_LABELS, ...)
{
    ggplot2::scale_x_discrete(title, labels=labels, ...)
}


# ---------------- Device ----------------
AES_DEVICE_LABELS <- c("cs7"="CS7",
                       "thunderPlatform"="Thunder Platform",
                       "x5000"="X5000",
                       "x0862"="X0862",
                       "definium5000" = "Definium 5000",
                       "discoveryXr656" = "Discovery XR 656",
                       "Other"="Other",
                       "pooled"="Pooled"
                       )

AES_DEVICE_COLORS <- c("cs7" = "#377EB8",
                       "thunderPlatform" = "#E41A1C",
                       "x5000" = "#4DAF4A",
                       "x0862" = "#984EA3",
                       "definium5000" = "#F781BF",
                       "discoveryXr656" = "#FF7F00",
                       "Other" = "#A65628",
                       "pooled" = "#999999")

#' ggplot scale color by device
#'
#' @family gg_aes
#' @export
#' @examples
#' pal_col(AES_DEVICE_COLORS)
scale_color_device <- function(title = "Scanner Model",
                               values = AES_DEVICE_COLORS,
                               labels = AES_DEVICE_LABELS) {
    ggplot2::scale_color_manual(title, values = values, labels = labels)
}

#' @rdname scale_color_device
#' @export
aes_device_col <- function() {
    list(ggplot2::aes(col = device_model), scale_color_device())
}

# Becuase of the massive class imbalance, I'm considering lumping levels
# AesDeviceAlpha <- function(df) {
#     freqs <- df %>%
#       group_by(device_model) %>%
#       summarise(n = n())
#
#     inv_prop <- 1 - ( freqs / sum(freqs) )
#
#
# }
#


# ---------------- Projection ----------------
AES_VIEW_LABELS <- c("ap_ll" = "Left",
                     "ap_lr" = "Right",
                     "bl" = "Bilateral"
                     )
# pal_col(RColorBrewer::brewer.pal(9, "Set1"))
AES_VIEW_COLORS <- c("ap_ll" = "#FC8D62",
                       "ap_lr" = "#A6D854",
                       "bl" = "#8DA0CB")

#' ggplot scale color by device
#'
#' @family gg_aes
#' @export
#' @examples
#' pal_col(AES_VIEW_COLORS)
scale_color_view <- function(title = "Laterality",
                               values = AES_VIEW_COLORS,
                               labels = AES_VIEW_LABELS) {
    ggplot2::scale_color_manual(title, values = values, labels = labels)
}


# Department ------------
AES_DEPT_LABELS <- c("em" = "Emergency Department",
                     "inpt" = "Inpatient",
                     "outpt" = "Outpatient",
                     "NA" = "(Missing)")

#' @export
#' @rdname PrettyPredictors
#' @examples
#' PrettyDept(c("em", "outpt", NA))
PrettyDept <- function(x) {
    mapvalues(x,
              from = names(AES_DEPT_LABELS),
              to = unname(AES_DEPT_LABELS)
    )
}
