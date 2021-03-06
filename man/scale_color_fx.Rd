% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_aes.R
\name{scale_color_fx}
\alias{scale_color_fx}
\title{Make a ggplot scale to color by fracture}
\usage{
scale_color_fx(title = "Fracture", values = AES_FX_COLORS,
  labels = AES_FX_LABELS)
}
\description{
Make a ggplot scale to color by fracture
}
\examples{
df <- data.frame(fx = rep(c("TRUE", "FALSE"), 50), PC1=rnorm(100), PC2=rnorm(100))
ggplot(df, aes(x=PC1, y=PC2, col=fx)) + geom_point() + scale_color_fx()
}
\seealso{
Other gg_aes: \code{\link{PrettyPredictors}},
  \code{\link{PrettyTargets}}, \code{\link{facet_cnn}},
  \code{\link{gg_x_target}}, \code{\link{scale_color_cnn}},
  \code{\link{scale_color_cohort}},
  \code{\link{scale_color_device}},
  \code{\link{scale_color_targetMode}},
  \code{\link{scale_color_target}},
  \code{\link{scale_color_view}},
  \code{\link{scale_fill_sex}},
  \code{\link{scale_predictor_}},
  \code{\link{scale_x_cohort}},
  \code{\link{scale_x_target}}
}
\concept{gg_aes}
