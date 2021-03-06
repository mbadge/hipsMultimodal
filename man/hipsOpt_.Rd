% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa.R
\name{hipsOpt_}
\alias{hipsOpt_}
\alias{hipsOpt}
\title{Convenience fxn to fetch an option set by the hips package.}
\usage{
hipsOpt_(opt_substr, ...)

hipsOpt(opt_substr, ...)
}
\arguments{
\item{opt_substr}{chr(1) specific hips option}

\item{...}{optional default if variable isn't set \code{\link[base]{options}}}
}
\value{
set option or ...
}
\description{
hipsOpt uses NSE to save typing, use hipOpt_ for SE
}
\details{
Package option schema:
[pkg name] DOT [snake_case_option_name]
eg: hips.option_one
}
\examples{
hipsOpt_('ehr_vars')
hipsOpt()
hipsOpt(ehr_vars)
}
