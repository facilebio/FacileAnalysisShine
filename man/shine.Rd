% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{shine}
\alias{shine}
\title{Immerses \code{FacileAnalysisResult} into a world of shiny.}
\usage{
shine(x, ...)
}
\description{
The \code{shine} functions generate shiny gadgets that provide a more interactive
view over a \code{FacileAnalysisResult}. This empowers the analyst to provide more
context around the results, likely by leveraging all of the data available
within the FacileDataStore.
}
\details{
The respective \code{shine} functions must return a \code{FacileAnalysisShine} object
invisibly to the caller. These should be able to be past into an overladed
\code{report} function, the result of which can be embedded into an Rmarkdown
report. In this way the analyst can embed a feature-reduced version of what
was observed in the gadget into an Rmarkdown report.

I'm not sure how exactly we can do this, but perhaps this will require some
code generation that the analyst can copy and paste paste into the Rmarkdown
document. This might simply be a parameterized version of the \code{report}
function call.
}
