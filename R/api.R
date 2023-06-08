# Visualization API ============================================================

#' Immerses `FacileAnalysisResult` into a world of shiny.
#' 
#' The `shine` functions generate shiny gadgets that provide a more interactive
#' view over a `FacileAnalysisResult`. This empowers the analyst to provide more
#' context around the results, likely by leveraging all of the data available
#' within the FacileDataStore.
#'
#' The respective `shine` functions must return a `FacileAnalysisShine` object
#' invisibly to the caller. These should be able to be past into an overladed
#' `report` function, the result of which can be embedded into an Rmarkdown
#' report. In this way the analyst can embed a feature-reduced version of what
#' was observed in the gadget into an Rmarkdown report.
#'
#' I'm not sure how exactly we can do this, but perhaps this will require some
#' code generation that the analyst can copy and paste paste into the Rmarkdown
#' document. This might simply be a parameterized version of the `report`
#' function call.
#'
#' @export
shine <- function(x, ...) {
  UseMethod("shine", x)
}

#' @noRd
#' @export
report.FacileGadgetResult <- function(x, ...) {
  report(result(x), ...)
}

# Generic API calls on results from running an analysis through a gadget =======

#' #' @noRd
#' report.FacileGadgetResult <- function(x, ...) {
#'   report(result(x), ...)
#' }
#'
#' #' @noRd
#' viz.FacileGadgetResult <- function(x, ...) {
#'   viz(result(x), ...)
#' }
#'
#' #' @noRd
#' shine.FacileGadgetResult <- function(x, ...) {
#'   shine(result(x), ...)
#' }
#'
#' #' @noRd
#' compare.FacileGadgetResult <- function(x, y, ...) {
#'   if (is(y, "FacileGadgetResult")) y <- result(y)
#'   compare(result(x), y)
#' }


# Reactive implementation of base FacileAnalysis API ===========================
# These function provide implementations of the FacileAnalysis API over 
# reactive objects.

#' @noRd
#' @export
faro.ReactiveFacileAnalysisResult <- function(x, ...) {
  assert_class(x$faro, "reactive")
  x$faro()
}

#' ReactiveFacileAnalysisResultContainer are results from a shiny module that
#' encapsulate a single complete analysis step, such as the fdgeAnalysis, for
#' instance.
#'
#' These `*Analysis` will store the main FacileAnalysisResult in their
#' `"main"` list element.
#'
#' @noRd
#' @export
faro.ReactiveFacileAnalysisResultContainer <- function(x, main = "main", ...) {
  # assert_choice(main, names(x))
  req(test_choice(main, names(x)))
  main. <- x[[main]]
  # assert_class(main., "ReactiveFacileAnalysisResult")
  req(test_class(main., "ReactiveFacileAnalysisResult"))
  faro(main.)
}

#' See combined fDgeSeaAnalysis module
#' @noRd
#' @export
faro.ReactiveFacileMultiAnalysisResult <- function(x, main = names(x$names)[1L],
                                                   ...) {
  results <- x[["main"]]
  assert_list(results, names = "unique")
  assert_choice(main, names(results))
  res <- results[[main]]
  faro(res)
}

#' A shiny module that produces a FacileAnalysisResult as its main
#' result will store it as a reactive() in its result$faro element.
#'
#' Triggering its reactivity will result in the return of that object.
#' If the analysis-result has not be generated yet within that shiny module,
#' then triggering it MUST raise an error, due to the module ensuring
#' req() is in all the right places
#'
#' For instance, the fdgeRun module is responsible for everything being
#' set up properly, and when the Run button is hit, it includes a req()
#' in response to make sure the result is built correctly
#'
#' @noRd
#' @export
#' @importFrom FacileShine initialized
initialized.ReactiveFacileAnalysisResult <- function(x, ...) {
  obj <- try(faro(x), silent = TRUE)
  is(obj, "FacileAnalysisResult") && initialized(obj)
}

#' A ReactiveFacileMultiAnalysisResult is an analysis module that combines
#' two individual analyses together, like fdge and ffsea.
#'
#' For now, this might just be the fDgeSeaAnalysis module, as I'm not sure that
#' this is a good idea ...
#'
#' @noRd
#' @export
initialized.ReactiveFacileMultiAnalysisResult <- function(x, ...) {
  assert_list(x$main)
  resnames <- names(x$main)
  all(sapply(x$main, initialized))
}

#' @noRd
#' @export
initialized.FacileAnalysisResult <- function(x, ...) {
  length(x$errors) == 0L
}

#' @export
#' @noRd
result.ReactiveFacileAnalysisResult <- function(x, name = "result", ...) {
  result(faro(x, ...), name = name, ...)
}

#' @export
#' @noRd
tidy.ReactiveFacileAnalysisResult <- function(x, name = "result", ...) {
  tidy(faro(x, ...), name = name, ...)
}


#' @noRd
#' @export
param.ReactiveFacileAnalysisResult <- function(x, name = NULL, ...) {
  assert_choice("result", names(x))
  param(x$result(), name, ...)
}

#' @noRd
#' @export
viz.FacileGadgetResult <- function(x, ...) {
  viz(result(x), ...)
}
