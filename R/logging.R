# Override the flogging functions to provide the "fanalshine" namespace as defult

#' @noRd
flog <- function(..., ns = "fanalshine") {
  FacileData::flog(..., ns = ns)
}

#' @noRd
ftrace <- function(..., ns = "fanalshine", session = NULL) {
  if (missing(session)) {
    session <- try(get("session", envir = parent.frame()), silent = TRUE)
  }
  flog(..., level = "trace", ns = ns, session = session)
}

#' @noRd
fdebug <- function(..., ns = "fanalshine", session = NULL) {
  if (missing(session)) {
    session <- try(get("session", envir = parent.frame()), silent = TRUE)
  }
  flog(..., level = "debug", ns = ns, session = session)
}

#' @noRd
finfo <- function(..., ns = "fanalshine", session = NULL) {
  if (missing(session)) {
    session <- try(get("session", envir = parent.frame()), silent = TRUE)
  }
  flog(..., level = "info", ns = ns, session = session)
}

#' @noRd
fwarn <- function(..., ns = "fanalshine", session = NULL) {
  if (missing(session)) {
    session <- try(get("session", envir = parent.frame()), silent = TRUE)
  }
  flog(..., level = "warn", ns = ns, session = session)
}

#' @noRd
ferror <- function(..., ns = "fanalshine", session = NULL) {
  if (missing(session)) {
    session <- try(get("session", envir = parent.frame()), silent = TRUE)
  }
  flog(..., level = "error", ns = ns, session = session)
}

#' @noRd
ffatal <- function(..., ns = "fanalshine", session = NULL) {
  if (missing(session)) {
    session <- try(get("session", envir = parent.frame()), silent = TRUE)
  }
  flog(..., level = "fatal", ns = ns, session = session)
}
