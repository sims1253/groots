#' Abort with a typed dagriculture error
#'
#' @param class Error class.
#' @param message Error message.
#' @param details Optional details list.
#' @export
abort_dagriculture <- function(class, message, details = list()) {
  rlang::abort(message, class = class, details = details)
}
