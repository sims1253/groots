#' Define a dagriculture kind
#'
#' @param name The name of the kind.
#' @param input_contract Input contract list.
#' @param output_type Output type string.
#' @param param_schema Parameter schema.
#' @export
dagri_kind <- function(name, input_contract = NULL, output_type = NULL, param_schema = NULL) {
  if (!is.null(param_schema)) {
    has_closure <- function(x) {
      if (is.function(x)) {
        return(TRUE)
      }
      if (is.list(x)) {
        return(any(vapply(x, has_closure, logical(1))))
      }
      FALSE
    }
    if (has_closure(param_schema)) {
      abort_dagri(
        "dagri_error_invalid_argument",
        "Executable closures are not allowed in param_schema."
      )
    }
  }

  list(
    name = name,
    input_contract = input_contract,
    output_type = output_type,
    param_schema = param_schema,
    metadata = list()
  )
}

#' Define a dagriculture registry
#'
#' @param ... \code{dagri_kind} objects.
#' @export
dagri_registry <- function(...) {
  kinds_list <- list(...)
  kinds_env <- list()
  for (k in kinds_list) {
    kinds_env[[k$name]] <- k
  }
  list(
    kinds = kinds_env,
    metadata = list()
  )
}

#' Create an empty dagriculture graph
#'
#' @param registry A \code{dagri_registry} object.
#' @export
dagri_graph <- function(registry) {
  list(
    registry = registry,
    nodes = stats::setNames(list(), character(0)),
    edges = stats::setNames(list(), character(0)),
    gates = stats::setNames(list(), character(0)),
    version = 0L,
    metadata = list()
  )
}
