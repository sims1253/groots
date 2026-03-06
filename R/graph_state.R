#' Recompute graph state
#'
#' @param graph A \code{dagri_graph}.
#' @export
dagri_recompute_state <- function(graph) {
  topo <- dagri_topo_order(graph)

  for (n_id in topo) {
    up_edges <- Filter(function(e) e$to == n_id, graph$edges)

    is_upstream_blocked <- FALSE
    for (e in up_edges) {
      up_node_state <- graph$nodes[[e$from]]$state
      if (up_node_state != "ready") {
        is_upstream_blocked <- TRUE
        break
      }
    }

    if (is_upstream_blocked) {
      graph$nodes[[n_id]]$state <- "blocked"
      graph$nodes[[n_id]]$block_reason <- "upstream_blocked"
      next
    }

    is_gate_blocked <- FALSE
    for (e in up_edges) {
      gates_on_edge <- Filter(function(g) g$edge_id == e$id && g$status == "pending", graph$gates)
      if (length(gates_on_edge) > 0) {
        is_gate_blocked <- TRUE
        break
      }
    }

    if (is_gate_blocked) {
      graph$nodes[[n_id]]$state <- "blocked"
      graph$nodes[[n_id]]$block_reason <- "gate"
      next
    }

    graph$nodes[[n_id]]$state <- "ready"
    graph$nodes[[n_id]]$block_reason <- "none"
  }

  graph
}

#' Get eligible nodes
#'
#' @param graph A \code{dagri_graph}.
#' @export
dagri_eligible <- function(graph) {
  if (length(graph$nodes) == 0) {
    return(character(0))
  }
  names(graph$nodes)[vapply(graph$nodes, function(n) n$state == "ready", logical(1))]
}

#' Get blocked nodes
#'
#' @param graph A \code{dagri_graph}.
#' @export
dagri_blocked <- function(graph) {
  blocked_nodes <- Filter(function(n) n$state == "blocked", graph$nodes)
  res <- lapply(blocked_nodes, function(n) n$block_reason)
  if (length(res) == 0) {
    return(stats::setNames(list(), character(0)))
  }
  res
}

#' Get terminal nodes
#'
#' @param graph A \code{dagri_graph}.
#' @export
dagri_terminal <- function(graph) {
  dagri_leaves(graph)
}

dagri_empty_named_list <- function() {
  stats::setNames(list(), character(0))
}

dagri_validate_external_holds <- function(graph, external_holds) {
  if (!is.list(external_holds)) {
    abort_dagri(
      "dagri_error_invalid_argument",
      "`external_holds` must be a named list mapping node ids to reason strings."
    )
  }

  if (length(external_holds) == 0) {
    return(dagri_empty_named_list())
  }

  hold_ids <- names(external_holds)
  if (is.null(hold_ids) || anyNA(hold_ids) || any(hold_ids == "")) {
    abort_dagri(
      "dagri_error_invalid_argument",
      "`external_holds` must be a named list mapping node ids to reason strings."
    )
  }

  unknown_ids <- setdiff(hold_ids, names(graph$nodes))
  if (length(unknown_ids) > 0) {
    abort_dagri(
      "dagri_error_not_found",
      "Missing node.",
      details = list(node_ids = unknown_ids)
    )
  }

  invalid_reason <- !vapply(
    external_holds,
    function(reason) is.character(reason) && length(reason) == 1 && !is.na(reason),
    logical(1)
  )
  if (any(invalid_reason)) {
    abort_dagri(
      "dagri_error_invalid_argument",
      "Each external hold reason must be a single string.",
      details = list(node_ids = hold_ids[invalid_reason])
    )
  }

  external_holds
}

dagri_external_blocked <- function(graph, targets, topo_order, external_holds) {
  if (length(targets) == 0) {
    return(dagri_empty_named_list())
  }

  holds_in_scope <- external_holds[intersect(names(external_holds), targets)]
  if (length(holds_in_scope) == 0) {
    return(dagri_empty_named_list())
  }

  topo_rank <- stats::setNames(seq_along(topo_order), topo_order)
  external_blocked <- dagri_empty_named_list()

  for (node_id in topo_order) {
    if (node_id %in% names(holds_in_scope)) {
      external_blocked[[node_id]] <- holds_in_scope[[node_id]]
      next
    }

    upstream_blockers <- intersect(dagri_upstream(graph, node_id), names(external_blocked))
    if (length(upstream_blockers) == 0) {
      next
    }

    inherited_from <- upstream_blockers[[which.min(topo_rank[upstream_blockers])]]
    external_blocked[[node_id]] <- external_blocked[[inherited_from]]
  }

  if (length(external_blocked) == 0) {
    return(dagri_empty_named_list())
  }

  external_blocked
}

#' Create an execution plan
#'
#' @param graph A \code{dagri_graph}.
#' @param targets Optional target nodes.
#' @param external_holds Optional named list mapping node ids to external hold
#'   reason strings. These affect planning output without mutating graph state.
#' @export
dagri_plan <- function(graph, targets = NULL, external_holds = list()) {
  external_holds <- dagri_validate_external_holds(graph, external_holds)

  if (is.null(targets)) {
    targets <- names(graph$nodes)
  } else {
    all_targets <- character(0)
    for (t in targets) {
      all_targets <- unique(c(all_targets, t, dagri_ancestors(graph, t)))
    }
    targets <- all_targets
  }

  topo <- dagri_topo_order(graph, subset = targets)

  eligible_nodes <- intersect(targets, dagri_eligible(graph))

  blocked_list <- list()
  all_blocked <- dagri_blocked(graph)
  for (t in targets) {
    if (t %in% names(all_blocked)) {
      blocked_list[[t]] <- all_blocked[[t]]
    }
  }
  if (length(blocked_list) == 0) {
    blocked_list <- dagri_empty_named_list()
  }

  external_blocked <- dagri_external_blocked(graph, targets, topo, external_holds)

  target_leaves <- character(0)
  for (t in targets) {
    down <- dagri_downstream(graph, t)
    if (length(intersect(down, targets)) == 0) {
      target_leaves <- c(target_leaves, t)
    }
  }

  pending_gates <- character(0)
  for (g in graph$gates) {
    if (g$status == "pending") {
      e <- graph$edges[[g$edge_id]]
      if (e$to %in% targets) {
        pending_gates <- c(pending_gates, g$id)
      }
    }
  }

  list(
    targets = targets,
    topo_order = topo,
    eligible = eligible_nodes,
    blocked = blocked_list,
    external_blocked = external_blocked,
    terminal = target_leaves,
    pending_gates = pending_gates
  )
}
