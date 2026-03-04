describe("graph editing operations", {
  # Setup basic registry and empty graph
  reg <- dagriculture_registry(dagriculture_kind("source"), dagriculture_kind("process"))
  g0 <- dagriculture_graph(reg)

  describe("dagriculture_add_node()", {
    it("adds a node and increments graph version", {
      g1 <- dagriculture_add_node(g0, id = "n1", kind = "source")
      expect_identical(g1$version, g0$version + 1L)
      expect_true("n1" %in% names(g1$nodes))

      n1 <- g1$nodes[["n1"]]
      expect_identical(n1$id, "n1")
      expect_identical(n1$kind, "source")
      expect_identical(n1$state, "new")
      expect_identical(n1$block_reason, "none")
    })

    it("rejects unknown kinds", {
      expect_error(
        dagriculture_add_node(g0, "n1", kind = "unknown"),
        class = "dagriculture_error_unknown_kind"
      )
    })

    it("rejects duplicate ids", {
      g1 <- dagriculture_add_node(g0, "n1", "source")
      expect_error(
        dagriculture_add_node(g1, "n1", "source"),
        class = "dagriculture_error_duplicate_id"
      )
    })
  })

  describe("dagriculture_update_node()", {
    it("updates a node and increments graph version", {
      g1 <- dagriculture_add_node(g0, id = "n1", kind = "source")
      g2 <- dagriculture_update_node(g1, node_id = "n1", label = "Updated Source")

      expect_identical(g2$version, g1$version + 1L)
      expect_identical(g2$nodes[["n1"]]$label, "Updated Source")
    })

    it("errors if node is not found", {
      expect_error(
        dagriculture_update_node(g0, "missing", label = "Test"),
        class = "dagriculture_error_not_found"
      )
    })
  })

  describe("dagriculture_remove_node()", {
    it("removes a node and increments graph version", {
      g1 <- dagriculture_add_node(g0, id = "n1", kind = "source")
      g2 <- dagriculture_remove_node(g1, "n1")

      expect_identical(g2$version, g1$version + 1L)
      expect_false("n1" %in% names(g2$nodes))
    })
  })

  describe("dagriculture_add_edge()", {
    g1 <- dagriculture_add_node(g0, "n1", "source")
    g2 <- dagriculture_add_node(g1, "n2", "process")
    g3 <- dagriculture_add_node(g2, "n3", "process")

    it("adds an edge and increments version", {
      g_edge <- dagriculture_add_edge(g2, from = "n1", to = "n2", id = "e1")
      expect_identical(g_edge$version, g2$version + 1L)
      expect_true("e1" %in% names(g_edge$edges))
      expect_identical(g_edge$edges[["e1"]]$from, "n1")
      expect_identical(g_edge$edges[["e1"]]$to, "n2")
    })

    it("prevents cycles", {
      g_edge <- dagriculture_add_edge(g3, "n1", "n2", id = "e1")
      g_edge <- dagriculture_add_edge(g_edge, "n2", "n3", id = "e2")
      expect_error(
        dagriculture_add_edge(g_edge, "n3", "n1"),
        class = "dagriculture_error_cycle"
      )
    })

    it("rejects edges between missing nodes", {
      expect_error(
        dagriculture_add_edge(g2, from = "n1", to = "n_missing"),
        class = "dagriculture_error_not_found"
      )
    })
  })

  describe("dagriculture_remove_edge()", {
    it("removes an edge and increments version", {
      g2 <- dagriculture_add_node(g0, "n1", "source") |> dagriculture_add_node("n2", "process")
      g_edge <- dagriculture_add_edge(g2, "n1", "n2", id = "e1")

      g_removed <- dagriculture_remove_edge(g_edge, "e1")
      expect_identical(g_removed$version, g_edge$version + 1L)
      expect_false("e1" %in% names(g_removed$edges))
    })
  })

  describe("dagriculture_add_gate()", {
    g1 <- dagriculture_add_node(g0, "n1", "source")
    g2 <- dagriculture_add_node(g1, "n2", "process")
    g_edge <- dagriculture_add_edge(g2, from = "n1", to = "n2", id = "e1")

    it("adds a gate targeting an existing edge", {
      g_gate <- dagriculture_add_gate(g_edge, edge_id = "e1", id = "gate1")
      expect_identical(g_gate$version, g_edge$version + 1L)
      expect_true("gate1" %in% names(g_gate$gates))
      expect_identical(g_gate$gates[["gate1"]]$status, "pending")
    })

    it("rejects gates for missing edges", {
      expect_error(
        dagriculture_add_gate(g_edge, edge_id = "e_missing", id = "gate2"),
        class = "dagriculture_error_not_found"
      )
    })
  })

  describe("dagriculture_resolve_gate()", {
    it("resolves a pending gate", {
      g2 <- dagriculture_add_node(g0, "n1", "source") |> dagriculture_add_node("n2", "process")
      g_gate <- dagriculture_add_edge(g2, "n1", "n2", id = "e1") |>
        dagriculture_add_gate(edge_id = "e1", id = "gate1")

      g_resolved <- dagriculture_resolve_gate(g_gate, "gate1")
      expect_identical(g_resolved$version, g_gate$version + 1L)
      expect_identical(g_resolved$gates[["gate1"]]$status, "resolved")
    })
  })

  describe("dagriculture_reopen_gate()", {
    it("reopens a resolved gate", {
      g2 <- dagriculture_add_node(g0, "n1", "source") |> dagriculture_add_node("n2", "process")
      g_gate <- dagriculture_add_edge(g2, "n1", "n2", id = "e1") |>
        dagriculture_add_gate(edge_id = "e1", id = "gate1") |>
        dagriculture_resolve_gate("gate1")

      g_reopened <- dagriculture_reopen_gate(g_gate, "gate1")
      expect_identical(g_reopened$version, g_gate$version + 1L)
      expect_identical(g_reopened$gates[["gate1"]]$status, "pending")
    })
  })
})
