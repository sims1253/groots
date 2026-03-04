describe("graph querying and topology", {
  reg <- dagri_registry(dagri_kind("a"), dagri_kind("b"))
  g <- dagri_graph(reg) |>
    dagri_add_node("n1", "a") |>
    dagri_add_node("n2", "b") |>
    dagri_add_node("n3", "b") |>
    dagri_add_node("n4", "b") |>
    dagri_add_edge("n1", "n2", id = "e1") |>
    dagri_add_edge("n2", "n3", id = "e2") |>
    dagri_add_edge("n1", "n4", id = "e3") |>
    dagri_add_gate("e2", id = "g1")

  describe("node and edge accessors", {
    it("dagri_node() retrieves a node", {
      expect_identical(dagri_node(g, "n1")$id, "n1")
    })

    it("dagri_node() errors if missing", {
      expect_error(dagri_node(g, "missing"), class = "dagri_error_not_found")
    })

    it("dagri_edge() retrieves an edge", {
      expect_identical(dagri_edge(g, "e1")$id, "e1")
    })

    it("dagri_gate() retrieves a gate", {
      expect_identical(dagri_gate(g, "g1")$id, "g1")
    })

    it("plural accessors retrieve lists", {
      expect_length(dagri_nodes(g), 4)
      expect_length(dagri_edges(g), 3)
      expect_length(dagri_gates(g), 1)
    })
  })

  describe("topology functions", {
    it("dagri_upstream() and downstream() return immediate neighbors", {
      expect_setequal(dagri_upstream(g, "n3"), "n2")
      expect_setequal(dagri_downstream(g, "n1"), c("n2", "n4"))
    })

    it("dagri_ancestors() and descendants() traverse the full graph", {
      expect_setequal(dagri_ancestors(g, "n3"), c("n1", "n2"))
      expect_setequal(dagri_descendants(g, "n1"), c("n2", "n3", "n4"))
    })

    it("dagri_roots() and leaves() find endpoints", {
      expect_setequal(dagri_roots(g), "n1")
      expect_setequal(dagri_leaves(g), c("n3", "n4"))
    })

    it("dagri_has_path() correctly identifies reachability", {
      expect_true(dagri_has_path(g, "n1", "n3"))
      expect_false(dagri_has_path(g, "n3", "n1"))
      expect_false(dagri_has_path(g, "n2", "n4"))
    })

    it("dagri_topo_order() returns a valid sorting", {
      order <- dagri_topo_order(g)
      expect_length(order, 4)
      expect_true(which(order == "n1") < which(order == "n2"))
      expect_true(which(order == "n2") < which(order == "n3"))
      expect_true(which(order == "n1") < which(order == "n4"))
    })

    it("dagri_topo_order(subset) returns valid sorting for subset", {
      order <- dagri_topo_order(g, subset = c("n1", "n3"))
      expect_length(order, 2)
      expect_true(which(order == "n1") < which(order == "n3"))
    })
  })
})
