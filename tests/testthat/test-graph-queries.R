describe("graph querying and topology", {
  reg <- dagriculture_registry(dagriculture_kind("a"), dagriculture_kind("b"))
  g <- dagriculture_graph(reg) |>
    dagriculture_add_node("n1", "a") |>
    dagriculture_add_node("n2", "b") |>
    dagriculture_add_node("n3", "b") |>
    dagriculture_add_node("n4", "b") |>
    dagriculture_add_edge("n1", "n2", id = "e1") |>
    dagriculture_add_edge("n2", "n3", id = "e2") |>
    dagriculture_add_edge("n1", "n4", id = "e3") |>
    dagriculture_add_gate("e2", id = "g1")

  describe("node and edge accessors", {
    it("dagriculture_node() retrieves a node", {
      expect_identical(dagriculture_node(g, "n1")$id, "n1")
    })

    it("dagriculture_node() errors if missing", {
      expect_error(dagriculture_node(g, "missing"), class = "dagriculture_error_not_found")
    })

    it("dagriculture_edge() retrieves an edge", {
      expect_identical(dagriculture_edge(g, "e1")$id, "e1")
    })

    it("dagriculture_gate() retrieves a gate", {
      expect_identical(dagriculture_gate(g, "g1")$id, "g1")
    })

    it("plural accessors retrieve lists", {
      expect_length(dagriculture_nodes(g), 4)
      expect_length(dagriculture_edges(g), 3)
      expect_length(dagriculture_gates(g), 1)
    })
  })

  describe("topology functions", {
    it("dagriculture_upstream() and downstream() return immediate neighbors", {
      expect_setequal(dagriculture_upstream(g, "n3"), "n2")
      expect_setequal(dagriculture_downstream(g, "n1"), c("n2", "n4"))
    })

    it("dagriculture_ancestors() and descendants() traverse the full graph", {
      expect_setequal(dagriculture_ancestors(g, "n3"), c("n1", "n2"))
      expect_setequal(dagriculture_descendants(g, "n1"), c("n2", "n3", "n4"))
    })

    it("dagriculture_roots() and leaves() find endpoints", {
      expect_setequal(dagriculture_roots(g), "n1")
      expect_setequal(dagriculture_leaves(g), c("n3", "n4"))
    })

    it("dagriculture_has_path() correctly identifies reachability", {
      expect_true(dagriculture_has_path(g, "n1", "n3"))
      expect_false(dagriculture_has_path(g, "n3", "n1"))
      expect_false(dagriculture_has_path(g, "n2", "n4"))
    })

    it("dagriculture_topo_order() returns a valid sorting", {
      order <- dagriculture_topo_order(g)
      expect_length(order, 4)
      expect_true(which(order == "n1") < which(order == "n2"))
      expect_true(which(order == "n2") < which(order == "n3"))
      expect_true(which(order == "n1") < which(order == "n4"))
    })

    it("dagriculture_topo_order(subset) returns valid sorting for subset", {
      order <- dagriculture_topo_order(g, subset = c("n1", "n3"))
      expect_length(order, 2)
      expect_true(which(order == "n1") < which(order == "n3"))
    })
  })
})
