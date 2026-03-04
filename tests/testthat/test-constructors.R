describe("dagriculture_kind()", {
  it("creates a valid kind record with correct shape", {
    kind <- dagriculture_kind(
      name = "data_source",
      output_type = "data.frame",
      param_schema = list(required = c("path"))
    )
    expect_type(kind, "list")
    expect_identical(kind$name, "data_source")
    expect_identical(kind$output_type, "data.frame")
    expect_identical(kind$param_schema, list(required = c("path")))
    expect_identical(
      names(kind),
      c("name", "input_contract", "output_type", "param_schema", "metadata")
    )
  })

  it("rejects executable closures in param_schema", {
    expect_error(
      dagriculture_kind("bad", param_schema = list(fn = function() 1)),
      class = "dagriculture_error_invalid_argument"
    )
  })
})

describe("dagriculture_registry()", {
  it("creates a registry from kinds", {
    kind1 <- dagriculture_kind("source")
    kind2 <- dagriculture_kind("fit")
    reg <- dagriculture_registry(kind1, kind2)

    expect_type(reg, "list")
    expect_identical(names(reg), c("kinds", "metadata"))
    expect_identical(names(reg$kinds), c("source", "fit"))
  })
})

describe("dagriculture_graph()", {
  it("creates an empty graph with version 0 and required fields", {
    reg <- dagriculture_registry(dagriculture_kind("test"))
    graph <- dagriculture_graph(reg)

    expect_type(graph, "list")
    expect_identical(graph$version, 0L)
    expect_identical(names(graph$nodes), character(0))
    expect_identical(names(graph$edges), character(0))
    expect_identical(names(graph$gates), character(0))
    expect_identical(names(graph), c("registry", "nodes", "edges", "gates", "version", "metadata"))
  })
})
