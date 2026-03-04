
<!-- README.md is generated from README.Rmd. Re-knit after editing. -->

# dagriculture

<!-- badges: start -->

[![R-CMD-check](https://github.com/sims1253/dagriculture/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sims1253/dagriculture/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/sims1253/dagriculture/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/sims1253/dagriculture/actions/workflows/pkgdown.yaml)
[![Codecov test
coverage](https://codecov.io/gh/sims1253/dagriculture/branch/main/graph/badge.svg)](https://app.codecov.io/gh/sims1253/dagriculture)
<!-- badges: end -->

`dagriculture` is a pure, value-oriented graph library for R. It is designed
to manage graph structures, topological dependencies, structural
validity, and planning, specifically without coupling itself to external
state or runtime execution engines.

## Installation

``` r
# install.packages("pak")
pak::pak("sims1253/dagriculture")
```

## Example

`dagriculture` provides a purely functional API to build graphs, establish
dependencies, and track structural blockers such as “gates”:

``` r
library(dagriculture)

# 1. Define the kinds of nodes allowed in your graph
registry <- dagriculture_registry(
  dagriculture_kind("data_source"),
  dagriculture_kind("transform")
)

# 2. Create an empty graph
graph <- dagriculture_graph(registry)

# 3. Add nodes and edges
graph <- graph |>
  dagriculture_add_node("raw_data", "data_source") |>
  dagriculture_add_node("cleaned_data", "transform") |>
  dagriculture_add_edge(from = "raw_data", to = "cleaned_data", id = "edge_1")

# 4. Add a structural blocker (gate) to an edge
graph <- dagriculture_add_gate(graph, edge_id = "edge_1", id = "review_gate")

# 5. Compute the structural state
graph <- dagriculture_recompute_state(graph)

# See which nodes are eligible to proceed
dagriculture_eligible(graph)
```

    ## [1] "raw_data"

``` r
# See which nodes are blocked, and why
dagriculture_blocked(graph)
```

    ## $cleaned_data
    ## [1] "gate"

``` r
# 6. Resolve the gate to unblock the downstream node
graph <- dagriculture_resolve_gate(graph, "review_gate") |>
  dagriculture_recompute_state()

dagriculture_eligible(graph)
```

    ## [1] "raw_data"     "cleaned_data"
