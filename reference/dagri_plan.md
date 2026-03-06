# Create an execution plan

Create an execution plan

## Usage

``` r
dagri_plan(graph, targets = NULL, external_holds = list())
```

## Arguments

- graph:

  A `dagri_graph`.

- targets:

  Optional target nodes.

- external_holds:

  Optional named list mapping node ids to external hold reason strings.
  These affect planning output without mutating graph state.
