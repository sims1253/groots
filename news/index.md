# Changelog

## dagriculture 0.1.0

### Features

- **Constructors**: Functions to create and define kinds, registries,
  and the core graph structure
  ([`dagri_kind()`](https://sims1253.github.io/dagriculture/reference/dagri_kind.md),
  [`dagri_registry()`](https://sims1253.github.io/dagriculture/reference/dagri_registry.md),
  [`dagri_graph()`](https://sims1253.github.io/dagriculture/reference/dagri_graph.md)).
- **Graph Editing**: Implemented structural modifiers to
  add/remove/update nodes, edges, and explicit blockers called gates.
- **Topology Queries**: Topological sorting and traversal utilities
  (ancestors, descendants, upstream, downstream, roots, leaves,
  reachability).
- **Structural Planning**: Determine node eligibility and blocked
  reasons through declarative resolution without side-effects or
  executing runtime jobs
  ([`dagri_recompute_state()`](https://sims1253.github.io/dagriculture/reference/dagri_recompute_state.md),
  [`dagri_plan()`](https://sims1253.github.io/dagriculture/reference/dagri_plan.md)).

### Internal

- All structures fully align with the plain-data spec and are serialized
  purely as nested named lists (JSON compatible).
- Defined explicit typed errors in
  [`abort_dagri()`](https://sims1253.github.io/dagriculture/reference/abort_dagri.md).
- Removed `hello()` template code.

## dagriculture 0.0.0.9000

- Initial development version.
