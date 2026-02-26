# Allow the visualization of intransitivity loops of recruitment depression

with nodes in different SCC identified by different colors (in grey if
they do not belong to any SCC). It may be frequent that in recruitment
enhancement (i.e. facilitation) or depression (i.e.competition)
networks, there is not any SCC as they tend to have smaller dimensions
that general recruitment networks.

## Usage

``` r
visu_topol_depre(
  int_data,
  cover_data,
  layout_fun = igraph::layout_with_fr,
  vertex_size = 20,
  edge_arrow_size = 0.4
)
```

## Arguments

- int_data:

  data frame containing interaction data.

- cover_data:

  data frame with the abundance of each canopy species in each plot.

- layout_fun:

  type of igraph layout (see
  [`igraph::layout_()`](https://r.igraph.org/reference/layout_.html))

- vertex_size:

  numeric

- edge_arrow_size:

  numeric

## Value

a plot

## Examples

``` r
visu_topol_depre(test_data$com,test_data$cov)

```
