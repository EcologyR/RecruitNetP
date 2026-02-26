# visualize the functional topology of general recruitment networks

visualize the functional topology of general recruitment networks

## Usage

``` r
visu_funtopol_rec(int_data, cover_data)
```

## Arguments

- int_data:

  data frame containing interaction data.

- cover_data:

  data frame with the abundance of each canopy species in each plot.

## Value

a plot with teh different species role

## Examples

``` r
visu_funtopol_rec(Amoladeras_int,Amoladeras_cover)
#> Different tests were used for different canopy-recruit pairs.
#>               Check column Test_type
#> Error in visNetwork::visIgraphLayout(layout = "layout_with_fr"): argument "graph" is missing, with no default
```
