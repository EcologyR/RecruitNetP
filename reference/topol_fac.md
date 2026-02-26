# summary and node-based information of species in loops and simple paths

provides summary and node-based information of number of species
involved in loops of reciprocal facilitation or simple indirect
facilitation linear paths from (or to) any given species.

## Usage

``` r
topol_fac(int_data, cover_data, direction = c("in", "out"))
```

## Arguments

- int_data:

  data frame containing interaction data.

- cover_data:

  data frame with the abundance of each canopy species in each plot.

- direction:

  the direction of the link

## Value

A list with two elements, one with the information of reciprocal
facilitation loops and another with information of the simple linear
indirect facilitation paths beginning of ending in each species.

The first element of the list **loops** provides the following
information within two levels:

- **summary**: A data frame with as many rows as SCC are present in the
  facilitation network (with more than one species, as autofacilitation
  is not considered) and in columns, an scc identifier **scc_id** and
  the number of species involved in that SCC **n_nodos**

- **nodes**: a list with as many elements as SCCs found, each of them
  including a vector with the name of the species involved in that SCC
  The second element of the list **simple**, provides the following
  information within two levels:

- **summary**: A data frame with as many rows as distinct simple linear
  indirect facilitation paths (i.e. their overlap in species composition
  is less than 75%) that begin ("from") or end ("to") in any species,
  including the linear versions of the SCCs. In columns, the species in
  which that distinct linear path begin or end **nodo**, and numeric
  index that identifies the paths within species **path_index** and the
  number of species involved in that path **n_nodes_in_path** which
  includes the focal species, independently of whether there is
  autofacilitation.

- **nodes**: a list with as many elements as species in the facilitation
  network (either as nurse or facilitated), each of them including as
  many elements as distinct paths begin or ends in it, and in each of
  them, a vector with the name of the species involved in that simple
  indirect facilitation linear path. All arguments (options):

- **direction** = c("in","out") Argument 1.

- **direction**: Indicates the direction in which the indirect
  facilitation is assessed. Explanation of its options:

- **in**: Estimates the paths based on the incoming links to each node,
  representing the set of nurse species from which a recruit species
  benefits, either directly (by enhancing its recruitment) or indirectly
  (by facilitating other nurse species that enhance its recruitment).
  Link direction is considered for both reciprocal and simple paths.

- **out**: Estimates the paths based on the outgoing links from each
  nodes, representing the set of species that a nurse species benefits,
  either directly or indirectly (i.e. through other intermediate
  species). The direction of the links is applied to both reciprocal and
  simple paths

## Examples

``` r
topol_fac(test_data$com,test_data$cov, direction="out")
#> $loops
#> $loops$summary
#>   scc_id n_nodos
#> 4      1       3
#> 5      2       3
#> 
#> $loops$nodes
#> $loops$nodes$`4`
#> [1] "E" "H" "K"
#> 
#> $loops$nodes$`5`
#> [1] "B" "I" "J"
#> 
#> 
#> 
#> $simple
#> $simple$summary
#>    nodo path_index n_nodes_in_path
#> 1     B          1               3
#> 2     B          2               3
#> 3     C          1               2
#> 4     D          1               1
#> 5     E          1               5
#> 6     E          2               3
#> 7     G          1               1
#> 8     H          1               4
#> 9     H          2               4
#> 10    H          3               3
#> 11    I          1               5
#> 12    J          1               4
#> 13    J          2               3
#> 14    K          1               6
#> 15    A          1               4
#> 16    A          2               4
#> 17    F          1               2
#> 
#> $simple$nodes
#> $simple$nodes$B
#> $simple$nodes$B[[1]]
#> [1] "B" "C" "D"
#> 
#> $simple$nodes$B[[2]]
#> [1] "B" "I" "J"
#> 
#> 
#> $simple$nodes$C
#> $simple$nodes$C[[1]]
#> [1] "C" "D"
#> 
#> 
#> $simple$nodes$D
#> $simple$nodes$D[[1]]
#> [1] "D"
#> 
#> 
#> $simple$nodes$E
#> $simple$nodes$E[[1]]
#> [1] "E" "H" "B" "C" "D"
#> 
#> $simple$nodes$E[[2]]
#> [1] "E" "H" "K"
#> 
#> 
#> $simple$nodes$G
#> $simple$nodes$G[[1]]
#> [1] "G"
#> 
#> 
#> $simple$nodes$H
#> $simple$nodes$H[[1]]
#> [1] "H" "B" "C" "D"
#> 
#> $simple$nodes$H[[2]]
#> [1] "H" "B" "I" "J"
#> 
#> $simple$nodes$H[[3]]
#> [1] "H" "K" "E"
#> 
#> 
#> $simple$nodes$I
#> $simple$nodes$I[[1]]
#> [1] "I" "J" "B" "C" "D"
#> 
#> 
#> $simple$nodes$J
#> $simple$nodes$J[[1]]
#> [1] "J" "B" "C" "D"
#> 
#> $simple$nodes$J[[2]]
#> [1] "J" "B" "I"
#> 
#> 
#> $simple$nodes$K
#> $simple$nodes$K[[1]]
#> [1] "K" "E" "H" "B" "C" "D"
#> 
#> 
#> $simple$nodes$A
#> $simple$nodes$A[[1]]
#> [1] "A" "B" "C" "D"
#> 
#> $simple$nodes$A[[2]]
#> [1] "A" "B" "I" "J"
#> 
#> 
#> $simple$nodes$F
#> $simple$nodes$F[[1]]
#> [1] "F" "G"
#> 
#> 
#> 
#> 

topol_fac(test_data$com,test_data$cov, direction="in")
#> $loops
#> $loops$summary
#>   scc_id n_nodos
#> 4      1       3
#> 5      2       3
#> 
#> $loops$nodes
#> $loops$nodes$`4`
#> [1] "E" "H" "K"
#> 
#> $loops$nodes$`5`
#> [1] "B" "I" "J"
#> 
#> 
#> 
#> $simple
#> $simple$summary
#>    nodo path_index n_nodes_in_path
#> 1     B          1               4
#> 2     B          2               3
#> 3     B          3               2
#> 4     C          1               5
#> 5     C          2               4
#> 6     C          3               3
#> 7     D          1               6
#> 8     E          1               3
#> 9     G          1               2
#> 10    H          1               3
#> 11    I          1               5
#> 12    I          2               3
#> 13    I          3               3
#> 14    J          1               6
#> 15    K          1               3
#> 16    A          1               1
#> 17    F          1               1
#> 
#> $simple$nodes
#> $simple$nodes$B
#> $simple$nodes$B[[1]]
#> [1] "B" "H" "E" "K"
#> 
#> $simple$nodes$B[[2]]
#> [1] "B" "J" "I"
#> 
#> $simple$nodes$B[[3]]
#> [1] "B" "A"
#> 
#> 
#> $simple$nodes$C
#> $simple$nodes$C[[1]]
#> [1] "C" "B" "H" "E" "K"
#> 
#> $simple$nodes$C[[2]]
#> [1] "C" "B" "J" "I"
#> 
#> $simple$nodes$C[[3]]
#> [1] "C" "B" "A"
#> 
#> 
#> $simple$nodes$D
#> $simple$nodes$D[[1]]
#> [1] "D" "C" "B" "H" "E" "K"
#> 
#> 
#> $simple$nodes$E
#> $simple$nodes$E[[1]]
#> [1] "E" "K" "H"
#> 
#> 
#> $simple$nodes$G
#> $simple$nodes$G[[1]]
#> [1] "G" "F"
#> 
#> 
#> $simple$nodes$H
#> $simple$nodes$H[[1]]
#> [1] "H" "E" "K"
#> 
#> 
#> $simple$nodes$I
#> $simple$nodes$I[[1]]
#> [1] "I" "B" "H" "E" "K"
#> 
#> $simple$nodes$I[[2]]
#> [1] "I" "B" "J"
#> 
#> $simple$nodes$I[[3]]
#> [1] "I" "B" "A"
#> 
#> 
#> $simple$nodes$J
#> $simple$nodes$J[[1]]
#> [1] "J" "I" "B" "H" "E" "K"
#> 
#> 
#> $simple$nodes$K
#> $simple$nodes$K[[1]]
#> [1] "K" "H" "E"
#> 
#> 
#> $simple$nodes$A
#> $simple$nodes$A[[1]]
#> [1] "A"
#> 
#> 
#> $simple$nodes$F
#> $simple$nodes$F[[1]]
#> [1] "F"
#> 
#> 
#> 
#> 

```
