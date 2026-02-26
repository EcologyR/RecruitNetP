# Summary and node-based information of species in loops and simple paths

provides summary and node-based information of number of species
involved in intransitive loops of recruitment depression or simple
indirect recruitment depression linear paths from (or to) any given
species.

## Usage

``` r
topol_depre(int_data, cover_data, direction = c("in", "out"))
```

## Arguments

- int_data:

  data frame containing interaction data.

- cover_data:

  data frame with the abundance of each canopy species in each plot.

- direction:

  direction of teh links, in or out

## Value

A list with two elements, one with the information of intransitive loops
of recruitment depression and another with information of the simple
indirect recruitment depression linear paths beginning of ending in each
species.

The first element of the list **loops** provides the following
information within two levels:

- **summary**: A data frame with as many rows as SCC are present in the
  recruitment depression network (with more than one species, as
  autodepression is not considered) and in columns, an scc identifier
  **scc_id** and the number of species involved in that SCC **n_nodos**

- **nodes**: a list with as many elements as SCCs found, each of them
  including a vector with the name of the species involved in that SCC
  The second element of the list **simple**, provides the following
  information within two levels:

- **summary**: A data frame with as many rows as distinct simple linear
  indirect recruitment depression paths (i.e. their overlap in species
  composition is less than 75%) that begin ("from") or end ("to") in any
  species, including the linear versions of the SCCs. In columns, the
  species in which that distinct linear path begin or end **nodo**, and
  numeric index that identifies the paths within species **path_index**
  and the number of species involved in that path **n_nodes_in_path**
  which includes the focal species, independently of whether there is
  autodepression.

- **nodes**: a list with as many elements as species in the recruitment
  depression network (either as depresor or depressed), each of them
  including as many elements as distinct paths begin or ends in it, and
  in each of them, a vector with the name of the species involved in
  that simple indirect recruitment depression linear paths. All
  arguments (options)\*\*:

- **direction** = c("in","out") Argument 1.

- **direction**: Indicates the direction in which the indirect
  recruitment depression is assessed. Explanation of its options:

- **in**: Estimates the paths based on the incoming links to each node,
  representing the set of canopy species that affect the recruitment of
  a given species through depressive effects, either directly (by
  depressing its recruitment) or indirectly (by depressing the
  recruitment of other canopy species connected to it through
  recruitment depression interactions). Link direction is considered for
  both reciprocal and simple paths.

- **out**: Estimates the paths based on the outgoing links from each
  nodes, representing the set of species that canopy species affect
  through depressive effects, either directly or indirectly (i.e.
  through other intermediate species). The direction of the links is
  applied to both reciprocal and simple paths

## Examples

``` r
topol_depre(test_data$com,test_data$cov, direction="out")
#> $loops
#> $loops$summary
#>   scc_id n_nodos
#> 3      1       3
#> 
#> $loops$nodes
#> $loops$nodes$`3`
#> [1] "C" "E" "F"
#> 
#> 
#> 
#> $simple
#> $simple$summary
#>    nodo path_index n_nodes_in_path
#> 1     A          1               5
#> 2     B          1               1
#> 3     C          1               3
#> 4     D          1               2
#> 5     D          2               2
#> 6     E          1               3
#> 7     F          1               3
#> 8     G          1               1
#> 9     H          1               1
#> 10    I          1               3
#> 11    I          2               3
#> 12    J          1               4
#> 13    K          1               1
#> 
#> $simple$nodes
#> $simple$nodes$A
#> $simple$nodes$A[[1]]
#> [1] "A" "J" "I" "D" "B"
#> 
#> 
#> $simple$nodes$B
#> $simple$nodes$B[[1]]
#> [1] "B"
#> 
#> 
#> $simple$nodes$C
#> $simple$nodes$C[[1]]
#> [1] "C" "E" "F"
#> 
#> 
#> $simple$nodes$D
#> $simple$nodes$D[[1]]
#> [1] "D" "B"
#> 
#> $simple$nodes$D[[2]]
#> [1] "D" "H"
#> 
#> 
#> $simple$nodes$E
#> $simple$nodes$E[[1]]
#> [1] "E" "F" "C"
#> 
#> 
#> $simple$nodes$F
#> $simple$nodes$F[[1]]
#> [1] "F" "C" "E"
#> 
#> 
#> $simple$nodes$G
#> $simple$nodes$G[[1]]
#> [1] "G"
#> 
#> 
#> $simple$nodes$H
#> $simple$nodes$H[[1]]
#> [1] "H"
#> 
#> 
#> $simple$nodes$I
#> $simple$nodes$I[[1]]
#> [1] "I" "D" "B"
#> 
#> $simple$nodes$I[[2]]
#> [1] "I" "D" "H"
#> 
#> 
#> $simple$nodes$J
#> $simple$nodes$J[[1]]
#> [1] "J" "I" "D" "B"
#> 
#> 
#> $simple$nodes$K
#> $simple$nodes$K[[1]]
#> [1] "K"
#> 
#> 
#> 
#> 

topol_depre(test_data$com,test_data$cov, direction="in")
#> $loops
#> $loops$summary
#>   scc_id n_nodos
#> 3      1       3
#> 
#> $loops$nodes
#> $loops$nodes$`3`
#> [1] "C" "E" "F"
#> 
#> 
#> 
#> $simple
#> $simple$summary
#>    nodo path_index n_nodes_in_path
#> 1     A          1               1
#> 2     B          1               5
#> 3     C          1               3
#> 4     D          1               4
#> 5     E          1               3
#> 6     F          1               3
#> 7     G          1               1
#> 8     H          1               5
#> 9     I          1               3
#> 10    J          1               2
#> 11    K          1               1
#> 
#> $simple$nodes
#> $simple$nodes$A
#> $simple$nodes$A[[1]]
#> [1] "A"
#> 
#> 
#> $simple$nodes$B
#> $simple$nodes$B[[1]]
#> [1] "B" "D" "I" "J" "A"
#> 
#> 
#> $simple$nodes$C
#> $simple$nodes$C[[1]]
#> [1] "C" "F" "E"
#> 
#> 
#> $simple$nodes$D
#> $simple$nodes$D[[1]]
#> [1] "D" "I" "J" "A"
#> 
#> 
#> $simple$nodes$E
#> $simple$nodes$E[[1]]
#> [1] "E" "C" "F"
#> 
#> 
#> $simple$nodes$F
#> $simple$nodes$F[[1]]
#> [1] "F" "E" "C"
#> 
#> 
#> $simple$nodes$G
#> $simple$nodes$G[[1]]
#> [1] "G"
#> 
#> 
#> $simple$nodes$H
#> $simple$nodes$H[[1]]
#> [1] "H" "D" "I" "J" "A"
#> 
#> 
#> $simple$nodes$I
#> $simple$nodes$I[[1]]
#> [1] "I" "J" "A"
#> 
#> 
#> $simple$nodes$J
#> $simple$nodes$J[[1]]
#> [1] "J" "A"
#> 
#> 
#> $simple$nodes$K
#> $simple$nodes$K[[1]]
#> [1] "K"
#> 
#> 
#> 
#> 

```
