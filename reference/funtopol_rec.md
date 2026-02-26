# Species functional role in the general recruitment network

provides summary information of the frequency of species of each
functional role and the classification of species in each role.

## Usage

``` r
funtopol_rec(int_data, cover_data)
```

## Arguments

- int_data:

  data frame containing interaction data.

- cover_data:

  data frame with the abundance of each canopy species in each plot.

## Value

A list with two elements, one with descriptors of the networks and
another with the role of each species.

The first element of the list provides the following information:

- *Num. nodes*: Total number of nodes in the network.

- *Num. edges*: Total number of links in the network.

- *Connectance*: Proportion of links observed relative to all the
  possible links (*C*). In the case of recruitment networks, we use the
  formula \$C = L /(N^2 - N)\$ since the node "Open" does not act as a
  recruit (i.e. Open is represented by a row of zeroes in the adjacency
  matrix).

- *Num. non-trivial SCCs*: Number of SCCs formed by more than one node
  in the network.

- *Num. core species*: Number of species in the core SCC.

- *Prop. core species*: Proportion of species in the core relative to
  the total number of species in the network.

- *Num. satellite species*: Number of satellite species in the network.

- *Prop. satellite species*: Proportion of satellite species relative to
  the total number of species in the network.

- *Num. disturbance-dependent transients*: Number of
  disturbance-dependent transient species in the network.

- *Prop. disturbance-dependent transients*: Proportion of
  disturbance-dependent transient species relative to the total number
  of species in the network.

- *Num. strict transients*: Number of strict transient species in the
  network.

- *Prop. strict transients*: Proportion of strict transient species
  relative to the total number of species in the network.

- *Qualitative Persistence*: The sum of the proportion of core and
  satellite species. The second element of the list, provides the role
  for each plant species in the network, whose interpretations differ in
  each plant-plant interaction type. Each node (i.e. species) can be:

- Part of the core **core**. In the case of *general recruitment
  networks*, the direction of the links represents that the space
  occupied by canopy species (predecessor node) will be replaced by the
  species recruiting under its canopy (successor node). In this case,
  all species in the core must recruit, at least, under the canopy of
  another core species, and allow the recruitment of at least another
  core species.

- A **satellite** species: these are non-core species that can be
  reached from some core species, following the direction of the arrows.
  In *general recruitment networks*, these are species that recruit
  under the canopy of some core species but that do not show recruitment
  of any core species under their canopy.

- A **disturbance-dependent transient** species: these are species that
  can be reached from the "Open" node but not from core or satellite
  species (i.e. for example, pioneer species that only recruit away from
  established plants).

- A **strict transient** species: a species that cannot be reached from
  any other node. In the case of *general recruitment networks* strict
  transients are those species that do not recruit in the studied local
  assemblage (e.g., a pioneer species in a mature forest patch or a
  relict species at it distribution limit).

## Examples

``` r
funtopol_rec(Amoladeras_int,Amoladeras_cover)
#> $Descriptors
#>                                               Value
#> Num. nodes                              24.00000000
#> Num. edges                             221.00000000
#> Connectance                              0.40036232
#> Num. non-trivial SCCs                    1.00000000
#> Num. core species                       19.00000000
#> Prop. core species                       0.82608696
#> Num. satellite species                   2.00000000
#> Prop. satellite species                  0.08695652
#> Num. disturbance-dependent transients    2.00000000
#> Prop. disturbance-dependent transients   0.08695652
#> Num. strict transients                   0.00000000
#> Prop. strict transients                  0.00000000
#> Qualitative Persistence                  0.91304348
#> 
#> $Functional_classification
#>                       id                            group
#> 3   Artemisia_barrelieri                             Core
#> 1   Artemisia_campestris                        Satellite
#> 4        Asparagus_albus                             Core
#> 5     Asparagus_horridus                             Core
#> 6        Ballota_hirsuta                             Core
#> 7   Helichrysum_stoechas                             Core
#> 8      Hyparrhenia_hirta                             Core
#> 9    Launaea_arborescens                             Core
#> 2       Launaea_lanifera                        Satellite
#> 10    Lycium_intrincatum                             Core
#> 11        Lygeum_spartum                             Core
#> 12 Maytenus_senegalensis                             Core
#> 13         Ononis_natrix                             Core
#> 14    Phagnalon_saxatile                             Core
#> 15 Piptatherum_miliaceum                             Core
#> 16 Salsola_oppositifolia                             Core
#> 23     Stipa_tenacissima Disturbance_dependent_transients
#> 17  Teucrium_lusitanicum                             Core
#> 18       Teucrium_polium                             Core
#> 19     Thymelaea_hirsuta                             Core
#> 20       Thymus_hyemalis                             Core
#> 21   Whitania_frutescens                             Core
#> 22        Ziziphus_lotus Disturbance_dependent_transients
#> 


```
