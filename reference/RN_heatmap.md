# Visualize a heatmap of the interactions matrix.

Visualize a heatmap of the interactions matrix.

## Usage

``` r
RN_heatmap(
  int_data,
  cover_data,
  int_type = c("rec", "fac", "comp"),
  weight = c("Pcr", "Fcr", "Dcr", "Dro", "Ns", "NintC", "NintA", "RII")
)
```

## Arguments

- int_data:

  data frame containing interaction data.

- cover_data:

  data frame with the abundance of each canopy species in each plot.

- int_type:

  \#' Indicates the type of plant-plant interaction that will be
  presented in the output matrix: general recruitment, recruitment
  enhancement (i.e. facilitation) or recruitment depression (i.e.
  competition). It may take three possible values:

  - *rec*: Estimates the node degree based on all plant-plant
    interaction that contribute to recruitment.

  - *fac*: Estimates the node degree based on only those pairwise
    interactions that enhance recruitment. Not every observed
    interaction in the field has to be present in the matrix. In this
    case non-detected interactions are not considered and "Open" is not
    included as a canopy species category.

  - *comp*: Estimates the node degree based on only those pairwise
    interactions that depress recruitment. Not every observed
    interaction in the field has to be present in the matrix. However,
    in this case non-detected interactions are considered (i.e.
    expanding with 0 all possible interactions in the study system).
    "Open" is not included as a canopy species category.

- weight:

  Specifies the metric used to represent interaction strength (i.e., the
  weight) assigned to each pair of species in the matrix. See function
  **associndex** for further details. The possible options are:

  - *Fcr*: **frequency of recruitment** in number of recruits by
    canopy-recruit pair.

  - *Dcr*: **density of recruitment** as number of recruits per unit
    area of canopy species.

  - *Ns*: The index **Normalized Neighbour Suitability index** (proposed
    by Mingo, 2014), suitable for comparisons of interaction strength
    between pairs of species within a local community, which should be
    preferred in general recruitment networks (Alcantara et al. 2025).

  - *NIntA*: The index **additive symmetry intensity index** proposed by
    Diaz-Sierra et al. (2017).

  - *NIntC*: The index **commutative symmetry intensity index** proposed
    by Diaz-Sierra et al. (2017).

  - *RII*: The index **Relative Interaction Index** (Armas et al.,
    2004).

## Value

Heatmap plot of the weighted network.

## Examples

``` r
RN_heatmap(Amoladeras_int, Amoladeras_cover, int_type="fac", weight ="Ns")

```
