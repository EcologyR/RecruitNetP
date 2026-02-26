# Calculate width of canopy service and recruitment niches

Calculate width of canopy service and recruitment niches

## Usage

``` r
node_degrees(int_data, cover_data, int_type = c("rec", "fac", "comp"))
```

## Arguments

- int_data:

  data frame containing interaction data.

- cover_data:

  data frame with the abundance of each canopy species in each plot.

- int_type:

  Indicates the type of plant-plant interaction that will be presented
  in the output matrix: general recruitment, recruitment enhancement
  (i.e. facilitation) or recruitment depression (i.e. competition). It
  may take three possible values:

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

## Value

The output depends on the type of interaction considered. For the
recruitment networks it returns a single data frame with the following
information:

- *Node*: plant species (either as canopy or recruit)

- *Ac*: Area of the canopy species (and "Open" interspaces), measured as
  the distance (m, in transects) or area (m^2^, in plots) used to
  estimate canopy cover in the study site.

- *canopy_service_width*: the number of species that recruit under a
  given canopy species (i.e., the canopy species' out-degree); this can
  be interpreted as the width of its canopy service.

- *canopy_contribution*: number of recruits of any species associated
  with the canopy species. When the degrees are weighted by the
  frequency of recruitment (Fcr), then it can be interpreted as the
  contribution of the canopy species to the multispecific sapling bank.

- *effective_canopy_service*: The weighted degrees are transformed to
  the effective number of partners, accounting for the dominance of
  certain interactions in the recruit bank. The effective canopy service
  is estimated as e^H, where *H* is Shannon's index of diversity
  calculated for a canopy species from Fcr.

- *recruitment_niche_width*: number of canopy species that allow its
  recruitment (the recruit species' in-degree); this can be interpreted
  as the width of its recruitment niche.

- *recruit_bank_abundance*: number of recruits of a species in the study
  site. When the degrees are weighted by the frequency of recruitment
  (Fcr), then it can be interpreted as species abundance in the recruit
  bank.

- *effective_recruitment_niche*: The weighted degrees are transformed to
  the effective number of partners, accounting for the dominance of
  certain interactions in the recruit bank. The effective recruitment
  niche width is estimated as \$e^H\$, where *H* is Shannon's index of
  diversity calculated for a recruit species from Fcr.

For facilitation or recruitment depressing interactions, the output
consists in two data frames, one for the **canopy** species and another
for the **recruit** species. The data frame with information for the
canopy species provides the following information:

- *Nurse (or Canopy) sp*: Canopy species.

- *Ac*: Area of the canopy species (and "Open" interspaces), measured as
  the distance (m, in transects) or area (m^2^, in plots) used to
  estimate canopy cover in the study site.

- *N enhanced (or depressed) recruit sp*: number of species whose
  recruitment is enhanced (or depressed) under its canopy, this can be
  interpreted as the width of its enhancing of (depressing) effect.

The data frame with information for the recruit species contains the
following information:

- *Facilitated (or recruit) sp*: Recruit species.

- *N_ind*: number of recruits of the facilitated (or depressed) species
  in the study site.

- *N enhancing (or depressing) canopy sp*: number of canopy species that
  enhance its recruitment (i.e. nurse species) or depress it (i.e.
  competing species). These can be interpreted as the width of its niche
  of nurses or competitors.

## Examples

``` r
out <- node_degrees(Amoladeras_int, Amoladeras_cover, int_type="rec")
head(out)
#>                   Node      Ac canopy_service_width canopy_contribution
#> 1 Artemisia_barrelieri  1.0000                    5                  18
#> 2 Artemisia_campestris  0.1100                    0                   0
#> 3      Asparagus_albus 27.7250                    7                  14
#> 4   Asparagus_horridus  1.5725                    3                   6
#> 5      Ballota_hirsuta 38.4550                    7                  31
#> 6 Helichrysum_stoechas 29.0700                    9                  62
#>   effective_canopy_service recruitment_niche_width recruit_bank_abundance
#> 1                 3.851794                       7                     90
#> 2                 1.000000                       3                      6
#> 3                 6.107480                       7                     25
#> 4                 2.381102                      11                     53
#> 5                 4.992495                       9                     82
#> 6                 6.827787                      12                   1055
#>   effective_recruitment_niche
#> 1                    3.152785
#> 2                    2.749459
#> 3                    5.910236
#> 4                    7.855732
#> 5                    5.461981
#> 6                    2.754019
```
