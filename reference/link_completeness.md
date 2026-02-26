# Estimates the completeness and coverage of the sampled interactions

Estimates the completeness and coverage of the sampled interactions,
providing the probability of detecting a new canopy-recruit pair if we
increased our sampling effort in one unit (i.e., locating one more
recruit or surveying one more plot). This function uses
[`iNEXT::iNEXT()`](https://rdrr.io/pkg/iNEXT/man/iNEXT.html) (Hsieh et
al. 2016).

## Usage

``` r
link_completeness(int_data = NULL, type = c("incidence", "abundance"))
```

## Arguments

- int_data:

  data frame containing interaction data.

- type:

  indicates whether the completeness and coverage will be calculated
  based on the number of individual recruits under each canopy species
  (i.e., abundance) or on the presence/absence of canopy-recruit pairs
  across plots (i.e., incidence). If the survey was based in multiple
  independent plots, it is good practice to use the "incidence"
  approach. The "abundance" approach can be used more generally, but it
  assumes that each recruit represents an independent canopy-recruit
  interaction event, so it may easily overestimate coverage when
  recruits of the same species tend to occur aggregated under the same
  canopy plant (as it occurs frequently). Explanation of its options:

  - **incidence**: Uses the incidence of a given interaction as number
    of plots where it was observed.

  - **abundance**: Uses the abundance of a given interaction as the
    number of recruits by canopy-recruit.

## Value

A data frame indicating the number of links observed (Lobs), the number
of links estimated (Lest), the completeness of the sampling of links as
the proportion of canopy-recruit pairs observed from those expected in
the study site (Completeness of links: Lobs/Lest) and the probability
that if one more recruit (or plot) had been sampled, it would have
corresponded to (or contained) an already detected canopy-recruit
species interaction (Coverage of links).

## Examples

``` r
link_completeness(Amoladeras_int, type="abundance")
#> Warning: Abundance-based approach assumes that each individual recruit provides independent data about the canopy-recruit interaction. If conspecific recruits frequently occur aggregated under individual canopy plants, the estimates of completeness and coverage may be severely overestimated.
#> Warning: Your data is structured in multiple plots. Incidence-based approach is recommended.
#>                                Abundance based estimate
#> Num. plants (recruits) sampled             7395.0000000
#> Lobs                                        229.0000000
#> Lest                                        265.1558244
#> Completeness of links (q=0)                   0.8636431
#> Coverage of links (q=1)                       0.9939000
link_completeness(Amoladeras_int, type="incidence")
#>                          Incidence based estimate
#> Num. Plots sampled                     20.0000000
#> Lobs                                  229.0000000
#> Lest                                  315.5687500
#> Completeness Links (q=0)                0.7256739
#> Coverage Links (q=1)                    0.9061000
```
