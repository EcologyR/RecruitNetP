# Combine cover data with interaction data.

Combine interactions and cover data sets from a study site into a single
data frame. For study sites where data has been collected in several
plots, this function collapses the information into a single value per
pairwise interaction. You can choose whether or not the output contains
all possible pairs of interacting species (including those that were not
observed, which are assigned weight = 0) and whether or not to remove
species lacking information on canopy cover.

## Usage

``` r
comm_to_RN(
  int_data,
  cover_data,
  expand = c("yes", "no"),
  rm_sp_no_cover = c("allsp", "onlycanopy")
)
```

## Arguments

- int_data:

  data frame containing interaction data.

- cover_data:

  data frame with the abundance of each canopy species in each plot.

- expand:

  Indicates whether to expand the dataframe to include the non-detected
  interactions between every pair of species that occurs in the study
  site, assigning values of 0 to their frequency. It can take two
  possible values:

  - *yes*: Generates all possible interactions between canopy and
    recruit species in the study site (i.e. detected and non-detected),
    adding values of interaction strength of 0 to the non-detected
    interactions.

  - *no*: Considers only the pairwise interactions that were actually
    detected in the study site.

- rm_sp_no_cover:

  Defines the filtering behavior, specifying which species should be
  removed based on the availability of cover data. It can take two
  options:

  - *allsp*: removes all species lacking cover data (both canopy and
    recruit species).

  - *onlycanopy*: removes only canopy species lacking cover data,
    retaining recruit species even if they lack cover data.

## Value

Data frame with the following information for each pair of species:

- *Canopy*. Name of the canopy species, including "Open" interspaces. If
  you use full scientific names, concatenate the epithets with a lower
  dash (e.g. Olea_europaea).

- *Recruit*. Name of the recruit species. If you use full scientific
  names, concatenate the epithets with a lower dash (e.g.
  Olea_europaea).

- *Ac*: Area (in m^2^), (or m when relative cover is measured with
  transects) occupied by the canopy species (or "Open" interspaces) in
  the total area sampled in the study site.

- *Ar*. Area (in m^2^), (or m when relative cover is measured with
  transects) occupied by the recruit species (observed as adults), in
  the total area sampled in the study site.

- *Fcr*: frequency of recruitment, in number of recruits by
  canopy-recruit pair.

- *Icr*: incidence of the interaction, as number of plots where it was
  observed.

- *Pcr*: presence/absence (1/0) of the interaction in the study site.
  This provides the "unweighted" version of the adjacency matrix.

## Examples

``` r
df <- comm_to_RN(Amoladeras_int, Amoladeras_cover, expand="yes",
rm_sp_no_cover="allsp")
head(df)
#>                  Canopy              Recruit Fcr Icr Pcr Ac      Ar
#> 1  Artemisia_barrelieri Artemisia_barrelieri   3   1   1  1  1.0000
#> 22 Artemisia_barrelieri Artemisia_campestris   0   0   0  1  0.1100
#> 20 Artemisia_barrelieri      Asparagus_albus   0   0   0  1 27.7250
#> 16 Artemisia_barrelieri   Asparagus_horridus   0   0   0  1  1.5725
#> 12 Artemisia_barrelieri      Ballota_hirsuta   0   0   0  1 38.4550
#> 13 Artemisia_barrelieri Helichrysum_stoechas   3   1   1  1 29.0700
```
