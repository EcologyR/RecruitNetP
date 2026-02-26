# Check interaction data format

A canopy-recruit interactions data set must contain, at least, the raw
data of the observed frequency of each canopy-recruit pair in each
sampling unit (e.g. plot, quadrat, transect). For standardization, the
columns in this data set must be named:

- **Plot**. Plot ID. Uniquely identifies each of the sampling units over
  which the interactions have been surveyed. Include this column even if
  you only surveyed one plot.

- **Canopy**. Name of the canopy species. If your survey included any
  type of open interspaces (e.g., canopy gaps, open ground, spaces away
  from a canopy plant), include these as a single node named "Open"
  (note the capital "O"). If you use full scientific names (e.g. latin
  binomials), concatenate the epithets with a lower dash (e.g.
  Olea_europaea).

- **Recruit**. Name of the recruit species. If you use full scientific
  names, concatenate the epithets with a lower dash (e.g.
  Olea_europaea).

- **Frequency**. Frequency of the canopy-recruit interaction in the
  study plot, indicated as number of individuals of the recruit species
  found under individuals of the canopy species.

## Usage

``` r
check_interactions(int_data = NULL)
```

## Arguments

- int_data:

  data frame containing interaction data.

## Value

The function will return error(s) if problems are detected. Otherwise an
OK message.

## Examples

``` r
int_data <- load_interactions()
check_interactions(int_data)
#> Format OK!
```
