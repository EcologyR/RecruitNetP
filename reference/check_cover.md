# Check cover data format

The **canopy cover data set** contains the abundance of each canopy
species in each plot. The minimum columns required are:

- **Plot**. As in the interactions data set.

- **Canopy**. As in the interactions data set.

- **Cover**. Cover of the canopy species (and "Open" interspaces),
  measured as % of the total area sampled where a recruit would be
  ascribed to the interaction with the canopy plant (or to be recruiting
  in "Open", away from established plants). For example, following
  Alcantara et al. (2019), in plants with branches less than 1.5m above
  ground (e.g. small shrubs and treelets), it would be the area of
  projection of their canopy on the ground, while in tall trees it can
  be the area extending 0.5m around the trunk's base or large surfacing
  roots.

- **Sampled_distance_or_area**. Total area of each plot or distance of
  each transect (in m^2^ or m, respectively).

## Usage

``` r
check_cover(cover_data = NULL)
```

## Arguments

- cover_data:

  data frame with the abundance of each canopy species in each plot.

## Value

The function will return error(s) if problems are detected. Otherwise an
OK message.

## Examples

``` r
check_cover(Amoladeras_cover)
#> Format OK!
```
