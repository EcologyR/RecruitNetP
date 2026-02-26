# Load canopy cover data from RecruitNet database

Load canopy cover data from RecruitNet database

## Usage

``` r
load_cover(path = getwd())
```

## Arguments

- path:

  Path to folder containing the 'CanopyCover.csv' file. If the file is
  not present, the latest version of the RecruitNet database (Verdú et
  al. 2023, [doi:10.1002/ecy.3923](https://doi.org/10.1002/ecy.3923) )
  will be downloaded automatically.

## Value

a data frame containing canopy cover data

## Examples

``` r
cover_data <- load_cover()
```
