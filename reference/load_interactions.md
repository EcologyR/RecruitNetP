# Load interaction data from RecruitNet database

Load interaction data from RecruitNet database

## Usage

``` r
load_interactions(path = getwd())
```

## Arguments

- path:

  Path to folder containing the 'RecruitNet.csv' file. If the file is
  not present, the latest version of the RecruitNet database (Verdú et
  al. 2023, [doi:10.1002/ecy.3923](https://doi.org/10.1002/ecy.3923) )
  will be downloaded automatically.

## Value

a data frame containing interaction data

## Examples

``` r
int_data <- load_interactions()
```
