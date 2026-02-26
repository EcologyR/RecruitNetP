# Download RecruitNet database from Zenodo

Download latest version of the RecruitNet database (Verdú et al. 2023,
[doi:10.1002/ecy.3923](https://doi.org/10.1002/ecy.3923) ).

## Usage

``` r
download_RN(path = getwd(), destfile = "RN.zip", unzip = TRUE)
```

## Arguments

- path:

  character. Path to folder where the RecruitNet database should be
  saved.

- destfile:

  character. Name of the zip file to be saved.

- unzip:

  Logical. Uncompress the zip file? Default is TRUE.

## Value

A zip file or two CSV files, depending if unzip is TRUE.
