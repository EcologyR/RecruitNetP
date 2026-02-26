# Identifies whether a species acts as depressing, enhancing or neutral

Tests whether a canopy species has a depressing, enhancing or neutral
effect on recruitment in general (i.e., at the community level) compared
to the "Open", independently considering all the recruit species
together.

## Usage

``` r
canopy_service_test(int_data, cover_data)
```

## Arguments

- int_data:

  data frame containing interaction data.

- cover_data:

  data frame with the abundance of each canopy species in each plot.

## Value

Data frame with the same variables provided by the function
[`int_significance()`](https://ecologyr.github.io/RecruitNetP/reference/int_significance.md)
but without distinguishing between recruit species.

## Examples

``` r
canopy_service_test(Amoladeras_int, Amoladeras_cover)
#> Based on Weibull distribution fitted to the observed values, the threshold density has been set to: 1.12346719046902.
#> Different tests were used for different canopy-recruit pairs. Check column Test_type
#>                   Canopy  Fc       Ac  Fro       Ao testability  Significance
#> 1   Artemisia_barrelieri   1   1.0000 3425 6926.992           0  3.901555e-01
#> 2   Artemisia_campestris   0   0.1100 5111 6926.992           0  1.000000e+00
#> 3        Asparagus_albus  14  27.7250 5111 6926.992           0  1.539899e-01
#> 4     Asparagus_horridus   2   1.5725 4583 6926.992           0  2.791773e-01
#> 5        Ballota_hirsuta  31  38.4550 5111 6926.992           0  6.230153e-01
#> 6   Helichrysum_stoechas  62  29.0700 5111 6926.992           0  3.225618e-18
#> 7      Hyparrhenia_hirta  22  13.2550 5111 6926.992           0  9.654367e-05
#> 8    Launaea_arborescens 452 473.8650 5111 6926.992           0  1.543148e-07
#> 9       Launaea_lanifera   0   1.1600 5111 6926.992           0  1.000000e+00
#> 10    Lycium_intrincatum 121 161.8000 5111 6926.992           0  8.836527e-01
#> 11        Lygeum_spartum 177 643.7150 5111 6926.992           0  3.473186e-41
#> 12 Maytenus_senegalensis  16 116.4500 5111 6926.992           0  5.016894e-14
#> 13         Ononis_natrix  73 244.9650 5111 6926.992           0  1.753805e-15
#> 14    Phagnalon_saxatile  41  33.9250 5111 6926.992           0  1.477572e-03
#> 15 Piptatherum_miliaceum  14  15.4830 5111 6926.992           0  4.465865e-01
#> 16 Salsola_oppositifolia 145  61.6100 5111 6926.992           0  5.137918e-48
#> 17     Stipa_tenacissima   0   0.6000 5111 6926.992           0  1.000000e+00
#> 18  Teucrium_lusitanicum   3   5.2825 5111 6926.992           0  1.000000e+00
#> 19       Teucrium_polium  44  17.0450 5111 6926.992           0  1.114653e-18
#> 20     Thymelaea_hirsuta 392 140.9300 5111 6926.992           0 3.766061e-163
#> 21       Thymus_hyemalis 445 522.5800 5111 6926.992           0  3.703660e-03
#> 22   Whitania_frutescens  41  56.8150 5111 6926.992           0  8.874189e-01
#> 23        Ziziphus_lotus 167 465.1950 5111 6926.992           0  7.899946e-21
#>     Test_type Canopy_effect
#> 1    Binomial       Neutral
#> 2    Binomial       Neutral
#> 3  Chi-square       Neutral
#> 4    Binomial       Neutral
#> 5  Chi-square       Neutral
#> 6  Chi-square  Facilitative
#> 7  Chi-square  Facilitative
#> 8  Chi-square  Facilitative
#> 9    Binomial       Neutral
#> 10 Chi-square       Neutral
#> 11 Chi-square    Depressive
#> 12 Chi-square    Depressive
#> 13 Chi-square    Depressive
#> 14 Chi-square  Facilitative
#> 15 Chi-square       Neutral
#> 16 Chi-square  Facilitative
#> 17   Binomial       Neutral
#> 18   Binomial       Neutral
#> 19 Chi-square  Facilitative
#> 20 Chi-square  Facilitative
#> 21 Chi-square  Facilitative
#> 22 Chi-square       Neutral
#> 23 Chi-square    Depressive
```
