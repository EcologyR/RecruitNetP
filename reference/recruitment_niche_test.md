# Recruitment niche: community-level effects on species recruitment

tests whether the presence of vegetation (i.e any canopy species)
compared to the "Open", enhances, depresses or has a neutral effect on
the recruitment of a given recruit species. **Input**:The canopy-recruit
interactions dataset and the canopy cover dataset (details as explained
in
[`int_significance()`](https://ecologyr.github.io/RecruitNetP/reference/int_significance.md)
documentation).

## Usage

``` r
recruitment_niche_test(int_data, cover_data)
```

## Arguments

- int_data:

  data frame containing interaction data.

- cover_data:

  data frame with the abundance of each canopy species in each plot.

## Value

The same variables provided by the function
[`int_significance()`](https://ecologyr.github.io/RecruitNetP/reference/int_significance.md)
but without distinguishing between canopy species.

## Examples

``` r
recruitment_niche_test (Amoladeras_int, Amoladeras_cover)
#> Based on Weibull distribution fitted to the observed values, the threshold density has been set to: 1.12346719046902.
#> Warning: Different tests were used for different canopy-recruit pairs. Check column Test_type
#>                   Recruit  Fr       Av  Fro       Ao   testability Significance
#> 1    Artemisia_barrelieri  33 3071.608   54 6926.992  2.547498e-45 1.448769e-01
#> 2    Artemisia_campestris   5 3072.608    1 6926.992  8.416803e-04 1.222676e-02
#> 3         Asparagus_albus  18 3072.608    7 6926.992  1.542102e-13 7.716473e-06
#> 4      Asparagus_horridus  35 3072.608   18 6926.992  6.899221e-28 2.520820e-08
#> 5         Ballota_hirsuta  71 3072.608   11 6926.992  9.484433e-43 5.723811e-28
#> 6    Helichrysum_stoechas 256 3071.608  796 6926.992  0.000000e+00 7.136350e-06
#> 7       Hyparrhenia_hirta  19 3072.608   34 6926.992  6.899221e-28 4.189819e-01
#> 8     Launaea_arborescens  80 3072.608   89 6926.992  2.464027e-87 2.865279e-06
#> 9        Launaea_lanifera   2 3072.608    6 6926.992  7.946872e-05 1.000000e+00
#> 10     Lycium_intrincatum  44 3072.608   20 6926.992  1.590633e-33 4.307415e-11
#> 11         Lygeum_spartum  23 3072.608   80 6926.992  1.640690e-53 6.472031e-02
#> 12  Maytenus_senegalensis   4 1202.120    0 6926.992  4.782102e-04 4.782102e-04
#> 13          Ononis_natrix 273 3072.608 1839 6926.992  0.000000e+00 2.383765e-70
#> 14 Periploca_angustifolia   0 3072.608    1 6926.992  3.072731e-01 1.000000e+00
#> 15     Phagnalon_saxatile 672 3070.035  528 6926.992  0.000000e+00 1.978804e-80
#> 16  Piptatherum_miliaceum   8 3072.608   19 6926.992  1.456003e-14 9.016104e-01
#> 17  Salsola_oppositifolia 112 3072.608   60 6926.992  7.148567e-89 1.434116e-22
#> 18      Stipa_tenacissima   0 3072.608    1 6926.992  3.072731e-01 1.000000e+00
#> 19     Teucrium_charidemi   8 3072.608   14 6926.992  5.315445e-12 5.666361e-01
#> 20   Teucrium_lusitanicum  27 3072.608   16 6926.992  9.195064e-23 5.183420e-06
#> 21        Teucrium_polium 239 3071.608  308 6926.992 4.191290e-281 4.813511e-11
#> 22      Thymelaea_hirsuta 175 3072.608  170 6926.992 1.570227e-177 8.226457e-16
#> 23        Thymus_hyemalis 114 3072.608 1030 6926.992  0.000000e+00 2.563792e-52
#> 24    Whitania_frutescens  45 3072.608    5 6926.992  2.378080e-26 1.042701e-19
#> 25         Ziziphus_lotus   0 3072.608    4 6926.992  8.914523e-03 3.195791e-01
#>     Test_type   Veg_effect
#> 1  Chi-square      Neutral
#> 2    Binomial  Facilitated
#> 3  Chi-square  Facilitated
#> 4  Chi-square  Facilitated
#> 5  Chi-square  Facilitated
#> 6  Chi-square    Depressed
#> 7  Chi-square      Neutral
#> 8  Chi-square  Facilitated
#> 9    Binomial      Neutral
#> 10 Chi-square  Facilitated
#> 11 Chi-square      Neutral
#> 12   Binomial  Facilitated
#> 13 Chi-square    Depressed
#> 14   Binomial Not testable
#> 15 Chi-square  Facilitated
#> 16 Chi-square      Neutral
#> 17 Chi-square  Facilitated
#> 18   Binomial Not testable
#> 19 Chi-square      Neutral
#> 20 Chi-square  Facilitated
#> 21 Chi-square  Facilitated
#> 22 Chi-square  Facilitated
#> 23 Chi-square    Depressed
#> 24 Chi-square  Facilitated
#> 25   Binomial      Neutral
```
