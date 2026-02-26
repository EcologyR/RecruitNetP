# Statistical significance of interactions

conducts statistical tests for each pairwise interaction, indicating
whether the effect of the canopy species on recruitment is enhancing
(i.e. positive), depressing (i.e. negative), neutral, or whether it
could not be tested due to low sample size. **IMPORTANT NOTE: Data on
recruitment in Open is required for the tests.** To assess whether
recruitment is affected by a given canopy species compared to the "Open"
it is used an exact binomial test or a chi square test (if the number of
recruits is large enough so that the expected frequencies are larger
than 5). The tests address the null hypothesis that recruitment is as
frequent under a given canopy species as it is in open interspaces.
Thus, we use these tests as a goodness-of-fit tests. The logic is that,
if recruitment were neutral regarding the microhabitat, we would observe
that the number of recruits under canopy would be exactly proportional
to the relative cover of each microhabitat. If the null hypothesis is
rejected, we would conclude that recruitment is affected (enhanced or
depressed) by the canopy species compared with the prospects of
recruitment when seeds are dispersed away from established plants. When
the exact binomial test is applied to canopy-recruit pairs with very low
number of recruits, it may be impossible to reject the null hypothesis
even if all the recruits occurred in the less likely microhabitat. In
such cases, one might conclude that the interaction has a neutral effect
when it is actually not possible to reach a conclusion. We classify
these cases as "not testable".

## Usage

``` r
int_significance(int_data, cover_data, int_type = c("rec", "fac", "comp"))
```

## Arguments

- int_data:

  data frame containing interaction data.

- cover_data:

  data frame with the abundance of each canopy species in each plot.

- int_type:

  Indicates the type of plant-plant interaction that will be analyzed:
  general recruitment, recruitment enhancement (i.e. facilitation) or
  recruitment depression (i.e. competition). Explanation of its options:

  - *rec*: All the pairwise interactions observed will be in the output.
    Focuses on canopy-recruit interactions considering that every
    recruit growing under the canopy of another plant may occupy that
    space in the future, thus having a potentially positive effect on
    the recruit species population. Therefore, even a single observation
    is considered an interaction. This type of networks considers every
    species present in the study system, whether as a canopy or as a
    recruit, as a node in the network. It also includes "Open" as a
    particular node since some species may recruit away from established
    plants. Non-detected interactions are also considered since zero
    frequency can provide evidence of a very negative interaction if the
    expected frequency under the canopy species is large.

  - *fac*: Only those pairwise interactions that resulted in recruitment
    enhancement will be in the output. Focuses on interactions with a
    significantly higher recruitment density under canopy than in "Open"
    (i.e. facilitation). Non-detected interactions are not considered
    and "Open" is not included as a node, although its relative cover is
    considered as part of the sampling area.

  - *comp*: Only those pairwise interactions that resulted in a
    recruitment depression will be in the output. Focuses on
    interactions with a significantly lower recruitment density under
    canopy than "Open" (i.e. competition). Non-detected interactions are
    considered (i.e. expanding with 0 all possible interactions in the
    study system), as the absence of recruitment of a species under a
    given canopy can reflect a particularly strong depression of
    recruitment under that canopy species. "Open" is not included as a
    node, although its relative cover is considered as part of the
    sampling area.

## Value

a data frame with the following information for each recruit-canopy
interaction with the following information:

- *Canopy*: Canopy species

- *Recruit*: Recruit species

- *Fcr*: frequency of recruitment, as the number of recruits found under
  that canopy species.

- *Ac*: Area (in m^2^), (or m when relative cover is measured with
  transects) occupied by the canopy species in the study site.

- *Fro*: frequency of recruitment in open interspaces.

- *Ao*: Area (in m^2^), (or m when relative cover is measured with
  transects) occupied by the open interspaces in the study site.

- *testability*: Testability indicates the smallest p-value that a
  binomial test will estimate based in a given number of recruits. If
  this p-value is above the reference p-value (typically 0.05), then you
  have too few cases (number of recruits) to ever reject the null
  hypothesis, and the interaction is not testable.

- *Significance*: p-value of the chi square or binomial test assessing
  the null hypothesis that Fcr and Fro are equal to the expected
  frequencies based on the relative cover of each of the two
  microhabitats: \$Ac/(Ac+Ao)\$ and \$1-(Ac/(Ac+Ao))\$

- *Test_type*: Indicates whether, depending on the sample size, a chi
  square or binomial test has been conducted.

- *Effect_int*: Indicates, for each interaction, whether the analysis
  classifies it as "Enhancing", "Depressing", "Neutral" or "Not
  Testable".

## Examples

``` r
int_signif_rec <- int_significance(Amoladeras_int, Amoladeras_cover, int_type = "rec")
#> Different tests were used for different canopy-recruit pairs.
#>               Check column Test_type
head(int_signif_rec)
#>                  Canopy              Recruit Fcr Ac Fro       Ao   testability
#> 1  Artemisia_barrelieri Artemisia_barrelieri   3  1  54 6926.992 1.217249e-219
#> 22 Artemisia_barrelieri Artemisia_campestris   0  1   1 6926.992  1.443420e-04
#> 20 Artemisia_barrelieri      Asparagus_albus   0  1   7 6926.992  1.305414e-27
#> 16 Artemisia_barrelieri   Asparagus_horridus   0  1  18 6926.992  7.397197e-70
#> 12 Artemisia_barrelieri      Ballota_hirsuta   0  1  11 6926.992  5.666552e-43
#> 13 Artemisia_barrelieri Helichrysum_stoechas   3  1 796 6926.992  0.000000e+00
#>    Significance Test_type Effect_int
#> 1  8.748100e-08  Binomial  Enhancing
#> 22 1.000000e+00  Binomial    Neutral
#> 20 1.000000e+00  Binomial    Neutral
#> 16 1.000000e+00  Binomial    Neutral
#> 12 1.000000e+00  Binomial    Neutral
#> 13 2.337311e-04  Binomial  Enhancing

int_signif_fac <- int_significance(Amoladeras_int, Amoladeras_cover, int_type = "fac")
#> Different tests were used for different canopy-recruit pairs.
#>               Check column Test_type
head(int_signif_fac)
#>                 Recruit               Canopy Fcr      Ac Fro       Ao
#> 1  Artemisia_barrelieri    Thymelaea_hirsuta   5 140.930  54 6926.992
#> 2  Artemisia_barrelieri Artemisia_barrelieri   3   1.000  54 6926.992
#> 6  Artemisia_barrelieri       Lygeum_spartum  23 643.715  54 6926.992
#> 7  Artemisia_campestris       Lygeum_spartum   3 643.715   1 6926.992
#> 8  Artemisia_campestris      Thymus_hyemalis   2 522.580   1 6926.992
#> 10      Asparagus_albus       Ziziphus_lotus   4 465.195   7 6926.992
#>      testability Significance  Test_type Effect_int
#> 1  4.819373e-101 6.464030e-03   Binomial  Enhancing
#> 2  1.217249e-219 8.748100e-08   Binomial  Enhancing
#> 6   3.766203e-83 1.789370e-11 Chi-square  Enhancing
#> 7   5.226714e-05 2.302046e-03   Binomial  Enhancing
#> 8   3.451949e-04 1.407226e-02   Binomial  Enhancing
#> 10  6.130318e-14 3.614203e-03   Binomial  Enhancing

int_signif_comp <- int_significance(Amoladeras_int, Amoladeras_cover, int_type = "comp")
#> Different tests were used for different canopy-recruit pairs.
#>               Check column Test_type
head(int_signif_comp)
#>                  Recruit                Canopy Fcr      Ac Fro       Ao
#> 9   Artemisia_barrelieri        Ziziphus_lotus   0 465.195  54 6926.992
#> 21  Artemisia_barrelieri   Launaea_arborescens   0 473.865  54 6926.992
#> 117 Helichrysum_stoechas    Phagnalon_saxatile   0  33.925 796 6926.992
#> 120 Helichrysum_stoechas        Lygeum_spartum   4 643.715 796 6926.992
#> 125 Helichrysum_stoechas Maytenus_senegalensis   0 116.450 796 6926.992
#> 127 Helichrysum_stoechas         Ononis_natrix   2 244.965 796 6926.992
#>      testability Significance  Test_type Effect_int
#> 9   1.375793e-65 4.899294e-02   Binomial Depressing
#> 21  3.500431e-65 4.901722e-02   Binomial Depressing
#> 117 0.000000e+00 3.818974e-02   Binomial Depressing
#> 120 0.000000e+00 4.849900e-16 Chi-square Depressing
#> 125 0.000000e+00 2.541056e-04 Chi-square Depressing
#> 127 0.000000e+00 8.545344e-07 Chi-square Depressing
```
