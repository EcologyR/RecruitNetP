# Calculate different association indices considering different interaction types.

Calculates different metrics to characterize the interaction strength
for each pairwise interaction, based on the number of recruits found
under the canopy species and in the "Open" ground. Several indices have
been proposed to assess how much recruitment is affected by the canopy
species relative to the species performance in the absence of canopy
plants (i.e. in open), each of them with its own particularities. In the
context of recruitment interactions, the most frequently used index is
the *Relative Interaction Index* (**RII**; Armas et al., 2004), and some
studies have recently used the *additive and commutative symmetry
intensity indices* (**NIntA** and **NIntC**) y Diaz-Sierra et al.
(2017). However, these indices are not well suited for comparisons of
interaction strength between pairs of species within a local community,
so the *Normalized Neighbour Suitability* index (**Ns**; Mingo, 2014)
should be preferred (Alcantara et al. 2025). The function *`associndex`*
calculates all the four indices besides providing the density of
recruits under the canopy and in the open, and the data used to
calculate it (relative cover and number of recruits in each
microhabitat). \*\*IMPORTANT NOTE: Data on recruitment in Open is
required to calculate these indices. **Input**: The canopy-recruit
interactions data set and the canopy cover data set

## Usage

``` r
associndex(
  int_data,
  cover_data,
  expand = c("yes", "no"),
  rm_sp_no_cover = c("allsp", "onlycanopy"),
  threshold_density = NULL
)
```

## Arguments

- int_data:

  data frame containing interaction data.

- cover_data:

  data frame with the abundance of each canopy species in each plot.

- expand:

  Expands the data frame to include the non-detected interactions
  between every pair of species that occur in the study site, assigning
  values of 0 to the variables Fcr and Dcr. Explanation of its options:

  - *yes*: Generates all possible interactions between canopy and
    recruit species in the study site (i.e. detected and non-detected),
    adding values of interaction frequency of 0 to the non-detected
    interactions.

  - *no*: Considers only the pairwise interactions actually detected in
    the study site.

- rm_sp_no_cover:

  **rm_sp_no_cover**: Defines the filtering behavior, specifying which
  species should be removed based on the availability of cover data and
  the type of interaction of interest. Explanation of its options:

  - *allsp*: removes any species that lacks cover data (both canopy and
    recruit species).

  - *onlycanopy*: removes only canopy species lacking cover data,
    retaining recruit species even if they lack cover data.

- threshold_density:

  **threshold_density**: indicates a threshold to retain only those
  interactions in which the observed density is below the threshold
  (i.e. Dcr \< thr & Dro \< thr). This is required to avoid the use of
  interactions based on very few recruits which may have occurred by
  chance under a very scarce canopy species. These cases will result in
  a large overestimation of Dcr, likely being spurious outlayers. If no
  threshold value is provided or it is set to NULL, the function
  estimates a threshold value based on recruitment density outliers
  according to a Weibull distribution: a value is an outlier if it is
  above the limit where less than 1 observation is expected. Explanation
  of its options:

  - A positive value. High values provide more chance to include
    potentially spurious interactions, unless such a high density is
    reliable based on the knowledge of the study system. A way to decide
    the threshold value would consist in identifying possible outlayers
    in the distribution of recruitment densities (*Dcr* and *Dro*).

  - NULL: use package **extremevalues** to find the density threshold
    value (*K*) above which, according to the Weibull distribution, less
    than 1 observation is expected given the number of observations in
    the sample.

## Value

a data frame with the following information for each canopy-recruit
interaction with the following information:

- *Canopy*: **canopy species**.

- *Recruit*: **recruit species**.

- *Fcr*: **frequency of recruitment**, as the number of recruits found
  under that canopy species.

- *Ac*: **Area occupied by the canopy species** in the study site, in
  m^2^ (or m when relative cover is measured with transects).

- *Fro*: **frequency of recruitment**, as the number of recruits in Open
  interspaces. - *Ao*: **Area occupied by open interspaces** in the
  study site in m^2^ (or m when relative cover is measured with
  transects).

- *Dcr*: **Density of recruits under a canopy species**: \$Fcr/Ac\$.

- *Dro*: **Density of recruits in open interspaces**: \$Fro/Ao\$.

- *max_Recr*: **maximum recruitment density** of a species in the study
  site (i.e., under any canopy species or "Open").

- *Ns*: **Normalized Neighbour Suitability** index: \$(Dcr - Dro) /
  maxRecr\$.

- *NintC*: **Commutative symmetry intensity** index: \$2\*(Dcr -
  Dro)/((Dcr + Dro)+abs(Dcr-Dro))\$

- *NintA*: **Additive symmetry intensity** index: \$2\*(Dcr -
  Dro)/((Dro) + abs(Dcr-Dro))\$

- *RII*: **Relative Interaction Index**: \$(Dcr - Dro)/(Dcr + Dro)\$

## Examples

``` r
associndex (Amoladeras_int, Amoladeras_cover, expand = "yes",
rm_sp_no_cover = "allsp", threshold_density=NULL)
#> Based on Weibull distribution fitted to the observed values, the threshold density has been set to: 1.12346719046902.
#>                    Canopy               Recruit Fcr       Ac  Fro       Ao
#> 22   Artemisia_barrelieri  Artemisia_campestris   0   1.0000    1 6926.992
#> 20   Artemisia_barrelieri       Asparagus_albus   0   1.0000    7 6926.992
#> 16   Artemisia_barrelieri    Asparagus_horridus   0   1.0000   18 6926.992
#> 12   Artemisia_barrelieri       Ballota_hirsuta   0   1.0000   11 6926.992
#> 18   Artemisia_barrelieri     Hyparrhenia_hirta   0   1.0000   34 6926.992
#> 8    Artemisia_barrelieri   Launaea_arborescens   0   1.0000   89 6926.992
#> 23   Artemisia_barrelieri      Launaea_lanifera   0   1.0000    6 6926.992
#> 9    Artemisia_barrelieri    Lycium_intrincatum   0   1.0000   20 6926.992
#> 2    Artemisia_barrelieri        Lygeum_spartum   0   1.0000   80 6926.992
#> 3    Artemisia_barrelieri         Ononis_natrix   0   1.0000 1839 6926.992
#> 19   Artemisia_barrelieri Piptatherum_miliaceum   0   1.0000   19 6926.992
#> 5    Artemisia_barrelieri Salsola_oppositifolia   0   1.0000   60 6926.992
#> 25   Artemisia_barrelieri     Stipa_tenacissima   0   1.0000    1 6926.992
#> 21   Artemisia_barrelieri  Teucrium_lusitanicum   0   1.0000   16 6926.992
#> 6    Artemisia_barrelieri     Thymelaea_hirsuta   0   1.0000  170 6926.992
#> 7    Artemisia_barrelieri       Thymus_hyemalis   1   1.0000 1030 6926.992
#> 15   Artemisia_barrelieri   Whitania_frutescens   0   1.0000    5 6926.992
#> 11   Artemisia_barrelieri        Ziziphus_lotus   0   1.0000    4 6926.992
#> 547  Artemisia_campestris  Artemisia_barrelieri   0   0.1100   54 6926.992
#> 568  Artemisia_campestris  Artemisia_campestris   0   0.1100    1 6926.992
#> 566  Artemisia_campestris       Asparagus_albus   0   0.1100    7 6926.992
#> 562  Artemisia_campestris    Asparagus_horridus   0   0.1100   18 6926.992
#> 558  Artemisia_campestris       Ballota_hirsuta   0   0.1100   11 6926.992
#> 559  Artemisia_campestris  Helichrysum_stoechas   0   0.1100  796 6926.992
#> 564  Artemisia_campestris     Hyparrhenia_hirta   0   0.1100   34 6926.992
#> 554  Artemisia_campestris   Launaea_arborescens   0   0.1100   89 6926.992
#> 569  Artemisia_campestris      Launaea_lanifera   0   0.1100    6 6926.992
#> 555  Artemisia_campestris    Lycium_intrincatum   0   0.1100   20 6926.992
#> 548  Artemisia_campestris        Lygeum_spartum   0   0.1100   80 6926.992
#> 549  Artemisia_campestris         Ononis_natrix   0   0.1100 1839 6926.992
#> 560  Artemisia_campestris    Phagnalon_saxatile   0   0.1100  528 6926.992
#> 565  Artemisia_campestris Piptatherum_miliaceum   0   0.1100   19 6926.992
#> 551  Artemisia_campestris Salsola_oppositifolia   0   0.1100   60 6926.992
#> 571  Artemisia_campestris     Stipa_tenacissima   0   0.1100    1 6926.992
#> 567  Artemisia_campestris  Teucrium_lusitanicum   0   0.1100   16 6926.992
#> 563  Artemisia_campestris       Teucrium_polium   0   0.1100  308 6926.992
#> 552  Artemisia_campestris     Thymelaea_hirsuta   0   0.1100  170 6926.992
#> 553  Artemisia_campestris       Thymus_hyemalis   0   0.1100 1030 6926.992
#> 561  Artemisia_campestris   Whitania_frutescens   0   0.1100    5 6926.992
#> 557  Artemisia_campestris        Ziziphus_lotus   0   0.1100    4 6926.992
#> 495       Asparagus_albus  Artemisia_barrelieri   0  27.7250   54 6926.992
#> 516       Asparagus_albus  Artemisia_campestris   0  27.7250    1 6926.992
#> 514       Asparagus_albus       Asparagus_albus   0  27.7250    7 6926.992
#> 510       Asparagus_albus    Asparagus_horridus   0  27.7250   18 6926.992
#> 506       Asparagus_albus       Ballota_hirsuta   0  27.7250   11 6926.992
#> 507       Asparagus_albus  Helichrysum_stoechas   0  27.7250  796 6926.992
#> 512       Asparagus_albus     Hyparrhenia_hirta   0  27.7250   34 6926.992
#> 502       Asparagus_albus   Launaea_arborescens   0  27.7250   89 6926.992
#> 517       Asparagus_albus      Launaea_lanifera   0  27.7250    6 6926.992
#> 503       Asparagus_albus    Lycium_intrincatum   2  27.7250   20 6926.992
#> 496       Asparagus_albus        Lygeum_spartum   1  27.7250   80 6926.992
#> 504       Asparagus_albus Maytenus_senegalensis   1  27.7250    0 6926.992
#> 497       Asparagus_albus         Ononis_natrix   1  27.7250 1839 6926.992
#> 508       Asparagus_albus    Phagnalon_saxatile   4  27.7250  528 6926.992
#> 513       Asparagus_albus Piptatherum_miliaceum   0  27.7250   19 6926.992
#> 499       Asparagus_albus Salsola_oppositifolia   0  27.7250   60 6926.992
#> 519       Asparagus_albus     Stipa_tenacissima   0  27.7250    1 6926.992
#> 515       Asparagus_albus  Teucrium_lusitanicum   0  27.7250   16 6926.992
#> 511       Asparagus_albus       Teucrium_polium   0  27.7250  308 6926.992
#> 500       Asparagus_albus     Thymelaea_hirsuta   2  27.7250  170 6926.992
#> 501       Asparagus_albus       Thymus_hyemalis   0  27.7250 1030 6926.992
#> 509       Asparagus_albus   Whitania_frutescens   3  27.7250    5 6926.992
#> 505       Asparagus_albus        Ziziphus_lotus   0  27.7250    4 6926.992
#> 391    Asparagus_horridus  Artemisia_barrelieri   0   1.5725   54 6926.992
#> 412    Asparagus_horridus  Artemisia_campestris   0   1.5725    1 6926.992
#> 410    Asparagus_horridus       Asparagus_albus   0   1.5725    7 6926.992
#> 406    Asparagus_horridus    Asparagus_horridus   0   1.5725   18 6926.992
#> 402    Asparagus_horridus       Ballota_hirsuta   1   1.5725   11 6926.992
#> 403    Asparagus_horridus  Helichrysum_stoechas   0   1.5725  796 6926.992
#> 408    Asparagus_horridus     Hyparrhenia_hirta   0   1.5725   34 6926.992
#> 398    Asparagus_horridus   Launaea_arborescens   0   1.5725   89 6926.992
#> 413    Asparagus_horridus      Launaea_lanifera   0   1.5725    6 6926.992
#> 399    Asparagus_horridus    Lycium_intrincatum   0   1.5725   20 6926.992
#> 392    Asparagus_horridus        Lygeum_spartum   0   1.5725   80 6926.992
#> 393    Asparagus_horridus         Ononis_natrix   0   1.5725 1839 6926.992
#> 409    Asparagus_horridus Piptatherum_miliaceum   0   1.5725   19 6926.992
#> 395    Asparagus_horridus Salsola_oppositifolia   0   1.5725   60 6926.992
#> 415    Asparagus_horridus     Stipa_tenacissima   0   1.5725    1 6926.992
#> 411    Asparagus_horridus  Teucrium_lusitanicum   0   1.5725   16 6926.992
#> 407    Asparagus_horridus       Teucrium_polium   0   1.5725  308 6926.992
#> 396    Asparagus_horridus     Thymelaea_hirsuta   0   1.5725  170 6926.992
#> 397    Asparagus_horridus       Thymus_hyemalis   1   1.5725 1030 6926.992
#> 405    Asparagus_horridus   Whitania_frutescens   0   1.5725    5 6926.992
#> 401    Asparagus_horridus        Ziziphus_lotus   0   1.5725    4 6926.992
#> 287       Ballota_hirsuta  Artemisia_barrelieri   0  38.4550   54 6926.992
#> 308       Ballota_hirsuta  Artemisia_campestris   0  38.4550    1 6926.992
#> 306       Ballota_hirsuta       Asparagus_albus   0  38.4550    7 6926.992
#> 302       Ballota_hirsuta    Asparagus_horridus   2  38.4550   18 6926.992
#> 298       Ballota_hirsuta       Ballota_hirsuta   0  38.4550   11 6926.992
#> 299       Ballota_hirsuta  Helichrysum_stoechas   4  38.4550  796 6926.992
#> 304       Ballota_hirsuta     Hyparrhenia_hirta   0  38.4550   34 6926.992
#> 294       Ballota_hirsuta   Launaea_arborescens   0  38.4550   89 6926.992
#> 309       Ballota_hirsuta      Launaea_lanifera   0  38.4550    6 6926.992
#> 295       Ballota_hirsuta    Lycium_intrincatum   0  38.4550   20 6926.992
#> 288       Ballota_hirsuta        Lygeum_spartum   0  38.4550   80 6926.992
#> 289       Ballota_hirsuta         Ononis_natrix   2  38.4550 1839 6926.992
#> 300       Ballota_hirsuta    Phagnalon_saxatile  14  38.4550  528 6926.992
#> 305       Ballota_hirsuta Piptatherum_miliaceum   0  38.4550   19 6926.992
#> 291       Ballota_hirsuta Salsola_oppositifolia   5  38.4550   60 6926.992
#> 311       Ballota_hirsuta     Stipa_tenacissima   0  38.4550    1 6926.992
#> 307       Ballota_hirsuta  Teucrium_lusitanicum   0  38.4550   16 6926.992
#> 303       Ballota_hirsuta       Teucrium_polium   1  38.4550  308 6926.992
#> 292       Ballota_hirsuta     Thymelaea_hirsuta   3  38.4550  170 6926.992
#> 293       Ballota_hirsuta       Thymus_hyemalis   0  38.4550 1030 6926.992
#> 301       Ballota_hirsuta   Whitania_frutescens   0  38.4550    5 6926.992
#> 297       Ballota_hirsuta        Ziziphus_lotus   0  38.4550    4 6926.992
#> 313  Helichrysum_stoechas  Artemisia_barrelieri   0  29.0700   54 6926.992
#> 334  Helichrysum_stoechas  Artemisia_campestris   0  29.0700    1 6926.992
#> 332  Helichrysum_stoechas       Asparagus_albus   0  29.0700    7 6926.992
#> 328  Helichrysum_stoechas    Asparagus_horridus   2  29.0700   18 6926.992
#> 324  Helichrysum_stoechas       Ballota_hirsuta   3  29.0700   11 6926.992
#> 325  Helichrysum_stoechas  Helichrysum_stoechas   0  29.0700  796 6926.992
#> 330  Helichrysum_stoechas     Hyparrhenia_hirta   0  29.0700   34 6926.992
#> 320  Helichrysum_stoechas   Launaea_arborescens   1  29.0700   89 6926.992
#> 335  Helichrysum_stoechas      Launaea_lanifera   0  29.0700    6 6926.992
#> 321  Helichrysum_stoechas    Lycium_intrincatum   0  29.0700   20 6926.992
#> 314  Helichrysum_stoechas        Lygeum_spartum   0  29.0700   80 6926.992
#> 315  Helichrysum_stoechas         Ononis_natrix  15  29.0700 1839 6926.992
#> 326  Helichrysum_stoechas    Phagnalon_saxatile   4  29.0700  528 6926.992
#> 331  Helichrysum_stoechas Piptatherum_miliaceum   0  29.0700   19 6926.992
#> 317  Helichrysum_stoechas Salsola_oppositifolia   7  29.0700   60 6926.992
#> 337  Helichrysum_stoechas     Stipa_tenacissima   0  29.0700    1 6926.992
#> 333  Helichrysum_stoechas  Teucrium_lusitanicum   0  29.0700   16 6926.992
#> 329  Helichrysum_stoechas       Teucrium_polium  12  29.0700  308 6926.992
#> 318  Helichrysum_stoechas     Thymelaea_hirsuta  14  29.0700  170 6926.992
#> 319  Helichrysum_stoechas       Thymus_hyemalis   4  29.0700 1030 6926.992
#> 327  Helichrysum_stoechas   Whitania_frutescens   0  29.0700    5 6926.992
#> 323  Helichrysum_stoechas        Ziziphus_lotus   0  29.0700    4 6926.992
#> 443     Hyparrhenia_hirta  Artemisia_barrelieri   0  13.2550   54 6926.992
#> 464     Hyparrhenia_hirta  Artemisia_campestris   0  13.2550    1 6926.992
#> 462     Hyparrhenia_hirta       Asparagus_albus   0  13.2550    7 6926.992
#> 458     Hyparrhenia_hirta    Asparagus_horridus   0  13.2550   18 6926.992
#> 454     Hyparrhenia_hirta       Ballota_hirsuta   0  13.2550   11 6926.992
#> 455     Hyparrhenia_hirta  Helichrysum_stoechas   0  13.2550  796 6926.992
#> 460     Hyparrhenia_hirta     Hyparrhenia_hirta   0  13.2550   34 6926.992
#> 450     Hyparrhenia_hirta   Launaea_arborescens   1  13.2550   89 6926.992
#> 465     Hyparrhenia_hirta      Launaea_lanifera   0  13.2550    6 6926.992
#> 451     Hyparrhenia_hirta    Lycium_intrincatum   1  13.2550   20 6926.992
#> 444     Hyparrhenia_hirta        Lygeum_spartum   0  13.2550   80 6926.992
#> 445     Hyparrhenia_hirta         Ononis_natrix   1  13.2550 1839 6926.992
#> 456     Hyparrhenia_hirta    Phagnalon_saxatile   8  13.2550  528 6926.992
#> 461     Hyparrhenia_hirta Piptatherum_miliaceum   0  13.2550   19 6926.992
#> 447     Hyparrhenia_hirta Salsola_oppositifolia   0  13.2550   60 6926.992
#> 467     Hyparrhenia_hirta     Stipa_tenacissima   0  13.2550    1 6926.992
#> 463     Hyparrhenia_hirta  Teucrium_lusitanicum   5  13.2550   16 6926.992
#> 459     Hyparrhenia_hirta       Teucrium_polium   2  13.2550  308 6926.992
#> 448     Hyparrhenia_hirta     Thymelaea_hirsuta   2  13.2550  170 6926.992
#> 449     Hyparrhenia_hirta       Thymus_hyemalis   1  13.2550 1030 6926.992
#> 457     Hyparrhenia_hirta   Whitania_frutescens   1  13.2550    5 6926.992
#> 453     Hyparrhenia_hirta        Ziziphus_lotus   0  13.2550    4 6926.992
#> 183   Launaea_arborescens  Artemisia_barrelieri   0 473.8650   54 6926.992
#> 204   Launaea_arborescens  Artemisia_campestris   0 473.8650    1 6926.992
#> 202   Launaea_arborescens       Asparagus_albus   5 473.8650    7 6926.992
#> 198   Launaea_arborescens    Asparagus_horridus   7 473.8650   18 6926.992
#> 194   Launaea_arborescens       Ballota_hirsuta   5 473.8650   11 6926.992
#> 195   Launaea_arborescens  Helichrysum_stoechas  35 473.8650  796 6926.992
#> 200   Launaea_arborescens     Hyparrhenia_hirta   4 473.8650   34 6926.992
#> 190   Launaea_arborescens   Launaea_arborescens   9 473.8650   89 6926.992
#> 205   Launaea_arborescens      Launaea_lanifera   0 473.8650    6 6926.992
#> 191   Launaea_arborescens    Lycium_intrincatum   6 473.8650   20 6926.992
#> 184   Launaea_arborescens        Lygeum_spartum   1 473.8650   80 6926.992
#> 192   Launaea_arborescens Maytenus_senegalensis   1 473.8650    0 6926.992
#> 185   Launaea_arborescens         Ononis_natrix 113 473.8650 1839 6926.992
#> 196   Launaea_arborescens    Phagnalon_saxatile 168 473.8650  528 6926.992
#> 201   Launaea_arborescens Piptatherum_miliaceum   2 473.8650   19 6926.992
#> 187   Launaea_arborescens Salsola_oppositifolia   8 473.8650   60 6926.992
#> 207   Launaea_arborescens     Stipa_tenacissima   0 473.8650    1 6926.992
#> 203   Launaea_arborescens  Teucrium_lusitanicum   4 473.8650   16 6926.992
#> 199   Launaea_arborescens       Teucrium_polium  14 473.8650  308 6926.992
#> 188   Launaea_arborescens     Thymelaea_hirsuta  20 473.8650  170 6926.992
#> 189   Launaea_arborescens       Thymus_hyemalis  14 473.8650 1030 6926.992
#> 197   Launaea_arborescens   Whitania_frutescens  36 473.8650    5 6926.992
#> 193   Launaea_arborescens        Ziziphus_lotus   0 473.8650    4 6926.992
#> 573      Launaea_lanifera  Artemisia_barrelieri   0   1.1600   54 6926.992
#> 594      Launaea_lanifera  Artemisia_campestris   0   1.1600    1 6926.992
#> 592      Launaea_lanifera       Asparagus_albus   0   1.1600    7 6926.992
#> 588      Launaea_lanifera    Asparagus_horridus   0   1.1600   18 6926.992
#> 584      Launaea_lanifera       Ballota_hirsuta   0   1.1600   11 6926.992
#> 585      Launaea_lanifera  Helichrysum_stoechas   0   1.1600  796 6926.992
#> 590      Launaea_lanifera     Hyparrhenia_hirta   0   1.1600   34 6926.992
#> 580      Launaea_lanifera   Launaea_arborescens   0   1.1600   89 6926.992
#> 595      Launaea_lanifera      Launaea_lanifera   0   1.1600    6 6926.992
#> 581      Launaea_lanifera    Lycium_intrincatum   0   1.1600   20 6926.992
#> 574      Launaea_lanifera        Lygeum_spartum   0   1.1600   80 6926.992
#> 575      Launaea_lanifera         Ononis_natrix   0   1.1600 1839 6926.992
#> 586      Launaea_lanifera    Phagnalon_saxatile   0   1.1600  528 6926.992
#> 591      Launaea_lanifera Piptatherum_miliaceum   0   1.1600   19 6926.992
#> 577      Launaea_lanifera Salsola_oppositifolia   0   1.1600   60 6926.992
#> 597      Launaea_lanifera     Stipa_tenacissima   0   1.1600    1 6926.992
#> 593      Launaea_lanifera  Teucrium_lusitanicum   0   1.1600   16 6926.992
#> 589      Launaea_lanifera       Teucrium_polium   0   1.1600  308 6926.992
#> 578      Launaea_lanifera     Thymelaea_hirsuta   0   1.1600  170 6926.992
#> 579      Launaea_lanifera       Thymus_hyemalis   0   1.1600 1030 6926.992
#> 587      Launaea_lanifera   Whitania_frutescens   0   1.1600    5 6926.992
#> 583      Launaea_lanifera        Ziziphus_lotus   0   1.1600    4 6926.992
#> 209    Lycium_intrincatum  Artemisia_barrelieri   0 161.8000   54 6926.992
#> 230    Lycium_intrincatum  Artemisia_campestris   0 161.8000    1 6926.992
#> 228    Lycium_intrincatum       Asparagus_albus   1 161.8000    7 6926.992
#> 224    Lycium_intrincatum    Asparagus_horridus   2 161.8000   18 6926.992
#> 220    Lycium_intrincatum       Ballota_hirsuta  20 161.8000   11 6926.992
#> 221    Lycium_intrincatum  Helichrysum_stoechas  17 161.8000  796 6926.992
#> 226    Lycium_intrincatum     Hyparrhenia_hirta   0 161.8000   34 6926.992
#> 216    Lycium_intrincatum   Launaea_arborescens   3 161.8000   89 6926.992
#> 231    Lycium_intrincatum      Launaea_lanifera   0 161.8000    6 6926.992
#> 217    Lycium_intrincatum    Lycium_intrincatum   4 161.8000   20 6926.992
#> 210    Lycium_intrincatum        Lygeum_spartum   0 161.8000   80 6926.992
#> 211    Lycium_intrincatum         Ononis_natrix   9 161.8000 1839 6926.992
#> 222    Lycium_intrincatum    Phagnalon_saxatile  42 161.8000  528 6926.992
#> 227    Lycium_intrincatum Piptatherum_miliaceum   0 161.8000   19 6926.992
#> 213    Lycium_intrincatum Salsola_oppositifolia  12 161.8000   60 6926.992
#> 233    Lycium_intrincatum     Stipa_tenacissima   0 161.8000    1 6926.992
#> 229    Lycium_intrincatum  Teucrium_lusitanicum   0 161.8000   16 6926.992
#> 225    Lycium_intrincatum       Teucrium_polium   7 161.8000  308 6926.992
#> 214    Lycium_intrincatum     Thymelaea_hirsuta   4 161.8000  170 6926.992
#> 215    Lycium_intrincatum       Thymus_hyemalis   0 161.8000 1030 6926.992
#> 223    Lycium_intrincatum   Whitania_frutescens   0 161.8000    5 6926.992
#> 219    Lycium_intrincatum        Ziziphus_lotus   0 161.8000    4 6926.992
#> 27         Lygeum_spartum  Artemisia_barrelieri  23 643.7150   54 6926.992
#> 48         Lygeum_spartum  Artemisia_campestris   3 643.7150    1 6926.992
#> 46         Lygeum_spartum       Asparagus_albus   4 643.7150    7 6926.992
#> 42         Lygeum_spartum    Asparagus_horridus   7 643.7150   18 6926.992
#> 38         Lygeum_spartum       Ballota_hirsuta   0 643.7150   11 6926.992
#> 39         Lygeum_spartum  Helichrysum_stoechas   4 643.7150  796 6926.992
#> 44         Lygeum_spartum     Hyparrhenia_hirta   0 643.7150   34 6926.992
#> 34         Lygeum_spartum   Launaea_arborescens   6 643.7150   89 6926.992
#> 49         Lygeum_spartum      Launaea_lanifera   0 643.7150    6 6926.992
#> 35         Lygeum_spartum    Lycium_intrincatum   2 643.7150   20 6926.992
#> 28         Lygeum_spartum        Lygeum_spartum  10 643.7150   80 6926.992
#> 36         Lygeum_spartum Maytenus_senegalensis   1 643.7150    0 6926.992
#> 29         Lygeum_spartum         Ononis_natrix  10 643.7150 1839 6926.992
#> 40         Lygeum_spartum    Phagnalon_saxatile  41 643.7150  528 6926.992
#> 45         Lygeum_spartum Piptatherum_miliaceum   0 643.7150   19 6926.992
#> 31         Lygeum_spartum Salsola_oppositifolia  17 643.7150   60 6926.992
#> 51         Lygeum_spartum     Stipa_tenacissima   0 643.7150    1 6926.992
#> 47         Lygeum_spartum  Teucrium_lusitanicum   6 643.7150   16 6926.992
#> 43         Lygeum_spartum       Teucrium_polium  17 643.7150  308 6926.992
#> 32         Lygeum_spartum     Thymelaea_hirsuta  13 643.7150  170 6926.992
#> 33         Lygeum_spartum       Thymus_hyemalis  12 643.7150 1030 6926.992
#> 41         Lygeum_spartum   Whitania_frutescens   0 643.7150    5 6926.992
#> 37         Lygeum_spartum        Ziziphus_lotus   0 643.7150    4 6926.992
#> 235 Maytenus_senegalensis  Artemisia_barrelieri   0 116.4500   54 6926.992
#> 256 Maytenus_senegalensis  Artemisia_campestris   0 116.4500    1 6926.992
#> 254 Maytenus_senegalensis       Asparagus_albus   1 116.4500    7 6926.992
#> 250 Maytenus_senegalensis    Asparagus_horridus   0 116.4500   18 6926.992
#> 246 Maytenus_senegalensis       Ballota_hirsuta   0 116.4500   11 6926.992
#> 247 Maytenus_senegalensis  Helichrysum_stoechas   0 116.4500  796 6926.992
#> 252 Maytenus_senegalensis     Hyparrhenia_hirta   0 116.4500   34 6926.992
#> 242 Maytenus_senegalensis   Launaea_arborescens   1 116.4500   89 6926.992
#> 257 Maytenus_senegalensis      Launaea_lanifera   0 116.4500    6 6926.992
#> 243 Maytenus_senegalensis    Lycium_intrincatum   1 116.4500   20 6926.992
#> 236 Maytenus_senegalensis        Lygeum_spartum   0 116.4500   80 6926.992
#> 237 Maytenus_senegalensis         Ononis_natrix   3 116.4500 1839 6926.992
#> 248 Maytenus_senegalensis    Phagnalon_saxatile   7 116.4500  528 6926.992
#> 253 Maytenus_senegalensis Piptatherum_miliaceum   0 116.4500   19 6926.992
#> 239 Maytenus_senegalensis Salsola_oppositifolia   0 116.4500   60 6926.992
#> 259 Maytenus_senegalensis     Stipa_tenacissima   0 116.4500    1 6926.992
#> 255 Maytenus_senegalensis  Teucrium_lusitanicum   0 116.4500   16 6926.992
#> 251 Maytenus_senegalensis       Teucrium_polium   0 116.4500  308 6926.992
#> 240 Maytenus_senegalensis     Thymelaea_hirsuta   0 116.4500  170 6926.992
#> 241 Maytenus_senegalensis       Thymus_hyemalis   2 116.4500 1030 6926.992
#> 249 Maytenus_senegalensis   Whitania_frutescens   1 116.4500    5 6926.992
#> 245 Maytenus_senegalensis        Ziziphus_lotus   0 116.4500    4 6926.992
#> 53          Ononis_natrix  Artemisia_barrelieri   1 244.9650   54 6926.992
#> 74          Ononis_natrix  Artemisia_campestris   0 244.9650    1 6926.992
#> 72          Ononis_natrix       Asparagus_albus   0 244.9650    7 6926.992
#> 68          Ononis_natrix    Asparagus_horridus   0 244.9650   18 6926.992
#> 64          Ononis_natrix       Ballota_hirsuta   0 244.9650   11 6926.992
#> 65          Ononis_natrix  Helichrysum_stoechas   2 244.9650  796 6926.992
#> 70          Ononis_natrix     Hyparrhenia_hirta   2 244.9650   34 6926.992
#> 60          Ononis_natrix   Launaea_arborescens  19 244.9650   89 6926.992
#> 75          Ononis_natrix      Launaea_lanifera   0 244.9650    6 6926.992
#> 61          Ononis_natrix    Lycium_intrincatum   2 244.9650   20 6926.992
#> 54          Ononis_natrix        Lygeum_spartum   0 244.9650   80 6926.992
#> 55          Ononis_natrix         Ononis_natrix  36 244.9650 1839 6926.992
#> 66          Ononis_natrix    Phagnalon_saxatile   3 244.9650  528 6926.992
#> 71          Ononis_natrix Piptatherum_miliaceum   0 244.9650   19 6926.992
#> 57          Ononis_natrix Salsola_oppositifolia   3 244.9650   60 6926.992
#> 77          Ononis_natrix     Stipa_tenacissima   0 244.9650    1 6926.992
#> 73          Ononis_natrix  Teucrium_lusitanicum   0 244.9650   16 6926.992
#> 69          Ononis_natrix       Teucrium_polium   4 244.9650  308 6926.992
#> 58          Ononis_natrix     Thymelaea_hirsuta   1 244.9650  170 6926.992
#> 59          Ononis_natrix       Thymus_hyemalis   0 244.9650 1030 6926.992
#> 67          Ononis_natrix   Whitania_frutescens   0 244.9650    5 6926.992
#> 63          Ononis_natrix        Ziziphus_lotus   0 244.9650    4 6926.992
#> 339    Phagnalon_saxatile  Artemisia_barrelieri   0  33.9250   54 6926.992
#> 360    Phagnalon_saxatile  Artemisia_campestris   0  33.9250    1 6926.992
#> 358    Phagnalon_saxatile       Asparagus_albus   0  33.9250    7 6926.992
#> 354    Phagnalon_saxatile    Asparagus_horridus   2  33.9250   18 6926.992
#> 350    Phagnalon_saxatile       Ballota_hirsuta   0  33.9250   11 6926.992
#> 351    Phagnalon_saxatile  Helichrysum_stoechas   0  33.9250  796 6926.992
#> 356    Phagnalon_saxatile     Hyparrhenia_hirta   0  33.9250   34 6926.992
#> 346    Phagnalon_saxatile   Launaea_arborescens   3  33.9250   89 6926.992
#> 361    Phagnalon_saxatile      Launaea_lanifera   0  33.9250    6 6926.992
#> 347    Phagnalon_saxatile    Lycium_intrincatum   1  33.9250   20 6926.992
#> 340    Phagnalon_saxatile        Lygeum_spartum   0  33.9250   80 6926.992
#> 341    Phagnalon_saxatile         Ononis_natrix   5  33.9250 1839 6926.992
#> 352    Phagnalon_saxatile    Phagnalon_saxatile   3  33.9250  528 6926.992
#> 357    Phagnalon_saxatile Piptatherum_miliaceum   0  33.9250   19 6926.992
#> 343    Phagnalon_saxatile Salsola_oppositifolia   5  33.9250   60 6926.992
#> 363    Phagnalon_saxatile     Stipa_tenacissima   0  33.9250    1 6926.992
#> 359    Phagnalon_saxatile  Teucrium_lusitanicum   0  33.9250   16 6926.992
#> 355    Phagnalon_saxatile       Teucrium_polium   8  33.9250  308 6926.992
#> 344    Phagnalon_saxatile     Thymelaea_hirsuta   9  33.9250  170 6926.992
#> 345    Phagnalon_saxatile       Thymus_hyemalis   4  33.9250 1030 6926.992
#> 353    Phagnalon_saxatile   Whitania_frutescens   0  33.9250    5 6926.992
#> 349    Phagnalon_saxatile        Ziziphus_lotus   0  33.9250    4 6926.992
#> 469 Piptatherum_miliaceum  Artemisia_barrelieri   0  15.4830   54 6926.992
#> 490 Piptatherum_miliaceum  Artemisia_campestris   0  15.4830    1 6926.992
#> 488 Piptatherum_miliaceum       Asparagus_albus   0  15.4830    7 6926.992
#> 484 Piptatherum_miliaceum    Asparagus_horridus   0  15.4830   18 6926.992
#> 480 Piptatherum_miliaceum       Ballota_hirsuta   0  15.4830   11 6926.992
#> 481 Piptatherum_miliaceum  Helichrysum_stoechas   0  15.4830  796 6926.992
#> 486 Piptatherum_miliaceum     Hyparrhenia_hirta   0  15.4830   34 6926.992
#> 476 Piptatherum_miliaceum   Launaea_arborescens   5  15.4830   89 6926.992
#> 491 Piptatherum_miliaceum      Launaea_lanifera   0  15.4830    6 6926.992
#> 477 Piptatherum_miliaceum    Lycium_intrincatum   1  15.4830   20 6926.992
#> 470 Piptatherum_miliaceum        Lygeum_spartum   0  15.4830   80 6926.992
#> 471 Piptatherum_miliaceum         Ononis_natrix   0  15.4830 1839 6926.992
#> 482 Piptatherum_miliaceum    Phagnalon_saxatile   4  15.4830  528 6926.992
#> 487 Piptatherum_miliaceum Piptatherum_miliaceum   0  15.4830   19 6926.992
#> 473 Piptatherum_miliaceum Salsola_oppositifolia   0  15.4830   60 6926.992
#> 493 Piptatherum_miliaceum     Stipa_tenacissima   0  15.4830    1 6926.992
#> 489 Piptatherum_miliaceum  Teucrium_lusitanicum   0  15.4830   16 6926.992
#> 485 Piptatherum_miliaceum       Teucrium_polium   4  15.4830  308 6926.992
#> 474 Piptatherum_miliaceum     Thymelaea_hirsuta   0  15.4830  170 6926.992
#> 475 Piptatherum_miliaceum       Thymus_hyemalis   0  15.4830 1030 6926.992
#> 483 Piptatherum_miliaceum   Whitania_frutescens   0  15.4830    5 6926.992
#> 479 Piptatherum_miliaceum        Ziziphus_lotus   0  15.4830    4 6926.992
#> 105 Salsola_oppositifolia  Artemisia_barrelieri   2  61.6100   54 6926.992
#> 126 Salsola_oppositifolia  Artemisia_campestris   0  61.6100    1 6926.992
#> 124 Salsola_oppositifolia       Asparagus_albus   0  61.6100    7 6926.992
#> 120 Salsola_oppositifolia    Asparagus_horridus   2  61.6100   18 6926.992
#> 116 Salsola_oppositifolia       Ballota_hirsuta   4  61.6100   11 6926.992
#> 117 Salsola_oppositifolia  Helichrysum_stoechas  32  61.6100  796 6926.992
#> 122 Salsola_oppositifolia     Hyparrhenia_hirta   1  61.6100   34 6926.992
#> 112 Salsola_oppositifolia   Launaea_arborescens   9  61.6100   89 6926.992
#> 127 Salsola_oppositifolia      Launaea_lanifera   0  61.6100    6 6926.992
#> 113 Salsola_oppositifolia    Lycium_intrincatum   5  61.6100   20 6926.992
#> 106 Salsola_oppositifolia        Lygeum_spartum   1  61.6100   80 6926.992
#> 107 Salsola_oppositifolia         Ononis_natrix   6  61.6100 1839 6926.992
#> 118 Salsola_oppositifolia    Phagnalon_saxatile  46  61.6100  528 6926.992
#> 123 Salsola_oppositifolia Piptatherum_miliaceum   0  61.6100   19 6926.992
#> 109 Salsola_oppositifolia Salsola_oppositifolia   0  61.6100   60 6926.992
#> 129 Salsola_oppositifolia     Stipa_tenacissima   0  61.6100    1 6926.992
#> 125 Salsola_oppositifolia  Teucrium_lusitanicum   0  61.6100   16 6926.992
#> 121 Salsola_oppositifolia       Teucrium_polium  17  61.6100  308 6926.992
#> 110 Salsola_oppositifolia     Thymelaea_hirsuta   5  61.6100  170 6926.992
#> 111 Salsola_oppositifolia       Thymus_hyemalis  14  61.6100 1030 6926.992
#> 119 Salsola_oppositifolia   Whitania_frutescens   0  61.6100    5 6926.992
#> 115 Salsola_oppositifolia        Ziziphus_lotus   0  61.6100    4 6926.992
#> 625     Stipa_tenacissima  Artemisia_barrelieri   0   0.6000   54 6926.992
#> 646     Stipa_tenacissima  Artemisia_campestris   0   0.6000    1 6926.992
#> 644     Stipa_tenacissima       Asparagus_albus   0   0.6000    7 6926.992
#> 640     Stipa_tenacissima    Asparagus_horridus   0   0.6000   18 6926.992
#> 636     Stipa_tenacissima       Ballota_hirsuta   0   0.6000   11 6926.992
#> 637     Stipa_tenacissima  Helichrysum_stoechas   0   0.6000  796 6926.992
#> 642     Stipa_tenacissima     Hyparrhenia_hirta   0   0.6000   34 6926.992
#> 632     Stipa_tenacissima   Launaea_arborescens   0   0.6000   89 6926.992
#> 647     Stipa_tenacissima      Launaea_lanifera   0   0.6000    6 6926.992
#> 633     Stipa_tenacissima    Lycium_intrincatum   0   0.6000   20 6926.992
#> 626     Stipa_tenacissima        Lygeum_spartum   0   0.6000   80 6926.992
#> 627     Stipa_tenacissima         Ononis_natrix   0   0.6000 1839 6926.992
#> 638     Stipa_tenacissima    Phagnalon_saxatile   0   0.6000  528 6926.992
#> 643     Stipa_tenacissima Piptatherum_miliaceum   0   0.6000   19 6926.992
#> 629     Stipa_tenacissima Salsola_oppositifolia   0   0.6000   60 6926.992
#> 649     Stipa_tenacissima     Stipa_tenacissima   0   0.6000    1 6926.992
#> 645     Stipa_tenacissima  Teucrium_lusitanicum   0   0.6000   16 6926.992
#> 641     Stipa_tenacissima       Teucrium_polium   0   0.6000  308 6926.992
#> 630     Stipa_tenacissima     Thymelaea_hirsuta   0   0.6000  170 6926.992
#> 631     Stipa_tenacissima       Thymus_hyemalis   0   0.6000 1030 6926.992
#> 639     Stipa_tenacissima   Whitania_frutescens   0   0.6000    5 6926.992
#> 635     Stipa_tenacissima        Ziziphus_lotus   0   0.6000    4 6926.992
#> 521  Teucrium_lusitanicum  Artemisia_barrelieri   0   5.2825   54 6926.992
#> 542  Teucrium_lusitanicum  Artemisia_campestris   0   5.2825    1 6926.992
#> 540  Teucrium_lusitanicum       Asparagus_albus   0   5.2825    7 6926.992
#> 536  Teucrium_lusitanicum    Asparagus_horridus   0   5.2825   18 6926.992
#> 532  Teucrium_lusitanicum       Ballota_hirsuta   0   5.2825   11 6926.992
#> 533  Teucrium_lusitanicum  Helichrysum_stoechas   0   5.2825  796 6926.992
#> 538  Teucrium_lusitanicum     Hyparrhenia_hirta   0   5.2825   34 6926.992
#> 528  Teucrium_lusitanicum   Launaea_arborescens   0   5.2825   89 6926.992
#> 543  Teucrium_lusitanicum      Launaea_lanifera   0   5.2825    6 6926.992
#> 529  Teucrium_lusitanicum    Lycium_intrincatum   0   5.2825   20 6926.992
#> 522  Teucrium_lusitanicum        Lygeum_spartum   0   5.2825   80 6926.992
#> 523  Teucrium_lusitanicum         Ononis_natrix   1   5.2825 1839 6926.992
#> 534  Teucrium_lusitanicum    Phagnalon_saxatile   0   5.2825  528 6926.992
#> 539  Teucrium_lusitanicum Piptatherum_miliaceum   0   5.2825   19 6926.992
#> 525  Teucrium_lusitanicum Salsola_oppositifolia   0   5.2825   60 6926.992
#> 545  Teucrium_lusitanicum     Stipa_tenacissima   0   5.2825    1 6926.992
#> 541  Teucrium_lusitanicum  Teucrium_lusitanicum   0   5.2825   16 6926.992
#> 537  Teucrium_lusitanicum       Teucrium_polium   0   5.2825  308 6926.992
#> 526  Teucrium_lusitanicum     Thymelaea_hirsuta   1   5.2825  170 6926.992
#> 527  Teucrium_lusitanicum       Thymus_hyemalis   1   5.2825 1030 6926.992
#> 535  Teucrium_lusitanicum   Whitania_frutescens   0   5.2825    5 6926.992
#> 531  Teucrium_lusitanicum        Ziziphus_lotus   0   5.2825    4 6926.992
#> 417       Teucrium_polium  Artemisia_barrelieri   0  17.0450   54 6926.992
#> 438       Teucrium_polium  Artemisia_campestris   0  17.0450    1 6926.992
#> 436       Teucrium_polium       Asparagus_albus   0  17.0450    7 6926.992
#> 432       Teucrium_polium    Asparagus_horridus   0  17.0450   18 6926.992
#> 428       Teucrium_polium       Ballota_hirsuta   0  17.0450   11 6926.992
#> 429       Teucrium_polium  Helichrysum_stoechas   6  17.0450  796 6926.992
#> 434       Teucrium_polium     Hyparrhenia_hirta   3  17.0450   34 6926.992
#> 424       Teucrium_polium   Launaea_arborescens   0  17.0450   89 6926.992
#> 439       Teucrium_polium      Launaea_lanifera   0  17.0450    6 6926.992
#> 425       Teucrium_polium    Lycium_intrincatum   0  17.0450   20 6926.992
#> 418       Teucrium_polium        Lygeum_spartum   0  17.0450   80 6926.992
#> 419       Teucrium_polium         Ononis_natrix   8  17.0450 1839 6926.992
#> 430       Teucrium_polium    Phagnalon_saxatile   7  17.0450  528 6926.992
#> 435       Teucrium_polium Piptatherum_miliaceum   2  17.0450   19 6926.992
#> 421       Teucrium_polium Salsola_oppositifolia   3  17.0450   60 6926.992
#> 441       Teucrium_polium     Stipa_tenacissima   0  17.0450    1 6926.992
#> 437       Teucrium_polium  Teucrium_lusitanicum   0  17.0450   16 6926.992
#> 433       Teucrium_polium       Teucrium_polium   0  17.0450  308 6926.992
#> 422       Teucrium_polium     Thymelaea_hirsuta  12  17.0450  170 6926.992
#> 423       Teucrium_polium       Thymus_hyemalis   3  17.0450 1030 6926.992
#> 431       Teucrium_polium   Whitania_frutescens   0  17.0450    5 6926.992
#> 427       Teucrium_polium        Ziziphus_lotus   0  17.0450    4 6926.992
#> 131     Thymelaea_hirsuta  Artemisia_barrelieri   5 140.9300   54 6926.992
#> 152     Thymelaea_hirsuta  Artemisia_campestris   0 140.9300    1 6926.992
#> 150     Thymelaea_hirsuta       Asparagus_albus   0 140.9300    7 6926.992
#> 146     Thymelaea_hirsuta    Asparagus_horridus   5 140.9300   18 6926.992
#> 142     Thymelaea_hirsuta       Ballota_hirsuta   5 140.9300   11 6926.992
#> 143     Thymelaea_hirsuta  Helichrysum_stoechas  58 140.9300  796 6926.992
#> 148     Thymelaea_hirsuta     Hyparrhenia_hirta   2 140.9300   34 6926.992
#> 138     Thymelaea_hirsuta   Launaea_arborescens   7 140.9300   89 6926.992
#> 153     Thymelaea_hirsuta      Launaea_lanifera   1 140.9300    6 6926.992
#> 139     Thymelaea_hirsuta    Lycium_intrincatum   1 140.9300   20 6926.992
#> 132     Thymelaea_hirsuta        Lygeum_spartum   2 140.9300   80 6926.992
#> 133     Thymelaea_hirsuta         Ononis_natrix  43 140.9300 1839 6926.992
#> 144     Thymelaea_hirsuta    Phagnalon_saxatile 138 140.9300  528 6926.992
#> 149     Thymelaea_hirsuta Piptatherum_miliaceum   0 140.9300   19 6926.992
#> 135     Thymelaea_hirsuta Salsola_oppositifolia  11 140.9300   60 6926.992
#> 155     Thymelaea_hirsuta     Stipa_tenacissima   0 140.9300    1 6926.992
#> 151     Thymelaea_hirsuta  Teucrium_lusitanicum   5 140.9300   16 6926.992
#> 147     Thymelaea_hirsuta       Teucrium_polium  54 140.9300  308 6926.992
#> 136     Thymelaea_hirsuta     Thymelaea_hirsuta  19 140.9300  170 6926.992
#> 137     Thymelaea_hirsuta       Thymus_hyemalis  34 140.9300 1030 6926.992
#> 145     Thymelaea_hirsuta   Whitania_frutescens   1 140.9300    5 6926.992
#> 141     Thymelaea_hirsuta        Ziziphus_lotus   0 140.9300    4 6926.992
#> 157       Thymus_hyemalis  Artemisia_barrelieri   2 522.5800   54 6926.992
#> 178       Thymus_hyemalis  Artemisia_campestris   2 522.5800    1 6926.992
#> 176       Thymus_hyemalis       Asparagus_albus   3 522.5800    7 6926.992
#> 172       Thymus_hyemalis    Asparagus_horridus   2 522.5800   18 6926.992
#> 168       Thymus_hyemalis       Ballota_hirsuta   1 522.5800   11 6926.992
#> 169       Thymus_hyemalis  Helichrysum_stoechas  86 522.5800  796 6926.992
#> 174       Thymus_hyemalis     Hyparrhenia_hirta   3 522.5800   34 6926.992
#> 164       Thymus_hyemalis   Launaea_arborescens   4 522.5800   89 6926.992
#> 179       Thymus_hyemalis      Launaea_lanifera   1 522.5800    6 6926.992
#> 165       Thymus_hyemalis    Lycium_intrincatum   2 522.5800   20 6926.992
#> 158       Thymus_hyemalis        Lygeum_spartum   6 522.5800   80 6926.992
#> 159       Thymus_hyemalis         Ononis_natrix   7 522.5800 1839 6926.992
#> 170       Thymus_hyemalis    Phagnalon_saxatile 133 522.5800  528 6926.992
#> 175       Thymus_hyemalis Piptatherum_miliaceum   4 522.5800   19 6926.992
#> 161       Thymus_hyemalis Salsola_oppositifolia  27 522.5800   60 6926.992
#> 181       Thymus_hyemalis     Stipa_tenacissima   0 522.5800    1 6926.992
#> 177       Thymus_hyemalis  Teucrium_lusitanicum   0 522.5800   16 6926.992
#> 173       Thymus_hyemalis       Teucrium_polium  87 522.5800  308 6926.992
#> 162       Thymus_hyemalis     Thymelaea_hirsuta  65 522.5800  170 6926.992
#> 163       Thymus_hyemalis       Thymus_hyemalis   9 522.5800 1030 6926.992
#> 171       Thymus_hyemalis   Whitania_frutescens   0 522.5800    5 6926.992
#> 167       Thymus_hyemalis        Ziziphus_lotus   0 522.5800    4 6926.992
#> 365   Whitania_frutescens  Artemisia_barrelieri   0  56.8150   54 6926.992
#> 386   Whitania_frutescens  Artemisia_campestris   0  56.8150    1 6926.992
#> 384   Whitania_frutescens       Asparagus_albus   0  56.8150    7 6926.992
#> 380   Whitania_frutescens    Asparagus_horridus   4  56.8150   18 6926.992
#> 376   Whitania_frutescens       Ballota_hirsuta   0  56.8150   11 6926.992
#> 377   Whitania_frutescens  Helichrysum_stoechas   0  56.8150  796 6926.992
#> 382   Whitania_frutescens     Hyparrhenia_hirta   0  56.8150   34 6926.992
#> 372   Whitania_frutescens   Launaea_arborescens   8  56.8150   89 6926.992
#> 387   Whitania_frutescens      Launaea_lanifera   0  56.8150    6 6926.992
#> 373   Whitania_frutescens    Lycium_intrincatum   3  56.8150   20 6926.992
#> 366   Whitania_frutescens        Lygeum_spartum   0  56.8150   80 6926.992
#> 374   Whitania_frutescens Maytenus_senegalensis   1  56.8150    0 6926.992
#> 367   Whitania_frutescens         Ononis_natrix   4  56.8150 1839 6926.992
#> 378   Whitania_frutescens    Phagnalon_saxatile  10  56.8150  528 6926.992
#> 383   Whitania_frutescens Piptatherum_miliaceum   0  56.8150   19 6926.992
#> 369   Whitania_frutescens Salsola_oppositifolia   0  56.8150   60 6926.992
#> 389   Whitania_frutescens     Stipa_tenacissima   0  56.8150    1 6926.992
#> 385   Whitania_frutescens  Teucrium_lusitanicum   0  56.8150   16 6926.992
#> 381   Whitania_frutescens       Teucrium_polium   3  56.8150  308 6926.992
#> 370   Whitania_frutescens     Thymelaea_hirsuta   2  56.8150  170 6926.992
#> 371   Whitania_frutescens       Thymus_hyemalis   5  56.8150 1030 6926.992
#> 379   Whitania_frutescens   Whitania_frutescens   1  56.8150    5 6926.992
#> 375   Whitania_frutescens        Ziziphus_lotus   0  56.8150    4 6926.992
#> 261        Ziziphus_lotus  Artemisia_barrelieri   0 465.1950   54 6926.992
#> 282        Ziziphus_lotus  Artemisia_campestris   0 465.1950    1 6926.992
#> 280        Ziziphus_lotus       Asparagus_albus   4 465.1950    7 6926.992
#> 276        Ziziphus_lotus    Asparagus_horridus   0 465.1950   18 6926.992
#> 272        Ziziphus_lotus       Ballota_hirsuta  32 465.1950   11 6926.992
#> 273        Ziziphus_lotus  Helichrysum_stoechas  12 465.1950  796 6926.992
#> 278        Ziziphus_lotus     Hyparrhenia_hirta   4 465.1950   34 6926.992
#> 268        Ziziphus_lotus   Launaea_arborescens   4 465.1950   89 6926.992
#> 283        Ziziphus_lotus      Launaea_lanifera   0 465.1950    6 6926.992
#> 269        Ziziphus_lotus    Lycium_intrincatum  13 465.1950   20 6926.992
#> 262        Ziziphus_lotus        Lygeum_spartum   2 465.1950   80 6926.992
#> 263        Ziziphus_lotus         Ononis_natrix   9 465.1950 1839 6926.992
#> 274        Ziziphus_lotus    Phagnalon_saxatile  40 465.1950  528 6926.992
#> 279        Ziziphus_lotus Piptatherum_miliaceum   0 465.1950   19 6926.992
#> 265        Ziziphus_lotus Salsola_oppositifolia  14 465.1950   60 6926.992
#> 285        Ziziphus_lotus     Stipa_tenacissima   0 465.1950    1 6926.992
#> 281        Ziziphus_lotus  Teucrium_lusitanicum   7 465.1950   16 6926.992
#> 277        Ziziphus_lotus       Teucrium_polium   9 465.1950  308 6926.992
#> 266        Ziziphus_lotus     Thymelaea_hirsuta   3 465.1950  170 6926.992
#> 267        Ziziphus_lotus       Thymus_hyemalis   9 465.1950 1030 6926.992
#> 275        Ziziphus_lotus   Whitania_frutescens   2 465.1950    5 6926.992
#> 271        Ziziphus_lotus        Ziziphus_lotus   0 465.1950    4 6926.992
#>             Dcr          Dro     max_Recr            Ns        NintC
#> 22  0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 20  0.000000000 0.0010105396 0.0105515284 -0.0957718733 -1.000000000
#> 16  0.000000000 0.0025985305 0.0704039426 -0.0369088776 -1.000000000
#> 12  0.000000000 0.0015879909 0.6359300477 -0.0024971156 -1.000000000
#> 18  0.000000000 0.0049083354 0.1760046935 -0.0278875256 -1.000000000
#> 8   0.000000000 0.0128482897 0.3229348318 -0.0397860139 -1.000000000
#> 23  0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 9   0.000000000 0.0028872561 0.0811556565 -0.0355767698 -1.000000000
#> 2   0.000000000 0.0115490245 0.0360685302 -0.3201967030 -1.000000000
#> 3   0.000000000 0.2654831996 0.5159958720 -0.5145064409 -1.000000000
#> 19  0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 5   0.000000000 0.0086617683 0.2407980736 -0.0359710865 -1.000000000
#> 25  0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 21  0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 6   0.000000000 0.0245416770 0.7040187738 -0.0348594070 -1.000000000
#> 7   1.000000000 0.1486936898 1.0000000000  0.8513063102  0.851306310
#> 15  0.000000000 0.0007218140 0.1082055906 -0.0066707646 -1.000000000
#> 11  0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 547 0.000000000 0.0077955915 0.0357300979 -0.2181799646 -1.000000000
#> 568 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 566 0.000000000 0.0010105396 0.0105515284 -0.0957718733 -1.000000000
#> 562 0.000000000 0.0025985305 0.0704039426 -0.0369088776 -1.000000000
#> 558 0.000000000 0.0015879909 0.6359300477 -0.0024971156 -1.000000000
#> 559 0.000000000 0.1149127933 0.5193962019 -0.2212430374 -1.000000000
#> 564 0.000000000 0.0049083354 0.1760046935 -0.0278875256 -1.000000000
#> 554 0.000000000 0.0128482897 0.3229348318 -0.0397860139 -1.000000000
#> 569 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 555 0.000000000 0.0028872561 0.0811556565 -0.0355767698 -1.000000000
#> 548 0.000000000 0.0115490245 0.0360685302 -0.3201967030 -1.000000000
#> 549 0.000000000 0.2654831996 0.5159958720 -0.5145064409 -1.000000000
#> 560 0.000000000 0.0762235614 0.9792095366 -0.0778419312 -1.000000000
#> 565 0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 551 0.000000000 0.0086617683 0.2407980736 -0.0359710865 -1.000000000
#> 571 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 567 0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 563 0.000000000 0.0444637441 0.4127966976 -0.1077134202 -1.000000000
#> 552 0.000000000 0.0245416770 0.7040187738 -0.0348594070 -1.000000000
#> 553 0.000000000 0.1486936898 1.0000000000 -0.1486936898 -1.000000000
#> 561 0.000000000 0.0007218140 0.1082055906 -0.0066707646 -1.000000000
#> 557 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 495 0.000000000 0.0077955915 0.0357300979 -0.2181799646 -1.000000000
#> 516 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 514 0.000000000 0.0010105396 0.0105515284 -0.0957718733 -1.000000000
#> 510 0.000000000 0.0025985305 0.0704039426 -0.0369088776 -1.000000000
#> 506 0.000000000 0.0015879909 0.6359300477 -0.0024971156 -1.000000000
#> 507 0.000000000 0.1149127933 0.5193962019 -0.2212430374 -1.000000000
#> 512 0.000000000 0.0049083354 0.1760046935 -0.0278875256 -1.000000000
#> 502 0.000000000 0.0128482897 0.3229348318 -0.0397860139 -1.000000000
#> 517 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 503 0.072137060 0.0028872561 0.0811556565  0.8532960886  0.959975412
#> 496 0.036068530 0.0115490245 0.0360685302  0.6798032970  0.679803297
#> 504 0.036068530 0.0000000000 0.0360685302  1.0000000000  1.000000000
#> 497 0.036068530 0.2654831996 0.5159958720 -0.4446056293 -0.864140065
#> 508 0.144274121 0.0762235614 0.9792095366  0.0694954010  0.471675440
#> 513 0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 499 0.000000000 0.0086617683 0.2407980736 -0.0359710865 -1.000000000
#> 519 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 515 0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 511 0.000000000 0.0444637441 0.4127966976 -0.1077134202 -1.000000000
#> 500 0.072137060 0.0245416770 0.7040187738  0.0676052759  0.659791003
#> 501 0.000000000 0.1486936898 1.0000000000 -0.1486936898 -1.000000000
#> 509 0.108205591 0.0007218140 0.1082055906  0.9933292354  0.993329235
#> 505 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 391 0.000000000 0.0077955915 0.0357300979 -0.2181799646 -1.000000000
#> 412 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 410 0.000000000 0.0010105396 0.0105515284 -0.0957718733 -1.000000000
#> 406 0.000000000 0.0025985305 0.0704039426 -0.0369088776 -1.000000000
#> 402 0.635930048 0.0015879909 0.6359300477  0.9975028844  0.997502884
#> 403 0.000000000 0.1149127933 0.5193962019 -0.2212430374 -1.000000000
#> 408 0.000000000 0.0049083354 0.1760046935 -0.0278875256 -1.000000000
#> 398 0.000000000 0.0128482897 0.3229348318 -0.0397860139 -1.000000000
#> 413 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 399 0.000000000 0.0028872561 0.0811556565 -0.0355767698 -1.000000000
#> 392 0.000000000 0.0115490245 0.0360685302 -0.3201967030 -1.000000000
#> 393 0.000000000 0.2654831996 0.5159958720 -0.5145064409 -1.000000000
#> 409 0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 395 0.000000000 0.0086617683 0.2407980736 -0.0359710865 -1.000000000
#> 415 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 411 0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 407 0.000000000 0.0444637441 0.4127966976 -0.1077134202 -1.000000000
#> 396 0.000000000 0.0245416770 0.7040187738 -0.0348594070 -1.000000000
#> 397 0.635930048 0.1486936898 1.0000000000  0.4872363579  0.766179173
#> 405 0.000000000 0.0007218140 0.1082055906 -0.0066707646 -1.000000000
#> 401 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 287 0.000000000 0.0077955915 0.0357300979 -0.2181799646 -1.000000000
#> 308 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 306 0.000000000 0.0010105396 0.0105515284 -0.0957718733 -1.000000000
#> 302 0.052008842 0.0025985305 0.0704039426  0.7018117049  0.950036755
#> 298 0.000000000 0.0015879909 0.6359300477 -0.0024971156 -1.000000000
#> 299 0.104017683 0.1149127933 0.5193962019 -0.0209764921 -0.094811987
#> 304 0.000000000 0.0049083354 0.1760046935 -0.0278875256 -1.000000000
#> 294 0.000000000 0.0128482897 0.3229348318 -0.0397860139 -1.000000000
#> 309 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 295 0.000000000 0.0028872561 0.0811556565 -0.0355767698 -1.000000000
#> 288 0.000000000 0.0115490245 0.0360685302 -0.3201967030 -1.000000000
#> 289 0.052008842 0.2654831996 0.5159958720 -0.4137133061 -0.804097428
#> 300 0.364061891 0.0762235614 0.9792095366  0.2939496792  0.790630210
#> 305 0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 291 0.130022104 0.0086617683 0.2407980736  0.5039921358  0.933382340
#> 311 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 307 0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 303 0.026004421 0.0444637441 0.4127966976 -0.0447177109 -0.415154498
#> 292 0.078013262 0.0245416770 0.7040187738  0.0759519309  0.685416604
#> 293 0.000000000 0.1486936898 1.0000000000 -0.1486936898 -1.000000000
#> 301 0.000000000 0.0007218140 0.1082055906 -0.0066707646 -1.000000000
#> 297 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 313 0.000000000 0.0077955915 0.0357300979 -0.2181799646 -1.000000000
#> 334 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 332 0.000000000 0.0010105396 0.0105515284 -0.0957718733 -1.000000000
#> 328 0.068799450 0.0025985305 0.0704039426  0.9403013047  0.962230359
#> 324 0.103199174 0.0015879909 0.6359300477  0.1597835861  0.984612369
#> 325 0.000000000 0.1149127933 0.5193962019 -0.2212430374 -1.000000000
#> 330 0.000000000 0.0049083354 0.1760046935 -0.0278875256 -1.000000000
#> 320 0.034399725 0.0128482897 0.3229348318  0.0667361739  0.626500218
#> 335 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 321 0.000000000 0.0028872561 0.0811556565 -0.0355767698 -1.000000000
#> 314 0.000000000 0.0115490245 0.0360685302 -0.3201967030 -1.000000000
#> 315 0.515995872 0.2654831996 0.5159958720  0.4854935591  0.485493559
#> 326 0.137598899 0.0762235614 0.9792095366  0.0626784519  0.446045268
#> 331 0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 317 0.240798074 0.0086617683 0.2407980736  0.9640289135  0.964028913
#> 337 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 333 0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 329 0.412796698 0.0444637441 0.4127966976  0.8922865798  0.892286580
#> 318 0.481596147 0.0245416770 0.7040187738  0.6492077871  0.949040961
#> 319 0.137598899 0.1486936898 1.0000000000 -0.0110947906 -0.074615074
#> 327 0.000000000 0.0007218140 0.1082055906 -0.0066707646 -1.000000000
#> 323 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 443 0.000000000 0.0077955915 0.0357300979 -0.2181799646 -1.000000000
#> 464 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 462 0.000000000 0.0010105396 0.0105515284 -0.0957718733 -1.000000000
#> 458 0.000000000 0.0025985305 0.0704039426 -0.0369088776 -1.000000000
#> 454 0.000000000 0.0015879909 0.6359300477 -0.0024971156 -1.000000000
#> 455 0.000000000 0.1149127933 0.5193962019 -0.2212430374 -1.000000000
#> 460 0.000000000 0.0049083354 0.1760046935 -0.0278875256 -1.000000000
#> 450 0.075443229 0.0128482897 0.3229348318  0.1938314889  0.829695920
#> 465 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 451 0.075443229 0.0028872561 0.0811556565  0.8940346975  0.961729420
#> 444 0.000000000 0.0115490245 0.0360685302 -0.3201967030 -1.000000000
#> 445 0.075443229 0.2654831996 0.5159958720 -0.3682974631 -0.715826730
#> 456 0.603545832 0.0762235614 0.9792095366  0.5385183157  0.873707087
#> 461 0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 447 0.000000000 0.0086617683 0.2407980736 -0.0359710865 -1.000000000
#> 467 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 463 0.377216145 0.0023098049 0.3772161449  0.9938767072  0.993876707
#> 459 0.150886458 0.0444637441 0.4127966976  0.2578090242  0.705316536
#> 448 0.150886458 0.0245416770 0.7040187738  0.1794622326  0.837350036
#> 449 0.075443229 0.1486936898 1.0000000000 -0.0732504609 -0.492626560
#> 457 0.075443229 0.0007218140 0.1082055906  0.6905504098  0.990432355
#> 453 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 183 0.000000000 0.0077955915 0.0357300979 -0.2181799646 -1.000000000
#> 204 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 202 0.010551528 0.0010105396 0.0105515284  0.9042281267  0.904228127
#> 198 0.014772140 0.0025985305 0.0704039426  0.1729109023  0.824092478
#> 194 0.010551528 0.0015879909 0.6359300477  0.0140951628  0.849501342
#> 195 0.073860699 0.1149127933 0.5193962019 -0.0790381109 -0.357245642
#> 200 0.008441223 0.0049083354 0.1760046935  0.0200726881  0.418527912
#> 190 0.018992751 0.0128482897 0.3229348318  0.0190269392  0.323516133
#> 205 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 191 0.012661834 0.0028872561 0.0811556565  0.1204423495  0.771971730
#> 184 0.002110306 0.0115490245 0.0360685302 -0.2616884781 -0.817274118
#> 192 0.002110306 0.0000000000 0.0360685302  0.0585082249  1.000000000
#> 185 0.238464542 0.2654831996 0.5159958720 -0.0523621593 -0.101771630
#> 196 0.354531354 0.0762235614 0.9792095366  0.2842167913  0.785001917
#> 201 0.004220611 0.0027428933 0.1173364623  0.0125938521  0.350119431
#> 187 0.016882445 0.0086617683 0.2407980736  0.0341392975  0.486936393
#> 207 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 203 0.008441223 0.0023098049 0.3772161449  0.0162543886  0.726366076
#> 199 0.029544279 0.0444637441 0.4127966976 -0.0361424031 -0.335542248
#> 188 0.042206114 0.0245416770 0.7040187738  0.0250908601  0.418527912
#> 189 0.029544279 0.1486936898 1.0000000000 -0.1191494104 -0.801307779
#> 197 0.075971004 0.0007218140 0.1082055906  0.6954279344  0.990498822
#> 193 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 573 0.000000000 0.0077955915 0.0357300979 -0.2181799646 -1.000000000
#> 594 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 592 0.000000000 0.0010105396 0.0105515284 -0.0957718733 -1.000000000
#> 588 0.000000000 0.0025985305 0.0704039426 -0.0369088776 -1.000000000
#> 584 0.000000000 0.0015879909 0.6359300477 -0.0024971156 -1.000000000
#> 585 0.000000000 0.1149127933 0.5193962019 -0.2212430374 -1.000000000
#> 590 0.000000000 0.0049083354 0.1760046935 -0.0278875256 -1.000000000
#> 580 0.000000000 0.0128482897 0.3229348318 -0.0397860139 -1.000000000
#> 595 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 581 0.000000000 0.0028872561 0.0811556565 -0.0355767698 -1.000000000
#> 574 0.000000000 0.0115490245 0.0360685302 -0.3201967030 -1.000000000
#> 575 0.000000000 0.2654831996 0.5159958720 -0.5145064409 -1.000000000
#> 586 0.000000000 0.0762235614 0.9792095366 -0.0778419312 -1.000000000
#> 591 0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 577 0.000000000 0.0086617683 0.2407980736 -0.0359710865 -1.000000000
#> 597 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 593 0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 589 0.000000000 0.0444637441 0.4127966976 -0.1077134202 -1.000000000
#> 578 0.000000000 0.0245416770 0.7040187738 -0.0348594070 -1.000000000
#> 579 0.000000000 0.1486936898 1.0000000000 -0.1486936898 -1.000000000
#> 587 0.000000000 0.0007218140 0.1082055906 -0.0066707646 -1.000000000
#> 583 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 209 0.000000000 0.0077955915 0.0357300979 -0.2181799646 -1.000000000
#> 230 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 228 0.006180470 0.0010105396 0.0105515284  0.4899697831  0.836494686
#> 224 0.012360939 0.0025985305 0.0704039426  0.1386628158  0.789778882
#> 220 0.123609394 0.0015879909 0.6359300477  0.1918786569  0.987153154
#> 221 0.105067985 0.1149127933 0.5193962019 -0.0189543322 -0.085671994
#> 226 0.000000000 0.0049083354 0.1760046935 -0.0278875256 -1.000000000
#> 216 0.018541409 0.0128482897 0.3229348318  0.0176293137  0.307048909
#> 231 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 217 0.024721879 0.0028872561 0.0811556565  0.2690462215  0.883210490
#> 210 0.000000000 0.0115490245 0.0360685302 -0.3201967030 -1.000000000
#> 211 0.055624227 0.2654831996 0.5159958720 -0.4067066881 -0.790479294
#> 222 0.259579728 0.0762235614 0.9792095366  0.1872491635  0.706357804
#> 227 0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 213 0.074165637 0.0086617683 0.2407980736  0.2720282071  0.883210490
#> 233 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 229 0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 225 0.043263288 0.0444637441 0.4127966976 -0.0029081050 -0.026998539
#> 214 0.024721879 0.0245416770 0.7040187738  0.0002559618  0.007289167
#> 215 0.000000000 0.1486936898 1.0000000000 -0.1486936898 -1.000000000
#> 223 0.000000000 0.0007218140 0.1082055906 -0.0066707646 -1.000000000
#> 219 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 27  0.035730098 0.0077955915 0.0357300979  0.7818200354  0.781820035
#> 48  0.004660448 0.0001443628 0.0046604476  0.9690238322  0.969023832
#> 46  0.006213930 0.0010105396 0.0105515284  0.4931409220  0.837375119
#> 42  0.010874378 0.0025985305 0.0704039426  0.1175480637  0.761040991
#> 38  0.000000000 0.0015879909 0.6359300477 -0.0024971156 -1.000000000
#> 39  0.006213930 0.1149127933 0.5193962019 -0.2092792801 -0.945924819
#> 44  0.000000000 0.0049083354 0.1760046935 -0.0278875256 -1.000000000
#> 34  0.009320895 0.0128482897 0.3229348318 -0.0109229301 -0.274541956
#> 49  0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 35  0.003106965 0.0028872561 0.0811556565  0.0027072534  0.070714965
#> 28  0.015534825 0.0115490245 0.0360685302  0.1105063255  0.256571972
#> 36  0.001553483 0.0000000000 0.0360685302  0.0430703029  1.000000000
#> 29  0.015534825 0.2654831996 0.5159958720 -0.4843999497 -0.941484715
#> 40  0.063692783 0.0762235614 0.9792095366 -0.0127968301 -0.164395075
#> 45  0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 31  0.026409203 0.0086617683 0.2407980736  0.0737025601  0.672017047
#> 51  0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 47  0.009320895 0.0023098049 0.3772161449  0.0185864002  0.752190657
#> 43  0.026409203 0.0444637441 0.4127966976 -0.0437371263 -0.406050855
#> 32  0.020195273 0.0245416770 0.7040187738 -0.0061737050 -0.177102984
#> 33  0.018641790 0.1486936898 1.0000000000 -0.1300518996 -0.874629581
#> 41  0.000000000 0.0007218140 0.1082055906 -0.0066707646 -1.000000000
#> 37  0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 235 0.000000000 0.0077955915 0.0357300979 -0.2181799646 -1.000000000
#> 256 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 254 0.008587377 0.0010105396 0.0105515284  0.7180795651  0.882322659
#> 250 0.000000000 0.0025985305 0.0704039426 -0.0369088776 -1.000000000
#> 246 0.000000000 0.0015879909 0.6359300477 -0.0024971156 -1.000000000
#> 247 0.000000000 0.1149127933 0.5193962019 -0.2212430374 -1.000000000
#> 252 0.000000000 0.0049083354 0.1760046935 -0.0278875256 -1.000000000
#> 242 0.008587377 0.0128482897 0.3229348318 -0.0131943437 -0.331632711
#> 257 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 243 0.008587377 0.0028872561 0.0811556565  0.0702368841  0.663779026
#> 236 0.000000000 0.0115490245 0.0360685302 -0.3201967030 -1.000000000
#> 237 0.025762130 0.2654831996 0.5159958720 -0.4645794336 -0.902961356
#> 248 0.060111636 0.0762235614 0.9792095366 -0.0164540120 -0.211377233
#> 253 0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 239 0.000000000 0.0086617683 0.2407980736 -0.0359710865 -1.000000000
#> 259 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 255 0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 251 0.000000000 0.0444637441 0.4127966976 -0.1077134202 -1.000000000
#> 240 0.000000000 0.0245416770 0.7040187738 -0.0348594070 -1.000000000
#> 241 0.017174753 0.1486936898 1.0000000000 -0.1315189367 -0.884495750
#> 249 0.008587377 0.0007218140 0.1082055906  0.0726909070  0.915944756
#> 245 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 53  0.004082216 0.0077955915 0.0357300979 -0.1039285055 -0.476343030
#> 74  0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 72  0.000000000 0.0010105396 0.0105515284 -0.0957718733 -1.000000000
#> 68  0.000000000 0.0025985305 0.0704039426 -0.0369088776 -1.000000000
#> 64  0.000000000 0.0015879909 0.6359300477 -0.0024971156 -1.000000000
#> 65  0.008164432 0.1149127933 0.5193962019 -0.2055239551 -0.928951064
#> 70  0.008164432 0.0049083354 0.1760046935  0.0185000536  0.398814810
#> 60  0.077562101 0.0128482897 0.3229348318  0.2003927872  0.834348353
#> 75  0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 61  0.008164432 0.0028872561 0.0811556565  0.0650253570  0.646361653
#> 54  0.000000000 0.0115490245 0.0360685302 -0.3201967030 -1.000000000
#> 55  0.146959770 0.2654831996 0.5159958720 -0.2296984071 -0.446444182
#> 66  0.012246647 0.0762235614 0.9792095366 -0.0653352643 -0.839332521
#> 71  0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 57  0.012246647 0.0086617683 0.2407980736  0.0148874909  0.292723306
#> 77  0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 73  0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 69  0.016328863 0.0444637441 0.4127966976 -0.0681567488 -0.632760047
#> 58  0.004082216 0.0245416770 0.7040187738 -0.0290609596 -0.833661904
#> 59  0.000000000 0.1486936898 1.0000000000 -0.1486936898 -1.000000000
#> 67  0.000000000 0.0007218140 0.1082055906 -0.0066707646 -1.000000000
#> 63  0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 339 0.000000000 0.0077955915 0.0357300979 -0.2181799646 -1.000000000
#> 360 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 358 0.000000000 0.0010105396 0.0105515284 -0.0957718733 -1.000000000
#> 354 0.058953574 0.0025985305 0.0704039426  0.8004529499  0.955922426
#> 350 0.000000000 0.0015879909 0.6359300477 -0.0024971156 -1.000000000
#> 351 0.000000000 0.1149127933 0.5193962019 -0.2212430374 -1.000000000
#> 356 0.000000000 0.0049083354 0.1760046935 -0.0278875256 -1.000000000
#> 346 0.088430361 0.0128482897 0.3229348318  0.2340474423  0.854707257
#> 361 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 347 0.029476787 0.0028872561 0.0811556565  0.3276362000  0.902049836
#> 340 0.000000000 0.0115490245 0.0360685302 -0.3201967030 -1.000000000
#> 341 0.147383935 0.2654831996 0.5159958720 -0.2288763746 -0.444846471
#> 352 0.088430361 0.0762235614 0.9792095366  0.0124659731  0.138038560
#> 357 0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 343 0.147383935 0.0086617683 0.2407980736  0.5760933413  0.941229902
#> 363 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 359 0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 355 0.235814296 0.0444637441 0.4127966976  0.4635467124  0.811445935
#> 344 0.265291083 0.0245416770 0.7040187738  0.3419644692  0.907491512
#> 345 0.117907148 0.1486936898 1.0000000000 -0.0307865417 -0.207046726
#> 353 0.000000000 0.0007218140 0.1082055906 -0.0066707646 -1.000000000
#> 349 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 469 0.000000000 0.0077955915 0.0357300979 -0.2181799646 -1.000000000
#> 490 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 488 0.000000000 0.0010105396 0.0105515284 -0.0957718733 -1.000000000
#> 484 0.000000000 0.0025985305 0.0704039426 -0.0369088776 -1.000000000
#> 480 0.000000000 0.0015879909 0.6359300477 -0.0024971156 -1.000000000
#> 481 0.000000000 0.1149127933 0.5193962019 -0.2212430374 -1.000000000
#> 486 0.000000000 0.0049083354 0.1760046935 -0.0278875256 -1.000000000
#> 476 0.322934832 0.0128482897 0.3229348318  0.9602139861  0.960213986
#> 491 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 477 0.064586966 0.0028872561 0.0811556565  0.7602638295  0.955296614
#> 470 0.000000000 0.0115490245 0.0360685302 -0.3201967030 -1.000000000
#> 471 0.000000000 0.2654831996 0.5159958720 -0.5145064409 -1.000000000
#> 482 0.258347865 0.0762235614 0.9792095366  0.1859911461  0.704957650
#> 487 0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 473 0.000000000 0.0086617683 0.2407980736 -0.0359710865 -1.000000000
#> 493 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 489 0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 485 0.258347865 0.0444637441 0.4127966976  0.5181342837  0.827891962
#> 474 0.000000000 0.0245416770 0.7040187738 -0.0348594070 -1.000000000
#> 475 0.000000000 0.1486936898 1.0000000000 -0.1486936898 -1.000000000
#> 483 0.000000000 0.0007218140 0.1082055906 -0.0066707646 -1.000000000
#> 479 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 105 0.032462263 0.0077955915 0.0357300979  0.6903611389  0.759856804
#> 126 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 124 0.000000000 0.0010105396 0.0105515284 -0.0957718733 -1.000000000
#> 120 0.032462263 0.0025985305 0.0704039426  0.4241769851  0.919952268
#> 116 0.064924525 0.0015879909 0.6359300477  0.0995967003  0.975540971
#> 117 0.519396202 0.1149127933 0.5193962019  0.7787569626  0.778756963
#> 122 0.016231131 0.0049083354 0.1760046935  0.0643323521  0.697597456
#> 112 0.146080182 0.0128482897 0.3229348318  0.4125658770  0.912046319
#> 127 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 113 0.081155657 0.0028872561 0.0811556565  0.9644232302  0.964423230
#> 106 0.016231131 0.0115490245 0.0360685302  0.1298114126  0.288464603
#> 107 0.097386788 0.2654831996 0.5159958720 -0.3257708460 -0.633171560
#> 118 0.746632040 0.0762235614 0.9792095366  0.6846425140  0.897910139
#> 123 0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 109 0.000000000 0.0086617683 0.2407980736 -0.0359710865 -1.000000000
#> 129 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 125 0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 121 0.275929232 0.0444637441 0.4127966976  0.5607251450  0.838858160
#> 110 0.081155657 0.0245416770 0.7040187738  0.0804154402  0.697597456
#> 111 0.227235838 0.1486936898 1.0000000000  0.0785421485  0.345641555
#> 119 0.000000000 0.0007218140 0.1082055906 -0.0066707646 -1.000000000
#> 115 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 625 0.000000000 0.0077955915 0.0357300979 -0.2181799646 -1.000000000
#> 646 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 644 0.000000000 0.0010105396 0.0105515284 -0.0957718733 -1.000000000
#> 640 0.000000000 0.0025985305 0.0704039426 -0.0369088776 -1.000000000
#> 636 0.000000000 0.0015879909 0.6359300477 -0.0024971156 -1.000000000
#> 637 0.000000000 0.1149127933 0.5193962019 -0.2212430374 -1.000000000
#> 642 0.000000000 0.0049083354 0.1760046935 -0.0278875256 -1.000000000
#> 632 0.000000000 0.0128482897 0.3229348318 -0.0397860139 -1.000000000
#> 647 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 633 0.000000000 0.0028872561 0.0811556565 -0.0355767698 -1.000000000
#> 626 0.000000000 0.0115490245 0.0360685302 -0.3201967030 -1.000000000
#> 627 0.000000000 0.2654831996 0.5159958720 -0.5145064409 -1.000000000
#> 638 0.000000000 0.0762235614 0.9792095366 -0.0778419312 -1.000000000
#> 643 0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 629 0.000000000 0.0086617683 0.2407980736 -0.0359710865 -1.000000000
#> 649 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 645 0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 641 0.000000000 0.0444637441 0.4127966976 -0.1077134202 -1.000000000
#> 630 0.000000000 0.0245416770 0.7040187738 -0.0348594070 -1.000000000
#> 631 0.000000000 0.1486936898 1.0000000000 -0.1486936898 -1.000000000
#> 639 0.000000000 0.0007218140 0.1082055906 -0.0066707646 -1.000000000
#> 635 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 521 0.000000000 0.0077955915 0.0357300979 -0.2181799646 -1.000000000
#> 542 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 540 0.000000000 0.0010105396 0.0105515284 -0.0957718733 -1.000000000
#> 536 0.000000000 0.0025985305 0.0704039426 -0.0369088776 -1.000000000
#> 532 0.000000000 0.0015879909 0.6359300477 -0.0024971156 -1.000000000
#> 533 0.000000000 0.1149127933 0.5193962019 -0.2212430374 -1.000000000
#> 538 0.000000000 0.0049083354 0.1760046935 -0.0278875256 -1.000000000
#> 528 0.000000000 0.0128482897 0.3229348318 -0.0397860139 -1.000000000
#> 543 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 529 0.000000000 0.0028872561 0.0811556565 -0.0355767698 -1.000000000
#> 522 0.000000000 0.0115490245 0.0360685302 -0.3201967030 -1.000000000
#> 523 0.189304307 0.2654831996 0.5159958720 -0.1476346946 -0.286944308
#> 534 0.000000000 0.0762235614 0.9792095366 -0.0778419312 -1.000000000
#> 539 0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 525 0.000000000 0.0086617683 0.2407980736 -0.0359710865 -1.000000000
#> 545 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 541 0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 537 0.000000000 0.0444637441 0.4127966976 -0.1077134202 -1.000000000
#> 526 0.189304307 0.0245416770 0.7040187738  0.2340315853  0.870358591
#> 527 0.189304307 0.1486936898 1.0000000000  0.0406106168  0.214525583
#> 535 0.000000000 0.0007218140 0.1082055906 -0.0066707646 -1.000000000
#> 531 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 417 0.000000000 0.0077955915 0.0357300979 -0.2181799646 -1.000000000
#> 438 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 436 0.000000000 0.0010105396 0.0105515284 -0.0957718733 -1.000000000
#> 432 0.000000000 0.0025985305 0.0704039426 -0.0369088776 -1.000000000
#> 428 0.000000000 0.0015879909 0.6359300477 -0.0024971156 -1.000000000
#> 429 0.352009387 0.1149127933 0.5193962019  0.4564850354  0.673551906
#> 434 0.176004693 0.0049083354 0.1760046935  0.9721124744  0.972112474
#> 424 0.000000000 0.0128482897 0.3229348318 -0.0397860139 -1.000000000
#> 439 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 425 0.000000000 0.0028872561 0.0811556565 -0.0355767698 -1.000000000
#> 418 0.000000000 0.0115490245 0.0360685302 -0.3201967030 -1.000000000
#> 419 0.469345849 0.2654831996 0.5159958720  0.3950858149  0.434354858
#> 430 0.410677618 0.0762235614 0.9792095366  0.3415551464  0.814395628
#> 435 0.117336462 0.0027428933 0.1173364623  0.9766236918  0.976623692
#> 421 0.176004693 0.0086617683 0.2407980736  0.6949512619  0.950786720
#> 441 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 437 0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 433 0.000000000 0.0444637441 0.4127966976 -0.1077134202 -1.000000000
#> 422 0.704018774 0.0245416770 0.7040187738  0.9651405930  0.965140593
#> 423 0.176004693 0.1486936898 1.0000000000  0.0273110036  0.155172019
#> 431 0.000000000 0.0007218140 0.1082055906 -0.0066707646 -1.000000000
#> 427 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 131 0.035478606 0.0077955915 0.0357300979  0.7747813884  0.780273458
#> 152 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 150 0.000000000 0.0010105396 0.0105515284 -0.0957718733 -1.000000000
#> 146 0.035478606 0.0025985305 0.0704039426  0.4670203780  0.926757819
#> 142 0.035478606 0.0015879909 0.6359300477  0.0532929929  0.955240890
#> 143 0.411551834 0.1149127933 0.5193962019  0.5711228535  0.720781725
#> 148 0.014191443 0.0049083354 0.1760046935  0.0527435206  0.654134147
#> 138 0.049670049 0.0128482897 0.3229348318  0.1140222597  0.741327219
#> 153 0.007095721 0.0008661768 0.0070957213  0.8779296988  0.877929699
#> 139 0.007095721 0.0028872561 0.0811556565  0.0518567078  0.593098996
#> 132 0.014191443 0.0115490245 0.0360685302  0.0732610420  0.186197992
#> 133 0.305116015 0.2654831996 0.5159958720  0.0768083963  0.129894248
#> 144 0.979209537 0.0762235614 0.9792095366  0.9221580688  0.922158069
#> 149 0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 135 0.078052934 0.0086617683 0.2407980736  0.2881715983  0.889026999
#> 155 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 151 0.035478606 0.0023098049 0.3772161449  0.0879304928  0.934895839
#> 147 0.383168949 0.0444637441 0.4127966976  0.8205133591  0.883957862
#> 136 0.134818704 0.0245416770 0.7040187738  0.1566393276  0.817965340
#> 137 0.241254524 0.1486936898 1.0000000000  0.0925608337  0.383664656
#> 145 0.007095721 0.0007218140 0.1082055906  0.0589055262  0.898274749
#> 141 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 157 0.003827165 0.0077955915 0.0357300979 -0.1110667621 -0.509060318
#> 178 0.003827165 0.0001443628 0.0046604476  0.7902250518  0.962279443
#> 176 0.005740748 0.0010105396 0.0105515284  0.4482960206  0.823970732
#> 172 0.003827165 0.0025985305 0.0704039426  0.0174512204  0.321029965
#> 168 0.001913583 0.0015879909 0.6359300477  0.0005119930  0.170147735
#> 169 0.164568104 0.1149127933 0.5193962019  0.0956019911  0.301731075
#> 174 0.005740748 0.0049083354 0.1760046935  0.0047294900  0.145000697
#> 164 0.007654330 0.0128482897 0.3229348318 -0.0160836143 -0.404252970
#> 179 0.001913583 0.0008661768 0.0070957213  0.1476108959  0.547353310
#> 165 0.003827165 0.0028872561 0.0811556565  0.0115815600  0.245588850
#> 158 0.011481496 0.0115490245 0.0360685302 -0.0018722359 -0.005847143
#> 159 0.013395078 0.2654831996 0.5159958720 -0.4885467792 -0.949544535
#> 170 0.254506487 0.0762235614 0.9792095366  0.1820682081  0.700504446
#> 175 0.007654330 0.0027428933 0.1173364623  0.0418577229  0.641654704
#> 161 0.051666730 0.0086617683 0.2407980736  0.1785934641  0.832353078
#> 181 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 177 0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 173 0.166481687 0.0444637441 0.4127966976  0.2955884666  0.732921110
#> 162 0.124382870 0.0245416770 0.7040187738  0.1418160941  0.802692468
#> 163 0.017222243 0.1486936898 1.0000000000 -0.1314714464 -0.884176366
#> 171 0.000000000 0.0007218140 0.1082055906 -0.0066707646 -1.000000000
#> 167 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 365 0.000000000 0.0077955915 0.0357300979 -0.2181799646 -1.000000000
#> 386 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 384 0.000000000 0.0010105396 0.0105515284 -0.0957718733 -1.000000000
#> 380 0.070403943 0.0025985305 0.0704039426  0.9630911224  0.963091122
#> 376 0.000000000 0.0015879909 0.6359300477 -0.0024971156 -1.000000000
#> 377 0.000000000 0.1149127933 0.5193962019 -0.2212430374 -1.000000000
#> 382 0.000000000 0.0049083354 0.1760046935 -0.0278875256 -1.000000000
#> 372 0.140807885 0.0128482897 0.3229348318  0.3962396835  0.908753053
#> 387 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 373 0.052802957 0.0028872561 0.0811556565  0.6150612659  0.945320181
#> 366 0.000000000 0.0115490245 0.0360685302 -0.3201967030 -1.000000000
#> 374 0.017600986 0.0000000000 0.0360685302  0.4879873273  1.000000000
#> 367 0.070403943 0.2654831996 0.5159958720 -0.3780636001 -0.734808294
#> 378 0.176009857 0.0762235614 0.9792095366  0.1019049462  0.566935836
#> 383 0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 369 0.000000000 0.0086617683 0.2407980736 -0.0359710865 -1.000000000
#> 389 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 385 0.000000000 0.0023098049 0.3772161449 -0.0061232928 -1.000000000
#> 381 0.052802957 0.0444637441 0.4127966976  0.0202017431  0.157930792
#> 370 0.035201971 0.0245416770 0.7040187738  0.0151420598  0.302832312
#> 371 0.088004928 0.1486936898 1.0000000000 -0.0606887616 -0.408146180
#> 379 0.017600986 0.0007218140 0.1082055906  0.1559916778  0.958990136
#> 375 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#> 261 0.000000000 0.0077955915 0.0357300979 -0.2181799646 -1.000000000
#> 282 0.000000000 0.0001443628 0.0046604476 -0.0309761678 -1.000000000
#> 280 0.008598545 0.0010105396 0.0105515284  0.7191380032  0.882475503
#> 276 0.000000000 0.0025985305 0.0704039426 -0.0369088776 -1.000000000
#> 272 0.068788358 0.0015879909 0.6359300477  0.1056725766  0.976914831
#> 273 0.025795634 0.1149127933 0.5193962019 -0.1715783806 -0.775519911
#> 278 0.008598545 0.0049083354 0.1760046935  0.0209665392  0.429166729
#> 268 0.008598545 0.0128482897 0.3229348318 -0.0131597604 -0.330763480
#> 283 0.000000000 0.0008661768 0.0070957213 -0.1220703012 -1.000000000
#> 269 0.027945270 0.0028872561 0.0811556565  0.3087648504  0.896681761
#> 262 0.004299272 0.0115490245 0.0360685302 -0.2009993771 -0.627737185
#> 263 0.019346726 0.2654831996 0.5159958720 -0.4770124867 -0.927126366
#> 274 0.085985447 0.0762235614 0.9792095366  0.0099691488  0.113529509
#> 279 0.000000000 0.0027428933 0.1173364623 -0.0233763082 -1.000000000
#> 265 0.030094906 0.0086617683 0.2407980736  0.0890087606  0.712184905
#> 285 0.000000000 0.0001443628 0.0001443628 -1.0000000000 -1.000000000
#> 281 0.015047453 0.0023098049 0.3772161449  0.0337675057  0.846498616
#> 277 0.019346726 0.0444637441 0.4127966976 -0.0608459775 -0.564887619
#> 266 0.006448909 0.0245416770 0.7040187738 -0.0256992698 -0.737226249
#> 267 0.019346726 0.1486936898 1.0000000000 -0.1293469643 -0.869888725
#> 275 0.004299272 0.0007218140 0.1082055906  0.0330616773  0.832107862
#> 271 0.000000000 0.0005774512 0.0005774512 -1.0000000000 -1.000000000
#>           NintA          RII
#> 22  -1.00000000 -1.000000000
#> 20  -1.00000000 -1.000000000
#> 16  -1.00000000 -1.000000000
#> 12  -1.00000000 -1.000000000
#> 18  -1.00000000 -1.000000000
#> 8   -1.00000000 -1.000000000
#> 23  -1.00000000 -1.000000000
#> 9   -1.00000000 -1.000000000
#> 2   -1.00000000 -1.000000000
#> 3   -1.00000000 -1.000000000
#> 19  -1.00000000 -1.000000000
#> 5   -1.00000000 -1.000000000
#> 25  -1.00000000 -1.000000000
#> 21  -1.00000000 -1.000000000
#> 6   -1.00000000 -1.000000000
#> 7    1.70261262  0.741108198
#> 15  -1.00000000 -1.000000000
#> 11  -1.00000000 -1.000000000
#> 547 -1.00000000 -1.000000000
#> 568 -1.00000000 -1.000000000
#> 566 -1.00000000 -1.000000000
#> 562 -1.00000000 -1.000000000
#> 558 -1.00000000 -1.000000000
#> 559 -1.00000000 -1.000000000
#> 564 -1.00000000 -1.000000000
#> 554 -1.00000000 -1.000000000
#> 569 -1.00000000 -1.000000000
#> 555 -1.00000000 -1.000000000
#> 548 -1.00000000 -1.000000000
#> 549 -1.00000000 -1.000000000
#> 560 -1.00000000 -1.000000000
#> 565 -1.00000000 -1.000000000
#> 551 -1.00000000 -1.000000000
#> 571 -1.00000000 -1.000000000
#> 567 -1.00000000 -1.000000000
#> 563 -1.00000000 -1.000000000
#> 552 -1.00000000 -1.000000000
#> 553 -1.00000000 -1.000000000
#> 561 -1.00000000 -1.000000000
#> 557 -1.00000000 -1.000000000
#> 495 -1.00000000 -1.000000000
#> 516 -1.00000000 -1.000000000
#> 514 -1.00000000 -1.000000000
#> 510 -1.00000000 -1.000000000
#> 506 -1.00000000 -1.000000000
#> 507 -1.00000000 -1.000000000
#> 512 -1.00000000 -1.000000000
#> 502 -1.00000000 -1.000000000
#> 517 -1.00000000 -1.000000000
#> 503  1.91995082  0.923031458
#> 496  1.35960659  0.514925765
#> 504  2.00000000  1.000000000
#> 497 -0.92711925 -0.760780479
#> 508  0.94335088  0.308622561
#> 513 -1.00000000 -1.000000000
#> 499 -1.00000000 -1.000000000
#> 519 -1.00000000 -1.000000000
#> 515 -1.00000000 -1.000000000
#> 511 -1.00000000 -1.000000000
#> 500  1.31958201  0.492304562
#> 501 -1.00000000 -1.000000000
#> 509  1.98665847  0.986746879
#> 505 -1.00000000 -1.000000000
#> 391 -1.00000000 -1.000000000
#> 412 -1.00000000 -1.000000000
#> 410 -1.00000000 -1.000000000
#> 406 -1.00000000 -1.000000000
#> 402  1.99500577  0.995018209
#> 403 -1.00000000 -1.000000000
#> 408 -1.00000000 -1.000000000
#> 398 -1.00000000 -1.000000000
#> 413 -1.00000000 -1.000000000
#> 399 -1.00000000 -1.000000000
#> 392 -1.00000000 -1.000000000
#> 393 -1.00000000 -1.000000000
#> 409 -1.00000000 -1.000000000
#> 395 -1.00000000 -1.000000000
#> 415 -1.00000000 -1.000000000
#> 411 -1.00000000 -1.000000000
#> 407 -1.00000000 -1.000000000
#> 396 -1.00000000 -1.000000000
#> 397  1.53235835  0.620980904
#> 405 -1.00000000 -1.000000000
#> 401 -1.00000000 -1.000000000
#> 287 -1.00000000 -1.000000000
#> 308 -1.00000000 -1.000000000
#> 306 -1.00000000 -1.000000000
#> 302  1.90007351  0.904828582
#> 298 -1.00000000 -1.000000000
#> 299 -0.17320232 -0.049765161
#> 304 -1.00000000 -1.000000000
#> 294 -1.00000000 -1.000000000
#> 309 -1.00000000 -1.000000000
#> 295 -1.00000000 -1.000000000
#> 288 -1.00000000 -1.000000000
#> 289 -0.89141242 -0.672377038
#> 300  1.58126042  0.653753895
#> 305 -1.00000000 -1.000000000
#> 291  1.86676468  0.875086148
#> 311 -1.00000000 -1.000000000
#> 307 -1.00000000 -1.000000000
#> 303 -0.58672675 -0.261952662
#> 292  1.37083321  0.521394539
#> 293 -1.00000000 -1.000000000
#> 301 -1.00000000 -1.000000000
#> 297 -1.00000000 -1.000000000
#> 313 -1.00000000 -1.000000000
#> 334 -1.00000000 -1.000000000
#> 332 -1.00000000 -1.000000000
#> 328  1.92446072  0.927209972
#> 324  1.96922474  0.969691119
#> 325 -1.00000000 -1.000000000
#> 330 -1.00000000 -1.000000000
#> 320  1.25300044  0.456134196
#> 335 -1.00000000 -1.000000000
#> 321 -1.00000000 -1.000000000
#> 314 -1.00000000 -1.000000000
#> 315  0.97098712  0.320562228
#> 326  0.89209054  0.287038778
#> 331 -1.00000000 -1.000000000
#> 317  1.92805783  0.930555810
#> 337 -1.00000000 -1.000000000
#> 333 -1.00000000 -1.000000000
#> 329  1.78457316  0.805521143
#> 318  1.89808192  0.903023739
#> 319 -0.13886847 -0.038753328
#> 327 -1.00000000 -1.000000000
#> 323 -1.00000000 -1.000000000
#> 443 -1.00000000 -1.000000000
#> 464 -1.00000000 -1.000000000
#> 462 -1.00000000 -1.000000000
#> 458 -1.00000000 -1.000000000
#> 454 -1.00000000 -1.000000000
#> 455 -1.00000000 -1.000000000
#> 460 -1.00000000 -1.000000000
#> 450  1.65939184  0.708957556
#> 465 -1.00000000 -1.000000000
#> 451  1.92345884  0.926280142
#> 444 -1.00000000 -1.000000000
#> 445 -0.83438114 -0.557422232
#> 456  1.74741417  0.775737001
#> 461 -1.00000000 -1.000000000
#> 447 -1.00000000 -1.000000000
#> 467 -1.00000000 -1.000000000
#> 463  1.98775341  0.987827948
#> 459  1.41063307  0.544779133
#> 448  1.67470007  0.720208198
#> 449 -0.66008012 -0.326811224
#> 457  1.98086471  0.981046055
#> 453 -1.00000000 -1.000000000
#> 183 -1.00000000 -1.000000000
#> 204 -1.00000000 -1.000000000
#> 202  1.80845625  0.825197424
#> 198  1.64818496  0.700814020
#> 194  1.69900268  0.738376647
#> 195 -0.52642739 -0.217467475
#> 200  0.83705582  0.264644514
#> 190  0.64703227  0.192973007
#> 205 -1.00000000 -1.000000000
#> 191  1.54394346  0.628627003
#> 184 -0.89945057 -0.691008906
#> 192  2.00000000  1.000000000
#> 185 -0.18474179 -0.053614008
#> 196  1.57000383  0.646093132
#> 201  0.70023886  0.212208955
#> 187  0.97387279  0.321821496
#> 207 -1.00000000 -1.000000000
#> 203  1.45273215  0.570309932
#> 199 -0.50248092 -0.201592529
#> 188  0.83705582  0.264644514
#> 189 -0.88969557 -0.668485008
#> 197  1.98099764  0.981176490
#> 193 -1.00000000 -1.000000000
#> 573 -1.00000000 -1.000000000
#> 594 -1.00000000 -1.000000000
#> 592 -1.00000000 -1.000000000
#> 588 -1.00000000 -1.000000000
#> 584 -1.00000000 -1.000000000
#> 585 -1.00000000 -1.000000000
#> 590 -1.00000000 -1.000000000
#> 580 -1.00000000 -1.000000000
#> 595 -1.00000000 -1.000000000
#> 581 -1.00000000 -1.000000000
#> 574 -1.00000000 -1.000000000
#> 575 -1.00000000 -1.000000000
#> 586 -1.00000000 -1.000000000
#> 591 -1.00000000 -1.000000000
#> 577 -1.00000000 -1.000000000
#> 597 -1.00000000 -1.000000000
#> 593 -1.00000000 -1.000000000
#> 589 -1.00000000 -1.000000000
#> 578 -1.00000000 -1.000000000
#> 579 -1.00000000 -1.000000000
#> 587 -1.00000000 -1.000000000
#> 583 -1.00000000 -1.000000000
#> 209 -1.00000000 -1.000000000
#> 230 -1.00000000 -1.000000000
#> 228  1.67298937  0.718943589
#> 224  1.57955776  0.652590565
#> 220  1.97430631  0.974632204
#> 221 -0.15782298 -0.044753038
#> 226 -1.00000000 -1.000000000
#> 216  0.61409782  0.181369037
#> 231 -1.00000000 -1.000000000
#> 217  1.76642098  0.790847767
#> 210 -1.00000000 -1.000000000
#> 211 -0.88298066 -0.653547550
#> 222  1.41271561  0.546022545
#> 227 -1.00000000 -1.000000000
#> 213  1.76642098  0.790847767
#> 233 -1.00000000 -1.000000000
#> 229 -1.00000000 -1.000000000
#> 225 -0.05257756 -0.013683993
#> 214  0.01457833  0.003657915
#> 215 -1.00000000 -1.000000000
#> 223 -1.00000000 -1.000000000
#> 219 -1.00000000 -1.000000000
#> 27   1.56364007  0.641793543
#> 48   1.93804766  0.939909052
#> 46   1.67475024  0.720245311
#> 42   1.52208198  0.614258410
#> 38  -1.00000000 -1.000000000
#> 39  -0.97221106 -0.897397867
#> 44  -1.00000000 -1.000000000
#> 34  -0.43080882 -0.159112508
#> 49  -1.00000000 -1.000000000
#> 35   0.14142993  0.036653457
#> 28   0.51314394  0.147165222
#> 36   2.00000000  1.000000000
#> 29  -0.96986055 -0.889438941
#> 40  -0.28236993 -0.089559073
#> 45  -1.00000000 -1.000000000
#> 31   1.34403409  0.506043428
#> 51  -1.00000000 -1.000000000
#> 47   1.50438131  0.602808964
#> 43  -0.57757634 -0.254745176
#> 32  -0.30091332 -0.097154684
#> 33  -0.93312256 -0.777192617
#> 41  -1.00000000 -1.000000000
#> 37  -1.00000000 -1.000000000
#> 235 -1.00000000 -1.000000000
#> 256 -1.00000000 -1.000000000
#> 254  1.76464532  0.789425200
#> 250 -1.00000000 -1.000000000
#> 246 -1.00000000 -1.000000000
#> 247 -1.00000000 -1.000000000
#> 252 -1.00000000 -1.000000000
#> 242 -0.49808436 -0.198776800
#> 257 -1.00000000 -1.000000000
#> 243  1.32755805  0.496758424
#> 236 -1.00000000 -1.000000000
#> 237 -0.94900651 -0.823089835
#> 248 -0.34898664 -0.118178767
#> 253 -1.00000000 -1.000000000
#> 239 -1.00000000 -1.000000000
#> 259 -1.00000000 -1.000000000
#> 255 -1.00000000 -1.000000000
#> 251 -1.00000000 -1.000000000
#> 240 -1.00000000 -1.000000000
#> 241 -0.93870814 -0.792911143
#> 249  1.83188951  0.844924428
#> 245 -1.00000000 -1.000000000
#> 53  -0.64530129 -0.312631412
#> 74  -1.00000000 -1.000000000
#> 72  -1.00000000 -1.000000000
#> 68  -1.00000000 -1.000000000
#> 64  -1.00000000 -1.000000000
#> 65  -0.96316706 -0.867328311
#> 70   0.79762962  0.249074756
#> 60   1.66869671  0.715778471
#> 75  -1.00000000 -1.000000000
#> 61   1.29272331  0.477499514
#> 54  -1.00000000 -1.000000000
#> 55  -0.61729887 -0.287369257
#> 66  -0.91264903 -0.723146410
#> 71  -1.00000000 -1.000000000
#> 57   0.58544661  0.171456277
#> 77  -1.00000000 -1.000000000
#> 73  -1.00000000 -1.000000000
#> 69  -0.77508027 -0.462801022
#> 58  -0.90928639 -0.714768648
#> 59  -1.00000000 -1.000000000
#> 67  -1.00000000 -1.000000000
#> 63  -1.00000000 -1.000000000
#> 339 -1.00000000 -1.000000000
#> 360 -1.00000000 -1.000000000
#> 358 -1.00000000 -1.000000000
#> 354  1.91184485  0.915566478
#> 350 -1.00000000 -1.000000000
#> 351 -1.00000000 -1.000000000
#> 356 -1.00000000 -1.000000000
#> 346  1.70941451  0.746278419
#> 361 -1.00000000 -1.000000000
#> 347  1.80409967  0.821576303
#> 340 -1.00000000 -1.000000000
#> 341 -0.61576988 -0.286046659
#> 352  0.27607712  0.074136100
#> 357 -1.00000000 -1.000000000
#> 343  1.88245980  0.888984212
#> 363 -1.00000000 -1.000000000
#> 359 -1.00000000 -1.000000000
#> 355  1.62289187  0.682716890
#> 344  1.81498302  0.830649393
#> 345 -0.34306332 -0.115478038
#> 353 -1.00000000 -1.000000000
#> 349 -1.00000000 -1.000000000
#> 469 -1.00000000 -1.000000000
#> 490 -1.00000000 -1.000000000
#> 488 -1.00000000 -1.000000000
#> 484 -1.00000000 -1.000000000
#> 480 -1.00000000 -1.000000000
#> 481 -1.00000000 -1.000000000
#> 486 -1.00000000 -1.000000000
#> 476  1.92042797  0.923472689
#> 491 -1.00000000 -1.000000000
#> 477  1.91059323  0.914418988
#> 470 -1.00000000 -1.000000000
#> 471 -1.00000000 -1.000000000
#> 482  1.40991530  0.544351040
#> 487 -1.00000000 -1.000000000
#> 473 -1.00000000 -1.000000000
#> 493 -1.00000000 -1.000000000
#> 489 -1.00000000 -1.000000000
#> 485  1.65578392  0.706327348
#> 474 -1.00000000 -1.000000000
#> 475 -1.00000000 -1.000000000
#> 483 -1.00000000 -1.000000000
#> 479 -1.00000000 -1.000000000
#> 105  1.51971361  0.612716988
#> 126 -1.00000000 -1.000000000
#> 124 -1.00000000 -1.000000000
#> 120  1.83990454  0.851770010
#> 116  1.95108194  0.952249863
#> 117  1.55751393  0.637675662
#> 122  1.39519491  0.535623537
#> 112  1.82409264  0.838313556
#> 127 -1.00000000 -1.000000000
#> 113  1.92884646  0.931290908
#> 106  0.57692921  0.168541418
#> 107 -0.77538891 -0.463241430
#> 118  1.79582028  0.814734052
#> 123 -1.00000000 -1.000000000
#> 109 -1.00000000 -1.000000000
#> 129 -1.00000000 -1.000000000
#> 125 -1.00000000 -1.000000000
#> 121  1.67771632  0.722442454
#> 110  1.39519491  0.535623537
#> 111  0.69128311  0.208927851
#> 119 -1.00000000 -1.000000000
#> 115 -1.00000000 -1.000000000
#> 625 -1.00000000 -1.000000000
#> 646 -1.00000000 -1.000000000
#> 644 -1.00000000 -1.000000000
#> 640 -1.00000000 -1.000000000
#> 636 -1.00000000 -1.000000000
#> 637 -1.00000000 -1.000000000
#> 642 -1.00000000 -1.000000000
#> 632 -1.00000000 -1.000000000
#> 647 -1.00000000 -1.000000000
#> 633 -1.00000000 -1.000000000
#> 626 -1.00000000 -1.000000000
#> 627 -1.00000000 -1.000000000
#> 638 -1.00000000 -1.000000000
#> 643 -1.00000000 -1.000000000
#> 629 -1.00000000 -1.000000000
#> 649 -1.00000000 -1.000000000
#> 645 -1.00000000 -1.000000000
#> 641 -1.00000000 -1.000000000
#> 630 -1.00000000 -1.000000000
#> 631 -1.00000000 -1.000000000
#> 639 -1.00000000 -1.000000000
#> 635 -1.00000000 -1.000000000
#> 521 -1.00000000 -1.000000000
#> 542 -1.00000000 -1.000000000
#> 540 -1.00000000 -1.000000000
#> 536 -1.00000000 -1.000000000
#> 532 -1.00000000 -1.000000000
#> 533 -1.00000000 -1.000000000
#> 538 -1.00000000 -1.000000000
#> 528 -1.00000000 -1.000000000
#> 543 -1.00000000 -1.000000000
#> 529 -1.00000000 -1.000000000
#> 522 -1.00000000 -1.000000000
#> 523 -0.44593120 -0.167504366
#> 534 -1.00000000 -1.000000000
#> 539 -1.00000000 -1.000000000
#> 525 -1.00000000 -1.000000000
#> 545 -1.00000000 -1.000000000
#> 541 -1.00000000 -1.000000000
#> 537 -1.00000000 -1.000000000
#> 526  1.74071718  0.770473342
#> 527  0.42905117  0.120150466
#> 535 -1.00000000 -1.000000000
#> 531 -1.00000000 -1.000000000
#> 417 -1.00000000 -1.000000000
#> 438 -1.00000000 -1.000000000
#> 436 -1.00000000 -1.000000000
#> 432 -1.00000000 -1.000000000
#> 428 -1.00000000 -1.000000000
#> 429  1.34710381  0.507786101
#> 434  1.94422495  0.945738177
#> 424 -1.00000000 -1.000000000
#> 439 -1.00000000 -1.000000000
#> 425 -1.00000000 -1.000000000
#> 418 -1.00000000 -1.000000000
#> 419  0.86870972  0.277428675
#> 430  1.62879126  0.686903361
#> 435  1.95324738  0.954315323
#> 421  1.90157344  0.906190131
#> 441 -1.00000000 -1.000000000
#> 437 -1.00000000 -1.000000000
#> 433 -1.00000000 -1.000000000
#> 422  1.93028119  0.932629676
#> 423  0.31034404  0.084111917
#> 431 -1.00000000 -1.000000000
#> 427 -1.00000000 -1.000000000
#> 131  1.56054692  0.639711797
#> 152 -1.00000000 -1.000000000
#> 150 -1.00000000 -1.000000000
#> 146  1.85351564  0.863512296
#> 142  1.91048178  0.914316879
#> 143  1.44156345  0.563454837
#> 148  1.30826829  0.486032204
#> 138  1.48265444  0.588975332
#> 153  1.75585940  0.782419513
#> 139  1.18619799  0.421564129
#> 132  0.37239598  0.102656184
#> 133  0.25978850  0.069458237
#> 144  1.84431614  0.855559653
#> 149 -1.00000000 -1.000000000
#> 135  1.77805400  0.800223766
#> 155 -1.00000000 -1.000000000
#> 151  1.86979168  0.877750622
#> 147  1.76791572  0.792047031
#> 136  1.63593068  0.691997763
#> 137  0.76732931  0.237366990
#> 145  1.79654950  0.815334629
#> 141 -1.00000000 -1.000000000
#> 157 -0.67467193 -0.341435890
#> 178  1.92455889  0.927301127
#> 176  1.64794146  0.700637947
#> 172  0.64205993  0.191206489
#> 168  0.34029547  0.092984411
#> 169  0.60346215  0.177669785
#> 174  0.29000139  0.078167521
#> 164 -0.57575519 -0.253331488
#> 179  1.09470662  0.376797272
#> 165  0.49117770  0.139983635
#> 158 -0.01162631 -0.002932144
#> 159 -0.97411936 -0.903936023
#> 170  1.40100889  0.539058747
#> 175  1.28330941  0.472379671
#> 161  1.66470616  0.712846548
#> 181 -1.00000000 -1.000000000
#> 177 -1.00000000 -1.000000000
#> 173  1.46584222  0.578433684
#> 162  1.60538494  0.670414616
#> 163 -0.93852824 -0.792397956
#> 171 -1.00000000 -1.000000000
#> 167 -1.00000000 -1.000000000
#> 365 -1.00000000 -1.000000000
#> 386 -1.00000000 -1.000000000
#> 384 -1.00000000 -1.000000000
#> 380  1.92618224  0.928809795
#> 376 -1.00000000 -1.000000000
#> 377 -1.00000000 -1.000000000
#> 382 -1.00000000 -1.000000000
#> 372  1.81750611  0.832765722
#> 387 -1.00000000 -1.000000000
#> 373  1.89064036  0.896310107
#> 366 -1.00000000 -1.000000000
#> 374  2.00000000  1.000000000
#> 367 -0.84713486 -0.580788106
#> 378  1.13387167  0.395610922
#> 383 -1.00000000 -1.000000000
#> 369 -1.00000000 -1.000000000
#> 389 -1.00000000 -1.000000000
#> 385 -1.00000000 -1.000000000
#> 381  0.31586158  0.085735537
#> 370  0.60566462  0.178433937
#> 371 -0.57969291 -0.256396772
#> 379  1.91798027  0.921211382
#> 375 -1.00000000 -1.000000000
#> 261 -1.00000000 -1.000000000
#> 282 -1.00000000 -1.000000000
#> 280  1.76495101  0.789669941
#> 276 -1.00000000 -1.000000000
#> 272  1.95382966  0.954871462
#> 273 -0.87356938 -0.633346281
#> 278  0.85833346  0.273209600
#> 268 -0.49710333 -0.198152554
#> 283 -1.00000000 -1.000000000
#> 269  1.79336352  0.812713621
#> 262 -0.77130042 -0.457446765
#> 263 -0.96218534 -0.864152437
#> 274  0.22705902  0.060180909
#> 279 -1.00000000 -1.000000000
#> 265  1.42436981  0.553017983
#> 285 -1.00000000 -1.000000000
#> 281  1.69299723  0.733851409
#> 277 -0.72195295 -0.393619083
#> 266 -0.84873948 -0.583814993
#> 267 -0.93041764 -0.769737232
#> 275  1.66421572  0.712486911
#> 271 -1.00000000 -1.000000000
associndex (Amoladeras_int, Amoladeras_cover, expand = "yes",
rm_sp_no_cover = "onlycanopy", threshold_density=NULL)
#> Based on Weibull distribution fitted to the observed values, the threshold density has been set to: 1.12346719046902.
#>                    Recruit                Canopy Fcr       Ac  Fro       Ao
#> 2     Artemisia_barrelieri Piptatherum_miliaceum   0  15.4830   54 6926.992
#> 3     Artemisia_barrelieri     Thymelaea_hirsuta   5 140.9300   54 6926.992
#> 4     Artemisia_barrelieri    Phagnalon_saxatile   0  33.9250   54 6926.992
#> 5     Artemisia_barrelieri Maytenus_senegalensis   0 116.4500   54 6926.992
#> 6     Artemisia_barrelieri        Lygeum_spartum  23 643.7150   54 6926.992
#> 7     Artemisia_barrelieri Salsola_oppositifolia   2  61.6100   54 6926.992
#> 8     Artemisia_barrelieri      Launaea_lanifera   0   1.1600   54 6926.992
#> 9     Artemisia_barrelieri        Ziziphus_lotus   0 465.1950   54 6926.992
#> 10    Artemisia_barrelieri  Helichrysum_stoechas   0  29.0700   54 6926.992
#> 11    Artemisia_barrelieri   Whitania_frutescens   0  56.8150   54 6926.992
#> 12    Artemisia_barrelieri       Ballota_hirsuta   0  38.4550   54 6926.992
#> 13    Artemisia_barrelieri       Asparagus_albus   0  27.7250   54 6926.992
#> 14    Artemisia_barrelieri     Stipa_tenacissima   0   0.6000   54 6926.992
#> 15    Artemisia_barrelieri  Artemisia_campestris   0   0.1100   54 6926.992
#> 16    Artemisia_barrelieri       Thymus_hyemalis   2 522.5800   54 6926.992
#> 17    Artemisia_barrelieri    Lycium_intrincatum   0 161.8000   54 6926.992
#> 18    Artemisia_barrelieri         Ononis_natrix   1 244.9650   54 6926.992
#> 19    Artemisia_barrelieri    Asparagus_horridus   0   1.5725   54 6926.992
#> 20    Artemisia_barrelieri     Hyparrhenia_hirta   0  13.2550   54 6926.992
#> 21    Artemisia_barrelieri   Launaea_arborescens   0 473.8650   54 6926.992
#> 22    Artemisia_barrelieri  Teucrium_lusitanicum   0   5.2825   54 6926.992
#> 23    Artemisia_barrelieri       Teucrium_polium   0  17.0450   54 6926.992
#> 24    Artemisia_campestris  Artemisia_barrelieri   0   1.0000    1 6926.992
#> 25    Artemisia_campestris Piptatherum_miliaceum   0  15.4830    1 6926.992
#> 26    Artemisia_campestris     Thymelaea_hirsuta   0 140.9300    1 6926.992
#> 27    Artemisia_campestris        Lygeum_spartum   3 643.7150    1 6926.992
#> 28    Artemisia_campestris Salsola_oppositifolia   0  61.6100    1 6926.992
#> 29    Artemisia_campestris       Asparagus_albus   0  27.7250    1 6926.992
#> 30    Artemisia_campestris  Artemisia_campestris   0   0.1100    1 6926.992
#> 31    Artemisia_campestris Maytenus_senegalensis   0 116.4500    1 6926.992
#> 32    Artemisia_campestris     Stipa_tenacissima   0   0.6000    1 6926.992
#> 33    Artemisia_campestris   Whitania_frutescens   0  56.8150    1 6926.992
#> 34    Artemisia_campestris       Thymus_hyemalis   2 522.5800    1 6926.992
#> 35    Artemisia_campestris    Phagnalon_saxatile   0  33.9250    1 6926.992
#> 36    Artemisia_campestris         Ononis_natrix   0 244.9650    1 6926.992
#> 37    Artemisia_campestris  Teucrium_lusitanicum   0   5.2825    1 6926.992
#> 38    Artemisia_campestris    Asparagus_horridus   0   1.5725    1 6926.992
#> 39    Artemisia_campestris    Lycium_intrincatum   0 161.8000    1 6926.992
#> 40    Artemisia_campestris  Helichrysum_stoechas   0  29.0700    1 6926.992
#> 41    Artemisia_campestris     Hyparrhenia_hirta   0  13.2550    1 6926.992
#> 42    Artemisia_campestris        Ziziphus_lotus   0 465.1950    1 6926.992
#> 43    Artemisia_campestris   Launaea_arborescens   0 473.8650    1 6926.992
#> 44    Artemisia_campestris      Launaea_lanifera   0   1.1600    1 6926.992
#> 45    Artemisia_campestris       Teucrium_polium   0  17.0450    1 6926.992
#> 46    Artemisia_campestris       Ballota_hirsuta   0  38.4550    1 6926.992
#> 47         Asparagus_albus         Ononis_natrix   0 244.9650    7 6926.992
#> 48         Asparagus_albus Maytenus_senegalensis   1 116.4500    7 6926.992
#> 49         Asparagus_albus     Thymelaea_hirsuta   0 140.9300    7 6926.992
#> 50         Asparagus_albus        Lygeum_spartum   4 643.7150    7 6926.992
#> 51         Asparagus_albus Piptatherum_miliaceum   0  15.4830    7 6926.992
#> 52         Asparagus_albus  Artemisia_barrelieri   0   1.0000    7 6926.992
#> 53         Asparagus_albus  Artemisia_campestris   0   0.1100    7 6926.992
#> 54         Asparagus_albus    Phagnalon_saxatile   0  33.9250    7 6926.992
#> 55         Asparagus_albus    Lycium_intrincatum   1 161.8000    7 6926.992
#> 56         Asparagus_albus Salsola_oppositifolia   0  61.6100    7 6926.992
#> 57         Asparagus_albus       Asparagus_albus   0  27.7250    7 6926.992
#> 58         Asparagus_albus       Thymus_hyemalis   3 522.5800    7 6926.992
#> 59         Asparagus_albus  Helichrysum_stoechas   0  29.0700    7 6926.992
#> 60         Asparagus_albus     Stipa_tenacissima   0   0.6000    7 6926.992
#> 61         Asparagus_albus        Ziziphus_lotus   4 465.1950    7 6926.992
#> 62         Asparagus_albus   Whitania_frutescens   0  56.8150    7 6926.992
#> 63         Asparagus_albus   Launaea_arborescens   5 473.8650    7 6926.992
#> 64         Asparagus_albus     Hyparrhenia_hirta   0  13.2550    7 6926.992
#> 65         Asparagus_albus  Teucrium_lusitanicum   0   5.2825    7 6926.992
#> 66         Asparagus_albus    Asparagus_horridus   0   1.5725    7 6926.992
#> 67         Asparagus_albus      Launaea_lanifera   0   1.1600    7 6926.992
#> 68         Asparagus_albus       Teucrium_polium   0  17.0450    7 6926.992
#> 69         Asparagus_albus       Ballota_hirsuta   0  38.4550    7 6926.992
#> 70      Asparagus_horridus Maytenus_senegalensis   0 116.4500   18 6926.992
#> 71      Asparagus_horridus Piptatherum_miliaceum   0  15.4830   18 6926.992
#> 72      Asparagus_horridus  Artemisia_campestris   0   0.1100   18 6926.992
#> 73      Asparagus_horridus        Lygeum_spartum   7 643.7150   18 6926.992
#> 74      Asparagus_horridus Salsola_oppositifolia   2  61.6100   18 6926.992
#> 75      Asparagus_horridus    Phagnalon_saxatile   2  33.9250   18 6926.992
#> 76      Asparagus_horridus    Asparagus_horridus   0   1.5725   18 6926.992
#> 77      Asparagus_horridus         Ononis_natrix   0 244.9650   18 6926.992
#> 78      Asparagus_horridus     Stipa_tenacissima   0   0.6000   18 6926.992
#> 79      Asparagus_horridus  Artemisia_barrelieri   0   1.0000   18 6926.992
#> 80      Asparagus_horridus   Whitania_frutescens   4  56.8150   18 6926.992
#> 81      Asparagus_horridus     Thymelaea_hirsuta   5 140.9300   18 6926.992
#> 82      Asparagus_horridus    Lycium_intrincatum   2 161.8000   18 6926.992
#> 83      Asparagus_horridus       Asparagus_albus   0  27.7250   18 6926.992
#> 84      Asparagus_horridus       Teucrium_polium   0  17.0450   18 6926.992
#> 85      Asparagus_horridus       Ballota_hirsuta   2  38.4550   18 6926.992
#> 86      Asparagus_horridus       Thymus_hyemalis   2 522.5800   18 6926.992
#> 87      Asparagus_horridus  Teucrium_lusitanicum   0   5.2825   18 6926.992
#> 88      Asparagus_horridus     Hyparrhenia_hirta   0  13.2550   18 6926.992
#> 89      Asparagus_horridus        Ziziphus_lotus   0 465.1950   18 6926.992
#> 90      Asparagus_horridus  Helichrysum_stoechas   2  29.0700   18 6926.992
#> 91      Asparagus_horridus      Launaea_lanifera   0   1.1600   18 6926.992
#> 92      Asparagus_horridus   Launaea_arborescens   7 473.8650   18 6926.992
#> 93         Ballota_hirsuta Maytenus_senegalensis   0 116.4500   11 6926.992
#> 94         Ballota_hirsuta  Artemisia_campestris   0   0.1100   11 6926.992
#> 95         Ballota_hirsuta    Phagnalon_saxatile   0  33.9250   11 6926.992
#> 96         Ballota_hirsuta  Artemisia_barrelieri   0   1.0000   11 6926.992
#> 97         Ballota_hirsuta Piptatherum_miliaceum   0  15.4830   11 6926.992
#> 98         Ballota_hirsuta Salsola_oppositifolia   4  61.6100   11 6926.992
#> 99         Ballota_hirsuta         Ononis_natrix   0 244.9650   11 6926.992
#> 100        Ballota_hirsuta        Lygeum_spartum   0 643.7150   11 6926.992
#> 101        Ballota_hirsuta  Helichrysum_stoechas   3  29.0700   11 6926.992
#> 102        Ballota_hirsuta     Stipa_tenacissima   0   0.6000   11 6926.992
#> 103        Ballota_hirsuta       Asparagus_albus   0  27.7250   11 6926.992
#> 104        Ballota_hirsuta     Thymelaea_hirsuta   5 140.9300   11 6926.992
#> 105        Ballota_hirsuta       Thymus_hyemalis   1 522.5800   11 6926.992
#> 106        Ballota_hirsuta    Lycium_intrincatum  20 161.8000   11 6926.992
#> 107        Ballota_hirsuta  Teucrium_lusitanicum   0   5.2825   11 6926.992
#> 108        Ballota_hirsuta    Asparagus_horridus   1   1.5725   11 6926.992
#> 109        Ballota_hirsuta        Ziziphus_lotus  32 465.1950   11 6926.992
#> 110        Ballota_hirsuta     Hyparrhenia_hirta   0  13.2550   11 6926.992
#> 111        Ballota_hirsuta   Launaea_arborescens   5 473.8650   11 6926.992
#> 112        Ballota_hirsuta       Teucrium_polium   0  17.0450   11 6926.992
#> 113        Ballota_hirsuta       Ballota_hirsuta   0  38.4550   11 6926.992
#> 114        Ballota_hirsuta   Whitania_frutescens   0  56.8150   11 6926.992
#> 115        Ballota_hirsuta      Launaea_lanifera   0   1.1600   11 6926.992
#> 116   Helichrysum_stoechas  Artemisia_campestris   0   0.1100  796 6926.992
#> 117   Helichrysum_stoechas    Phagnalon_saxatile   0  33.9250  796 6926.992
#> 118   Helichrysum_stoechas Piptatherum_miliaceum   0  15.4830  796 6926.992
#> 120   Helichrysum_stoechas        Lygeum_spartum   4 643.7150  796 6926.992
#> 121   Helichrysum_stoechas       Asparagus_albus   0  27.7250  796 6926.992
#> 122   Helichrysum_stoechas    Asparagus_horridus   0   1.5725  796 6926.992
#> 123   Helichrysum_stoechas Salsola_oppositifolia  32  61.6100  796 6926.992
#> 124   Helichrysum_stoechas     Stipa_tenacissima   0   0.6000  796 6926.992
#> 125   Helichrysum_stoechas Maytenus_senegalensis   0 116.4500  796 6926.992
#> 126   Helichrysum_stoechas       Teucrium_polium   6  17.0450  796 6926.992
#> 127   Helichrysum_stoechas         Ononis_natrix   2 244.9650  796 6926.992
#> 128   Helichrysum_stoechas     Thymelaea_hirsuta  58 140.9300  796 6926.992
#> 129   Helichrysum_stoechas  Teucrium_lusitanicum   0   5.2825  796 6926.992
#> 130   Helichrysum_stoechas    Lycium_intrincatum  17 161.8000  796 6926.992
#> 131   Helichrysum_stoechas       Ballota_hirsuta   4  38.4550  796 6926.992
#> 132   Helichrysum_stoechas        Ziziphus_lotus  12 465.1950  796 6926.992
#> 133   Helichrysum_stoechas  Helichrysum_stoechas   0  29.0700  796 6926.992
#> 134   Helichrysum_stoechas     Hyparrhenia_hirta   0  13.2550  796 6926.992
#> 135   Helichrysum_stoechas   Launaea_arborescens  35 473.8650  796 6926.992
#> 136   Helichrysum_stoechas       Thymus_hyemalis  86 522.5800  796 6926.992
#> 137   Helichrysum_stoechas   Whitania_frutescens   0  56.8150  796 6926.992
#> 138   Helichrysum_stoechas      Launaea_lanifera   0   1.1600  796 6926.992
#> 139      Hyparrhenia_hirta Piptatherum_miliaceum   0  15.4830   34 6926.992
#> 140      Hyparrhenia_hirta    Phagnalon_saxatile   0  33.9250   34 6926.992
#> 141      Hyparrhenia_hirta Salsola_oppositifolia   1  61.6100   34 6926.992
#> 142      Hyparrhenia_hirta  Artemisia_barrelieri   0   1.0000   34 6926.992
#> 143      Hyparrhenia_hirta  Artemisia_campestris   0   0.1100   34 6926.992
#> 144      Hyparrhenia_hirta       Ballota_hirsuta   0  38.4550   34 6926.992
#> 145      Hyparrhenia_hirta        Lygeum_spartum   0 643.7150   34 6926.992
#> 146      Hyparrhenia_hirta       Asparagus_albus   0  27.7250   34 6926.992
#> 147      Hyparrhenia_hirta    Asparagus_horridus   0   1.5725   34 6926.992
#> 148      Hyparrhenia_hirta     Stipa_tenacissima   0   0.6000   34 6926.992
#> 149      Hyparrhenia_hirta  Teucrium_lusitanicum   0   5.2825   34 6926.992
#> 150      Hyparrhenia_hirta         Ononis_natrix   2 244.9650   34 6926.992
#> 151      Hyparrhenia_hirta Maytenus_senegalensis   0 116.4500   34 6926.992
#> 152      Hyparrhenia_hirta     Hyparrhenia_hirta   0  13.2550   34 6926.992
#> 153      Hyparrhenia_hirta    Lycium_intrincatum   0 161.8000   34 6926.992
#> 154      Hyparrhenia_hirta     Thymelaea_hirsuta   2 140.9300   34 6926.992
#> 155      Hyparrhenia_hirta  Helichrysum_stoechas   0  29.0700   34 6926.992
#> 156      Hyparrhenia_hirta   Whitania_frutescens   0  56.8150   34 6926.992
#> 157      Hyparrhenia_hirta   Launaea_arborescens   4 473.8650   34 6926.992
#> 158      Hyparrhenia_hirta       Teucrium_polium   3  17.0450   34 6926.992
#> 159      Hyparrhenia_hirta       Thymus_hyemalis   3 522.5800   34 6926.992
#> 160      Hyparrhenia_hirta        Ziziphus_lotus   4 465.1950   34 6926.992
#> 161      Hyparrhenia_hirta      Launaea_lanifera   0   1.1600   34 6926.992
#> 162    Launaea_arborescens  Artemisia_campestris   0   0.1100   89 6926.992
#> 163    Launaea_arborescens        Lygeum_spartum   6 643.7150   89 6926.992
#> 164    Launaea_arborescens Piptatherum_miliaceum   5  15.4830   89 6926.992
#> 165    Launaea_arborescens  Artemisia_barrelieri   0   1.0000   89 6926.992
#> 166    Launaea_arborescens       Asparagus_albus   0  27.7250   89 6926.992
#> 167    Launaea_arborescens    Asparagus_horridus   0   1.5725   89 6926.992
#> 168    Launaea_arborescens    Phagnalon_saxatile   3  33.9250   89 6926.992
#> 169    Launaea_arborescens Salsola_oppositifolia   9  61.6100   89 6926.992
#> 170    Launaea_arborescens         Ononis_natrix  19 244.9650   89 6926.992
#> 171    Launaea_arborescens       Teucrium_polium   0  17.0450   89 6926.992
#> 172    Launaea_arborescens Maytenus_senegalensis   1 116.4500   89 6926.992
#> 173    Launaea_arborescens     Stipa_tenacissima   0   0.6000   89 6926.992
#> 174    Launaea_arborescens  Teucrium_lusitanicum   0   5.2825   89 6926.992
#> 175    Launaea_arborescens  Helichrysum_stoechas   1  29.0700   89 6926.992
#> 176    Launaea_arborescens       Ballota_hirsuta   0  38.4550   89 6926.992
#> 177    Launaea_arborescens    Lycium_intrincatum   3 161.8000   89 6926.992
#> 178    Launaea_arborescens     Thymelaea_hirsuta   7 140.9300   89 6926.992
#> 179    Launaea_arborescens       Thymus_hyemalis   4 522.5800   89 6926.992
#> 180    Launaea_arborescens     Hyparrhenia_hirta   1  13.2550   89 6926.992
#> 181    Launaea_arborescens   Launaea_arborescens   9 473.8650   89 6926.992
#> 182    Launaea_arborescens        Ziziphus_lotus   4 465.1950   89 6926.992
#> 183    Launaea_arborescens   Whitania_frutescens   8  56.8150   89 6926.992
#> 184    Launaea_arborescens      Launaea_lanifera   0   1.1600   89 6926.992
#> 185       Launaea_lanifera     Stipa_tenacissima   0   0.6000    6 6926.992
#> 186       Launaea_lanifera  Artemisia_barrelieri   0   1.0000    6 6926.992
#> 187       Launaea_lanifera       Asparagus_albus   0  27.7250    6 6926.992
#> 188       Launaea_lanifera  Artemisia_campestris   0   0.1100    6 6926.992
#> 189       Launaea_lanifera Salsola_oppositifolia   0  61.6100    6 6926.992
#> 190       Launaea_lanifera       Teucrium_polium   0  17.0450    6 6926.992
#> 191       Launaea_lanifera Piptatherum_miliaceum   0  15.4830    6 6926.992
#> 192       Launaea_lanifera    Asparagus_horridus   0   1.5725    6 6926.992
#> 193       Launaea_lanifera  Teucrium_lusitanicum   0   5.2825    6 6926.992
#> 194       Launaea_lanifera    Phagnalon_saxatile   0  33.9250    6 6926.992
#> 195       Launaea_lanifera    Lycium_intrincatum   0 161.8000    6 6926.992
#> 196       Launaea_lanifera  Helichrysum_stoechas   0  29.0700    6 6926.992
#> 197       Launaea_lanifera       Ballota_hirsuta   0  38.4550    6 6926.992
#> 198       Launaea_lanifera       Thymus_hyemalis   1 522.5800    6 6926.992
#> 199       Launaea_lanifera   Whitania_frutescens   0  56.8150    6 6926.992
#> 200       Launaea_lanifera         Ononis_natrix   0 244.9650    6 6926.992
#> 201       Launaea_lanifera     Thymelaea_hirsuta   1 140.9300    6 6926.992
#> 202       Launaea_lanifera Maytenus_senegalensis   0 116.4500    6 6926.992
#> 203       Launaea_lanifera   Launaea_arborescens   0 473.8650    6 6926.992
#> 204       Launaea_lanifera        Lygeum_spartum   0 643.7150    6 6926.992
#> 205       Launaea_lanifera     Hyparrhenia_hirta   0  13.2550    6 6926.992
#> 206       Launaea_lanifera        Ziziphus_lotus   0 465.1950    6 6926.992
#> 207       Launaea_lanifera      Launaea_lanifera   0   1.1600    6 6926.992
#> 208     Lycium_intrincatum       Teucrium_polium   0  17.0450   20 6926.992
#> 209     Lycium_intrincatum       Asparagus_albus   2  27.7250   20 6926.992
#> 210     Lycium_intrincatum     Stipa_tenacissima   0   0.6000   20 6926.992
#> 211     Lycium_intrincatum  Helichrysum_stoechas   0  29.0700   20 6926.992
#> 212     Lycium_intrincatum     Hyparrhenia_hirta   1  13.2550   20 6926.992
#> 213     Lycium_intrincatum    Asparagus_horridus   0   1.5725   20 6926.992
#> 214     Lycium_intrincatum     Thymelaea_hirsuta   1 140.9300   20 6926.992
#> 215     Lycium_intrincatum Salsola_oppositifolia   5  61.6100   20 6926.992
#> 216     Lycium_intrincatum  Artemisia_barrelieri   0   1.0000   20 6926.992
#> 217     Lycium_intrincatum Piptatherum_miliaceum   1  15.4830   20 6926.992
#> 218     Lycium_intrincatum  Artemisia_campestris   0   0.1100   20 6926.992
#> 219     Lycium_intrincatum       Thymus_hyemalis   2 522.5800   20 6926.992
#> 220     Lycium_intrincatum   Whitania_frutescens   3  56.8150   20 6926.992
#> 221     Lycium_intrincatum   Launaea_arborescens   6 473.8650   20 6926.992
#> 222     Lycium_intrincatum       Ballota_hirsuta   0  38.4550   20 6926.992
#> 223     Lycium_intrincatum  Teucrium_lusitanicum   0   5.2825   20 6926.992
#> 224     Lycium_intrincatum        Ziziphus_lotus  13 465.1950   20 6926.992
#> 225     Lycium_intrincatum      Launaea_lanifera   0   1.1600   20 6926.992
#> 226     Lycium_intrincatum         Ononis_natrix   2 244.9650   20 6926.992
#> 227     Lycium_intrincatum Maytenus_senegalensis   1 116.4500   20 6926.992
#> 228     Lycium_intrincatum    Lycium_intrincatum   4 161.8000   20 6926.992
#> 229     Lycium_intrincatum        Lygeum_spartum   2 643.7150   20 6926.992
#> 230     Lycium_intrincatum    Phagnalon_saxatile   1  33.9250   20 6926.992
#> 231         Lygeum_spartum   Launaea_arborescens   1 473.8650   80 6926.992
#> 232         Lygeum_spartum  Helichrysum_stoechas   0  29.0700   80 6926.992
#> 233         Lygeum_spartum     Hyparrhenia_hirta   0  13.2550   80 6926.992
#> 234         Lygeum_spartum     Stipa_tenacissima   0   0.6000   80 6926.992
#> 235         Lygeum_spartum       Teucrium_polium   0  17.0450   80 6926.992
#> 236         Lygeum_spartum       Ballota_hirsuta   0  38.4550   80 6926.992
#> 237         Lygeum_spartum Piptatherum_miliaceum   0  15.4830   80 6926.992
#> 238         Lygeum_spartum     Thymelaea_hirsuta   2 140.9300   80 6926.992
#> 239         Lygeum_spartum Salsola_oppositifolia   1  61.6100   80 6926.992
#> 240         Lygeum_spartum       Asparagus_albus   1  27.7250   80 6926.992
#> 241         Lygeum_spartum    Lycium_intrincatum   0 161.8000   80 6926.992
#> 242         Lygeum_spartum        Ziziphus_lotus   2 465.1950   80 6926.992
#> 243         Lygeum_spartum  Teucrium_lusitanicum   0   5.2825   80 6926.992
#> 244         Lygeum_spartum         Ononis_natrix   0 244.9650   80 6926.992
#> 245         Lygeum_spartum    Asparagus_horridus   0   1.5725   80 6926.992
#> 246         Lygeum_spartum  Artemisia_campestris   0   0.1100   80 6926.992
#> 247         Lygeum_spartum       Thymus_hyemalis   6 522.5800   80 6926.992
#> 248         Lygeum_spartum   Whitania_frutescens   0  56.8150   80 6926.992
#> 249         Lygeum_spartum    Phagnalon_saxatile   0  33.9250   80 6926.992
#> 250         Lygeum_spartum        Lygeum_spartum  10 643.7150   80 6926.992
#> 251         Lygeum_spartum  Artemisia_barrelieri   0   1.0000   80 6926.992
#> 252         Lygeum_spartum      Launaea_lanifera   0   1.1600   80 6926.992
#> 253         Lygeum_spartum Maytenus_senegalensis   0 116.4500   80 6926.992
#> 258  Maytenus_senegalensis   Launaea_arborescens   1 473.8650    0 6926.992
#> 265  Maytenus_senegalensis   Whitania_frutescens   1  56.8150    0 6926.992
#> 268  Maytenus_senegalensis        Lygeum_spartum   1 643.7150    0 6926.992
#> 272  Maytenus_senegalensis       Asparagus_albus   1  27.7250    0 6926.992
#> 277          Ononis_natrix     Hyparrhenia_hirta   1  13.2550 1839 6926.992
#> 278          Ononis_natrix  Helichrysum_stoechas  15  29.0700 1839 6926.992
#> 279          Ononis_natrix   Whitania_frutescens   4  56.8150 1839 6926.992
#> 280          Ononis_natrix   Launaea_arborescens 113 473.8650 1839 6926.992
#> 281          Ononis_natrix     Thymelaea_hirsuta  43 140.9300 1839 6926.992
#> 282          Ononis_natrix       Thymus_hyemalis   7 522.5800 1839 6926.992
#> 283          Ononis_natrix Piptatherum_miliaceum   0  15.4830 1839 6926.992
#> 284          Ononis_natrix      Launaea_lanifera   0   1.1600 1839 6926.992
#> 285          Ononis_natrix       Teucrium_polium   8  17.0450 1839 6926.992
#> 286          Ononis_natrix Maytenus_senegalensis   3 116.4500 1839 6926.992
#> 287          Ononis_natrix    Lycium_intrincatum   9 161.8000 1839 6926.992
#> 288          Ononis_natrix        Ziziphus_lotus   9 465.1950 1839 6926.992
#> 289          Ononis_natrix         Ononis_natrix  36 244.9650 1839 6926.992
#> 290          Ononis_natrix    Phagnalon_saxatile   5  33.9250 1839 6926.992
#> 291          Ononis_natrix Salsola_oppositifolia   6  61.6100 1839 6926.992
#> 292          Ononis_natrix       Ballota_hirsuta   2  38.4550 1839 6926.992
#> 293          Ononis_natrix       Asparagus_albus   1  27.7250 1839 6926.992
#> 294          Ononis_natrix  Artemisia_barrelieri   0   1.0000 1839 6926.992
#> 295          Ononis_natrix  Artemisia_campestris   0   0.1100 1839 6926.992
#> 296          Ononis_natrix  Teucrium_lusitanicum   1   5.2825 1839 6926.992
#> 297          Ononis_natrix    Asparagus_horridus   0   1.5725 1839 6926.992
#> 298          Ononis_natrix        Lygeum_spartum  10 643.7150 1839 6926.992
#> 299          Ononis_natrix     Stipa_tenacissima   0   0.6000 1839 6926.992
#> 323 Periploca_angustifolia    Lycium_intrincatum   0 161.8000    1 6926.992
#> 324 Periploca_angustifolia        Lygeum_spartum   0 643.7150    1 6926.992
#> 325 Periploca_angustifolia   Whitania_frutescens   0  56.8150    1 6926.992
#> 326 Periploca_angustifolia     Thymelaea_hirsuta   0 140.9300    1 6926.992
#> 327 Periploca_angustifolia       Thymus_hyemalis   0 522.5800    1 6926.992
#> 328 Periploca_angustifolia  Helichrysum_stoechas   0  29.0700    1 6926.992
#> 329 Periploca_angustifolia        Ziziphus_lotus   0 465.1950    1 6926.992
#> 330 Periploca_angustifolia      Launaea_lanifera   0   1.1600    1 6926.992
#> 331 Periploca_angustifolia         Ononis_natrix   0 244.9650    1 6926.992
#> 332 Periploca_angustifolia Piptatherum_miliaceum   0  15.4830    1 6926.992
#> 333 Periploca_angustifolia  Artemisia_barrelieri   0   1.0000    1 6926.992
#> 334 Periploca_angustifolia  Artemisia_campestris   0   0.1100    1 6926.992
#> 335 Periploca_angustifolia       Teucrium_polium   0  17.0450    1 6926.992
#> 336 Periploca_angustifolia Maytenus_senegalensis   0 116.4500    1 6926.992
#> 337 Periploca_angustifolia     Hyparrhenia_hirta   0  13.2550    1 6926.992
#> 338 Periploca_angustifolia     Stipa_tenacissima   0   0.6000    1 6926.992
#> 339 Periploca_angustifolia       Asparagus_albus   0  27.7250    1 6926.992
#> 340 Periploca_angustifolia   Launaea_arborescens   0 473.8650    1 6926.992
#> 341 Periploca_angustifolia Salsola_oppositifolia   0  61.6100    1 6926.992
#> 342 Periploca_angustifolia       Ballota_hirsuta   0  38.4550    1 6926.992
#> 343 Periploca_angustifolia  Teucrium_lusitanicum   0   5.2825    1 6926.992
#> 344 Periploca_angustifolia    Phagnalon_saxatile   0  33.9250    1 6926.992
#> 345 Periploca_angustifolia    Asparagus_horridus   0   1.5725    1 6926.992
#> 346     Phagnalon_saxatile Maytenus_senegalensis   7 116.4500  528 6926.992
#> 347     Phagnalon_saxatile Piptatherum_miliaceum   4  15.4830  528 6926.992
#> 348     Phagnalon_saxatile        Ziziphus_lotus  40 465.1950  528 6926.992
#> 349     Phagnalon_saxatile         Ononis_natrix   3 244.9650  528 6926.992
#> 350     Phagnalon_saxatile    Lycium_intrincatum  42 161.8000  528 6926.992
#> 351     Phagnalon_saxatile  Artemisia_campestris   0   0.1100  528 6926.992
#> 352     Phagnalon_saxatile       Asparagus_albus   4  27.7250  528 6926.992
#> 353     Phagnalon_saxatile   Whitania_frutescens  10  56.8150  528 6926.992
#> 354     Phagnalon_saxatile     Thymelaea_hirsuta 138 140.9300  528 6926.992
#> 355     Phagnalon_saxatile        Lygeum_spartum  41 643.7150  528 6926.992
#> 356     Phagnalon_saxatile  Helichrysum_stoechas   4  29.0700  528 6926.992
#> 358     Phagnalon_saxatile    Phagnalon_saxatile   3  33.9250  528 6926.992
#> 359     Phagnalon_saxatile       Thymus_hyemalis 133 522.5800  528 6926.992
#> 360     Phagnalon_saxatile   Launaea_arborescens 168 473.8650  528 6926.992
#> 362     Phagnalon_saxatile      Launaea_lanifera   0   1.1600  528 6926.992
#> 363     Phagnalon_saxatile       Teucrium_polium   7  17.0450  528 6926.992
#> 364     Phagnalon_saxatile       Ballota_hirsuta  14  38.4550  528 6926.992
#> 365     Phagnalon_saxatile     Hyparrhenia_hirta   8  13.2550  528 6926.992
#> 366     Phagnalon_saxatile     Stipa_tenacissima   0   0.6000  528 6926.992
#> 367     Phagnalon_saxatile  Teucrium_lusitanicum   0   5.2825  528 6926.992
#> 368     Phagnalon_saxatile Salsola_oppositifolia  46  61.6100  528 6926.992
#> 369  Piptatherum_miliaceum    Lycium_intrincatum   0 161.8000   19 6926.992
#> 370  Piptatherum_miliaceum         Ononis_natrix   0 244.9650   19 6926.992
#> 371  Piptatherum_miliaceum    Phagnalon_saxatile   0  33.9250   19 6926.992
#> 372  Piptatherum_miliaceum Maytenus_senegalensis   0 116.4500   19 6926.992
#> 373  Piptatherum_miliaceum        Lygeum_spartum   0 643.7150   19 6926.992
#> 374  Piptatherum_miliaceum Piptatherum_miliaceum   0  15.4830   19 6926.992
#> 375  Piptatherum_miliaceum  Artemisia_barrelieri   0   1.0000   19 6926.992
#> 376  Piptatherum_miliaceum  Helichrysum_stoechas   0  29.0700   19 6926.992
#> 377  Piptatherum_miliaceum       Teucrium_polium   2  17.0450   19 6926.992
#> 378  Piptatherum_miliaceum Salsola_oppositifolia   0  61.6100   19 6926.992
#> 379  Piptatherum_miliaceum  Artemisia_campestris   0   0.1100   19 6926.992
#> 380  Piptatherum_miliaceum       Asparagus_albus   0  27.7250   19 6926.992
#> 381  Piptatherum_miliaceum   Whitania_frutescens   0  56.8150   19 6926.992
#> 382  Piptatherum_miliaceum      Launaea_lanifera   0   1.1600   19 6926.992
#> 383  Piptatherum_miliaceum        Ziziphus_lotus   0 465.1950   19 6926.992
#> 384  Piptatherum_miliaceum   Launaea_arborescens   2 473.8650   19 6926.992
#> 385  Piptatherum_miliaceum    Asparagus_horridus   0   1.5725   19 6926.992
#> 386  Piptatherum_miliaceum     Thymelaea_hirsuta   0 140.9300   19 6926.992
#> 387  Piptatherum_miliaceum     Stipa_tenacissima   0   0.6000   19 6926.992
#> 388  Piptatherum_miliaceum  Teucrium_lusitanicum   0   5.2825   19 6926.992
#> 389  Piptatherum_miliaceum     Hyparrhenia_hirta   0  13.2550   19 6926.992
#> 390  Piptatherum_miliaceum       Thymus_hyemalis   4 522.5800   19 6926.992
#> 391  Piptatherum_miliaceum       Ballota_hirsuta   0  38.4550   19 6926.992
#> 392  Salsola_oppositifolia Piptatherum_miliaceum   0  15.4830   60 6926.992
#> 393  Salsola_oppositifolia  Artemisia_barrelieri   0   1.0000   60 6926.992
#> 394  Salsola_oppositifolia     Stipa_tenacissima   0   0.6000   60 6926.992
#> 395  Salsola_oppositifolia         Ononis_natrix   3 244.9650   60 6926.992
#> 396  Salsola_oppositifolia Salsola_oppositifolia   0  61.6100   60 6926.992
#> 397  Salsola_oppositifolia Maytenus_senegalensis   0 116.4500   60 6926.992
#> 398  Salsola_oppositifolia    Asparagus_horridus   0   1.5725   60 6926.992
#> 399  Salsola_oppositifolia        Lygeum_spartum  17 643.7150   60 6926.992
#> 400  Salsola_oppositifolia  Artemisia_campestris   0   0.1100   60 6926.992
#> 401  Salsola_oppositifolia       Asparagus_albus   0  27.7250   60 6926.992
#> 402  Salsola_oppositifolia       Teucrium_polium   3  17.0450   60 6926.992
#> 403  Salsola_oppositifolia  Teucrium_lusitanicum   0   5.2825   60 6926.992
#> 404  Salsola_oppositifolia    Phagnalon_saxatile   5  33.9250   60 6926.992
#> 405  Salsola_oppositifolia    Lycium_intrincatum  12 161.8000   60 6926.992
#> 406  Salsola_oppositifolia  Helichrysum_stoechas   7  29.0700   60 6926.992
#> 407  Salsola_oppositifolia     Thymelaea_hirsuta  11 140.9300   60 6926.992
#> 408  Salsola_oppositifolia      Launaea_lanifera   0   1.1600   60 6926.992
#> 409  Salsola_oppositifolia       Ballota_hirsuta   5  38.4550   60 6926.992
#> 410  Salsola_oppositifolia   Launaea_arborescens   8 473.8650   60 6926.992
#> 411  Salsola_oppositifolia   Whitania_frutescens   0  56.8150   60 6926.992
#> 412  Salsola_oppositifolia       Thymus_hyemalis  27 522.5800   60 6926.992
#> 413  Salsola_oppositifolia        Ziziphus_lotus  14 465.1950   60 6926.992
#> 414  Salsola_oppositifolia     Hyparrhenia_hirta   0  13.2550   60 6926.992
#> 415      Stipa_tenacissima Piptatherum_miliaceum   0  15.4830    1 6926.992
#> 416      Stipa_tenacissima  Artemisia_campestris   0   0.1100    1 6926.992
#> 417      Stipa_tenacissima  Artemisia_barrelieri   0   1.0000    1 6926.992
#> 418      Stipa_tenacissima Salsola_oppositifolia   0  61.6100    1 6926.992
#> 419      Stipa_tenacissima     Stipa_tenacissima   0   0.6000    1 6926.992
#> 420      Stipa_tenacissima       Asparagus_albus   0  27.7250    1 6926.992
#> 421      Stipa_tenacissima    Asparagus_horridus   0   1.5725    1 6926.992
#> 422      Stipa_tenacissima    Phagnalon_saxatile   0  33.9250    1 6926.992
#> 423      Stipa_tenacissima       Teucrium_polium   0  17.0450    1 6926.992
#> 424      Stipa_tenacissima  Teucrium_lusitanicum   0   5.2825    1 6926.992
#> 425      Stipa_tenacissima  Helichrysum_stoechas   0  29.0700    1 6926.992
#> 426      Stipa_tenacissima     Hyparrhenia_hirta   0  13.2550    1 6926.992
#> 427      Stipa_tenacissima Maytenus_senegalensis   0 116.4500    1 6926.992
#> 428      Stipa_tenacissima       Ballota_hirsuta   0  38.4550    1 6926.992
#> 429      Stipa_tenacissima        Lygeum_spartum   0 643.7150    1 6926.992
#> 430      Stipa_tenacissima   Launaea_arborescens   0 473.8650    1 6926.992
#> 431      Stipa_tenacissima    Lycium_intrincatum   0 161.8000    1 6926.992
#> 432      Stipa_tenacissima     Thymelaea_hirsuta   0 140.9300    1 6926.992
#> 433      Stipa_tenacissima       Thymus_hyemalis   0 522.5800    1 6926.992
#> 434      Stipa_tenacissima        Ziziphus_lotus   0 465.1950    1 6926.992
#> 435      Stipa_tenacissima      Launaea_lanifera   0   1.1600    1 6926.992
#> 436      Stipa_tenacissima         Ononis_natrix   0 244.9650    1 6926.992
#> 437      Stipa_tenacissima   Whitania_frutescens   0  56.8150    1 6926.992
#> 438     Teucrium_charidemi Piptatherum_miliaceum   0  15.4830   14 6926.992
#> 439     Teucrium_charidemi  Artemisia_barrelieri   0   1.0000   14 6926.992
#> 440     Teucrium_charidemi       Asparagus_albus   0  27.7250   14 6926.992
#> 441     Teucrium_charidemi       Ballota_hirsuta   0  38.4550   14 6926.992
#> 442     Teucrium_charidemi Salsola_oppositifolia   1  61.6100   14 6926.992
#> 443     Teucrium_charidemi  Artemisia_campestris   0   0.1100   14 6926.992
#> 444     Teucrium_charidemi  Teucrium_lusitanicum   0   5.2825   14 6926.992
#> 445     Teucrium_charidemi    Asparagus_horridus   0   1.5725   14 6926.992
#> 446     Teucrium_charidemi    Phagnalon_saxatile   1  33.9250   14 6926.992
#> 447     Teucrium_charidemi     Stipa_tenacissima   0   0.6000   14 6926.992
#> 448     Teucrium_charidemi Maytenus_senegalensis   0 116.4500   14 6926.992
#> 449     Teucrium_charidemi        Lygeum_spartum   1 643.7150   14 6926.992
#> 450     Teucrium_charidemi  Helichrysum_stoechas   0  29.0700   14 6926.992
#> 451     Teucrium_charidemi     Hyparrhenia_hirta   0  13.2550   14 6926.992
#> 452     Teucrium_charidemi     Thymelaea_hirsuta   1 140.9300   14 6926.992
#> 453     Teucrium_charidemi       Thymus_hyemalis   1 522.5800   14 6926.992
#> 454     Teucrium_charidemi         Ononis_natrix   0 244.9650   14 6926.992
#> 455     Teucrium_charidemi       Teucrium_polium   0  17.0450   14 6926.992
#> 456     Teucrium_charidemi   Whitania_frutescens   0  56.8150   14 6926.992
#> 457     Teucrium_charidemi        Ziziphus_lotus   3 465.1950   14 6926.992
#> 458     Teucrium_charidemi   Launaea_arborescens   0 473.8650   14 6926.992
#> 459     Teucrium_charidemi    Lycium_intrincatum   0 161.8000   14 6926.992
#> 460     Teucrium_charidemi      Launaea_lanifera   0   1.1600   14 6926.992
#> 461   Teucrium_lusitanicum Piptatherum_miliaceum   0  15.4830   16 6926.992
#> 462   Teucrium_lusitanicum  Artemisia_campestris   0   0.1100   16 6926.992
#> 463   Teucrium_lusitanicum       Asparagus_albus   0  27.7250   16 6926.992
#> 464   Teucrium_lusitanicum Salsola_oppositifolia   0  61.6100   16 6926.992
#> 465   Teucrium_lusitanicum     Stipa_tenacissima   0   0.6000   16 6926.992
#> 466   Teucrium_lusitanicum  Teucrium_lusitanicum   0   5.2825   16 6926.992
#> 467   Teucrium_lusitanicum  Artemisia_barrelieri   0   1.0000   16 6926.992
#> 468   Teucrium_lusitanicum       Teucrium_polium   0  17.0450   16 6926.992
#> 469   Teucrium_lusitanicum    Phagnalon_saxatile   0  33.9250   16 6926.992
#> 470   Teucrium_lusitanicum  Helichrysum_stoechas   0  29.0700   16 6926.992
#> 471   Teucrium_lusitanicum    Asparagus_horridus   0   1.5725   16 6926.992
#> 472   Teucrium_lusitanicum        Lygeum_spartum   6 643.7150   16 6926.992
#> 473   Teucrium_lusitanicum       Ballota_hirsuta   0  38.4550   16 6926.992
#> 474   Teucrium_lusitanicum Maytenus_senegalensis   0 116.4500   16 6926.992
#> 475   Teucrium_lusitanicum     Hyparrhenia_hirta   5  13.2550   16 6926.992
#> 476   Teucrium_lusitanicum   Launaea_arborescens   4 473.8650   16 6926.992
#> 477   Teucrium_lusitanicum     Thymelaea_hirsuta   5 140.9300   16 6926.992
#> 478   Teucrium_lusitanicum       Thymus_hyemalis   0 522.5800   16 6926.992
#> 479   Teucrium_lusitanicum         Ononis_natrix   0 244.9650   16 6926.992
#> 480   Teucrium_lusitanicum        Ziziphus_lotus   7 465.1950   16 6926.992
#> 481   Teucrium_lusitanicum      Launaea_lanifera   0   1.1600   16 6926.992
#> 482   Teucrium_lusitanicum    Lycium_intrincatum   0 161.8000   16 6926.992
#> 483   Teucrium_lusitanicum   Whitania_frutescens   0  56.8150   16 6926.992
#> 484        Teucrium_polium Piptatherum_miliaceum   4  15.4830  308 6926.992
#> 485        Teucrium_polium  Artemisia_campestris   0   0.1100  308 6926.992
#> 486        Teucrium_polium       Teucrium_polium   0  17.0450  308 6926.992
#> 487        Teucrium_polium       Ballota_hirsuta   1  38.4550  308 6926.992
#> 488        Teucrium_polium Salsola_oppositifolia  17  61.6100  308 6926.992
#> 489        Teucrium_polium     Stipa_tenacissima   0   0.6000  308 6926.992
#> 491        Teucrium_polium     Thymelaea_hirsuta  54 140.9300  308 6926.992
#> 492        Teucrium_polium  Helichrysum_stoechas  12  29.0700  308 6926.992
#> 493        Teucrium_polium    Phagnalon_saxatile   8  33.9250  308 6926.992
#> 494        Teucrium_polium       Asparagus_albus   0  27.7250  308 6926.992
#> 495        Teucrium_polium    Asparagus_horridus   0   1.5725  308 6926.992
#> 496        Teucrium_polium       Thymus_hyemalis  87 522.5800  308 6926.992
#> 497        Teucrium_polium     Hyparrhenia_hirta   2  13.2550  308 6926.992
#> 498        Teucrium_polium  Teucrium_lusitanicum   0   5.2825  308 6926.992
#> 499        Teucrium_polium         Ononis_natrix   4 244.9650  308 6926.992
#> 500        Teucrium_polium        Lygeum_spartum  17 643.7150  308 6926.992
#> 501        Teucrium_polium   Launaea_arborescens  14 473.8650  308 6926.992
#> 502        Teucrium_polium      Launaea_lanifera   0   1.1600  308 6926.992
#> 503        Teucrium_polium        Ziziphus_lotus   9 465.1950  308 6926.992
#> 504        Teucrium_polium    Lycium_intrincatum   7 161.8000  308 6926.992
#> 505        Teucrium_polium   Whitania_frutescens   3  56.8150  308 6926.992
#> 506        Teucrium_polium Maytenus_senegalensis   0 116.4500  308 6926.992
#> 507      Thymelaea_hirsuta Salsola_oppositifolia   5  61.6100  170 6926.992
#> 508      Thymelaea_hirsuta Piptatherum_miliaceum   0  15.4830  170 6926.992
#> 509      Thymelaea_hirsuta  Teucrium_lusitanicum   1   5.2825  170 6926.992
#> 510      Thymelaea_hirsuta  Artemisia_barrelieri   0   1.0000  170 6926.992
#> 511      Thymelaea_hirsuta     Stipa_tenacissima   0   0.6000  170 6926.992
#> 512      Thymelaea_hirsuta  Helichrysum_stoechas  14  29.0700  170 6926.992
#> 513      Thymelaea_hirsuta       Asparagus_albus   2  27.7250  170 6926.992
#> 514      Thymelaea_hirsuta       Teucrium_polium  12  17.0450  170 6926.992
#> 515      Thymelaea_hirsuta       Ballota_hirsuta   3  38.4550  170 6926.992
#> 516      Thymelaea_hirsuta       Thymus_hyemalis  65 522.5800  170 6926.992
#> 517      Thymelaea_hirsuta    Phagnalon_saxatile   9  33.9250  170 6926.992
#> 518      Thymelaea_hirsuta    Asparagus_horridus   0   1.5725  170 6926.992
#> 519      Thymelaea_hirsuta  Artemisia_campestris   0   0.1100  170 6926.992
#> 520      Thymelaea_hirsuta        Ziziphus_lotus   3 465.1950  170 6926.992
#> 521      Thymelaea_hirsuta     Hyparrhenia_hirta   2  13.2550  170 6926.992
#> 522      Thymelaea_hirsuta        Lygeum_spartum  13 643.7150  170 6926.992
#> 523      Thymelaea_hirsuta     Thymelaea_hirsuta  19 140.9300  170 6926.992
#> 524      Thymelaea_hirsuta    Lycium_intrincatum   4 161.8000  170 6926.992
#> 525      Thymelaea_hirsuta   Launaea_arborescens  20 473.8650  170 6926.992
#> 526      Thymelaea_hirsuta         Ononis_natrix   1 244.9650  170 6926.992
#> 527      Thymelaea_hirsuta   Whitania_frutescens   2  56.8150  170 6926.992
#> 528      Thymelaea_hirsuta      Launaea_lanifera   0   1.1600  170 6926.992
#> 529      Thymelaea_hirsuta Maytenus_senegalensis   0 116.4500  170 6926.992
#> 530        Thymus_hyemalis       Teucrium_polium   3  17.0450 1030 6926.992
#> 531        Thymus_hyemalis       Ballota_hirsuta   0  38.4550 1030 6926.992
#> 532        Thymus_hyemalis Piptatherum_miliaceum   0  15.4830 1030 6926.992
#> 533        Thymus_hyemalis     Thymelaea_hirsuta  34 140.9300 1030 6926.992
#> 534        Thymus_hyemalis  Helichrysum_stoechas   4  29.0700 1030 6926.992
#> 535        Thymus_hyemalis     Stipa_tenacissima   0   0.6000 1030 6926.992
#> 536        Thymus_hyemalis Salsola_oppositifolia  14  61.6100 1030 6926.992
#> 537        Thymus_hyemalis  Artemisia_campestris   0   0.1100 1030 6926.992
#> 538        Thymus_hyemalis  Artemisia_barrelieri   1   1.0000 1030 6926.992
#> 539        Thymus_hyemalis     Hyparrhenia_hirta   1  13.2550 1030 6926.992
#> 540        Thymus_hyemalis  Teucrium_lusitanicum   1   5.2825 1030 6926.992
#> 541        Thymus_hyemalis       Asparagus_albus   0  27.7250 1030 6926.992
#> 542        Thymus_hyemalis       Thymus_hyemalis   9 522.5800 1030 6926.992
#> 543        Thymus_hyemalis   Launaea_arborescens  14 473.8650 1030 6926.992
#> 544        Thymus_hyemalis    Phagnalon_saxatile   4  33.9250 1030 6926.992
#> 545        Thymus_hyemalis    Lycium_intrincatum   0 161.8000 1030 6926.992
#> 546        Thymus_hyemalis   Whitania_frutescens   5  56.8150 1030 6926.992
#> 547        Thymus_hyemalis        Ziziphus_lotus   9 465.1950 1030 6926.992
#> 548        Thymus_hyemalis      Launaea_lanifera   0   1.1600 1030 6926.992
#> 549        Thymus_hyemalis         Ononis_natrix   0 244.9650 1030 6926.992
#> 550        Thymus_hyemalis    Asparagus_horridus   1   1.5725 1030 6926.992
#> 551        Thymus_hyemalis Maytenus_senegalensis   2 116.4500 1030 6926.992
#> 552        Thymus_hyemalis        Lygeum_spartum  12 643.7150 1030 6926.992
#> 553    Whitania_frutescens       Teucrium_polium   0  17.0450    5 6926.992
#> 554    Whitania_frutescens       Ballota_hirsuta   0  38.4550    5 6926.992
#> 555    Whitania_frutescens Piptatherum_miliaceum   0  15.4830    5 6926.992
#> 556    Whitania_frutescens Salsola_oppositifolia   0  61.6100    5 6926.992
#> 557    Whitania_frutescens  Artemisia_barrelieri   0   1.0000    5 6926.992
#> 558    Whitania_frutescens     Stipa_tenacissima   0   0.6000    5 6926.992
#> 559    Whitania_frutescens  Teucrium_lusitanicum   0   5.2825    5 6926.992
#> 560    Whitania_frutescens    Phagnalon_saxatile   0  33.9250    5 6926.992
#> 561    Whitania_frutescens     Thymelaea_hirsuta   1 140.9300    5 6926.992
#> 562    Whitania_frutescens  Helichrysum_stoechas   0  29.0700    5 6926.992
#> 563    Whitania_frutescens        Lygeum_spartum   0 643.7150    5 6926.992
#> 564    Whitania_frutescens    Lycium_intrincatum   0 161.8000    5 6926.992
#> 565    Whitania_frutescens       Asparagus_albus   3  27.7250    5 6926.992
#> 566    Whitania_frutescens  Artemisia_campestris   0   0.1100    5 6926.992
#> 567    Whitania_frutescens     Hyparrhenia_hirta   1  13.2550    5 6926.992
#> 568    Whitania_frutescens Maytenus_senegalensis   1 116.4500    5 6926.992
#> 569    Whitania_frutescens   Whitania_frutescens   1  56.8150    5 6926.992
#> 570    Whitania_frutescens       Thymus_hyemalis   0 522.5800    5 6926.992
#> 571    Whitania_frutescens   Launaea_arborescens  36 473.8650    5 6926.992
#> 572    Whitania_frutescens         Ononis_natrix   0 244.9650    5 6926.992
#> 573    Whitania_frutescens    Asparagus_horridus   0   1.5725    5 6926.992
#> 574    Whitania_frutescens      Launaea_lanifera   0   1.1600    5 6926.992
#> 575    Whitania_frutescens        Ziziphus_lotus   2 465.1950    5 6926.992
#> 576         Ziziphus_lotus       Ballota_hirsuta   0  38.4550    4 6926.992
#> 577         Ziziphus_lotus  Helichrysum_stoechas   0  29.0700    4 6926.992
#> 578         Ziziphus_lotus Salsola_oppositifolia   0  61.6100    4 6926.992
#> 579         Ziziphus_lotus     Stipa_tenacissima   0   0.6000    4 6926.992
#> 580         Ziziphus_lotus     Thymelaea_hirsuta   0 140.9300    4 6926.992
#> 581         Ziziphus_lotus        Lygeum_spartum   0 643.7150    4 6926.992
#> 582         Ziziphus_lotus    Phagnalon_saxatile   0  33.9250    4 6926.992
#> 583         Ziziphus_lotus       Asparagus_albus   0  27.7250    4 6926.992
#> 584         Ziziphus_lotus   Whitania_frutescens   0  56.8150    4 6926.992
#> 585         Ziziphus_lotus     Hyparrhenia_hirta   0  13.2550    4 6926.992
#> 586         Ziziphus_lotus       Thymus_hyemalis   0 522.5800    4 6926.992
#> 587         Ziziphus_lotus       Teucrium_polium   0  17.0450    4 6926.992
#> 588         Ziziphus_lotus  Artemisia_campestris   0   0.1100    4 6926.992
#> 589         Ziziphus_lotus  Teucrium_lusitanicum   0   5.2825    4 6926.992
#> 590         Ziziphus_lotus  Artemisia_barrelieri   0   1.0000    4 6926.992
#> 591         Ziziphus_lotus Piptatherum_miliaceum   0  15.4830    4 6926.992
#> 592         Ziziphus_lotus   Launaea_arborescens   0 473.8650    4 6926.992
#> 593         Ziziphus_lotus         Ononis_natrix   0 244.9650    4 6926.992
#> 594         Ziziphus_lotus Maytenus_senegalensis   0 116.4500    4 6926.992
#> 595         Ziziphus_lotus    Lycium_intrincatum   0 161.8000    4 6926.992
#> 596         Ziziphus_lotus    Asparagus_horridus   0   1.5725    4 6926.992
#> 597         Ziziphus_lotus      Launaea_lanifera   0   1.1600    4 6926.992
#> 598         Ziziphus_lotus        Ziziphus_lotus   0 465.1950    4 6926.992
#>             Dcr          Dro            Ns        NintC       NintA
#> 2   0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 3   0.035478606 0.0077955915  0.7747813884  0.780273458  1.56054692
#> 4   0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 5   0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 6   0.035730098 0.0077955915  0.7818200354  0.781820035  1.56364007
#> 7   0.032462263 0.0077955915  0.6903611389  0.759856804  1.51971361
#> 8   0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 9   0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 10  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 11  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 12  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 13  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 14  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 15  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 16  0.003827165 0.0077955915 -0.1110667621 -0.509060318 -0.67467193
#> 17  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 18  0.004082216 0.0077955915 -0.1039285055 -0.476343030 -0.64530129
#> 19  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 20  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 21  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 22  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 23  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 24  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 25  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 26  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 27  0.004660448 0.0001443628  0.9690238322  0.969023832  1.93804766
#> 28  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 29  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 30  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 31  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 32  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 33  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 34  0.003827165 0.0001443628  0.7902250518  0.962279443  1.92455889
#> 35  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 36  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 37  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 38  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 39  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 40  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 41  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 42  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 43  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 44  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 45  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 46  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 47  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 48  0.008587377 0.0010105396  0.7180795651  0.882322659  1.76464532
#> 49  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 50  0.006213930 0.0010105396  0.4931409220  0.837375119  1.67475024
#> 51  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 52  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 53  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 54  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 55  0.006180470 0.0010105396  0.4899697831  0.836494686  1.67298937
#> 56  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 57  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 58  0.005740748 0.0010105396  0.4482960206  0.823970732  1.64794146
#> 59  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 60  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 61  0.008598545 0.0010105396  0.7191380032  0.882475503  1.76495101
#> 62  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 63  0.010551528 0.0010105396  0.9042281267  0.904228127  1.80845625
#> 64  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 65  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 66  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 67  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 68  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 69  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 70  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 71  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 72  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 73  0.010874378 0.0025985305  0.1175480637  0.761040991  1.52208198
#> 74  0.032462263 0.0025985305  0.4241769851  0.919952268  1.83990454
#> 75  0.058953574 0.0025985305  0.8004529499  0.955922426  1.91184485
#> 76  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 77  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 78  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 79  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 80  0.070403943 0.0025985305  0.9630911224  0.963091122  1.92618224
#> 81  0.035478606 0.0025985305  0.4670203780  0.926757819  1.85351564
#> 82  0.012360939 0.0025985305  0.1386628158  0.789778882  1.57955776
#> 83  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 84  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 85  0.052008842 0.0025985305  0.7018117049  0.950036755  1.90007351
#> 86  0.003827165 0.0025985305  0.0174512204  0.321029965  0.64205993
#> 87  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 88  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 89  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 90  0.068799450 0.0025985305  0.9403013047  0.962230359  1.92446072
#> 91  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 92  0.014772140 0.0025985305  0.1729109023  0.824092478  1.64818496
#> 93  0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 94  0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 95  0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 96  0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 97  0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 98  0.064924525 0.0015879909  0.0995967003  0.975540971  1.95108194
#> 99  0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 100 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 101 0.103199174 0.0015879909  0.1597835861  0.984612369  1.96922474
#> 102 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 103 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 104 0.035478606 0.0015879909  0.0532929929  0.955240890  1.91048178
#> 105 0.001913583 0.0015879909  0.0005119930  0.170147735  0.34029547
#> 106 0.123609394 0.0015879909  0.1918786569  0.987153154  1.97430631
#> 107 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 108 0.635930048 0.0015879909  0.9975028844  0.997502884  1.99500577
#> 109 0.068788358 0.0015879909  0.1056725766  0.976914831  1.95382966
#> 110 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 111 0.010551528 0.0015879909  0.0140951628  0.849501342  1.69900268
#> 112 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 113 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 114 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 115 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 116 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 117 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 118 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 120 0.006213930 0.1149127933 -0.2092792801 -0.945924819 -0.97221106
#> 121 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 122 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 123 0.519396202 0.1149127933  0.7787569626  0.778756963  1.55751393
#> 124 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 125 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 126 0.352009387 0.1149127933  0.4564850354  0.673551906  1.34710381
#> 127 0.008164432 0.1149127933 -0.2055239551 -0.928951064 -0.96316706
#> 128 0.411551834 0.1149127933  0.5711228535  0.720781725  1.44156345
#> 129 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 130 0.105067985 0.1149127933 -0.0189543322 -0.085671994 -0.15782298
#> 131 0.104017683 0.1149127933 -0.0209764921 -0.094811987 -0.17320232
#> 132 0.025795634 0.1149127933 -0.1715783806 -0.775519911 -0.87356938
#> 133 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 134 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 135 0.073860699 0.1149127933 -0.0790381109 -0.357245642 -0.52642739
#> 136 0.164568104 0.1149127933  0.0956019911  0.301731075  0.60346215
#> 137 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 138 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 139 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 140 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 141 0.016231131 0.0049083354  0.0643323521  0.697597456  1.39519491
#> 142 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 143 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 144 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 145 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 146 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 147 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 148 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 149 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 150 0.008164432 0.0049083354  0.0185000536  0.398814810  0.79762962
#> 151 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 152 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 153 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 154 0.014191443 0.0049083354  0.0527435206  0.654134147  1.30826829
#> 155 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 156 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 157 0.008441223 0.0049083354  0.0200726881  0.418527912  0.83705582
#> 158 0.176004693 0.0049083354  0.9721124744  0.972112474  1.94422495
#> 159 0.005740748 0.0049083354  0.0047294900  0.145000697  0.29000139
#> 160 0.008598545 0.0049083354  0.0209665392  0.429166729  0.85833346
#> 161 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 162 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 163 0.009320895 0.0128482897 -0.0109229301 -0.274541956 -0.43080882
#> 164 0.322934832 0.0128482897  0.9602139861  0.960213986  1.92042797
#> 165 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 166 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 167 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 168 0.088430361 0.0128482897  0.2340474423  0.854707257  1.70941451
#> 169 0.146080182 0.0128482897  0.4125658770  0.912046319  1.82409264
#> 170 0.077562101 0.0128482897  0.2003927872  0.834348353  1.66869671
#> 171 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 172 0.008587377 0.0128482897 -0.0131943437 -0.331632711 -0.49808436
#> 173 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 174 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 175 0.034399725 0.0128482897  0.0667361739  0.626500218  1.25300044
#> 176 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 177 0.018541409 0.0128482897  0.0176293137  0.307048909  0.61409782
#> 178 0.049670049 0.0128482897  0.1140222597  0.741327219  1.48265444
#> 179 0.007654330 0.0128482897 -0.0160836143 -0.404252970 -0.57575519
#> 180 0.075443229 0.0128482897  0.1938314889  0.829695920  1.65939184
#> 181 0.018992751 0.0128482897  0.0190269392  0.323516133  0.64703227
#> 182 0.008598545 0.0128482897 -0.0131597604 -0.330763480 -0.49710333
#> 183 0.140807885 0.0128482897  0.3962396835  0.908753053  1.81750611
#> 184 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 185 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 186 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 187 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 188 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 189 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 190 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 191 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 192 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 193 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 194 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 195 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 196 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 197 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 198 0.001913583 0.0008661768  0.1476108959  0.547353310  1.09470662
#> 199 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 200 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 201 0.007095721 0.0008661768  0.8779296988  0.877929699  1.75585940
#> 202 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 203 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 204 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 205 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 206 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 207 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 208 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 209 0.072137060 0.0028872561  0.8532960886  0.959975412  1.91995082
#> 210 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 211 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 212 0.075443229 0.0028872561  0.8940346975  0.961729420  1.92345884
#> 213 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 214 0.007095721 0.0028872561  0.0518567078  0.593098996  1.18619799
#> 215 0.081155657 0.0028872561  0.9644232302  0.964423230  1.92884646
#> 216 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 217 0.064586966 0.0028872561  0.7602638295  0.955296614  1.91059323
#> 218 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 219 0.003827165 0.0028872561  0.0115815600  0.245588850  0.49117770
#> 220 0.052802957 0.0028872561  0.6150612659  0.945320181  1.89064036
#> 221 0.012661834 0.0028872561  0.1204423495  0.771971730  1.54394346
#> 222 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 223 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 224 0.027945270 0.0028872561  0.3087648504  0.896681761  1.79336352
#> 225 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 226 0.008164432 0.0028872561  0.0650253570  0.646361653  1.29272331
#> 227 0.008587377 0.0028872561  0.0702368841  0.663779026  1.32755805
#> 228 0.024721879 0.0028872561  0.2690462215  0.883210490  1.76642098
#> 229 0.003106965 0.0028872561  0.0027072534  0.070714965  0.14142993
#> 230 0.029476787 0.0028872561  0.3276362000  0.902049836  1.80409967
#> 231 0.002110306 0.0115490245 -0.2616884781 -0.817274118 -0.89945057
#> 232 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 233 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 234 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 235 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 236 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 237 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 238 0.014191443 0.0115490245  0.0732610420  0.186197992  0.37239598
#> 239 0.016231131 0.0115490245  0.1298114126  0.288464603  0.57692921
#> 240 0.036068530 0.0115490245  0.6798032970  0.679803297  1.35960659
#> 241 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 242 0.004299272 0.0115490245 -0.2009993771 -0.627737185 -0.77130042
#> 243 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 244 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 245 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 246 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 247 0.011481496 0.0115490245 -0.0018722359 -0.005847143 -0.01162631
#> 248 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 249 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 250 0.015534825 0.0115490245  0.1105063255  0.256571972  0.51314394
#> 251 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 252 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 253 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 258 0.002110306 0.0000000000  0.0585082249  1.000000000  2.00000000
#> 265 0.017600986 0.0000000000  0.4879873273  1.000000000  2.00000000
#> 268 0.001553483 0.0000000000  0.0430703029  1.000000000  2.00000000
#> 272 0.036068530 0.0000000000  1.0000000000  1.000000000  2.00000000
#> 277 0.075443229 0.2654831996 -0.3682974631 -0.715826730 -0.83438114
#> 278 0.515995872 0.2654831996  0.4854935591  0.485493559  0.97098712
#> 279 0.070403943 0.2654831996 -0.3780636001 -0.734808294 -0.84713486
#> 280 0.238464542 0.2654831996 -0.0523621593 -0.101771630 -0.18474179
#> 281 0.305116015 0.2654831996  0.0768083963  0.129894248  0.25978850
#> 282 0.013395078 0.2654831996 -0.4885467792 -0.949544535 -0.97411936
#> 283 0.000000000 0.2654831996 -0.5145064409 -1.000000000 -1.00000000
#> 284 0.000000000 0.2654831996 -0.5145064409 -1.000000000 -1.00000000
#> 285 0.469345849 0.2654831996  0.3950858149  0.434354858  0.86870972
#> 286 0.025762130 0.2654831996 -0.4645794336 -0.902961356 -0.94900651
#> 287 0.055624227 0.2654831996 -0.4067066881 -0.790479294 -0.88298066
#> 288 0.019346726 0.2654831996 -0.4770124867 -0.927126366 -0.96218534
#> 289 0.146959770 0.2654831996 -0.2296984071 -0.446444182 -0.61729887
#> 290 0.147383935 0.2654831996 -0.2288763746 -0.444846471 -0.61576988
#> 291 0.097386788 0.2654831996 -0.3257708460 -0.633171560 -0.77538891
#> 292 0.052008842 0.2654831996 -0.4137133061 -0.804097428 -0.89141242
#> 293 0.036068530 0.2654831996 -0.4446056293 -0.864140065 -0.92711925
#> 294 0.000000000 0.2654831996 -0.5145064409 -1.000000000 -1.00000000
#> 295 0.000000000 0.2654831996 -0.5145064409 -1.000000000 -1.00000000
#> 296 0.189304307 0.2654831996 -0.1476346946 -0.286944308 -0.44593120
#> 297 0.000000000 0.2654831996 -0.5145064409 -1.000000000 -1.00000000
#> 298 0.015534825 0.2654831996 -0.4843999497 -0.941484715 -0.96986055
#> 299 0.000000000 0.2654831996 -0.5145064409 -1.000000000 -1.00000000
#> 323 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 324 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 325 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 326 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 327 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 328 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 329 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 330 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 331 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 332 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 333 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 334 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 335 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 336 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 337 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 338 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 339 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 340 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 341 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 342 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 343 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 344 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 345 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 346 0.060111636 0.0762235614 -0.0164540120 -0.211377233 -0.34898664
#> 347 0.258347865 0.0762235614  0.1859911461  0.704957650  1.40991530
#> 348 0.085985447 0.0762235614  0.0099691488  0.113529509  0.22705902
#> 349 0.012246647 0.0762235614 -0.0653352643 -0.839332521 -0.91264903
#> 350 0.259579728 0.0762235614  0.1872491635  0.706357804  1.41271561
#> 351 0.000000000 0.0762235614 -0.0778419312 -1.000000000 -1.00000000
#> 352 0.144274121 0.0762235614  0.0694954010  0.471675440  0.94335088
#> 353 0.176009857 0.0762235614  0.1019049462  0.566935836  1.13387167
#> 354 0.979209537 0.0762235614  0.9221580688  0.922158069  1.84431614
#> 355 0.063692783 0.0762235614 -0.0127968301 -0.164395075 -0.28236993
#> 356 0.137598899 0.0762235614  0.0626784519  0.446045268  0.89209054
#> 358 0.088430361 0.0762235614  0.0124659731  0.138038560  0.27607712
#> 359 0.254506487 0.0762235614  0.1820682081  0.700504446  1.40100889
#> 360 0.354531354 0.0762235614  0.2842167913  0.785001917  1.57000383
#> 362 0.000000000 0.0762235614 -0.0778419312 -1.000000000 -1.00000000
#> 363 0.410677618 0.0762235614  0.3415551464  0.814395628  1.62879126
#> 364 0.364061891 0.0762235614  0.2939496792  0.790630210  1.58126042
#> 365 0.603545832 0.0762235614  0.5385183157  0.873707087  1.74741417
#> 366 0.000000000 0.0762235614 -0.0778419312 -1.000000000 -1.00000000
#> 367 0.000000000 0.0762235614 -0.0778419312 -1.000000000 -1.00000000
#> 368 0.746632040 0.0762235614  0.6846425140  0.897910139  1.79582028
#> 369 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 370 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 371 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 372 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 373 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 374 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 375 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 376 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 377 0.117336462 0.0027428933  0.9766236918  0.976623692  1.95324738
#> 378 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 379 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 380 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 381 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 382 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 383 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 384 0.004220611 0.0027428933  0.0125938521  0.350119431  0.70023886
#> 385 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 386 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 387 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 388 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 389 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 390 0.007654330 0.0027428933  0.0418577229  0.641654704  1.28330941
#> 391 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 392 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 393 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 394 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 395 0.012246647 0.0086617683  0.0148874909  0.292723306  0.58544661
#> 396 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 397 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 398 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 399 0.026409203 0.0086617683  0.0737025601  0.672017047  1.34403409
#> 400 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 401 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 402 0.176004693 0.0086617683  0.6949512619  0.950786720  1.90157344
#> 403 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 404 0.147383935 0.0086617683  0.5760933413  0.941229902  1.88245980
#> 405 0.074165637 0.0086617683  0.2720282071  0.883210490  1.76642098
#> 406 0.240798074 0.0086617683  0.9640289135  0.964028913  1.92805783
#> 407 0.078052934 0.0086617683  0.2881715983  0.889026999  1.77805400
#> 408 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 409 0.130022104 0.0086617683  0.5039921358  0.933382340  1.86676468
#> 410 0.016882445 0.0086617683  0.0341392975  0.486936393  0.97387279
#> 411 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 412 0.051666730 0.0086617683  0.1785934641  0.832353078  1.66470616
#> 413 0.030094906 0.0086617683  0.0890087606  0.712184905  1.42436981
#> 414 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 415 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 416 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 417 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 418 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 419 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 420 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 421 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 422 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 423 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 424 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 425 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 426 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 427 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 428 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 429 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 430 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 431 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 432 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 433 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 434 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 435 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 436 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 437 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 438 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 439 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 440 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 441 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 442 0.016231131 0.0020210793  0.4820760151  0.875481306  1.75096261
#> 443 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 444 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 445 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 446 0.029476787 0.0020210793  0.9314348854  0.931434885  1.86286977
#> 447 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 448 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 449 0.001553483 0.0020210793 -0.0158632201 -0.231359930 -0.37577953
#> 450 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 451 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 452 0.007095721 0.0020210793  0.1721572299  0.715169297  1.43033859
#> 453 0.001913583 0.0020210793 -0.0036468245 -0.053187755 -0.10100337
#> 454 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 455 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 456 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 457 0.006448909 0.0020210793  0.1502141071  0.686601342  1.37320268
#> 458 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 459 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 460 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 461 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 462 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 463 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 464 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 465 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 466 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 467 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 468 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 469 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 470 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 471 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 472 0.009320895 0.0023098049  0.0185864002  0.752190657  1.50438131
#> 473 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 474 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 475 0.377216145 0.0023098049  0.9938767072  0.993876707  1.98775341
#> 476 0.008441223 0.0023098049  0.0162543886  0.726366076  1.45273215
#> 477 0.035478606 0.0023098049  0.0879304928  0.934895839  1.86979168
#> 478 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 479 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 480 0.015047453 0.0023098049  0.0337675057  0.846498616  1.69299723
#> 481 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 482 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 483 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 484 0.258347865 0.0444637441  0.5181342837  0.827891962  1.65578392
#> 485 0.000000000 0.0444637441 -0.1077134202 -1.000000000 -1.00000000
#> 486 0.000000000 0.0444637441 -0.1077134202 -1.000000000 -1.00000000
#> 487 0.026004421 0.0444637441 -0.0447177109 -0.415154498 -0.58672675
#> 488 0.275929232 0.0444637441  0.5607251450  0.838858160  1.67771632
#> 489 0.000000000 0.0444637441 -0.1077134202 -1.000000000 -1.00000000
#> 491 0.383168949 0.0444637441  0.8205133591  0.883957862  1.76791572
#> 492 0.412796698 0.0444637441  0.8922865798  0.892286580  1.78457316
#> 493 0.235814296 0.0444637441  0.4635467124  0.811445935  1.62289187
#> 494 0.000000000 0.0444637441 -0.1077134202 -1.000000000 -1.00000000
#> 495 0.000000000 0.0444637441 -0.1077134202 -1.000000000 -1.00000000
#> 496 0.166481687 0.0444637441  0.2955884666  0.732921110  1.46584222
#> 497 0.150886458 0.0444637441  0.2578090242  0.705316536  1.41063307
#> 498 0.000000000 0.0444637441 -0.1077134202 -1.000000000 -1.00000000
#> 499 0.016328863 0.0444637441 -0.0681567488 -0.632760047 -0.77508027
#> 500 0.026409203 0.0444637441 -0.0437371263 -0.406050855 -0.57757634
#> 501 0.029544279 0.0444637441 -0.0361424031 -0.335542248 -0.50248092
#> 502 0.000000000 0.0444637441 -0.1077134202 -1.000000000 -1.00000000
#> 503 0.019346726 0.0444637441 -0.0608459775 -0.564887619 -0.72195295
#> 504 0.043263288 0.0444637441 -0.0029081050 -0.026998539 -0.05257756
#> 505 0.052802957 0.0444637441  0.0202017431  0.157930792  0.31586158
#> 506 0.000000000 0.0444637441 -0.1077134202 -1.000000000 -1.00000000
#> 507 0.081155657 0.0245416770  0.0804154402  0.697597456  1.39519491
#> 508 0.000000000 0.0245416770 -0.0348594070 -1.000000000 -1.00000000
#> 509 0.189304307 0.0245416770  0.2340315853  0.870358591  1.74071718
#> 510 0.000000000 0.0245416770 -0.0348594070 -1.000000000 -1.00000000
#> 511 0.000000000 0.0245416770 -0.0348594070 -1.000000000 -1.00000000
#> 512 0.481596147 0.0245416770  0.6492077871  0.949040961  1.89808192
#> 513 0.072137060 0.0245416770  0.0676052759  0.659791003  1.31958201
#> 514 0.704018774 0.0245416770  0.9651405930  0.965140593  1.93028119
#> 515 0.078013262 0.0245416770  0.0759519309  0.685416604  1.37083321
#> 516 0.124382870 0.0245416770  0.1418160941  0.802692468  1.60538494
#> 517 0.265291083 0.0245416770  0.3419644692  0.907491512  1.81498302
#> 518 0.000000000 0.0245416770 -0.0348594070 -1.000000000 -1.00000000
#> 519 0.000000000 0.0245416770 -0.0348594070 -1.000000000 -1.00000000
#> 520 0.006448909 0.0245416770 -0.0256992698 -0.737226249 -0.84873948
#> 521 0.150886458 0.0245416770  0.1794622326  0.837350036  1.67470007
#> 522 0.020195273 0.0245416770 -0.0061737050 -0.177102984 -0.30091332
#> 523 0.134818704 0.0245416770  0.1566393276  0.817965340  1.63593068
#> 524 0.024721879 0.0245416770  0.0002559618  0.007289167  0.01457833
#> 525 0.042206114 0.0245416770  0.0250908601  0.418527912  0.83705582
#> 526 0.004082216 0.0245416770 -0.0290609596 -0.833661904 -0.90928639
#> 527 0.035201971 0.0245416770  0.0151420598  0.302832312  0.60566462
#> 528 0.000000000 0.0245416770 -0.0348594070 -1.000000000 -1.00000000
#> 529 0.000000000 0.0245416770 -0.0348594070 -1.000000000 -1.00000000
#> 530 0.176004693 0.1486936898  0.0273110036  0.155172019  0.31034404
#> 531 0.000000000 0.1486936898 -0.1486936898 -1.000000000 -1.00000000
#> 532 0.000000000 0.1486936898 -0.1486936898 -1.000000000 -1.00000000
#> 533 0.241254524 0.1486936898  0.0925608337  0.383664656  0.76732931
#> 534 0.137598899 0.1486936898 -0.0110947906 -0.074615074 -0.13886847
#> 535 0.000000000 0.1486936898 -0.1486936898 -1.000000000 -1.00000000
#> 536 0.227235838 0.1486936898  0.0785421485  0.345641555  0.69128311
#> 537 0.000000000 0.1486936898 -0.1486936898 -1.000000000 -1.00000000
#> 538 1.000000000 0.1486936898  0.8513063102  0.851306310  1.70261262
#> 539 0.075443229 0.1486936898 -0.0732504609 -0.492626560 -0.66008012
#> 540 0.189304307 0.1486936898  0.0406106168  0.214525583  0.42905117
#> 541 0.000000000 0.1486936898 -0.1486936898 -1.000000000 -1.00000000
#> 542 0.017222243 0.1486936898 -0.1314714464 -0.884176366 -0.93852824
#> 543 0.029544279 0.1486936898 -0.1191494104 -0.801307779 -0.88969557
#> 544 0.117907148 0.1486936898 -0.0307865417 -0.207046726 -0.34306332
#> 545 0.000000000 0.1486936898 -0.1486936898 -1.000000000 -1.00000000
#> 546 0.088004928 0.1486936898 -0.0606887616 -0.408146180 -0.57969291
#> 547 0.019346726 0.1486936898 -0.1293469643 -0.869888725 -0.93041764
#> 548 0.000000000 0.1486936898 -0.1486936898 -1.000000000 -1.00000000
#> 549 0.000000000 0.1486936898 -0.1486936898 -1.000000000 -1.00000000
#> 550 0.635930048 0.1486936898  0.4872363579  0.766179173  1.53235835
#> 551 0.017174753 0.1486936898 -0.1315189367 -0.884495750 -0.93870814
#> 552 0.018641790 0.1486936898 -0.1300518996 -0.874629581 -0.93312256
#> 553 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 554 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 555 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 556 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 557 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 558 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 559 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 560 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 561 0.007095721 0.0007218140  0.0589055262  0.898274749  1.79654950
#> 562 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 563 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 564 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 565 0.108205591 0.0007218140  0.9933292354  0.993329235  1.98665847
#> 566 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 567 0.075443229 0.0007218140  0.6905504098  0.990432355  1.98086471
#> 568 0.008587377 0.0007218140  0.0726909070  0.915944756  1.83188951
#> 569 0.017600986 0.0007218140  0.1559916778  0.958990136  1.91798027
#> 570 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 571 0.075971004 0.0007218140  0.6954279344  0.990498822  1.98099764
#> 572 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 573 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 574 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 575 0.004299272 0.0007218140  0.0330616773  0.832107862  1.66421572
#> 576 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 577 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 578 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 579 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 580 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 581 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 582 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 583 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 584 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 585 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 586 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 587 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 588 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 589 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 590 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 591 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 592 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 593 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 594 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 595 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 596 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 597 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 598 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#>              RII
#> 2   -1.000000000
#> 3    0.639711797
#> 4   -1.000000000
#> 5   -1.000000000
#> 6    0.641793543
#> 7    0.612716988
#> 8   -1.000000000
#> 9   -1.000000000
#> 10  -1.000000000
#> 11  -1.000000000
#> 12  -1.000000000
#> 13  -1.000000000
#> 14  -1.000000000
#> 15  -1.000000000
#> 16  -0.341435890
#> 17  -1.000000000
#> 18  -0.312631412
#> 19  -1.000000000
#> 20  -1.000000000
#> 21  -1.000000000
#> 22  -1.000000000
#> 23  -1.000000000
#> 24  -1.000000000
#> 25  -1.000000000
#> 26  -1.000000000
#> 27   0.939909052
#> 28  -1.000000000
#> 29  -1.000000000
#> 30  -1.000000000
#> 31  -1.000000000
#> 32  -1.000000000
#> 33  -1.000000000
#> 34   0.927301127
#> 35  -1.000000000
#> 36  -1.000000000
#> 37  -1.000000000
#> 38  -1.000000000
#> 39  -1.000000000
#> 40  -1.000000000
#> 41  -1.000000000
#> 42  -1.000000000
#> 43  -1.000000000
#> 44  -1.000000000
#> 45  -1.000000000
#> 46  -1.000000000
#> 47  -1.000000000
#> 48   0.789425200
#> 49  -1.000000000
#> 50   0.720245311
#> 51  -1.000000000
#> 52  -1.000000000
#> 53  -1.000000000
#> 54  -1.000000000
#> 55   0.718943589
#> 56  -1.000000000
#> 57  -1.000000000
#> 58   0.700637947
#> 59  -1.000000000
#> 60  -1.000000000
#> 61   0.789669941
#> 62  -1.000000000
#> 63   0.825197424
#> 64  -1.000000000
#> 65  -1.000000000
#> 66  -1.000000000
#> 67  -1.000000000
#> 68  -1.000000000
#> 69  -1.000000000
#> 70  -1.000000000
#> 71  -1.000000000
#> 72  -1.000000000
#> 73   0.614258410
#> 74   0.851770010
#> 75   0.915566478
#> 76  -1.000000000
#> 77  -1.000000000
#> 78  -1.000000000
#> 79  -1.000000000
#> 80   0.928809795
#> 81   0.863512296
#> 82   0.652590565
#> 83  -1.000000000
#> 84  -1.000000000
#> 85   0.904828582
#> 86   0.191206489
#> 87  -1.000000000
#> 88  -1.000000000
#> 89  -1.000000000
#> 90   0.927209972
#> 91  -1.000000000
#> 92   0.700814020
#> 93  -1.000000000
#> 94  -1.000000000
#> 95  -1.000000000
#> 96  -1.000000000
#> 97  -1.000000000
#> 98   0.952249863
#> 99  -1.000000000
#> 100 -1.000000000
#> 101  0.969691119
#> 102 -1.000000000
#> 103 -1.000000000
#> 104  0.914316879
#> 105  0.092984411
#> 106  0.974632204
#> 107 -1.000000000
#> 108  0.995018209
#> 109  0.954871462
#> 110 -1.000000000
#> 111  0.738376647
#> 112 -1.000000000
#> 113 -1.000000000
#> 114 -1.000000000
#> 115 -1.000000000
#> 116 -1.000000000
#> 117 -1.000000000
#> 118 -1.000000000
#> 120 -0.897397867
#> 121 -1.000000000
#> 122 -1.000000000
#> 123  0.637675662
#> 124 -1.000000000
#> 125 -1.000000000
#> 126  0.507786101
#> 127 -0.867328311
#> 128  0.563454837
#> 129 -1.000000000
#> 130 -0.044753038
#> 131 -0.049765161
#> 132 -0.633346281
#> 133 -1.000000000
#> 134 -1.000000000
#> 135 -0.217467475
#> 136  0.177669785
#> 137 -1.000000000
#> 138 -1.000000000
#> 139 -1.000000000
#> 140 -1.000000000
#> 141  0.535623537
#> 142 -1.000000000
#> 143 -1.000000000
#> 144 -1.000000000
#> 145 -1.000000000
#> 146 -1.000000000
#> 147 -1.000000000
#> 148 -1.000000000
#> 149 -1.000000000
#> 150  0.249074756
#> 151 -1.000000000
#> 152 -1.000000000
#> 153 -1.000000000
#> 154  0.486032204
#> 155 -1.000000000
#> 156 -1.000000000
#> 157  0.264644514
#> 158  0.945738177
#> 159  0.078167521
#> 160  0.273209600
#> 161 -1.000000000
#> 162 -1.000000000
#> 163 -0.159112508
#> 164  0.923472689
#> 165 -1.000000000
#> 166 -1.000000000
#> 167 -1.000000000
#> 168  0.746278419
#> 169  0.838313556
#> 170  0.715778471
#> 171 -1.000000000
#> 172 -0.198776800
#> 173 -1.000000000
#> 174 -1.000000000
#> 175  0.456134196
#> 176 -1.000000000
#> 177  0.181369037
#> 178  0.588975332
#> 179 -0.253331488
#> 180  0.708957556
#> 181  0.192973007
#> 182 -0.198152554
#> 183  0.832765722
#> 184 -1.000000000
#> 185 -1.000000000
#> 186 -1.000000000
#> 187 -1.000000000
#> 188 -1.000000000
#> 189 -1.000000000
#> 190 -1.000000000
#> 191 -1.000000000
#> 192 -1.000000000
#> 193 -1.000000000
#> 194 -1.000000000
#> 195 -1.000000000
#> 196 -1.000000000
#> 197 -1.000000000
#> 198  0.376797272
#> 199 -1.000000000
#> 200 -1.000000000
#> 201  0.782419513
#> 202 -1.000000000
#> 203 -1.000000000
#> 204 -1.000000000
#> 205 -1.000000000
#> 206 -1.000000000
#> 207 -1.000000000
#> 208 -1.000000000
#> 209  0.923031458
#> 210 -1.000000000
#> 211 -1.000000000
#> 212  0.926280142
#> 213 -1.000000000
#> 214  0.421564129
#> 215  0.931290908
#> 216 -1.000000000
#> 217  0.914418988
#> 218 -1.000000000
#> 219  0.139983635
#> 220  0.896310107
#> 221  0.628627003
#> 222 -1.000000000
#> 223 -1.000000000
#> 224  0.812713621
#> 225 -1.000000000
#> 226  0.477499514
#> 227  0.496758424
#> 228  0.790847767
#> 229  0.036653457
#> 230  0.821576303
#> 231 -0.691008906
#> 232 -1.000000000
#> 233 -1.000000000
#> 234 -1.000000000
#> 235 -1.000000000
#> 236 -1.000000000
#> 237 -1.000000000
#> 238  0.102656184
#> 239  0.168541418
#> 240  0.514925765
#> 241 -1.000000000
#> 242 -0.457446765
#> 243 -1.000000000
#> 244 -1.000000000
#> 245 -1.000000000
#> 246 -1.000000000
#> 247 -0.002932144
#> 248 -1.000000000
#> 249 -1.000000000
#> 250  0.147165222
#> 251 -1.000000000
#> 252 -1.000000000
#> 253 -1.000000000
#> 258  1.000000000
#> 265  1.000000000
#> 268  1.000000000
#> 272  1.000000000
#> 277 -0.557422232
#> 278  0.320562228
#> 279 -0.580788106
#> 280 -0.053614008
#> 281  0.069458237
#> 282 -0.903936023
#> 283 -1.000000000
#> 284 -1.000000000
#> 285  0.277428675
#> 286 -0.823089835
#> 287 -0.653547550
#> 288 -0.864152437
#> 289 -0.287369257
#> 290 -0.286046659
#> 291 -0.463241430
#> 292 -0.672377038
#> 293 -0.760780479
#> 294 -1.000000000
#> 295 -1.000000000
#> 296 -0.167504366
#> 297 -1.000000000
#> 298 -0.889438941
#> 299 -1.000000000
#> 323 -1.000000000
#> 324 -1.000000000
#> 325 -1.000000000
#> 326 -1.000000000
#> 327 -1.000000000
#> 328 -1.000000000
#> 329 -1.000000000
#> 330 -1.000000000
#> 331 -1.000000000
#> 332 -1.000000000
#> 333 -1.000000000
#> 334 -1.000000000
#> 335 -1.000000000
#> 336 -1.000000000
#> 337 -1.000000000
#> 338 -1.000000000
#> 339 -1.000000000
#> 340 -1.000000000
#> 341 -1.000000000
#> 342 -1.000000000
#> 343 -1.000000000
#> 344 -1.000000000
#> 345 -1.000000000
#> 346 -0.118178767
#> 347  0.544351040
#> 348  0.060180909
#> 349 -0.723146410
#> 350  0.546022545
#> 351 -1.000000000
#> 352  0.308622561
#> 353  0.395610922
#> 354  0.855559653
#> 355 -0.089559073
#> 356  0.287038778
#> 358  0.074136100
#> 359  0.539058747
#> 360  0.646093132
#> 362 -1.000000000
#> 363  0.686903361
#> 364  0.653753895
#> 365  0.775737001
#> 366 -1.000000000
#> 367 -1.000000000
#> 368  0.814734052
#> 369 -1.000000000
#> 370 -1.000000000
#> 371 -1.000000000
#> 372 -1.000000000
#> 373 -1.000000000
#> 374 -1.000000000
#> 375 -1.000000000
#> 376 -1.000000000
#> 377  0.954315323
#> 378 -1.000000000
#> 379 -1.000000000
#> 380 -1.000000000
#> 381 -1.000000000
#> 382 -1.000000000
#> 383 -1.000000000
#> 384  0.212208955
#> 385 -1.000000000
#> 386 -1.000000000
#> 387 -1.000000000
#> 388 -1.000000000
#> 389 -1.000000000
#> 390  0.472379671
#> 391 -1.000000000
#> 392 -1.000000000
#> 393 -1.000000000
#> 394 -1.000000000
#> 395  0.171456277
#> 396 -1.000000000
#> 397 -1.000000000
#> 398 -1.000000000
#> 399  0.506043428
#> 400 -1.000000000
#> 401 -1.000000000
#> 402  0.906190131
#> 403 -1.000000000
#> 404  0.888984212
#> 405  0.790847767
#> 406  0.930555810
#> 407  0.800223766
#> 408 -1.000000000
#> 409  0.875086148
#> 410  0.321821496
#> 411 -1.000000000
#> 412  0.712846548
#> 413  0.553017983
#> 414 -1.000000000
#> 415 -1.000000000
#> 416 -1.000000000
#> 417 -1.000000000
#> 418 -1.000000000
#> 419 -1.000000000
#> 420 -1.000000000
#> 421 -1.000000000
#> 422 -1.000000000
#> 423 -1.000000000
#> 424 -1.000000000
#> 425 -1.000000000
#> 426 -1.000000000
#> 427 -1.000000000
#> 428 -1.000000000
#> 429 -1.000000000
#> 430 -1.000000000
#> 431 -1.000000000
#> 432 -1.000000000
#> 433 -1.000000000
#> 434 -1.000000000
#> 435 -1.000000000
#> 436 -1.000000000
#> 437 -1.000000000
#> 438 -1.000000000
#> 439 -1.000000000
#> 440 -1.000000000
#> 441 -1.000000000
#> 442  0.778538685
#> 443 -1.000000000
#> 444 -1.000000000
#> 445 -1.000000000
#> 446  0.871668813
#> 447 -1.000000000
#> 448 -1.000000000
#> 449 -0.130812331
#> 450 -1.000000000
#> 451 -1.000000000
#> 452  0.556625317
#> 453 -0.027320434
#> 454 -1.000000000
#> 455 -1.000000000
#> 456 -1.000000000
#> 457  0.522766897
#> 458 -1.000000000
#> 459 -1.000000000
#> 460 -1.000000000
#> 461 -1.000000000
#> 462 -1.000000000
#> 463 -1.000000000
#> 464 -1.000000000
#> 465 -1.000000000
#> 466 -1.000000000
#> 467 -1.000000000
#> 468 -1.000000000
#> 469 -1.000000000
#> 470 -1.000000000
#> 471 -1.000000000
#> 472  0.602808964
#> 473 -1.000000000
#> 474 -1.000000000
#> 475  0.987827948
#> 476  0.570309932
#> 477  0.877750622
#> 478 -1.000000000
#> 479 -1.000000000
#> 480  0.733851409
#> 481 -1.000000000
#> 482 -1.000000000
#> 483 -1.000000000
#> 484  0.706327348
#> 485 -1.000000000
#> 486 -1.000000000
#> 487 -0.261952662
#> 488  0.722442454
#> 489 -1.000000000
#> 491  0.792047031
#> 492  0.805521143
#> 493  0.682716890
#> 494 -1.000000000
#> 495 -1.000000000
#> 496  0.578433684
#> 497  0.544779133
#> 498 -1.000000000
#> 499 -0.462801022
#> 500 -0.254745176
#> 501 -0.201592529
#> 502 -1.000000000
#> 503 -0.393619083
#> 504 -0.013683993
#> 505  0.085735537
#> 506 -1.000000000
#> 507  0.535623537
#> 508 -1.000000000
#> 509  0.770473342
#> 510 -1.000000000
#> 511 -1.000000000
#> 512  0.903023739
#> 513  0.492304562
#> 514  0.932629676
#> 515  0.521394539
#> 516  0.670414616
#> 517  0.830649393
#> 518 -1.000000000
#> 519 -1.000000000
#> 520 -0.583814993
#> 521  0.720208198
#> 522 -0.097154684
#> 523  0.691997763
#> 524  0.003657915
#> 525  0.264644514
#> 526 -0.714768648
#> 527  0.178433937
#> 528 -1.000000000
#> 529 -1.000000000
#> 530  0.084111917
#> 531 -1.000000000
#> 532 -1.000000000
#> 533  0.237366990
#> 534 -0.038753328
#> 535 -1.000000000
#> 536  0.208927851
#> 537 -1.000000000
#> 538  0.741108198
#> 539 -0.326811224
#> 540  0.120150466
#> 541 -1.000000000
#> 542 -0.792397956
#> 543 -0.668485008
#> 544 -0.115478038
#> 545 -1.000000000
#> 546 -0.256396772
#> 547 -0.769737232
#> 548 -1.000000000
#> 549 -1.000000000
#> 550  0.620980904
#> 551 -0.792911143
#> 552 -0.777192617
#> 553 -1.000000000
#> 554 -1.000000000
#> 555 -1.000000000
#> 556 -1.000000000
#> 557 -1.000000000
#> 558 -1.000000000
#> 559 -1.000000000
#> 560 -1.000000000
#> 561  0.815334629
#> 562 -1.000000000
#> 563 -1.000000000
#> 564 -1.000000000
#> 565  0.986746879
#> 566 -1.000000000
#> 567  0.981046055
#> 568  0.844924428
#> 569  0.921211382
#> 570 -1.000000000
#> 571  0.981176490
#> 572 -1.000000000
#> 573 -1.000000000
#> 574 -1.000000000
#> 575  0.712486911
#> 576 -1.000000000
#> 577 -1.000000000
#> 578 -1.000000000
#> 579 -1.000000000
#> 580 -1.000000000
#> 581 -1.000000000
#> 582 -1.000000000
#> 583 -1.000000000
#> 584 -1.000000000
#> 585 -1.000000000
#> 586 -1.000000000
#> 587 -1.000000000
#> 588 -1.000000000
#> 589 -1.000000000
#> 590 -1.000000000
#> 591 -1.000000000
#> 592 -1.000000000
#> 593 -1.000000000
#> 594 -1.000000000
#> 595 -1.000000000
#> 596 -1.000000000
#> 597 -1.000000000
#> 598 -1.000000000
associndex (Amoladeras_int, Amoladeras_cover, expand = "no",
rm_sp_no_cover = "allsp", threshold_density=NULL)
#> Based on Weibull distribution fitted to the observed values, the threshold density has been set to: 1.12346719046902.
#> Warning: The object is not provided because the combination of arguments
#>             is not commonly used in plant-plant interaction networks
#> NULL
associndex (Amoladeras_int, Amoladeras_cover, expand = "yes",
rm_sp_no_cover = "onlycanopy", threshold_density=NULL)
#> Based on Weibull distribution fitted to the observed values, the threshold density has been set to: 1.12346719046902.
#>                    Recruit                Canopy Fcr       Ac  Fro       Ao
#> 2     Artemisia_barrelieri Piptatherum_miliaceum   0  15.4830   54 6926.992
#> 3     Artemisia_barrelieri     Thymelaea_hirsuta   5 140.9300   54 6926.992
#> 4     Artemisia_barrelieri    Phagnalon_saxatile   0  33.9250   54 6926.992
#> 5     Artemisia_barrelieri Maytenus_senegalensis   0 116.4500   54 6926.992
#> 6     Artemisia_barrelieri        Lygeum_spartum  23 643.7150   54 6926.992
#> 7     Artemisia_barrelieri Salsola_oppositifolia   2  61.6100   54 6926.992
#> 8     Artemisia_barrelieri      Launaea_lanifera   0   1.1600   54 6926.992
#> 9     Artemisia_barrelieri        Ziziphus_lotus   0 465.1950   54 6926.992
#> 10    Artemisia_barrelieri  Helichrysum_stoechas   0  29.0700   54 6926.992
#> 11    Artemisia_barrelieri   Whitania_frutescens   0  56.8150   54 6926.992
#> 12    Artemisia_barrelieri       Ballota_hirsuta   0  38.4550   54 6926.992
#> 13    Artemisia_barrelieri       Asparagus_albus   0  27.7250   54 6926.992
#> 14    Artemisia_barrelieri     Stipa_tenacissima   0   0.6000   54 6926.992
#> 15    Artemisia_barrelieri  Artemisia_campestris   0   0.1100   54 6926.992
#> 16    Artemisia_barrelieri       Thymus_hyemalis   2 522.5800   54 6926.992
#> 17    Artemisia_barrelieri    Lycium_intrincatum   0 161.8000   54 6926.992
#> 18    Artemisia_barrelieri         Ononis_natrix   1 244.9650   54 6926.992
#> 19    Artemisia_barrelieri    Asparagus_horridus   0   1.5725   54 6926.992
#> 20    Artemisia_barrelieri     Hyparrhenia_hirta   0  13.2550   54 6926.992
#> 21    Artemisia_barrelieri   Launaea_arborescens   0 473.8650   54 6926.992
#> 22    Artemisia_barrelieri  Teucrium_lusitanicum   0   5.2825   54 6926.992
#> 23    Artemisia_barrelieri       Teucrium_polium   0  17.0450   54 6926.992
#> 24    Artemisia_campestris  Artemisia_barrelieri   0   1.0000    1 6926.992
#> 25    Artemisia_campestris Piptatherum_miliaceum   0  15.4830    1 6926.992
#> 26    Artemisia_campestris     Thymelaea_hirsuta   0 140.9300    1 6926.992
#> 27    Artemisia_campestris        Lygeum_spartum   3 643.7150    1 6926.992
#> 28    Artemisia_campestris Salsola_oppositifolia   0  61.6100    1 6926.992
#> 29    Artemisia_campestris       Asparagus_albus   0  27.7250    1 6926.992
#> 30    Artemisia_campestris  Artemisia_campestris   0   0.1100    1 6926.992
#> 31    Artemisia_campestris Maytenus_senegalensis   0 116.4500    1 6926.992
#> 32    Artemisia_campestris     Stipa_tenacissima   0   0.6000    1 6926.992
#> 33    Artemisia_campestris   Whitania_frutescens   0  56.8150    1 6926.992
#> 34    Artemisia_campestris       Thymus_hyemalis   2 522.5800    1 6926.992
#> 35    Artemisia_campestris    Phagnalon_saxatile   0  33.9250    1 6926.992
#> 36    Artemisia_campestris         Ononis_natrix   0 244.9650    1 6926.992
#> 37    Artemisia_campestris  Teucrium_lusitanicum   0   5.2825    1 6926.992
#> 38    Artemisia_campestris    Asparagus_horridus   0   1.5725    1 6926.992
#> 39    Artemisia_campestris    Lycium_intrincatum   0 161.8000    1 6926.992
#> 40    Artemisia_campestris  Helichrysum_stoechas   0  29.0700    1 6926.992
#> 41    Artemisia_campestris     Hyparrhenia_hirta   0  13.2550    1 6926.992
#> 42    Artemisia_campestris        Ziziphus_lotus   0 465.1950    1 6926.992
#> 43    Artemisia_campestris   Launaea_arborescens   0 473.8650    1 6926.992
#> 44    Artemisia_campestris      Launaea_lanifera   0   1.1600    1 6926.992
#> 45    Artemisia_campestris       Teucrium_polium   0  17.0450    1 6926.992
#> 46    Artemisia_campestris       Ballota_hirsuta   0  38.4550    1 6926.992
#> 47         Asparagus_albus         Ononis_natrix   0 244.9650    7 6926.992
#> 48         Asparagus_albus Maytenus_senegalensis   1 116.4500    7 6926.992
#> 49         Asparagus_albus     Thymelaea_hirsuta   0 140.9300    7 6926.992
#> 50         Asparagus_albus        Lygeum_spartum   4 643.7150    7 6926.992
#> 51         Asparagus_albus Piptatherum_miliaceum   0  15.4830    7 6926.992
#> 52         Asparagus_albus  Artemisia_barrelieri   0   1.0000    7 6926.992
#> 53         Asparagus_albus  Artemisia_campestris   0   0.1100    7 6926.992
#> 54         Asparagus_albus    Phagnalon_saxatile   0  33.9250    7 6926.992
#> 55         Asparagus_albus    Lycium_intrincatum   1 161.8000    7 6926.992
#> 56         Asparagus_albus Salsola_oppositifolia   0  61.6100    7 6926.992
#> 57         Asparagus_albus       Asparagus_albus   0  27.7250    7 6926.992
#> 58         Asparagus_albus       Thymus_hyemalis   3 522.5800    7 6926.992
#> 59         Asparagus_albus  Helichrysum_stoechas   0  29.0700    7 6926.992
#> 60         Asparagus_albus     Stipa_tenacissima   0   0.6000    7 6926.992
#> 61         Asparagus_albus        Ziziphus_lotus   4 465.1950    7 6926.992
#> 62         Asparagus_albus   Whitania_frutescens   0  56.8150    7 6926.992
#> 63         Asparagus_albus   Launaea_arborescens   5 473.8650    7 6926.992
#> 64         Asparagus_albus     Hyparrhenia_hirta   0  13.2550    7 6926.992
#> 65         Asparagus_albus  Teucrium_lusitanicum   0   5.2825    7 6926.992
#> 66         Asparagus_albus    Asparagus_horridus   0   1.5725    7 6926.992
#> 67         Asparagus_albus      Launaea_lanifera   0   1.1600    7 6926.992
#> 68         Asparagus_albus       Teucrium_polium   0  17.0450    7 6926.992
#> 69         Asparagus_albus       Ballota_hirsuta   0  38.4550    7 6926.992
#> 70      Asparagus_horridus Maytenus_senegalensis   0 116.4500   18 6926.992
#> 71      Asparagus_horridus Piptatherum_miliaceum   0  15.4830   18 6926.992
#> 72      Asparagus_horridus  Artemisia_campestris   0   0.1100   18 6926.992
#> 73      Asparagus_horridus        Lygeum_spartum   7 643.7150   18 6926.992
#> 74      Asparagus_horridus Salsola_oppositifolia   2  61.6100   18 6926.992
#> 75      Asparagus_horridus    Phagnalon_saxatile   2  33.9250   18 6926.992
#> 76      Asparagus_horridus    Asparagus_horridus   0   1.5725   18 6926.992
#> 77      Asparagus_horridus         Ononis_natrix   0 244.9650   18 6926.992
#> 78      Asparagus_horridus     Stipa_tenacissima   0   0.6000   18 6926.992
#> 79      Asparagus_horridus  Artemisia_barrelieri   0   1.0000   18 6926.992
#> 80      Asparagus_horridus   Whitania_frutescens   4  56.8150   18 6926.992
#> 81      Asparagus_horridus     Thymelaea_hirsuta   5 140.9300   18 6926.992
#> 82      Asparagus_horridus    Lycium_intrincatum   2 161.8000   18 6926.992
#> 83      Asparagus_horridus       Asparagus_albus   0  27.7250   18 6926.992
#> 84      Asparagus_horridus       Teucrium_polium   0  17.0450   18 6926.992
#> 85      Asparagus_horridus       Ballota_hirsuta   2  38.4550   18 6926.992
#> 86      Asparagus_horridus       Thymus_hyemalis   2 522.5800   18 6926.992
#> 87      Asparagus_horridus  Teucrium_lusitanicum   0   5.2825   18 6926.992
#> 88      Asparagus_horridus     Hyparrhenia_hirta   0  13.2550   18 6926.992
#> 89      Asparagus_horridus        Ziziphus_lotus   0 465.1950   18 6926.992
#> 90      Asparagus_horridus  Helichrysum_stoechas   2  29.0700   18 6926.992
#> 91      Asparagus_horridus      Launaea_lanifera   0   1.1600   18 6926.992
#> 92      Asparagus_horridus   Launaea_arborescens   7 473.8650   18 6926.992
#> 93         Ballota_hirsuta Maytenus_senegalensis   0 116.4500   11 6926.992
#> 94         Ballota_hirsuta  Artemisia_campestris   0   0.1100   11 6926.992
#> 95         Ballota_hirsuta    Phagnalon_saxatile   0  33.9250   11 6926.992
#> 96         Ballota_hirsuta  Artemisia_barrelieri   0   1.0000   11 6926.992
#> 97         Ballota_hirsuta Piptatherum_miliaceum   0  15.4830   11 6926.992
#> 98         Ballota_hirsuta Salsola_oppositifolia   4  61.6100   11 6926.992
#> 99         Ballota_hirsuta         Ononis_natrix   0 244.9650   11 6926.992
#> 100        Ballota_hirsuta        Lygeum_spartum   0 643.7150   11 6926.992
#> 101        Ballota_hirsuta  Helichrysum_stoechas   3  29.0700   11 6926.992
#> 102        Ballota_hirsuta     Stipa_tenacissima   0   0.6000   11 6926.992
#> 103        Ballota_hirsuta       Asparagus_albus   0  27.7250   11 6926.992
#> 104        Ballota_hirsuta     Thymelaea_hirsuta   5 140.9300   11 6926.992
#> 105        Ballota_hirsuta       Thymus_hyemalis   1 522.5800   11 6926.992
#> 106        Ballota_hirsuta    Lycium_intrincatum  20 161.8000   11 6926.992
#> 107        Ballota_hirsuta  Teucrium_lusitanicum   0   5.2825   11 6926.992
#> 108        Ballota_hirsuta    Asparagus_horridus   1   1.5725   11 6926.992
#> 109        Ballota_hirsuta        Ziziphus_lotus  32 465.1950   11 6926.992
#> 110        Ballota_hirsuta     Hyparrhenia_hirta   0  13.2550   11 6926.992
#> 111        Ballota_hirsuta   Launaea_arborescens   5 473.8650   11 6926.992
#> 112        Ballota_hirsuta       Teucrium_polium   0  17.0450   11 6926.992
#> 113        Ballota_hirsuta       Ballota_hirsuta   0  38.4550   11 6926.992
#> 114        Ballota_hirsuta   Whitania_frutescens   0  56.8150   11 6926.992
#> 115        Ballota_hirsuta      Launaea_lanifera   0   1.1600   11 6926.992
#> 116   Helichrysum_stoechas  Artemisia_campestris   0   0.1100  796 6926.992
#> 117   Helichrysum_stoechas    Phagnalon_saxatile   0  33.9250  796 6926.992
#> 118   Helichrysum_stoechas Piptatherum_miliaceum   0  15.4830  796 6926.992
#> 120   Helichrysum_stoechas        Lygeum_spartum   4 643.7150  796 6926.992
#> 121   Helichrysum_stoechas       Asparagus_albus   0  27.7250  796 6926.992
#> 122   Helichrysum_stoechas    Asparagus_horridus   0   1.5725  796 6926.992
#> 123   Helichrysum_stoechas Salsola_oppositifolia  32  61.6100  796 6926.992
#> 124   Helichrysum_stoechas     Stipa_tenacissima   0   0.6000  796 6926.992
#> 125   Helichrysum_stoechas Maytenus_senegalensis   0 116.4500  796 6926.992
#> 126   Helichrysum_stoechas       Teucrium_polium   6  17.0450  796 6926.992
#> 127   Helichrysum_stoechas         Ononis_natrix   2 244.9650  796 6926.992
#> 128   Helichrysum_stoechas     Thymelaea_hirsuta  58 140.9300  796 6926.992
#> 129   Helichrysum_stoechas  Teucrium_lusitanicum   0   5.2825  796 6926.992
#> 130   Helichrysum_stoechas    Lycium_intrincatum  17 161.8000  796 6926.992
#> 131   Helichrysum_stoechas       Ballota_hirsuta   4  38.4550  796 6926.992
#> 132   Helichrysum_stoechas        Ziziphus_lotus  12 465.1950  796 6926.992
#> 133   Helichrysum_stoechas  Helichrysum_stoechas   0  29.0700  796 6926.992
#> 134   Helichrysum_stoechas     Hyparrhenia_hirta   0  13.2550  796 6926.992
#> 135   Helichrysum_stoechas   Launaea_arborescens  35 473.8650  796 6926.992
#> 136   Helichrysum_stoechas       Thymus_hyemalis  86 522.5800  796 6926.992
#> 137   Helichrysum_stoechas   Whitania_frutescens   0  56.8150  796 6926.992
#> 138   Helichrysum_stoechas      Launaea_lanifera   0   1.1600  796 6926.992
#> 139      Hyparrhenia_hirta Piptatherum_miliaceum   0  15.4830   34 6926.992
#> 140      Hyparrhenia_hirta    Phagnalon_saxatile   0  33.9250   34 6926.992
#> 141      Hyparrhenia_hirta Salsola_oppositifolia   1  61.6100   34 6926.992
#> 142      Hyparrhenia_hirta  Artemisia_barrelieri   0   1.0000   34 6926.992
#> 143      Hyparrhenia_hirta  Artemisia_campestris   0   0.1100   34 6926.992
#> 144      Hyparrhenia_hirta       Ballota_hirsuta   0  38.4550   34 6926.992
#> 145      Hyparrhenia_hirta        Lygeum_spartum   0 643.7150   34 6926.992
#> 146      Hyparrhenia_hirta       Asparagus_albus   0  27.7250   34 6926.992
#> 147      Hyparrhenia_hirta    Asparagus_horridus   0   1.5725   34 6926.992
#> 148      Hyparrhenia_hirta     Stipa_tenacissima   0   0.6000   34 6926.992
#> 149      Hyparrhenia_hirta  Teucrium_lusitanicum   0   5.2825   34 6926.992
#> 150      Hyparrhenia_hirta         Ononis_natrix   2 244.9650   34 6926.992
#> 151      Hyparrhenia_hirta Maytenus_senegalensis   0 116.4500   34 6926.992
#> 152      Hyparrhenia_hirta     Hyparrhenia_hirta   0  13.2550   34 6926.992
#> 153      Hyparrhenia_hirta    Lycium_intrincatum   0 161.8000   34 6926.992
#> 154      Hyparrhenia_hirta     Thymelaea_hirsuta   2 140.9300   34 6926.992
#> 155      Hyparrhenia_hirta  Helichrysum_stoechas   0  29.0700   34 6926.992
#> 156      Hyparrhenia_hirta   Whitania_frutescens   0  56.8150   34 6926.992
#> 157      Hyparrhenia_hirta   Launaea_arborescens   4 473.8650   34 6926.992
#> 158      Hyparrhenia_hirta       Teucrium_polium   3  17.0450   34 6926.992
#> 159      Hyparrhenia_hirta       Thymus_hyemalis   3 522.5800   34 6926.992
#> 160      Hyparrhenia_hirta        Ziziphus_lotus   4 465.1950   34 6926.992
#> 161      Hyparrhenia_hirta      Launaea_lanifera   0   1.1600   34 6926.992
#> 162    Launaea_arborescens  Artemisia_campestris   0   0.1100   89 6926.992
#> 163    Launaea_arborescens        Lygeum_spartum   6 643.7150   89 6926.992
#> 164    Launaea_arborescens Piptatherum_miliaceum   5  15.4830   89 6926.992
#> 165    Launaea_arborescens  Artemisia_barrelieri   0   1.0000   89 6926.992
#> 166    Launaea_arborescens       Asparagus_albus   0  27.7250   89 6926.992
#> 167    Launaea_arborescens    Asparagus_horridus   0   1.5725   89 6926.992
#> 168    Launaea_arborescens    Phagnalon_saxatile   3  33.9250   89 6926.992
#> 169    Launaea_arborescens Salsola_oppositifolia   9  61.6100   89 6926.992
#> 170    Launaea_arborescens         Ononis_natrix  19 244.9650   89 6926.992
#> 171    Launaea_arborescens       Teucrium_polium   0  17.0450   89 6926.992
#> 172    Launaea_arborescens Maytenus_senegalensis   1 116.4500   89 6926.992
#> 173    Launaea_arborescens     Stipa_tenacissima   0   0.6000   89 6926.992
#> 174    Launaea_arborescens  Teucrium_lusitanicum   0   5.2825   89 6926.992
#> 175    Launaea_arborescens  Helichrysum_stoechas   1  29.0700   89 6926.992
#> 176    Launaea_arborescens       Ballota_hirsuta   0  38.4550   89 6926.992
#> 177    Launaea_arborescens    Lycium_intrincatum   3 161.8000   89 6926.992
#> 178    Launaea_arborescens     Thymelaea_hirsuta   7 140.9300   89 6926.992
#> 179    Launaea_arborescens       Thymus_hyemalis   4 522.5800   89 6926.992
#> 180    Launaea_arborescens     Hyparrhenia_hirta   1  13.2550   89 6926.992
#> 181    Launaea_arborescens   Launaea_arborescens   9 473.8650   89 6926.992
#> 182    Launaea_arborescens        Ziziphus_lotus   4 465.1950   89 6926.992
#> 183    Launaea_arborescens   Whitania_frutescens   8  56.8150   89 6926.992
#> 184    Launaea_arborescens      Launaea_lanifera   0   1.1600   89 6926.992
#> 185       Launaea_lanifera     Stipa_tenacissima   0   0.6000    6 6926.992
#> 186       Launaea_lanifera  Artemisia_barrelieri   0   1.0000    6 6926.992
#> 187       Launaea_lanifera       Asparagus_albus   0  27.7250    6 6926.992
#> 188       Launaea_lanifera  Artemisia_campestris   0   0.1100    6 6926.992
#> 189       Launaea_lanifera Salsola_oppositifolia   0  61.6100    6 6926.992
#> 190       Launaea_lanifera       Teucrium_polium   0  17.0450    6 6926.992
#> 191       Launaea_lanifera Piptatherum_miliaceum   0  15.4830    6 6926.992
#> 192       Launaea_lanifera    Asparagus_horridus   0   1.5725    6 6926.992
#> 193       Launaea_lanifera  Teucrium_lusitanicum   0   5.2825    6 6926.992
#> 194       Launaea_lanifera    Phagnalon_saxatile   0  33.9250    6 6926.992
#> 195       Launaea_lanifera    Lycium_intrincatum   0 161.8000    6 6926.992
#> 196       Launaea_lanifera  Helichrysum_stoechas   0  29.0700    6 6926.992
#> 197       Launaea_lanifera       Ballota_hirsuta   0  38.4550    6 6926.992
#> 198       Launaea_lanifera       Thymus_hyemalis   1 522.5800    6 6926.992
#> 199       Launaea_lanifera   Whitania_frutescens   0  56.8150    6 6926.992
#> 200       Launaea_lanifera         Ononis_natrix   0 244.9650    6 6926.992
#> 201       Launaea_lanifera     Thymelaea_hirsuta   1 140.9300    6 6926.992
#> 202       Launaea_lanifera Maytenus_senegalensis   0 116.4500    6 6926.992
#> 203       Launaea_lanifera   Launaea_arborescens   0 473.8650    6 6926.992
#> 204       Launaea_lanifera        Lygeum_spartum   0 643.7150    6 6926.992
#> 205       Launaea_lanifera     Hyparrhenia_hirta   0  13.2550    6 6926.992
#> 206       Launaea_lanifera        Ziziphus_lotus   0 465.1950    6 6926.992
#> 207       Launaea_lanifera      Launaea_lanifera   0   1.1600    6 6926.992
#> 208     Lycium_intrincatum       Teucrium_polium   0  17.0450   20 6926.992
#> 209     Lycium_intrincatum       Asparagus_albus   2  27.7250   20 6926.992
#> 210     Lycium_intrincatum     Stipa_tenacissima   0   0.6000   20 6926.992
#> 211     Lycium_intrincatum  Helichrysum_stoechas   0  29.0700   20 6926.992
#> 212     Lycium_intrincatum     Hyparrhenia_hirta   1  13.2550   20 6926.992
#> 213     Lycium_intrincatum    Asparagus_horridus   0   1.5725   20 6926.992
#> 214     Lycium_intrincatum     Thymelaea_hirsuta   1 140.9300   20 6926.992
#> 215     Lycium_intrincatum Salsola_oppositifolia   5  61.6100   20 6926.992
#> 216     Lycium_intrincatum  Artemisia_barrelieri   0   1.0000   20 6926.992
#> 217     Lycium_intrincatum Piptatherum_miliaceum   1  15.4830   20 6926.992
#> 218     Lycium_intrincatum  Artemisia_campestris   0   0.1100   20 6926.992
#> 219     Lycium_intrincatum       Thymus_hyemalis   2 522.5800   20 6926.992
#> 220     Lycium_intrincatum   Whitania_frutescens   3  56.8150   20 6926.992
#> 221     Lycium_intrincatum   Launaea_arborescens   6 473.8650   20 6926.992
#> 222     Lycium_intrincatum       Ballota_hirsuta   0  38.4550   20 6926.992
#> 223     Lycium_intrincatum  Teucrium_lusitanicum   0   5.2825   20 6926.992
#> 224     Lycium_intrincatum        Ziziphus_lotus  13 465.1950   20 6926.992
#> 225     Lycium_intrincatum      Launaea_lanifera   0   1.1600   20 6926.992
#> 226     Lycium_intrincatum         Ononis_natrix   2 244.9650   20 6926.992
#> 227     Lycium_intrincatum Maytenus_senegalensis   1 116.4500   20 6926.992
#> 228     Lycium_intrincatum    Lycium_intrincatum   4 161.8000   20 6926.992
#> 229     Lycium_intrincatum        Lygeum_spartum   2 643.7150   20 6926.992
#> 230     Lycium_intrincatum    Phagnalon_saxatile   1  33.9250   20 6926.992
#> 231         Lygeum_spartum   Launaea_arborescens   1 473.8650   80 6926.992
#> 232         Lygeum_spartum  Helichrysum_stoechas   0  29.0700   80 6926.992
#> 233         Lygeum_spartum     Hyparrhenia_hirta   0  13.2550   80 6926.992
#> 234         Lygeum_spartum     Stipa_tenacissima   0   0.6000   80 6926.992
#> 235         Lygeum_spartum       Teucrium_polium   0  17.0450   80 6926.992
#> 236         Lygeum_spartum       Ballota_hirsuta   0  38.4550   80 6926.992
#> 237         Lygeum_spartum Piptatherum_miliaceum   0  15.4830   80 6926.992
#> 238         Lygeum_spartum     Thymelaea_hirsuta   2 140.9300   80 6926.992
#> 239         Lygeum_spartum Salsola_oppositifolia   1  61.6100   80 6926.992
#> 240         Lygeum_spartum       Asparagus_albus   1  27.7250   80 6926.992
#> 241         Lygeum_spartum    Lycium_intrincatum   0 161.8000   80 6926.992
#> 242         Lygeum_spartum        Ziziphus_lotus   2 465.1950   80 6926.992
#> 243         Lygeum_spartum  Teucrium_lusitanicum   0   5.2825   80 6926.992
#> 244         Lygeum_spartum         Ononis_natrix   0 244.9650   80 6926.992
#> 245         Lygeum_spartum    Asparagus_horridus   0   1.5725   80 6926.992
#> 246         Lygeum_spartum  Artemisia_campestris   0   0.1100   80 6926.992
#> 247         Lygeum_spartum       Thymus_hyemalis   6 522.5800   80 6926.992
#> 248         Lygeum_spartum   Whitania_frutescens   0  56.8150   80 6926.992
#> 249         Lygeum_spartum    Phagnalon_saxatile   0  33.9250   80 6926.992
#> 250         Lygeum_spartum        Lygeum_spartum  10 643.7150   80 6926.992
#> 251         Lygeum_spartum  Artemisia_barrelieri   0   1.0000   80 6926.992
#> 252         Lygeum_spartum      Launaea_lanifera   0   1.1600   80 6926.992
#> 253         Lygeum_spartum Maytenus_senegalensis   0 116.4500   80 6926.992
#> 258  Maytenus_senegalensis   Launaea_arborescens   1 473.8650    0 6926.992
#> 265  Maytenus_senegalensis   Whitania_frutescens   1  56.8150    0 6926.992
#> 268  Maytenus_senegalensis        Lygeum_spartum   1 643.7150    0 6926.992
#> 272  Maytenus_senegalensis       Asparagus_albus   1  27.7250    0 6926.992
#> 277          Ononis_natrix     Hyparrhenia_hirta   1  13.2550 1839 6926.992
#> 278          Ononis_natrix  Helichrysum_stoechas  15  29.0700 1839 6926.992
#> 279          Ononis_natrix   Whitania_frutescens   4  56.8150 1839 6926.992
#> 280          Ononis_natrix   Launaea_arborescens 113 473.8650 1839 6926.992
#> 281          Ononis_natrix     Thymelaea_hirsuta  43 140.9300 1839 6926.992
#> 282          Ononis_natrix       Thymus_hyemalis   7 522.5800 1839 6926.992
#> 283          Ononis_natrix Piptatherum_miliaceum   0  15.4830 1839 6926.992
#> 284          Ononis_natrix      Launaea_lanifera   0   1.1600 1839 6926.992
#> 285          Ononis_natrix       Teucrium_polium   8  17.0450 1839 6926.992
#> 286          Ononis_natrix Maytenus_senegalensis   3 116.4500 1839 6926.992
#> 287          Ononis_natrix    Lycium_intrincatum   9 161.8000 1839 6926.992
#> 288          Ononis_natrix        Ziziphus_lotus   9 465.1950 1839 6926.992
#> 289          Ononis_natrix         Ononis_natrix  36 244.9650 1839 6926.992
#> 290          Ononis_natrix    Phagnalon_saxatile   5  33.9250 1839 6926.992
#> 291          Ononis_natrix Salsola_oppositifolia   6  61.6100 1839 6926.992
#> 292          Ononis_natrix       Ballota_hirsuta   2  38.4550 1839 6926.992
#> 293          Ononis_natrix       Asparagus_albus   1  27.7250 1839 6926.992
#> 294          Ononis_natrix  Artemisia_barrelieri   0   1.0000 1839 6926.992
#> 295          Ononis_natrix  Artemisia_campestris   0   0.1100 1839 6926.992
#> 296          Ononis_natrix  Teucrium_lusitanicum   1   5.2825 1839 6926.992
#> 297          Ononis_natrix    Asparagus_horridus   0   1.5725 1839 6926.992
#> 298          Ononis_natrix        Lygeum_spartum  10 643.7150 1839 6926.992
#> 299          Ononis_natrix     Stipa_tenacissima   0   0.6000 1839 6926.992
#> 323 Periploca_angustifolia    Lycium_intrincatum   0 161.8000    1 6926.992
#> 324 Periploca_angustifolia        Lygeum_spartum   0 643.7150    1 6926.992
#> 325 Periploca_angustifolia   Whitania_frutescens   0  56.8150    1 6926.992
#> 326 Periploca_angustifolia     Thymelaea_hirsuta   0 140.9300    1 6926.992
#> 327 Periploca_angustifolia       Thymus_hyemalis   0 522.5800    1 6926.992
#> 328 Periploca_angustifolia  Helichrysum_stoechas   0  29.0700    1 6926.992
#> 329 Periploca_angustifolia        Ziziphus_lotus   0 465.1950    1 6926.992
#> 330 Periploca_angustifolia      Launaea_lanifera   0   1.1600    1 6926.992
#> 331 Periploca_angustifolia         Ononis_natrix   0 244.9650    1 6926.992
#> 332 Periploca_angustifolia Piptatherum_miliaceum   0  15.4830    1 6926.992
#> 333 Periploca_angustifolia  Artemisia_barrelieri   0   1.0000    1 6926.992
#> 334 Periploca_angustifolia  Artemisia_campestris   0   0.1100    1 6926.992
#> 335 Periploca_angustifolia       Teucrium_polium   0  17.0450    1 6926.992
#> 336 Periploca_angustifolia Maytenus_senegalensis   0 116.4500    1 6926.992
#> 337 Periploca_angustifolia     Hyparrhenia_hirta   0  13.2550    1 6926.992
#> 338 Periploca_angustifolia     Stipa_tenacissima   0   0.6000    1 6926.992
#> 339 Periploca_angustifolia       Asparagus_albus   0  27.7250    1 6926.992
#> 340 Periploca_angustifolia   Launaea_arborescens   0 473.8650    1 6926.992
#> 341 Periploca_angustifolia Salsola_oppositifolia   0  61.6100    1 6926.992
#> 342 Periploca_angustifolia       Ballota_hirsuta   0  38.4550    1 6926.992
#> 343 Periploca_angustifolia  Teucrium_lusitanicum   0   5.2825    1 6926.992
#> 344 Periploca_angustifolia    Phagnalon_saxatile   0  33.9250    1 6926.992
#> 345 Periploca_angustifolia    Asparagus_horridus   0   1.5725    1 6926.992
#> 346     Phagnalon_saxatile Maytenus_senegalensis   7 116.4500  528 6926.992
#> 347     Phagnalon_saxatile Piptatherum_miliaceum   4  15.4830  528 6926.992
#> 348     Phagnalon_saxatile        Ziziphus_lotus  40 465.1950  528 6926.992
#> 349     Phagnalon_saxatile         Ononis_natrix   3 244.9650  528 6926.992
#> 350     Phagnalon_saxatile    Lycium_intrincatum  42 161.8000  528 6926.992
#> 351     Phagnalon_saxatile  Artemisia_campestris   0   0.1100  528 6926.992
#> 352     Phagnalon_saxatile       Asparagus_albus   4  27.7250  528 6926.992
#> 353     Phagnalon_saxatile   Whitania_frutescens  10  56.8150  528 6926.992
#> 354     Phagnalon_saxatile     Thymelaea_hirsuta 138 140.9300  528 6926.992
#> 355     Phagnalon_saxatile        Lygeum_spartum  41 643.7150  528 6926.992
#> 356     Phagnalon_saxatile  Helichrysum_stoechas   4  29.0700  528 6926.992
#> 358     Phagnalon_saxatile    Phagnalon_saxatile   3  33.9250  528 6926.992
#> 359     Phagnalon_saxatile       Thymus_hyemalis 133 522.5800  528 6926.992
#> 360     Phagnalon_saxatile   Launaea_arborescens 168 473.8650  528 6926.992
#> 362     Phagnalon_saxatile      Launaea_lanifera   0   1.1600  528 6926.992
#> 363     Phagnalon_saxatile       Teucrium_polium   7  17.0450  528 6926.992
#> 364     Phagnalon_saxatile       Ballota_hirsuta  14  38.4550  528 6926.992
#> 365     Phagnalon_saxatile     Hyparrhenia_hirta   8  13.2550  528 6926.992
#> 366     Phagnalon_saxatile     Stipa_tenacissima   0   0.6000  528 6926.992
#> 367     Phagnalon_saxatile  Teucrium_lusitanicum   0   5.2825  528 6926.992
#> 368     Phagnalon_saxatile Salsola_oppositifolia  46  61.6100  528 6926.992
#> 369  Piptatherum_miliaceum    Lycium_intrincatum   0 161.8000   19 6926.992
#> 370  Piptatherum_miliaceum         Ononis_natrix   0 244.9650   19 6926.992
#> 371  Piptatherum_miliaceum    Phagnalon_saxatile   0  33.9250   19 6926.992
#> 372  Piptatherum_miliaceum Maytenus_senegalensis   0 116.4500   19 6926.992
#> 373  Piptatherum_miliaceum        Lygeum_spartum   0 643.7150   19 6926.992
#> 374  Piptatherum_miliaceum Piptatherum_miliaceum   0  15.4830   19 6926.992
#> 375  Piptatherum_miliaceum  Artemisia_barrelieri   0   1.0000   19 6926.992
#> 376  Piptatherum_miliaceum  Helichrysum_stoechas   0  29.0700   19 6926.992
#> 377  Piptatherum_miliaceum       Teucrium_polium   2  17.0450   19 6926.992
#> 378  Piptatherum_miliaceum Salsola_oppositifolia   0  61.6100   19 6926.992
#> 379  Piptatherum_miliaceum  Artemisia_campestris   0   0.1100   19 6926.992
#> 380  Piptatherum_miliaceum       Asparagus_albus   0  27.7250   19 6926.992
#> 381  Piptatherum_miliaceum   Whitania_frutescens   0  56.8150   19 6926.992
#> 382  Piptatherum_miliaceum      Launaea_lanifera   0   1.1600   19 6926.992
#> 383  Piptatherum_miliaceum        Ziziphus_lotus   0 465.1950   19 6926.992
#> 384  Piptatherum_miliaceum   Launaea_arborescens   2 473.8650   19 6926.992
#> 385  Piptatherum_miliaceum    Asparagus_horridus   0   1.5725   19 6926.992
#> 386  Piptatherum_miliaceum     Thymelaea_hirsuta   0 140.9300   19 6926.992
#> 387  Piptatherum_miliaceum     Stipa_tenacissima   0   0.6000   19 6926.992
#> 388  Piptatherum_miliaceum  Teucrium_lusitanicum   0   5.2825   19 6926.992
#> 389  Piptatherum_miliaceum     Hyparrhenia_hirta   0  13.2550   19 6926.992
#> 390  Piptatherum_miliaceum       Thymus_hyemalis   4 522.5800   19 6926.992
#> 391  Piptatherum_miliaceum       Ballota_hirsuta   0  38.4550   19 6926.992
#> 392  Salsola_oppositifolia Piptatherum_miliaceum   0  15.4830   60 6926.992
#> 393  Salsola_oppositifolia  Artemisia_barrelieri   0   1.0000   60 6926.992
#> 394  Salsola_oppositifolia     Stipa_tenacissima   0   0.6000   60 6926.992
#> 395  Salsola_oppositifolia         Ononis_natrix   3 244.9650   60 6926.992
#> 396  Salsola_oppositifolia Salsola_oppositifolia   0  61.6100   60 6926.992
#> 397  Salsola_oppositifolia Maytenus_senegalensis   0 116.4500   60 6926.992
#> 398  Salsola_oppositifolia    Asparagus_horridus   0   1.5725   60 6926.992
#> 399  Salsola_oppositifolia        Lygeum_spartum  17 643.7150   60 6926.992
#> 400  Salsola_oppositifolia  Artemisia_campestris   0   0.1100   60 6926.992
#> 401  Salsola_oppositifolia       Asparagus_albus   0  27.7250   60 6926.992
#> 402  Salsola_oppositifolia       Teucrium_polium   3  17.0450   60 6926.992
#> 403  Salsola_oppositifolia  Teucrium_lusitanicum   0   5.2825   60 6926.992
#> 404  Salsola_oppositifolia    Phagnalon_saxatile   5  33.9250   60 6926.992
#> 405  Salsola_oppositifolia    Lycium_intrincatum  12 161.8000   60 6926.992
#> 406  Salsola_oppositifolia  Helichrysum_stoechas   7  29.0700   60 6926.992
#> 407  Salsola_oppositifolia     Thymelaea_hirsuta  11 140.9300   60 6926.992
#> 408  Salsola_oppositifolia      Launaea_lanifera   0   1.1600   60 6926.992
#> 409  Salsola_oppositifolia       Ballota_hirsuta   5  38.4550   60 6926.992
#> 410  Salsola_oppositifolia   Launaea_arborescens   8 473.8650   60 6926.992
#> 411  Salsola_oppositifolia   Whitania_frutescens   0  56.8150   60 6926.992
#> 412  Salsola_oppositifolia       Thymus_hyemalis  27 522.5800   60 6926.992
#> 413  Salsola_oppositifolia        Ziziphus_lotus  14 465.1950   60 6926.992
#> 414  Salsola_oppositifolia     Hyparrhenia_hirta   0  13.2550   60 6926.992
#> 415      Stipa_tenacissima Piptatherum_miliaceum   0  15.4830    1 6926.992
#> 416      Stipa_tenacissima  Artemisia_campestris   0   0.1100    1 6926.992
#> 417      Stipa_tenacissima  Artemisia_barrelieri   0   1.0000    1 6926.992
#> 418      Stipa_tenacissima Salsola_oppositifolia   0  61.6100    1 6926.992
#> 419      Stipa_tenacissima     Stipa_tenacissima   0   0.6000    1 6926.992
#> 420      Stipa_tenacissima       Asparagus_albus   0  27.7250    1 6926.992
#> 421      Stipa_tenacissima    Asparagus_horridus   0   1.5725    1 6926.992
#> 422      Stipa_tenacissima    Phagnalon_saxatile   0  33.9250    1 6926.992
#> 423      Stipa_tenacissima       Teucrium_polium   0  17.0450    1 6926.992
#> 424      Stipa_tenacissima  Teucrium_lusitanicum   0   5.2825    1 6926.992
#> 425      Stipa_tenacissima  Helichrysum_stoechas   0  29.0700    1 6926.992
#> 426      Stipa_tenacissima     Hyparrhenia_hirta   0  13.2550    1 6926.992
#> 427      Stipa_tenacissima Maytenus_senegalensis   0 116.4500    1 6926.992
#> 428      Stipa_tenacissima       Ballota_hirsuta   0  38.4550    1 6926.992
#> 429      Stipa_tenacissima        Lygeum_spartum   0 643.7150    1 6926.992
#> 430      Stipa_tenacissima   Launaea_arborescens   0 473.8650    1 6926.992
#> 431      Stipa_tenacissima    Lycium_intrincatum   0 161.8000    1 6926.992
#> 432      Stipa_tenacissima     Thymelaea_hirsuta   0 140.9300    1 6926.992
#> 433      Stipa_tenacissima       Thymus_hyemalis   0 522.5800    1 6926.992
#> 434      Stipa_tenacissima        Ziziphus_lotus   0 465.1950    1 6926.992
#> 435      Stipa_tenacissima      Launaea_lanifera   0   1.1600    1 6926.992
#> 436      Stipa_tenacissima         Ononis_natrix   0 244.9650    1 6926.992
#> 437      Stipa_tenacissima   Whitania_frutescens   0  56.8150    1 6926.992
#> 438     Teucrium_charidemi Piptatherum_miliaceum   0  15.4830   14 6926.992
#> 439     Teucrium_charidemi  Artemisia_barrelieri   0   1.0000   14 6926.992
#> 440     Teucrium_charidemi       Asparagus_albus   0  27.7250   14 6926.992
#> 441     Teucrium_charidemi       Ballota_hirsuta   0  38.4550   14 6926.992
#> 442     Teucrium_charidemi Salsola_oppositifolia   1  61.6100   14 6926.992
#> 443     Teucrium_charidemi  Artemisia_campestris   0   0.1100   14 6926.992
#> 444     Teucrium_charidemi  Teucrium_lusitanicum   0   5.2825   14 6926.992
#> 445     Teucrium_charidemi    Asparagus_horridus   0   1.5725   14 6926.992
#> 446     Teucrium_charidemi    Phagnalon_saxatile   1  33.9250   14 6926.992
#> 447     Teucrium_charidemi     Stipa_tenacissima   0   0.6000   14 6926.992
#> 448     Teucrium_charidemi Maytenus_senegalensis   0 116.4500   14 6926.992
#> 449     Teucrium_charidemi        Lygeum_spartum   1 643.7150   14 6926.992
#> 450     Teucrium_charidemi  Helichrysum_stoechas   0  29.0700   14 6926.992
#> 451     Teucrium_charidemi     Hyparrhenia_hirta   0  13.2550   14 6926.992
#> 452     Teucrium_charidemi     Thymelaea_hirsuta   1 140.9300   14 6926.992
#> 453     Teucrium_charidemi       Thymus_hyemalis   1 522.5800   14 6926.992
#> 454     Teucrium_charidemi         Ononis_natrix   0 244.9650   14 6926.992
#> 455     Teucrium_charidemi       Teucrium_polium   0  17.0450   14 6926.992
#> 456     Teucrium_charidemi   Whitania_frutescens   0  56.8150   14 6926.992
#> 457     Teucrium_charidemi        Ziziphus_lotus   3 465.1950   14 6926.992
#> 458     Teucrium_charidemi   Launaea_arborescens   0 473.8650   14 6926.992
#> 459     Teucrium_charidemi    Lycium_intrincatum   0 161.8000   14 6926.992
#> 460     Teucrium_charidemi      Launaea_lanifera   0   1.1600   14 6926.992
#> 461   Teucrium_lusitanicum Piptatherum_miliaceum   0  15.4830   16 6926.992
#> 462   Teucrium_lusitanicum  Artemisia_campestris   0   0.1100   16 6926.992
#> 463   Teucrium_lusitanicum       Asparagus_albus   0  27.7250   16 6926.992
#> 464   Teucrium_lusitanicum Salsola_oppositifolia   0  61.6100   16 6926.992
#> 465   Teucrium_lusitanicum     Stipa_tenacissima   0   0.6000   16 6926.992
#> 466   Teucrium_lusitanicum  Teucrium_lusitanicum   0   5.2825   16 6926.992
#> 467   Teucrium_lusitanicum  Artemisia_barrelieri   0   1.0000   16 6926.992
#> 468   Teucrium_lusitanicum       Teucrium_polium   0  17.0450   16 6926.992
#> 469   Teucrium_lusitanicum    Phagnalon_saxatile   0  33.9250   16 6926.992
#> 470   Teucrium_lusitanicum  Helichrysum_stoechas   0  29.0700   16 6926.992
#> 471   Teucrium_lusitanicum    Asparagus_horridus   0   1.5725   16 6926.992
#> 472   Teucrium_lusitanicum        Lygeum_spartum   6 643.7150   16 6926.992
#> 473   Teucrium_lusitanicum       Ballota_hirsuta   0  38.4550   16 6926.992
#> 474   Teucrium_lusitanicum Maytenus_senegalensis   0 116.4500   16 6926.992
#> 475   Teucrium_lusitanicum     Hyparrhenia_hirta   5  13.2550   16 6926.992
#> 476   Teucrium_lusitanicum   Launaea_arborescens   4 473.8650   16 6926.992
#> 477   Teucrium_lusitanicum     Thymelaea_hirsuta   5 140.9300   16 6926.992
#> 478   Teucrium_lusitanicum       Thymus_hyemalis   0 522.5800   16 6926.992
#> 479   Teucrium_lusitanicum         Ononis_natrix   0 244.9650   16 6926.992
#> 480   Teucrium_lusitanicum        Ziziphus_lotus   7 465.1950   16 6926.992
#> 481   Teucrium_lusitanicum      Launaea_lanifera   0   1.1600   16 6926.992
#> 482   Teucrium_lusitanicum    Lycium_intrincatum   0 161.8000   16 6926.992
#> 483   Teucrium_lusitanicum   Whitania_frutescens   0  56.8150   16 6926.992
#> 484        Teucrium_polium Piptatherum_miliaceum   4  15.4830  308 6926.992
#> 485        Teucrium_polium  Artemisia_campestris   0   0.1100  308 6926.992
#> 486        Teucrium_polium       Teucrium_polium   0  17.0450  308 6926.992
#> 487        Teucrium_polium       Ballota_hirsuta   1  38.4550  308 6926.992
#> 488        Teucrium_polium Salsola_oppositifolia  17  61.6100  308 6926.992
#> 489        Teucrium_polium     Stipa_tenacissima   0   0.6000  308 6926.992
#> 491        Teucrium_polium     Thymelaea_hirsuta  54 140.9300  308 6926.992
#> 492        Teucrium_polium  Helichrysum_stoechas  12  29.0700  308 6926.992
#> 493        Teucrium_polium    Phagnalon_saxatile   8  33.9250  308 6926.992
#> 494        Teucrium_polium       Asparagus_albus   0  27.7250  308 6926.992
#> 495        Teucrium_polium    Asparagus_horridus   0   1.5725  308 6926.992
#> 496        Teucrium_polium       Thymus_hyemalis  87 522.5800  308 6926.992
#> 497        Teucrium_polium     Hyparrhenia_hirta   2  13.2550  308 6926.992
#> 498        Teucrium_polium  Teucrium_lusitanicum   0   5.2825  308 6926.992
#> 499        Teucrium_polium         Ononis_natrix   4 244.9650  308 6926.992
#> 500        Teucrium_polium        Lygeum_spartum  17 643.7150  308 6926.992
#> 501        Teucrium_polium   Launaea_arborescens  14 473.8650  308 6926.992
#> 502        Teucrium_polium      Launaea_lanifera   0   1.1600  308 6926.992
#> 503        Teucrium_polium        Ziziphus_lotus   9 465.1950  308 6926.992
#> 504        Teucrium_polium    Lycium_intrincatum   7 161.8000  308 6926.992
#> 505        Teucrium_polium   Whitania_frutescens   3  56.8150  308 6926.992
#> 506        Teucrium_polium Maytenus_senegalensis   0 116.4500  308 6926.992
#> 507      Thymelaea_hirsuta Salsola_oppositifolia   5  61.6100  170 6926.992
#> 508      Thymelaea_hirsuta Piptatherum_miliaceum   0  15.4830  170 6926.992
#> 509      Thymelaea_hirsuta  Teucrium_lusitanicum   1   5.2825  170 6926.992
#> 510      Thymelaea_hirsuta  Artemisia_barrelieri   0   1.0000  170 6926.992
#> 511      Thymelaea_hirsuta     Stipa_tenacissima   0   0.6000  170 6926.992
#> 512      Thymelaea_hirsuta  Helichrysum_stoechas  14  29.0700  170 6926.992
#> 513      Thymelaea_hirsuta       Asparagus_albus   2  27.7250  170 6926.992
#> 514      Thymelaea_hirsuta       Teucrium_polium  12  17.0450  170 6926.992
#> 515      Thymelaea_hirsuta       Ballota_hirsuta   3  38.4550  170 6926.992
#> 516      Thymelaea_hirsuta       Thymus_hyemalis  65 522.5800  170 6926.992
#> 517      Thymelaea_hirsuta    Phagnalon_saxatile   9  33.9250  170 6926.992
#> 518      Thymelaea_hirsuta    Asparagus_horridus   0   1.5725  170 6926.992
#> 519      Thymelaea_hirsuta  Artemisia_campestris   0   0.1100  170 6926.992
#> 520      Thymelaea_hirsuta        Ziziphus_lotus   3 465.1950  170 6926.992
#> 521      Thymelaea_hirsuta     Hyparrhenia_hirta   2  13.2550  170 6926.992
#> 522      Thymelaea_hirsuta        Lygeum_spartum  13 643.7150  170 6926.992
#> 523      Thymelaea_hirsuta     Thymelaea_hirsuta  19 140.9300  170 6926.992
#> 524      Thymelaea_hirsuta    Lycium_intrincatum   4 161.8000  170 6926.992
#> 525      Thymelaea_hirsuta   Launaea_arborescens  20 473.8650  170 6926.992
#> 526      Thymelaea_hirsuta         Ononis_natrix   1 244.9650  170 6926.992
#> 527      Thymelaea_hirsuta   Whitania_frutescens   2  56.8150  170 6926.992
#> 528      Thymelaea_hirsuta      Launaea_lanifera   0   1.1600  170 6926.992
#> 529      Thymelaea_hirsuta Maytenus_senegalensis   0 116.4500  170 6926.992
#> 530        Thymus_hyemalis       Teucrium_polium   3  17.0450 1030 6926.992
#> 531        Thymus_hyemalis       Ballota_hirsuta   0  38.4550 1030 6926.992
#> 532        Thymus_hyemalis Piptatherum_miliaceum   0  15.4830 1030 6926.992
#> 533        Thymus_hyemalis     Thymelaea_hirsuta  34 140.9300 1030 6926.992
#> 534        Thymus_hyemalis  Helichrysum_stoechas   4  29.0700 1030 6926.992
#> 535        Thymus_hyemalis     Stipa_tenacissima   0   0.6000 1030 6926.992
#> 536        Thymus_hyemalis Salsola_oppositifolia  14  61.6100 1030 6926.992
#> 537        Thymus_hyemalis  Artemisia_campestris   0   0.1100 1030 6926.992
#> 538        Thymus_hyemalis  Artemisia_barrelieri   1   1.0000 1030 6926.992
#> 539        Thymus_hyemalis     Hyparrhenia_hirta   1  13.2550 1030 6926.992
#> 540        Thymus_hyemalis  Teucrium_lusitanicum   1   5.2825 1030 6926.992
#> 541        Thymus_hyemalis       Asparagus_albus   0  27.7250 1030 6926.992
#> 542        Thymus_hyemalis       Thymus_hyemalis   9 522.5800 1030 6926.992
#> 543        Thymus_hyemalis   Launaea_arborescens  14 473.8650 1030 6926.992
#> 544        Thymus_hyemalis    Phagnalon_saxatile   4  33.9250 1030 6926.992
#> 545        Thymus_hyemalis    Lycium_intrincatum   0 161.8000 1030 6926.992
#> 546        Thymus_hyemalis   Whitania_frutescens   5  56.8150 1030 6926.992
#> 547        Thymus_hyemalis        Ziziphus_lotus   9 465.1950 1030 6926.992
#> 548        Thymus_hyemalis      Launaea_lanifera   0   1.1600 1030 6926.992
#> 549        Thymus_hyemalis         Ononis_natrix   0 244.9650 1030 6926.992
#> 550        Thymus_hyemalis    Asparagus_horridus   1   1.5725 1030 6926.992
#> 551        Thymus_hyemalis Maytenus_senegalensis   2 116.4500 1030 6926.992
#> 552        Thymus_hyemalis        Lygeum_spartum  12 643.7150 1030 6926.992
#> 553    Whitania_frutescens       Teucrium_polium   0  17.0450    5 6926.992
#> 554    Whitania_frutescens       Ballota_hirsuta   0  38.4550    5 6926.992
#> 555    Whitania_frutescens Piptatherum_miliaceum   0  15.4830    5 6926.992
#> 556    Whitania_frutescens Salsola_oppositifolia   0  61.6100    5 6926.992
#> 557    Whitania_frutescens  Artemisia_barrelieri   0   1.0000    5 6926.992
#> 558    Whitania_frutescens     Stipa_tenacissima   0   0.6000    5 6926.992
#> 559    Whitania_frutescens  Teucrium_lusitanicum   0   5.2825    5 6926.992
#> 560    Whitania_frutescens    Phagnalon_saxatile   0  33.9250    5 6926.992
#> 561    Whitania_frutescens     Thymelaea_hirsuta   1 140.9300    5 6926.992
#> 562    Whitania_frutescens  Helichrysum_stoechas   0  29.0700    5 6926.992
#> 563    Whitania_frutescens        Lygeum_spartum   0 643.7150    5 6926.992
#> 564    Whitania_frutescens    Lycium_intrincatum   0 161.8000    5 6926.992
#> 565    Whitania_frutescens       Asparagus_albus   3  27.7250    5 6926.992
#> 566    Whitania_frutescens  Artemisia_campestris   0   0.1100    5 6926.992
#> 567    Whitania_frutescens     Hyparrhenia_hirta   1  13.2550    5 6926.992
#> 568    Whitania_frutescens Maytenus_senegalensis   1 116.4500    5 6926.992
#> 569    Whitania_frutescens   Whitania_frutescens   1  56.8150    5 6926.992
#> 570    Whitania_frutescens       Thymus_hyemalis   0 522.5800    5 6926.992
#> 571    Whitania_frutescens   Launaea_arborescens  36 473.8650    5 6926.992
#> 572    Whitania_frutescens         Ononis_natrix   0 244.9650    5 6926.992
#> 573    Whitania_frutescens    Asparagus_horridus   0   1.5725    5 6926.992
#> 574    Whitania_frutescens      Launaea_lanifera   0   1.1600    5 6926.992
#> 575    Whitania_frutescens        Ziziphus_lotus   2 465.1950    5 6926.992
#> 576         Ziziphus_lotus       Ballota_hirsuta   0  38.4550    4 6926.992
#> 577         Ziziphus_lotus  Helichrysum_stoechas   0  29.0700    4 6926.992
#> 578         Ziziphus_lotus Salsola_oppositifolia   0  61.6100    4 6926.992
#> 579         Ziziphus_lotus     Stipa_tenacissima   0   0.6000    4 6926.992
#> 580         Ziziphus_lotus     Thymelaea_hirsuta   0 140.9300    4 6926.992
#> 581         Ziziphus_lotus        Lygeum_spartum   0 643.7150    4 6926.992
#> 582         Ziziphus_lotus    Phagnalon_saxatile   0  33.9250    4 6926.992
#> 583         Ziziphus_lotus       Asparagus_albus   0  27.7250    4 6926.992
#> 584         Ziziphus_lotus   Whitania_frutescens   0  56.8150    4 6926.992
#> 585         Ziziphus_lotus     Hyparrhenia_hirta   0  13.2550    4 6926.992
#> 586         Ziziphus_lotus       Thymus_hyemalis   0 522.5800    4 6926.992
#> 587         Ziziphus_lotus       Teucrium_polium   0  17.0450    4 6926.992
#> 588         Ziziphus_lotus  Artemisia_campestris   0   0.1100    4 6926.992
#> 589         Ziziphus_lotus  Teucrium_lusitanicum   0   5.2825    4 6926.992
#> 590         Ziziphus_lotus  Artemisia_barrelieri   0   1.0000    4 6926.992
#> 591         Ziziphus_lotus Piptatherum_miliaceum   0  15.4830    4 6926.992
#> 592         Ziziphus_lotus   Launaea_arborescens   0 473.8650    4 6926.992
#> 593         Ziziphus_lotus         Ononis_natrix   0 244.9650    4 6926.992
#> 594         Ziziphus_lotus Maytenus_senegalensis   0 116.4500    4 6926.992
#> 595         Ziziphus_lotus    Lycium_intrincatum   0 161.8000    4 6926.992
#> 596         Ziziphus_lotus    Asparagus_horridus   0   1.5725    4 6926.992
#> 597         Ziziphus_lotus      Launaea_lanifera   0   1.1600    4 6926.992
#> 598         Ziziphus_lotus        Ziziphus_lotus   0 465.1950    4 6926.992
#>             Dcr          Dro            Ns        NintC       NintA
#> 2   0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 3   0.035478606 0.0077955915  0.7747813884  0.780273458  1.56054692
#> 4   0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 5   0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 6   0.035730098 0.0077955915  0.7818200354  0.781820035  1.56364007
#> 7   0.032462263 0.0077955915  0.6903611389  0.759856804  1.51971361
#> 8   0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 9   0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 10  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 11  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 12  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 13  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 14  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 15  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 16  0.003827165 0.0077955915 -0.1110667621 -0.509060318 -0.67467193
#> 17  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 18  0.004082216 0.0077955915 -0.1039285055 -0.476343030 -0.64530129
#> 19  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 20  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 21  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 22  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 23  0.000000000 0.0077955915 -0.2181799646 -1.000000000 -1.00000000
#> 24  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 25  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 26  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 27  0.004660448 0.0001443628  0.9690238322  0.969023832  1.93804766
#> 28  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 29  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 30  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 31  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 32  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 33  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 34  0.003827165 0.0001443628  0.7902250518  0.962279443  1.92455889
#> 35  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 36  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 37  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 38  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 39  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 40  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 41  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 42  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 43  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 44  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 45  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 46  0.000000000 0.0001443628 -0.0309761678 -1.000000000 -1.00000000
#> 47  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 48  0.008587377 0.0010105396  0.7180795651  0.882322659  1.76464532
#> 49  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 50  0.006213930 0.0010105396  0.4931409220  0.837375119  1.67475024
#> 51  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 52  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 53  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 54  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 55  0.006180470 0.0010105396  0.4899697831  0.836494686  1.67298937
#> 56  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 57  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 58  0.005740748 0.0010105396  0.4482960206  0.823970732  1.64794146
#> 59  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 60  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 61  0.008598545 0.0010105396  0.7191380032  0.882475503  1.76495101
#> 62  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 63  0.010551528 0.0010105396  0.9042281267  0.904228127  1.80845625
#> 64  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 65  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 66  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 67  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 68  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 69  0.000000000 0.0010105396 -0.0957718733 -1.000000000 -1.00000000
#> 70  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 71  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 72  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 73  0.010874378 0.0025985305  0.1175480637  0.761040991  1.52208198
#> 74  0.032462263 0.0025985305  0.4241769851  0.919952268  1.83990454
#> 75  0.058953574 0.0025985305  0.8004529499  0.955922426  1.91184485
#> 76  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 77  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 78  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 79  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 80  0.070403943 0.0025985305  0.9630911224  0.963091122  1.92618224
#> 81  0.035478606 0.0025985305  0.4670203780  0.926757819  1.85351564
#> 82  0.012360939 0.0025985305  0.1386628158  0.789778882  1.57955776
#> 83  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 84  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 85  0.052008842 0.0025985305  0.7018117049  0.950036755  1.90007351
#> 86  0.003827165 0.0025985305  0.0174512204  0.321029965  0.64205993
#> 87  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 88  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 89  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 90  0.068799450 0.0025985305  0.9403013047  0.962230359  1.92446072
#> 91  0.000000000 0.0025985305 -0.0369088776 -1.000000000 -1.00000000
#> 92  0.014772140 0.0025985305  0.1729109023  0.824092478  1.64818496
#> 93  0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 94  0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 95  0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 96  0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 97  0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 98  0.064924525 0.0015879909  0.0995967003  0.975540971  1.95108194
#> 99  0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 100 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 101 0.103199174 0.0015879909  0.1597835861  0.984612369  1.96922474
#> 102 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 103 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 104 0.035478606 0.0015879909  0.0532929929  0.955240890  1.91048178
#> 105 0.001913583 0.0015879909  0.0005119930  0.170147735  0.34029547
#> 106 0.123609394 0.0015879909  0.1918786569  0.987153154  1.97430631
#> 107 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 108 0.635930048 0.0015879909  0.9975028844  0.997502884  1.99500577
#> 109 0.068788358 0.0015879909  0.1056725766  0.976914831  1.95382966
#> 110 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 111 0.010551528 0.0015879909  0.0140951628  0.849501342  1.69900268
#> 112 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 113 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 114 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 115 0.000000000 0.0015879909 -0.0024971156 -1.000000000 -1.00000000
#> 116 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 117 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 118 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 120 0.006213930 0.1149127933 -0.2092792801 -0.945924819 -0.97221106
#> 121 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 122 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 123 0.519396202 0.1149127933  0.7787569626  0.778756963  1.55751393
#> 124 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 125 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 126 0.352009387 0.1149127933  0.4564850354  0.673551906  1.34710381
#> 127 0.008164432 0.1149127933 -0.2055239551 -0.928951064 -0.96316706
#> 128 0.411551834 0.1149127933  0.5711228535  0.720781725  1.44156345
#> 129 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 130 0.105067985 0.1149127933 -0.0189543322 -0.085671994 -0.15782298
#> 131 0.104017683 0.1149127933 -0.0209764921 -0.094811987 -0.17320232
#> 132 0.025795634 0.1149127933 -0.1715783806 -0.775519911 -0.87356938
#> 133 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 134 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 135 0.073860699 0.1149127933 -0.0790381109 -0.357245642 -0.52642739
#> 136 0.164568104 0.1149127933  0.0956019911  0.301731075  0.60346215
#> 137 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 138 0.000000000 0.1149127933 -0.2212430374 -1.000000000 -1.00000000
#> 139 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 140 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 141 0.016231131 0.0049083354  0.0643323521  0.697597456  1.39519491
#> 142 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 143 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 144 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 145 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 146 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 147 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 148 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 149 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 150 0.008164432 0.0049083354  0.0185000536  0.398814810  0.79762962
#> 151 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 152 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 153 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 154 0.014191443 0.0049083354  0.0527435206  0.654134147  1.30826829
#> 155 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 156 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 157 0.008441223 0.0049083354  0.0200726881  0.418527912  0.83705582
#> 158 0.176004693 0.0049083354  0.9721124744  0.972112474  1.94422495
#> 159 0.005740748 0.0049083354  0.0047294900  0.145000697  0.29000139
#> 160 0.008598545 0.0049083354  0.0209665392  0.429166729  0.85833346
#> 161 0.000000000 0.0049083354 -0.0278875256 -1.000000000 -1.00000000
#> 162 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 163 0.009320895 0.0128482897 -0.0109229301 -0.274541956 -0.43080882
#> 164 0.322934832 0.0128482897  0.9602139861  0.960213986  1.92042797
#> 165 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 166 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 167 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 168 0.088430361 0.0128482897  0.2340474423  0.854707257  1.70941451
#> 169 0.146080182 0.0128482897  0.4125658770  0.912046319  1.82409264
#> 170 0.077562101 0.0128482897  0.2003927872  0.834348353  1.66869671
#> 171 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 172 0.008587377 0.0128482897 -0.0131943437 -0.331632711 -0.49808436
#> 173 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 174 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 175 0.034399725 0.0128482897  0.0667361739  0.626500218  1.25300044
#> 176 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 177 0.018541409 0.0128482897  0.0176293137  0.307048909  0.61409782
#> 178 0.049670049 0.0128482897  0.1140222597  0.741327219  1.48265444
#> 179 0.007654330 0.0128482897 -0.0160836143 -0.404252970 -0.57575519
#> 180 0.075443229 0.0128482897  0.1938314889  0.829695920  1.65939184
#> 181 0.018992751 0.0128482897  0.0190269392  0.323516133  0.64703227
#> 182 0.008598545 0.0128482897 -0.0131597604 -0.330763480 -0.49710333
#> 183 0.140807885 0.0128482897  0.3962396835  0.908753053  1.81750611
#> 184 0.000000000 0.0128482897 -0.0397860139 -1.000000000 -1.00000000
#> 185 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 186 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 187 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 188 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 189 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 190 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 191 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 192 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 193 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 194 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 195 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 196 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 197 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 198 0.001913583 0.0008661768  0.1476108959  0.547353310  1.09470662
#> 199 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 200 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 201 0.007095721 0.0008661768  0.8779296988  0.877929699  1.75585940
#> 202 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 203 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 204 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 205 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 206 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 207 0.000000000 0.0008661768 -0.1220703012 -1.000000000 -1.00000000
#> 208 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 209 0.072137060 0.0028872561  0.8532960886  0.959975412  1.91995082
#> 210 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 211 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 212 0.075443229 0.0028872561  0.8940346975  0.961729420  1.92345884
#> 213 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 214 0.007095721 0.0028872561  0.0518567078  0.593098996  1.18619799
#> 215 0.081155657 0.0028872561  0.9644232302  0.964423230  1.92884646
#> 216 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 217 0.064586966 0.0028872561  0.7602638295  0.955296614  1.91059323
#> 218 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 219 0.003827165 0.0028872561  0.0115815600  0.245588850  0.49117770
#> 220 0.052802957 0.0028872561  0.6150612659  0.945320181  1.89064036
#> 221 0.012661834 0.0028872561  0.1204423495  0.771971730  1.54394346
#> 222 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 223 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 224 0.027945270 0.0028872561  0.3087648504  0.896681761  1.79336352
#> 225 0.000000000 0.0028872561 -0.0355767698 -1.000000000 -1.00000000
#> 226 0.008164432 0.0028872561  0.0650253570  0.646361653  1.29272331
#> 227 0.008587377 0.0028872561  0.0702368841  0.663779026  1.32755805
#> 228 0.024721879 0.0028872561  0.2690462215  0.883210490  1.76642098
#> 229 0.003106965 0.0028872561  0.0027072534  0.070714965  0.14142993
#> 230 0.029476787 0.0028872561  0.3276362000  0.902049836  1.80409967
#> 231 0.002110306 0.0115490245 -0.2616884781 -0.817274118 -0.89945057
#> 232 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 233 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 234 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 235 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 236 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 237 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 238 0.014191443 0.0115490245  0.0732610420  0.186197992  0.37239598
#> 239 0.016231131 0.0115490245  0.1298114126  0.288464603  0.57692921
#> 240 0.036068530 0.0115490245  0.6798032970  0.679803297  1.35960659
#> 241 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 242 0.004299272 0.0115490245 -0.2009993771 -0.627737185 -0.77130042
#> 243 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 244 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 245 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 246 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 247 0.011481496 0.0115490245 -0.0018722359 -0.005847143 -0.01162631
#> 248 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 249 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 250 0.015534825 0.0115490245  0.1105063255  0.256571972  0.51314394
#> 251 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 252 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 253 0.000000000 0.0115490245 -0.3201967030 -1.000000000 -1.00000000
#> 258 0.002110306 0.0000000000  0.0585082249  1.000000000  2.00000000
#> 265 0.017600986 0.0000000000  0.4879873273  1.000000000  2.00000000
#> 268 0.001553483 0.0000000000  0.0430703029  1.000000000  2.00000000
#> 272 0.036068530 0.0000000000  1.0000000000  1.000000000  2.00000000
#> 277 0.075443229 0.2654831996 -0.3682974631 -0.715826730 -0.83438114
#> 278 0.515995872 0.2654831996  0.4854935591  0.485493559  0.97098712
#> 279 0.070403943 0.2654831996 -0.3780636001 -0.734808294 -0.84713486
#> 280 0.238464542 0.2654831996 -0.0523621593 -0.101771630 -0.18474179
#> 281 0.305116015 0.2654831996  0.0768083963  0.129894248  0.25978850
#> 282 0.013395078 0.2654831996 -0.4885467792 -0.949544535 -0.97411936
#> 283 0.000000000 0.2654831996 -0.5145064409 -1.000000000 -1.00000000
#> 284 0.000000000 0.2654831996 -0.5145064409 -1.000000000 -1.00000000
#> 285 0.469345849 0.2654831996  0.3950858149  0.434354858  0.86870972
#> 286 0.025762130 0.2654831996 -0.4645794336 -0.902961356 -0.94900651
#> 287 0.055624227 0.2654831996 -0.4067066881 -0.790479294 -0.88298066
#> 288 0.019346726 0.2654831996 -0.4770124867 -0.927126366 -0.96218534
#> 289 0.146959770 0.2654831996 -0.2296984071 -0.446444182 -0.61729887
#> 290 0.147383935 0.2654831996 -0.2288763746 -0.444846471 -0.61576988
#> 291 0.097386788 0.2654831996 -0.3257708460 -0.633171560 -0.77538891
#> 292 0.052008842 0.2654831996 -0.4137133061 -0.804097428 -0.89141242
#> 293 0.036068530 0.2654831996 -0.4446056293 -0.864140065 -0.92711925
#> 294 0.000000000 0.2654831996 -0.5145064409 -1.000000000 -1.00000000
#> 295 0.000000000 0.2654831996 -0.5145064409 -1.000000000 -1.00000000
#> 296 0.189304307 0.2654831996 -0.1476346946 -0.286944308 -0.44593120
#> 297 0.000000000 0.2654831996 -0.5145064409 -1.000000000 -1.00000000
#> 298 0.015534825 0.2654831996 -0.4843999497 -0.941484715 -0.96986055
#> 299 0.000000000 0.2654831996 -0.5145064409 -1.000000000 -1.00000000
#> 323 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 324 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 325 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 326 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 327 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 328 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 329 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 330 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 331 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 332 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 333 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 334 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 335 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 336 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 337 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 338 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 339 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 340 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 341 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 342 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 343 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 344 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 345 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 346 0.060111636 0.0762235614 -0.0164540120 -0.211377233 -0.34898664
#> 347 0.258347865 0.0762235614  0.1859911461  0.704957650  1.40991530
#> 348 0.085985447 0.0762235614  0.0099691488  0.113529509  0.22705902
#> 349 0.012246647 0.0762235614 -0.0653352643 -0.839332521 -0.91264903
#> 350 0.259579728 0.0762235614  0.1872491635  0.706357804  1.41271561
#> 351 0.000000000 0.0762235614 -0.0778419312 -1.000000000 -1.00000000
#> 352 0.144274121 0.0762235614  0.0694954010  0.471675440  0.94335088
#> 353 0.176009857 0.0762235614  0.1019049462  0.566935836  1.13387167
#> 354 0.979209537 0.0762235614  0.9221580688  0.922158069  1.84431614
#> 355 0.063692783 0.0762235614 -0.0127968301 -0.164395075 -0.28236993
#> 356 0.137598899 0.0762235614  0.0626784519  0.446045268  0.89209054
#> 358 0.088430361 0.0762235614  0.0124659731  0.138038560  0.27607712
#> 359 0.254506487 0.0762235614  0.1820682081  0.700504446  1.40100889
#> 360 0.354531354 0.0762235614  0.2842167913  0.785001917  1.57000383
#> 362 0.000000000 0.0762235614 -0.0778419312 -1.000000000 -1.00000000
#> 363 0.410677618 0.0762235614  0.3415551464  0.814395628  1.62879126
#> 364 0.364061891 0.0762235614  0.2939496792  0.790630210  1.58126042
#> 365 0.603545832 0.0762235614  0.5385183157  0.873707087  1.74741417
#> 366 0.000000000 0.0762235614 -0.0778419312 -1.000000000 -1.00000000
#> 367 0.000000000 0.0762235614 -0.0778419312 -1.000000000 -1.00000000
#> 368 0.746632040 0.0762235614  0.6846425140  0.897910139  1.79582028
#> 369 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 370 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 371 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 372 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 373 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 374 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 375 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 376 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 377 0.117336462 0.0027428933  0.9766236918  0.976623692  1.95324738
#> 378 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 379 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 380 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 381 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 382 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 383 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 384 0.004220611 0.0027428933  0.0125938521  0.350119431  0.70023886
#> 385 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 386 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 387 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 388 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 389 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 390 0.007654330 0.0027428933  0.0418577229  0.641654704  1.28330941
#> 391 0.000000000 0.0027428933 -0.0233763082 -1.000000000 -1.00000000
#> 392 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 393 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 394 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 395 0.012246647 0.0086617683  0.0148874909  0.292723306  0.58544661
#> 396 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 397 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 398 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 399 0.026409203 0.0086617683  0.0737025601  0.672017047  1.34403409
#> 400 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 401 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 402 0.176004693 0.0086617683  0.6949512619  0.950786720  1.90157344
#> 403 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 404 0.147383935 0.0086617683  0.5760933413  0.941229902  1.88245980
#> 405 0.074165637 0.0086617683  0.2720282071  0.883210490  1.76642098
#> 406 0.240798074 0.0086617683  0.9640289135  0.964028913  1.92805783
#> 407 0.078052934 0.0086617683  0.2881715983  0.889026999  1.77805400
#> 408 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 409 0.130022104 0.0086617683  0.5039921358  0.933382340  1.86676468
#> 410 0.016882445 0.0086617683  0.0341392975  0.486936393  0.97387279
#> 411 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 412 0.051666730 0.0086617683  0.1785934641  0.832353078  1.66470616
#> 413 0.030094906 0.0086617683  0.0890087606  0.712184905  1.42436981
#> 414 0.000000000 0.0086617683 -0.0359710865 -1.000000000 -1.00000000
#> 415 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 416 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 417 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 418 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 419 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 420 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 421 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 422 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 423 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 424 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 425 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 426 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 427 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 428 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 429 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 430 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 431 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 432 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 433 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 434 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 435 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 436 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 437 0.000000000 0.0001443628 -1.0000000000 -1.000000000 -1.00000000
#> 438 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 439 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 440 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 441 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 442 0.016231131 0.0020210793  0.4820760151  0.875481306  1.75096261
#> 443 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 444 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 445 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 446 0.029476787 0.0020210793  0.9314348854  0.931434885  1.86286977
#> 447 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 448 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 449 0.001553483 0.0020210793 -0.0158632201 -0.231359930 -0.37577953
#> 450 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 451 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 452 0.007095721 0.0020210793  0.1721572299  0.715169297  1.43033859
#> 453 0.001913583 0.0020210793 -0.0036468245 -0.053187755 -0.10100337
#> 454 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 455 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 456 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 457 0.006448909 0.0020210793  0.1502141071  0.686601342  1.37320268
#> 458 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 459 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 460 0.000000000 0.0020210793 -0.0685651146 -1.000000000 -1.00000000
#> 461 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 462 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 463 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 464 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 465 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 466 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 467 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 468 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 469 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 470 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 471 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 472 0.009320895 0.0023098049  0.0185864002  0.752190657  1.50438131
#> 473 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 474 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 475 0.377216145 0.0023098049  0.9938767072  0.993876707  1.98775341
#> 476 0.008441223 0.0023098049  0.0162543886  0.726366076  1.45273215
#> 477 0.035478606 0.0023098049  0.0879304928  0.934895839  1.86979168
#> 478 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 479 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 480 0.015047453 0.0023098049  0.0337675057  0.846498616  1.69299723
#> 481 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 482 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 483 0.000000000 0.0023098049 -0.0061232928 -1.000000000 -1.00000000
#> 484 0.258347865 0.0444637441  0.5181342837  0.827891962  1.65578392
#> 485 0.000000000 0.0444637441 -0.1077134202 -1.000000000 -1.00000000
#> 486 0.000000000 0.0444637441 -0.1077134202 -1.000000000 -1.00000000
#> 487 0.026004421 0.0444637441 -0.0447177109 -0.415154498 -0.58672675
#> 488 0.275929232 0.0444637441  0.5607251450  0.838858160  1.67771632
#> 489 0.000000000 0.0444637441 -0.1077134202 -1.000000000 -1.00000000
#> 491 0.383168949 0.0444637441  0.8205133591  0.883957862  1.76791572
#> 492 0.412796698 0.0444637441  0.8922865798  0.892286580  1.78457316
#> 493 0.235814296 0.0444637441  0.4635467124  0.811445935  1.62289187
#> 494 0.000000000 0.0444637441 -0.1077134202 -1.000000000 -1.00000000
#> 495 0.000000000 0.0444637441 -0.1077134202 -1.000000000 -1.00000000
#> 496 0.166481687 0.0444637441  0.2955884666  0.732921110  1.46584222
#> 497 0.150886458 0.0444637441  0.2578090242  0.705316536  1.41063307
#> 498 0.000000000 0.0444637441 -0.1077134202 -1.000000000 -1.00000000
#> 499 0.016328863 0.0444637441 -0.0681567488 -0.632760047 -0.77508027
#> 500 0.026409203 0.0444637441 -0.0437371263 -0.406050855 -0.57757634
#> 501 0.029544279 0.0444637441 -0.0361424031 -0.335542248 -0.50248092
#> 502 0.000000000 0.0444637441 -0.1077134202 -1.000000000 -1.00000000
#> 503 0.019346726 0.0444637441 -0.0608459775 -0.564887619 -0.72195295
#> 504 0.043263288 0.0444637441 -0.0029081050 -0.026998539 -0.05257756
#> 505 0.052802957 0.0444637441  0.0202017431  0.157930792  0.31586158
#> 506 0.000000000 0.0444637441 -0.1077134202 -1.000000000 -1.00000000
#> 507 0.081155657 0.0245416770  0.0804154402  0.697597456  1.39519491
#> 508 0.000000000 0.0245416770 -0.0348594070 -1.000000000 -1.00000000
#> 509 0.189304307 0.0245416770  0.2340315853  0.870358591  1.74071718
#> 510 0.000000000 0.0245416770 -0.0348594070 -1.000000000 -1.00000000
#> 511 0.000000000 0.0245416770 -0.0348594070 -1.000000000 -1.00000000
#> 512 0.481596147 0.0245416770  0.6492077871  0.949040961  1.89808192
#> 513 0.072137060 0.0245416770  0.0676052759  0.659791003  1.31958201
#> 514 0.704018774 0.0245416770  0.9651405930  0.965140593  1.93028119
#> 515 0.078013262 0.0245416770  0.0759519309  0.685416604  1.37083321
#> 516 0.124382870 0.0245416770  0.1418160941  0.802692468  1.60538494
#> 517 0.265291083 0.0245416770  0.3419644692  0.907491512  1.81498302
#> 518 0.000000000 0.0245416770 -0.0348594070 -1.000000000 -1.00000000
#> 519 0.000000000 0.0245416770 -0.0348594070 -1.000000000 -1.00000000
#> 520 0.006448909 0.0245416770 -0.0256992698 -0.737226249 -0.84873948
#> 521 0.150886458 0.0245416770  0.1794622326  0.837350036  1.67470007
#> 522 0.020195273 0.0245416770 -0.0061737050 -0.177102984 -0.30091332
#> 523 0.134818704 0.0245416770  0.1566393276  0.817965340  1.63593068
#> 524 0.024721879 0.0245416770  0.0002559618  0.007289167  0.01457833
#> 525 0.042206114 0.0245416770  0.0250908601  0.418527912  0.83705582
#> 526 0.004082216 0.0245416770 -0.0290609596 -0.833661904 -0.90928639
#> 527 0.035201971 0.0245416770  0.0151420598  0.302832312  0.60566462
#> 528 0.000000000 0.0245416770 -0.0348594070 -1.000000000 -1.00000000
#> 529 0.000000000 0.0245416770 -0.0348594070 -1.000000000 -1.00000000
#> 530 0.176004693 0.1486936898  0.0273110036  0.155172019  0.31034404
#> 531 0.000000000 0.1486936898 -0.1486936898 -1.000000000 -1.00000000
#> 532 0.000000000 0.1486936898 -0.1486936898 -1.000000000 -1.00000000
#> 533 0.241254524 0.1486936898  0.0925608337  0.383664656  0.76732931
#> 534 0.137598899 0.1486936898 -0.0110947906 -0.074615074 -0.13886847
#> 535 0.000000000 0.1486936898 -0.1486936898 -1.000000000 -1.00000000
#> 536 0.227235838 0.1486936898  0.0785421485  0.345641555  0.69128311
#> 537 0.000000000 0.1486936898 -0.1486936898 -1.000000000 -1.00000000
#> 538 1.000000000 0.1486936898  0.8513063102  0.851306310  1.70261262
#> 539 0.075443229 0.1486936898 -0.0732504609 -0.492626560 -0.66008012
#> 540 0.189304307 0.1486936898  0.0406106168  0.214525583  0.42905117
#> 541 0.000000000 0.1486936898 -0.1486936898 -1.000000000 -1.00000000
#> 542 0.017222243 0.1486936898 -0.1314714464 -0.884176366 -0.93852824
#> 543 0.029544279 0.1486936898 -0.1191494104 -0.801307779 -0.88969557
#> 544 0.117907148 0.1486936898 -0.0307865417 -0.207046726 -0.34306332
#> 545 0.000000000 0.1486936898 -0.1486936898 -1.000000000 -1.00000000
#> 546 0.088004928 0.1486936898 -0.0606887616 -0.408146180 -0.57969291
#> 547 0.019346726 0.1486936898 -0.1293469643 -0.869888725 -0.93041764
#> 548 0.000000000 0.1486936898 -0.1486936898 -1.000000000 -1.00000000
#> 549 0.000000000 0.1486936898 -0.1486936898 -1.000000000 -1.00000000
#> 550 0.635930048 0.1486936898  0.4872363579  0.766179173  1.53235835
#> 551 0.017174753 0.1486936898 -0.1315189367 -0.884495750 -0.93870814
#> 552 0.018641790 0.1486936898 -0.1300518996 -0.874629581 -0.93312256
#> 553 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 554 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 555 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 556 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 557 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 558 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 559 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 560 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 561 0.007095721 0.0007218140  0.0589055262  0.898274749  1.79654950
#> 562 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 563 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 564 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 565 0.108205591 0.0007218140  0.9933292354  0.993329235  1.98665847
#> 566 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 567 0.075443229 0.0007218140  0.6905504098  0.990432355  1.98086471
#> 568 0.008587377 0.0007218140  0.0726909070  0.915944756  1.83188951
#> 569 0.017600986 0.0007218140  0.1559916778  0.958990136  1.91798027
#> 570 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 571 0.075971004 0.0007218140  0.6954279344  0.990498822  1.98099764
#> 572 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 573 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 574 0.000000000 0.0007218140 -0.0066707646 -1.000000000 -1.00000000
#> 575 0.004299272 0.0007218140  0.0330616773  0.832107862  1.66421572
#> 576 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 577 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 578 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 579 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 580 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 581 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 582 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 583 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 584 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 585 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 586 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 587 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 588 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 589 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 590 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 591 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 592 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 593 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 594 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 595 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 596 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 597 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#> 598 0.000000000 0.0005774512 -1.0000000000 -1.000000000 -1.00000000
#>              RII
#> 2   -1.000000000
#> 3    0.639711797
#> 4   -1.000000000
#> 5   -1.000000000
#> 6    0.641793543
#> 7    0.612716988
#> 8   -1.000000000
#> 9   -1.000000000
#> 10  -1.000000000
#> 11  -1.000000000
#> 12  -1.000000000
#> 13  -1.000000000
#> 14  -1.000000000
#> 15  -1.000000000
#> 16  -0.341435890
#> 17  -1.000000000
#> 18  -0.312631412
#> 19  -1.000000000
#> 20  -1.000000000
#> 21  -1.000000000
#> 22  -1.000000000
#> 23  -1.000000000
#> 24  -1.000000000
#> 25  -1.000000000
#> 26  -1.000000000
#> 27   0.939909052
#> 28  -1.000000000
#> 29  -1.000000000
#> 30  -1.000000000
#> 31  -1.000000000
#> 32  -1.000000000
#> 33  -1.000000000
#> 34   0.927301127
#> 35  -1.000000000
#> 36  -1.000000000
#> 37  -1.000000000
#> 38  -1.000000000
#> 39  -1.000000000
#> 40  -1.000000000
#> 41  -1.000000000
#> 42  -1.000000000
#> 43  -1.000000000
#> 44  -1.000000000
#> 45  -1.000000000
#> 46  -1.000000000
#> 47  -1.000000000
#> 48   0.789425200
#> 49  -1.000000000
#> 50   0.720245311
#> 51  -1.000000000
#> 52  -1.000000000
#> 53  -1.000000000
#> 54  -1.000000000
#> 55   0.718943589
#> 56  -1.000000000
#> 57  -1.000000000
#> 58   0.700637947
#> 59  -1.000000000
#> 60  -1.000000000
#> 61   0.789669941
#> 62  -1.000000000
#> 63   0.825197424
#> 64  -1.000000000
#> 65  -1.000000000
#> 66  -1.000000000
#> 67  -1.000000000
#> 68  -1.000000000
#> 69  -1.000000000
#> 70  -1.000000000
#> 71  -1.000000000
#> 72  -1.000000000
#> 73   0.614258410
#> 74   0.851770010
#> 75   0.915566478
#> 76  -1.000000000
#> 77  -1.000000000
#> 78  -1.000000000
#> 79  -1.000000000
#> 80   0.928809795
#> 81   0.863512296
#> 82   0.652590565
#> 83  -1.000000000
#> 84  -1.000000000
#> 85   0.904828582
#> 86   0.191206489
#> 87  -1.000000000
#> 88  -1.000000000
#> 89  -1.000000000
#> 90   0.927209972
#> 91  -1.000000000
#> 92   0.700814020
#> 93  -1.000000000
#> 94  -1.000000000
#> 95  -1.000000000
#> 96  -1.000000000
#> 97  -1.000000000
#> 98   0.952249863
#> 99  -1.000000000
#> 100 -1.000000000
#> 101  0.969691119
#> 102 -1.000000000
#> 103 -1.000000000
#> 104  0.914316879
#> 105  0.092984411
#> 106  0.974632204
#> 107 -1.000000000
#> 108  0.995018209
#> 109  0.954871462
#> 110 -1.000000000
#> 111  0.738376647
#> 112 -1.000000000
#> 113 -1.000000000
#> 114 -1.000000000
#> 115 -1.000000000
#> 116 -1.000000000
#> 117 -1.000000000
#> 118 -1.000000000
#> 120 -0.897397867
#> 121 -1.000000000
#> 122 -1.000000000
#> 123  0.637675662
#> 124 -1.000000000
#> 125 -1.000000000
#> 126  0.507786101
#> 127 -0.867328311
#> 128  0.563454837
#> 129 -1.000000000
#> 130 -0.044753038
#> 131 -0.049765161
#> 132 -0.633346281
#> 133 -1.000000000
#> 134 -1.000000000
#> 135 -0.217467475
#> 136  0.177669785
#> 137 -1.000000000
#> 138 -1.000000000
#> 139 -1.000000000
#> 140 -1.000000000
#> 141  0.535623537
#> 142 -1.000000000
#> 143 -1.000000000
#> 144 -1.000000000
#> 145 -1.000000000
#> 146 -1.000000000
#> 147 -1.000000000
#> 148 -1.000000000
#> 149 -1.000000000
#> 150  0.249074756
#> 151 -1.000000000
#> 152 -1.000000000
#> 153 -1.000000000
#> 154  0.486032204
#> 155 -1.000000000
#> 156 -1.000000000
#> 157  0.264644514
#> 158  0.945738177
#> 159  0.078167521
#> 160  0.273209600
#> 161 -1.000000000
#> 162 -1.000000000
#> 163 -0.159112508
#> 164  0.923472689
#> 165 -1.000000000
#> 166 -1.000000000
#> 167 -1.000000000
#> 168  0.746278419
#> 169  0.838313556
#> 170  0.715778471
#> 171 -1.000000000
#> 172 -0.198776800
#> 173 -1.000000000
#> 174 -1.000000000
#> 175  0.456134196
#> 176 -1.000000000
#> 177  0.181369037
#> 178  0.588975332
#> 179 -0.253331488
#> 180  0.708957556
#> 181  0.192973007
#> 182 -0.198152554
#> 183  0.832765722
#> 184 -1.000000000
#> 185 -1.000000000
#> 186 -1.000000000
#> 187 -1.000000000
#> 188 -1.000000000
#> 189 -1.000000000
#> 190 -1.000000000
#> 191 -1.000000000
#> 192 -1.000000000
#> 193 -1.000000000
#> 194 -1.000000000
#> 195 -1.000000000
#> 196 -1.000000000
#> 197 -1.000000000
#> 198  0.376797272
#> 199 -1.000000000
#> 200 -1.000000000
#> 201  0.782419513
#> 202 -1.000000000
#> 203 -1.000000000
#> 204 -1.000000000
#> 205 -1.000000000
#> 206 -1.000000000
#> 207 -1.000000000
#> 208 -1.000000000
#> 209  0.923031458
#> 210 -1.000000000
#> 211 -1.000000000
#> 212  0.926280142
#> 213 -1.000000000
#> 214  0.421564129
#> 215  0.931290908
#> 216 -1.000000000
#> 217  0.914418988
#> 218 -1.000000000
#> 219  0.139983635
#> 220  0.896310107
#> 221  0.628627003
#> 222 -1.000000000
#> 223 -1.000000000
#> 224  0.812713621
#> 225 -1.000000000
#> 226  0.477499514
#> 227  0.496758424
#> 228  0.790847767
#> 229  0.036653457
#> 230  0.821576303
#> 231 -0.691008906
#> 232 -1.000000000
#> 233 -1.000000000
#> 234 -1.000000000
#> 235 -1.000000000
#> 236 -1.000000000
#> 237 -1.000000000
#> 238  0.102656184
#> 239  0.168541418
#> 240  0.514925765
#> 241 -1.000000000
#> 242 -0.457446765
#> 243 -1.000000000
#> 244 -1.000000000
#> 245 -1.000000000
#> 246 -1.000000000
#> 247 -0.002932144
#> 248 -1.000000000
#> 249 -1.000000000
#> 250  0.147165222
#> 251 -1.000000000
#> 252 -1.000000000
#> 253 -1.000000000
#> 258  1.000000000
#> 265  1.000000000
#> 268  1.000000000
#> 272  1.000000000
#> 277 -0.557422232
#> 278  0.320562228
#> 279 -0.580788106
#> 280 -0.053614008
#> 281  0.069458237
#> 282 -0.903936023
#> 283 -1.000000000
#> 284 -1.000000000
#> 285  0.277428675
#> 286 -0.823089835
#> 287 -0.653547550
#> 288 -0.864152437
#> 289 -0.287369257
#> 290 -0.286046659
#> 291 -0.463241430
#> 292 -0.672377038
#> 293 -0.760780479
#> 294 -1.000000000
#> 295 -1.000000000
#> 296 -0.167504366
#> 297 -1.000000000
#> 298 -0.889438941
#> 299 -1.000000000
#> 323 -1.000000000
#> 324 -1.000000000
#> 325 -1.000000000
#> 326 -1.000000000
#> 327 -1.000000000
#> 328 -1.000000000
#> 329 -1.000000000
#> 330 -1.000000000
#> 331 -1.000000000
#> 332 -1.000000000
#> 333 -1.000000000
#> 334 -1.000000000
#> 335 -1.000000000
#> 336 -1.000000000
#> 337 -1.000000000
#> 338 -1.000000000
#> 339 -1.000000000
#> 340 -1.000000000
#> 341 -1.000000000
#> 342 -1.000000000
#> 343 -1.000000000
#> 344 -1.000000000
#> 345 -1.000000000
#> 346 -0.118178767
#> 347  0.544351040
#> 348  0.060180909
#> 349 -0.723146410
#> 350  0.546022545
#> 351 -1.000000000
#> 352  0.308622561
#> 353  0.395610922
#> 354  0.855559653
#> 355 -0.089559073
#> 356  0.287038778
#> 358  0.074136100
#> 359  0.539058747
#> 360  0.646093132
#> 362 -1.000000000
#> 363  0.686903361
#> 364  0.653753895
#> 365  0.775737001
#> 366 -1.000000000
#> 367 -1.000000000
#> 368  0.814734052
#> 369 -1.000000000
#> 370 -1.000000000
#> 371 -1.000000000
#> 372 -1.000000000
#> 373 -1.000000000
#> 374 -1.000000000
#> 375 -1.000000000
#> 376 -1.000000000
#> 377  0.954315323
#> 378 -1.000000000
#> 379 -1.000000000
#> 380 -1.000000000
#> 381 -1.000000000
#> 382 -1.000000000
#> 383 -1.000000000
#> 384  0.212208955
#> 385 -1.000000000
#> 386 -1.000000000
#> 387 -1.000000000
#> 388 -1.000000000
#> 389 -1.000000000
#> 390  0.472379671
#> 391 -1.000000000
#> 392 -1.000000000
#> 393 -1.000000000
#> 394 -1.000000000
#> 395  0.171456277
#> 396 -1.000000000
#> 397 -1.000000000
#> 398 -1.000000000
#> 399  0.506043428
#> 400 -1.000000000
#> 401 -1.000000000
#> 402  0.906190131
#> 403 -1.000000000
#> 404  0.888984212
#> 405  0.790847767
#> 406  0.930555810
#> 407  0.800223766
#> 408 -1.000000000
#> 409  0.875086148
#> 410  0.321821496
#> 411 -1.000000000
#> 412  0.712846548
#> 413  0.553017983
#> 414 -1.000000000
#> 415 -1.000000000
#> 416 -1.000000000
#> 417 -1.000000000
#> 418 -1.000000000
#> 419 -1.000000000
#> 420 -1.000000000
#> 421 -1.000000000
#> 422 -1.000000000
#> 423 -1.000000000
#> 424 -1.000000000
#> 425 -1.000000000
#> 426 -1.000000000
#> 427 -1.000000000
#> 428 -1.000000000
#> 429 -1.000000000
#> 430 -1.000000000
#> 431 -1.000000000
#> 432 -1.000000000
#> 433 -1.000000000
#> 434 -1.000000000
#> 435 -1.000000000
#> 436 -1.000000000
#> 437 -1.000000000
#> 438 -1.000000000
#> 439 -1.000000000
#> 440 -1.000000000
#> 441 -1.000000000
#> 442  0.778538685
#> 443 -1.000000000
#> 444 -1.000000000
#> 445 -1.000000000
#> 446  0.871668813
#> 447 -1.000000000
#> 448 -1.000000000
#> 449 -0.130812331
#> 450 -1.000000000
#> 451 -1.000000000
#> 452  0.556625317
#> 453 -0.027320434
#> 454 -1.000000000
#> 455 -1.000000000
#> 456 -1.000000000
#> 457  0.522766897
#> 458 -1.000000000
#> 459 -1.000000000
#> 460 -1.000000000
#> 461 -1.000000000
#> 462 -1.000000000
#> 463 -1.000000000
#> 464 -1.000000000
#> 465 -1.000000000
#> 466 -1.000000000
#> 467 -1.000000000
#> 468 -1.000000000
#> 469 -1.000000000
#> 470 -1.000000000
#> 471 -1.000000000
#> 472  0.602808964
#> 473 -1.000000000
#> 474 -1.000000000
#> 475  0.987827948
#> 476  0.570309932
#> 477  0.877750622
#> 478 -1.000000000
#> 479 -1.000000000
#> 480  0.733851409
#> 481 -1.000000000
#> 482 -1.000000000
#> 483 -1.000000000
#> 484  0.706327348
#> 485 -1.000000000
#> 486 -1.000000000
#> 487 -0.261952662
#> 488  0.722442454
#> 489 -1.000000000
#> 491  0.792047031
#> 492  0.805521143
#> 493  0.682716890
#> 494 -1.000000000
#> 495 -1.000000000
#> 496  0.578433684
#> 497  0.544779133
#> 498 -1.000000000
#> 499 -0.462801022
#> 500 -0.254745176
#> 501 -0.201592529
#> 502 -1.000000000
#> 503 -0.393619083
#> 504 -0.013683993
#> 505  0.085735537
#> 506 -1.000000000
#> 507  0.535623537
#> 508 -1.000000000
#> 509  0.770473342
#> 510 -1.000000000
#> 511 -1.000000000
#> 512  0.903023739
#> 513  0.492304562
#> 514  0.932629676
#> 515  0.521394539
#> 516  0.670414616
#> 517  0.830649393
#> 518 -1.000000000
#> 519 -1.000000000
#> 520 -0.583814993
#> 521  0.720208198
#> 522 -0.097154684
#> 523  0.691997763
#> 524  0.003657915
#> 525  0.264644514
#> 526 -0.714768648
#> 527  0.178433937
#> 528 -1.000000000
#> 529 -1.000000000
#> 530  0.084111917
#> 531 -1.000000000
#> 532 -1.000000000
#> 533  0.237366990
#> 534 -0.038753328
#> 535 -1.000000000
#> 536  0.208927851
#> 537 -1.000000000
#> 538  0.741108198
#> 539 -0.326811224
#> 540  0.120150466
#> 541 -1.000000000
#> 542 -0.792397956
#> 543 -0.668485008
#> 544 -0.115478038
#> 545 -1.000000000
#> 546 -0.256396772
#> 547 -0.769737232
#> 548 -1.000000000
#> 549 -1.000000000
#> 550  0.620980904
#> 551 -0.792911143
#> 552 -0.777192617
#> 553 -1.000000000
#> 554 -1.000000000
#> 555 -1.000000000
#> 556 -1.000000000
#> 557 -1.000000000
#> 558 -1.000000000
#> 559 -1.000000000
#> 560 -1.000000000
#> 561  0.815334629
#> 562 -1.000000000
#> 563 -1.000000000
#> 564 -1.000000000
#> 565  0.986746879
#> 566 -1.000000000
#> 567  0.981046055
#> 568  0.844924428
#> 569  0.921211382
#> 570 -1.000000000
#> 571  0.981176490
#> 572 -1.000000000
#> 573 -1.000000000
#> 574 -1.000000000
#> 575  0.712486911
#> 576 -1.000000000
#> 577 -1.000000000
#> 578 -1.000000000
#> 579 -1.000000000
#> 580 -1.000000000
#> 581 -1.000000000
#> 582 -1.000000000
#> 583 -1.000000000
#> 584 -1.000000000
#> 585 -1.000000000
#> 586 -1.000000000
#> 587 -1.000000000
#> 588 -1.000000000
#> 589 -1.000000000
#> 590 -1.000000000
#> 591 -1.000000000
#> 592 -1.000000000
#> 593 -1.000000000
#> 594 -1.000000000
#> 595 -1.000000000
#> 596 -1.000000000
#> 597 -1.000000000
#> 598 -1.000000000
```
