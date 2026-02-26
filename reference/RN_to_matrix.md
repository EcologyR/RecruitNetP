# Convert field data to matrix format

converts the community data collected in the field, where each row
represents a single canopy-recruit interaction, into a matrix format,
with recruit species as rows and canopy species as columns. For general
recruitment networks, the matrix should be square including all species
observed at the study site. This means that the same set of species
appears in both rows and columns, with "Open" areas treated as an
additional category in both the Canopy and Recruit variables. In
contrast, for recruitment enhancement (i.e. facilitation) or depression
(i.e. competition), the matrix can be non-square. In this case, rows
represent the species whose recruitment is enhanced or suppressed, while
columns represent the canopy species that influence this recruitment.
These two groups may not include the same species.

## Usage

``` r
RN_to_matrix(
  int_data,
  cover_data,
  int_type = c("rec", "fac", "comp"),
  weight = c("Fcr", "Dcr", "Dro", "Ns", "NintC", "NintA", "RII")
)
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

- weight:

  Specifies the metric used to represent interaction strength (i.e., the
  weight) assigned to each pair of species in the matrix. Explanation of
  its options (more mathematical information in the description of the
  function
  [`associndex()`](https://ecologyr.github.io/RecruitNetP/reference/associndex.md)
  :

  - *Fcr*: ***frequency of recruitment***, in number of recruits by
    canopy-recruit pair.

  - *Dcr*: ***density of recruitment***, as number of recruits per unit
    area of canopy species cover.

  - *Dro*: ***density of recruitment in open interspaces***, as number
    of recruits per unit area of open interspaces.

  - *Ns*: The ***Normalized Neighbour Suitability index***; suitable for
    comparisons of interaction strength between pairs of species within
    a local community.

  - *NIntA*: The ***Additive symmetry intensity index***.

  - *NIntC*: The ***Commutative symmetry intensity index***.

  - *RII*: The ***Relative Interaction Index***.

## Value

A matrix with recruit species in rows and canopy species in columns,
with cells describing the measure selected to describe the interaction
strength (i.e. weight) between each pair of species.

## Examples

``` r
RN_to_matrix(Amoladeras_int, Amoladeras_cover, int_type="rec", weight="Dcr")
#> Based on Weibull distribution fitted to the observed values, the threshold density has been set to: 1.12346719046902.
#>                       Artemisia_barrelieri Artemisia_campestris Asparagus_albus
#> Artemisia_barrelieri                     3                    0      0.00000000
#> Artemisia_campestris                     0                    0      0.00000000
#> Asparagus_albus                          0                    0      0.00000000
#> Asparagus_horridus                       0                    0      0.00000000
#> Ballota_hirsuta                          0                    0      0.00000000
#> Helichrysum_stoechas                     3                    0      0.00000000
#> Hyparrhenia_hirta                        0                    0      0.00000000
#> Launaea_arborescens                      0                    0      0.00000000
#> Launaea_lanifera                         0                    0      0.00000000
#> Lycium_intrincatum                       0                    0      0.07213706
#> Lygeum_spartum                           0                    0      0.03606853
#> Maytenus_senegalensis                    0                    0      0.03606853
#> Ononis_natrix                            0                    0      0.03606853
#> Open                                     0                    0      0.00000000
#> Phagnalon_saxatile                       9                    0      0.14427412
#> Piptatherum_miliaceum                    0                    0      0.00000000
#> Salsola_oppositifolia                    0                    0      0.00000000
#> Stipa_tenacissima                        0                    0      0.00000000
#> Teucrium_lusitanicum                     0                    0      0.00000000
#> Teucrium_polium                          2                    0      0.00000000
#> Thymelaea_hirsuta                        0                    0      0.07213706
#> Thymus_hyemalis                          1                    0      0.00000000
#> Whitania_frutescens                      0                    0      0.10820559
#> Ziziphus_lotus                           0                    0      0.00000000
#>                       Asparagus_horridus Ballota_hirsuta Helichrysum_stoechas
#> Artemisia_barrelieri             0.00000      0.00000000           0.00000000
#> Artemisia_campestris             0.00000      0.00000000           0.00000000
#> Asparagus_albus                  0.00000      0.00000000           0.00000000
#> Asparagus_horridus               0.00000      0.05200884           0.06879945
#> Ballota_hirsuta                  0.63593      0.00000000           0.10319917
#> Helichrysum_stoechas             0.00000      0.10401768           0.00000000
#> Hyparrhenia_hirta                0.00000      0.00000000           0.00000000
#> Launaea_arborescens              0.00000      0.00000000           0.03439972
#> Launaea_lanifera                 0.00000      0.00000000           0.00000000
#> Lycium_intrincatum               0.00000      0.00000000           0.00000000
#> Lygeum_spartum                   0.00000      0.00000000           0.00000000
#> Maytenus_senegalensis            0.00000      0.00000000           0.00000000
#> Ononis_natrix                    0.00000      0.05200884           0.51599587
#> Open                             0.00000      0.00000000           0.00000000
#> Phagnalon_saxatile               2.54372      0.36406189           0.13759890
#> Piptatherum_miliaceum            0.00000      0.00000000           0.00000000
#> Salsola_oppositifolia            0.00000      0.13002210           0.24079807
#> Stipa_tenacissima                0.00000      0.00000000           0.00000000
#> Teucrium_lusitanicum             0.00000      0.00000000           0.00000000
#> Teucrium_polium                  0.00000      0.02600442           0.41279670
#> Thymelaea_hirsuta                0.00000      0.07801326           0.48159615
#> Thymus_hyemalis                  0.63593      0.00000000           0.13759890
#> Whitania_frutescens              0.00000      0.00000000           0.00000000
#> Ziziphus_lotus                   0.00000      0.00000000           0.00000000
#>                       Hyparrhenia_hirta Launaea_arborescens Launaea_lanifera
#> Artemisia_barrelieri         0.00000000         0.000000000                0
#> Artemisia_campestris         0.00000000         0.000000000                0
#> Asparagus_albus              0.00000000         0.010551528                0
#> Asparagus_horridus           0.00000000         0.014772140                0
#> Ballota_hirsuta              0.00000000         0.010551528                0
#> Helichrysum_stoechas         0.00000000         0.073860699                0
#> Hyparrhenia_hirta            0.00000000         0.008441223                0
#> Launaea_arborescens          0.07544323         0.018992751                0
#> Launaea_lanifera             0.00000000         0.000000000                0
#> Lycium_intrincatum           0.07544323         0.012661834                0
#> Lygeum_spartum               0.00000000         0.002110306                0
#> Maytenus_senegalensis        0.00000000         0.002110306                0
#> Ononis_natrix                0.07544323         0.238464542                0
#> Open                         0.00000000         0.000000000                0
#> Phagnalon_saxatile           0.60354583         0.354531354                0
#> Piptatherum_miliaceum        0.00000000         0.004220611                0
#> Salsola_oppositifolia        0.00000000         0.016882445                0
#> Stipa_tenacissima            0.00000000         0.000000000                0
#> Teucrium_lusitanicum         0.37721614         0.008441223                0
#> Teucrium_polium              0.15088646         0.029544279                0
#> Thymelaea_hirsuta            0.15088646         0.042206114                0
#> Thymus_hyemalis              0.07544323         0.029544279                0
#> Whitania_frutescens          0.07544323         0.075971004                0
#> Ziziphus_lotus               0.00000000         0.000000000                0
#>                       Lycium_intrincatum Lygeum_spartum Maytenus_senegalensis
#> Artemisia_barrelieri          0.00000000    0.035730098           0.000000000
#> Artemisia_campestris          0.00000000    0.004660448           0.000000000
#> Asparagus_albus               0.00618047    0.006213930           0.008587377
#> Asparagus_horridus            0.01236094    0.010874378           0.000000000
#> Ballota_hirsuta               0.12360939    0.000000000           0.000000000
#> Helichrysum_stoechas          0.10506799    0.006213930           0.000000000
#> Hyparrhenia_hirta             0.00000000    0.000000000           0.000000000
#> Launaea_arborescens           0.01854141    0.009320895           0.008587377
#> Launaea_lanifera              0.00000000    0.000000000           0.000000000
#> Lycium_intrincatum            0.02472188    0.003106965           0.008587377
#> Lygeum_spartum                0.00000000    0.015534825           0.000000000
#> Maytenus_senegalensis         0.00000000    0.001553483           0.000000000
#> Ononis_natrix                 0.05562423    0.015534825           0.025762130
#> Open                          0.00000000    0.000000000           0.000000000
#> Phagnalon_saxatile            0.25957973    0.063692783           0.060111636
#> Piptatherum_miliaceum         0.00000000    0.000000000           0.000000000
#> Salsola_oppositifolia         0.07416564    0.026409203           0.000000000
#> Stipa_tenacissima             0.00000000    0.000000000           0.000000000
#> Teucrium_lusitanicum          0.00000000    0.009320895           0.000000000
#> Teucrium_polium               0.04326329    0.026409203           0.000000000
#> Thymelaea_hirsuta             0.02472188    0.020195273           0.000000000
#> Thymus_hyemalis               0.00000000    0.018641790           0.017174753
#> Whitania_frutescens           0.00000000    0.000000000           0.008587377
#> Ziziphus_lotus                0.00000000    0.000000000           0.000000000
#>                       Ononis_natrix         Open Phagnalon_saxatile
#> Artemisia_barrelieri    0.004082216 0.0077955915         0.00000000
#> Artemisia_campestris    0.000000000 0.0001443628         0.00000000
#> Asparagus_albus         0.000000000 0.0010105396         0.00000000
#> Asparagus_horridus      0.000000000 0.0025985305         0.05895357
#> Ballota_hirsuta         0.000000000 0.0015879909         0.00000000
#> Helichrysum_stoechas    0.008164432 0.1149127933         0.00000000
#> Hyparrhenia_hirta       0.008164432 0.0049083354         0.00000000
#> Launaea_arborescens     0.077562101 0.0128482897         0.08843036
#> Launaea_lanifera        0.000000000 0.0008661768         0.00000000
#> Lycium_intrincatum      0.008164432 0.0028872561         0.02947679
#> Lygeum_spartum          0.000000000 0.0115490245         0.00000000
#> Maytenus_senegalensis   0.000000000 0.0000000000         0.00000000
#> Ononis_natrix           0.146959770 0.2654831996         0.14738394
#> Open                    0.000000000 0.0000000000         0.00000000
#> Phagnalon_saxatile      0.012246647 0.0762235614         0.08843036
#> Piptatherum_miliaceum   0.000000000 0.0027428933         0.00000000
#> Salsola_oppositifolia   0.012246647 0.0086617683         0.14738394
#> Stipa_tenacissima       0.000000000 0.0001443628         0.00000000
#> Teucrium_lusitanicum    0.000000000 0.0023098049         0.00000000
#> Teucrium_polium         0.016328863 0.0444637441         0.23581430
#> Thymelaea_hirsuta       0.004082216 0.0245416770         0.26529108
#> Thymus_hyemalis         0.000000000 0.1486936898         0.11790715
#> Whitania_frutescens     0.000000000 0.0007218140         0.00000000
#> Ziziphus_lotus          0.000000000 0.0005774512         0.00000000
#>                       Piptatherum_miliaceum Salsola_oppositifolia
#> Artemisia_barrelieri             0.00000000            0.03246226
#> Artemisia_campestris             0.00000000            0.00000000
#> Asparagus_albus                  0.00000000            0.00000000
#> Asparagus_horridus               0.00000000            0.03246226
#> Ballota_hirsuta                  0.00000000            0.06492453
#> Helichrysum_stoechas             0.00000000            0.51939620
#> Hyparrhenia_hirta                0.00000000            0.01623113
#> Launaea_arborescens              0.32293483            0.14608018
#> Launaea_lanifera                 0.00000000            0.00000000
#> Lycium_intrincatum               0.06458697            0.08115566
#> Lygeum_spartum                   0.00000000            0.01623113
#> Maytenus_senegalensis            0.00000000            0.00000000
#> Ononis_natrix                    0.00000000            0.09738679
#> Open                             0.00000000            0.00000000
#> Phagnalon_saxatile               0.25834787            0.74663204
#> Piptatherum_miliaceum            0.00000000            0.00000000
#> Salsola_oppositifolia            0.00000000            0.00000000
#> Stipa_tenacissima                0.00000000            0.00000000
#> Teucrium_lusitanicum             0.00000000            0.00000000
#> Teucrium_polium                  0.25834787            0.27592923
#> Thymelaea_hirsuta                0.00000000            0.08115566
#> Thymus_hyemalis                  0.00000000            0.22723584
#> Whitania_frutescens              0.00000000            0.00000000
#> Ziziphus_lotus                   0.00000000            0.00000000
#>                       Stipa_tenacissima Teucrium_lusitanicum Teucrium_polium
#> Artemisia_barrelieri                  0            0.0000000       0.0000000
#> Artemisia_campestris                  0            0.0000000       0.0000000
#> Asparagus_albus                       0            0.0000000       0.0000000
#> Asparagus_horridus                    0            0.0000000       0.0000000
#> Ballota_hirsuta                       0            0.0000000       0.0000000
#> Helichrysum_stoechas                  0            0.0000000       0.3520094
#> Hyparrhenia_hirta                     0            0.0000000       0.1760047
#> Launaea_arborescens                   0            0.0000000       0.0000000
#> Launaea_lanifera                      0            0.0000000       0.0000000
#> Lycium_intrincatum                    0            0.0000000       0.0000000
#> Lygeum_spartum                        0            0.0000000       0.0000000
#> Maytenus_senegalensis                 0            0.0000000       0.0000000
#> Ononis_natrix                         0            0.1893043       0.4693458
#> Open                                  0            0.0000000       0.0000000
#> Phagnalon_saxatile                    0            0.0000000       0.4106776
#> Piptatherum_miliaceum                 0            0.0000000       0.1173365
#> Salsola_oppositifolia                 0            0.0000000       0.1760047
#> Stipa_tenacissima                     0            0.0000000       0.0000000
#> Teucrium_lusitanicum                  0            0.0000000       0.0000000
#> Teucrium_polium                       0            0.0000000       0.0000000
#> Thymelaea_hirsuta                     0            0.1893043       0.7040188
#> Thymus_hyemalis                       0            0.1893043       0.1760047
#> Whitania_frutescens                   0            0.0000000       0.0000000
#> Ziziphus_lotus                        0            0.0000000       0.0000000
#>                       Thymelaea_hirsuta Thymus_hyemalis Whitania_frutescens
#> Artemisia_barrelieri        0.035478606     0.003827165          0.00000000
#> Artemisia_campestris        0.000000000     0.003827165          0.00000000
#> Asparagus_albus             0.000000000     0.005740748          0.00000000
#> Asparagus_horridus          0.035478606     0.003827165          0.07040394
#> Ballota_hirsuta             0.035478606     0.001913583          0.00000000
#> Helichrysum_stoechas        0.411551834     0.164568104          0.00000000
#> Hyparrhenia_hirta           0.014191443     0.005740748          0.00000000
#> Launaea_arborescens         0.049670049     0.007654330          0.14080789
#> Launaea_lanifera            0.007095721     0.001913583          0.00000000
#> Lycium_intrincatum          0.007095721     0.003827165          0.05280296
#> Lygeum_spartum              0.014191443     0.011481496          0.00000000
#> Maytenus_senegalensis       0.000000000     0.000000000          0.01760099
#> Ononis_natrix               0.305116015     0.013395078          0.07040394
#> Open                        0.000000000     0.000000000          0.00000000
#> Phagnalon_saxatile          0.979209537     0.254506487          0.17600986
#> Piptatherum_miliaceum       0.000000000     0.007654330          0.00000000
#> Salsola_oppositifolia       0.078052934     0.051666730          0.00000000
#> Stipa_tenacissima           0.000000000     0.000000000          0.00000000
#> Teucrium_lusitanicum        0.035478606     0.000000000          0.00000000
#> Teucrium_polium             0.383168949     0.166481687          0.05280296
#> Thymelaea_hirsuta           0.134818704     0.124382870          0.03520197
#> Thymus_hyemalis             0.241254524     0.017222243          0.08800493
#> Whitania_frutescens         0.007095721     0.000000000          0.01760099
#> Ziziphus_lotus              0.000000000     0.000000000          0.00000000
#>                       Ziziphus_lotus
#> Artemisia_barrelieri     0.000000000
#> Artemisia_campestris     0.000000000
#> Asparagus_albus          0.008598545
#> Asparagus_horridus       0.000000000
#> Ballota_hirsuta          0.068788358
#> Helichrysum_stoechas     0.025795634
#> Hyparrhenia_hirta        0.008598545
#> Launaea_arborescens      0.008598545
#> Launaea_lanifera         0.000000000
#> Lycium_intrincatum       0.027945270
#> Lygeum_spartum           0.004299272
#> Maytenus_senegalensis    0.000000000
#> Ononis_natrix            0.019346726
#> Open                     0.000000000
#> Phagnalon_saxatile       0.085985447
#> Piptatherum_miliaceum    0.000000000
#> Salsola_oppositifolia    0.030094906
#> Stipa_tenacissima        0.000000000
#> Teucrium_lusitanicum     0.015047453
#> Teucrium_polium          0.019346726
#> Thymelaea_hirsuta        0.006448909
#> Thymus_hyemalis          0.019346726
#> Whitania_frutescens      0.004299272
#> Ziziphus_lotus           0.000000000

```
