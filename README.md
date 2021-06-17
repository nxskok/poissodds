Odds to probs to Poisson means
================
Ken
16/06/2021

# R project for converting bookmaker odds to probabilities to Poisson goalscoring means

Functions are in the file `functions.R`.

## packages

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.2     ✓ dplyr   1.0.6
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
source("functions.R")
```

Make a dataframe of competitions to get. These are samples; you can add
others. (I wrote this during the delayed Euro 2020.) The column `short`
appears in the output, rather than the long URL.

``` r
leagues <- tribble(
  ~sheet_url, ~short,
#  "https://www.oddschecker.com/football/english/fa-cup", "enf", 
  # "https://www.oddschecker.com/football/champions-league", "euc", 
  # "https://www.oddschecker.com/football/europa-league", "eue", 
  "https://www.oddschecker.com/football/euro-2020", "ec", 
)
leagues
```

    ## # A tibble: 1 x 2
    ##   sheet_url                                      short
    ##   <chr>                                          <chr>
    ## 1 https://www.oddschecker.com/football/euro-2020 ec

run for all leagues

``` r
make_lambdas(leagues) %>% knitr::kable()
```

| short | game                          |   prob\_H |   prob\_D |   prob\_A | lambda\_1 | lambda\_2 |
| :---- | :---------------------------- | --------: | --------: | --------: | --------: | --------: |
| ec    | Ukraine v North Macedonia     | 0.7661823 | 0.2630150 | 0.2338177 | 2.4412248 | 1.0540664 |
| ec    | Denmark v Belgium             | 0.3209877 | 0.2934838 | 0.6790123 | 1.3881088 | 2.3091364 |
| ec    | Netherlands v Austria         | 0.8029197 | 0.2281690 | 0.1970803 | 2.7421702 | 1.0588383 |
| ec    | Sweden v Slovakia             | 0.7313643 | 0.2882057 | 0.2686357 | 2.2571814 | 1.0959540 |
| ec    | Croatia v Czech Republic      | 0.6228956 | 0.2879678 | 0.3771044 | 2.4387920 | 1.7769753 |
| ec    | England v Scotland            | 0.8956386 | 0.1739317 | 0.1043614 | 2.7075877 | 0.4953281 |
| ec    | Hungary v France              | 0.0940767 | 0.1819124 | 0.9059233 | 0.3200657 | 2.4599957 |
| ec    | Portugal v Germany            | 0.4097222 | 0.2876398 | 0.5902778 | 1.9495172 | 2.4420928 |
| ec    | Spain v Poland                | 0.8726625 | 0.1965856 | 0.1273375 | 2.6219711 | 0.5917054 |
| ec    | Italy v Wales                 | 0.8466135 | 0.2306039 | 0.1533865 | 2.3854418 | 0.6084552 |
| ec    | Switzerland v Turkey          | 0.7186082 | 0.2458537 | 0.2813918 | 2.9109988 | 1.6484278 |
| ec    | North Macedonia v Netherlands | 0.0617284 | 0.1471044 | 0.9382716 | 0.1389993 | 2.5035810 |
| ec    | Ukraine v Austria             | 0.5249110 | 0.3013025 | 0.4750890 | 2.1698910 | 2.0375844 |
| ec    | Finland v Belgium             | 0.0769231 | 0.1507538 | 0.9230769 | 0.3101464 | 2.6899428 |
| ec    | Russia v Denmark              | 0.2957746 | 0.2520336 | 0.7042254 | 1.6867449 | 2.8567381 |
| ec    | Croatia v Scotland            | 0.6797583 | 0.2970648 | 0.3202417 | 2.2649362 | 1.3494130 |
| ec    | Czech Republic v England      | 0.1605456 | 0.2128418 | 0.8394544 | 0.8234952 | 2.6948546 |
| ec    | Slovakia v Spain              | 0.0746269 | 0.1510721 | 0.9253731 | 0.2776000 | 2.6470312 |
| ec    | Sweden v Poland               | 0.5583039 | 0.3004373 | 0.4416961 | 2.2381035 | 1.9290246 |
| ec    | Germany v Hungary             | 0.9242144 | 0.1316212 | 0.0757856 | 3.0432868 | 0.4495437 |
| ec    | Portugal v France             | 0.4237288 | 0.3104456 | 0.5762712 | 1.7422500 | 2.1339921 |

The bookies’ probability of a draw is near 30% for evenly-matched teams,
which seems too high. Maybe also the Poisson means are too low. Using a
discount like 0.9 makes the draw probability lower and also makes the
means higher (it makes the draw probability 90% of what it was, and
rescales the other probabilities in proportion):

``` r
make_lambdas(leagues, discount = 0.9) %>% knitr::kable()
```

| short | game                          |   prob\_H |   prob\_D |   prob\_A | lambda\_1 | lambda\_2 |
| :---- | :---------------------------- | --------: | --------: | --------: | --------: | --------: |
| ec    | Ukraine v North Macedonia     | 0.7397810 | 0.2367135 | 0.2257608 | 2.6124950 | 1.2203065 |
| ec    | Denmark v Belgium             | 0.3081857 | 0.2641355 | 0.6519314 | 1.5314449 | 2.4442363 |
| ec    | Netherlands v Austria         | 0.7798653 | 0.2053521 | 0.1914215 | 2.9579971 | 1.2502805 |
| ec    | Sweden v Slovakia             | 0.7029037 | 0.2593851 | 0.2581819 | 2.3973665 | 1.2424346 |
| ec    | Croatia v Czech Republic      | 0.5986830 | 0.2591710 | 0.3624459 | 2.5803358 | 1.9247111 |
| ec    | England v Scotland            | 0.8771695 | 0.1565385 | 0.1022093 | 2.9612421 | 0.6738681 |
| ec    | Hungary v France              | 0.0920302 | 0.1637212 | 0.8862172 | 0.4747476 | 2.6824650 |
| ec    | Portugal v Germany            | 0.3938204 | 0.2588758 | 0.5673683 | 2.0965172 | 2.5840971 |
| ec    | Spain v Poland                | 0.8518195 | 0.1769271 | 0.1242961 | 2.8567688 | 0.7709472 |
| ec    | Italy v Wales                 | 0.8219772 | 0.2075435 | 0.1489229 | 2.5793516 | 0.7725433 |
| ec    | Switzerland v Turkey          | 0.6959210 | 0.2212684 | 0.2725080 | 3.1074162 | 1.8395693 |
| ec    | North Macedonia v Netherlands | 0.0606818 | 0.1323939 | 0.9223630 | 0.2724715 | 2.7287299 |
| ec    | Ukraine v Austria             | 0.5032108 | 0.2711723 | 0.4554485 | 2.2965882 | 2.1661805 |
| ec    | Finland v Belgium             | 0.0755814 | 0.1356784 | 0.9069767 | 0.4698984 | 2.9445397 |
| ec    | Russia v Denmark              | 0.2861331 | 0.2268302 | 0.6812694 | 1.8729050 | 3.0457795 |
| ec    | Croatia v Scotland            | 0.6521961 | 0.2673584 | 0.3072568 | 2.3952633 | 1.4890671 |
| ec    | Czech Republic v England      | 0.1563189 | 0.1915576 | 0.8173537 | 1.0126157 | 2.9229166 |
| ec    | Slovakia v Spain              | 0.0733221 | 0.1359649 | 0.9091935 | 0.4323491 | 2.8951456 |
| ec    | Sweden v Poland               | 0.5353140 | 0.2703936 | 0.4235079 | 2.3645975 | 2.0599366 |
| ec    | Germany v Hungary             | 0.9104151 | 0.1184591 | 0.0746540 | 3.3236705 | 0.6228774 |
| ec    | Portugal v France             | 0.4054739 | 0.2794010 | 0.5514445 | 1.8632637 | 2.2483337 |

The means are now a bit higher.
