XGBoost model tuning
================
hampus Nordholm
2025-02-02

``` r
# LIBRARIES -- Loading data into R -- 

library(tidyverse)
```

    ## Warning: package 'ggplot2' was built under R version 4.3.3

    ## Warning: package 'readr' was built under R version 4.3.3

    ## Warning: package 'purrr' was built under R version 4.3.3

    ## Warning: package 'stringr' was built under R version 4.3.3

    ## Warning: package 'lubridate' was built under R version 4.3.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(tidymodels)
```

    ## Warning: package 'tidymodels' was built under R version 4.3.3

    ## ── Attaching packages ────────────────────────────────────── tidymodels 1.2.0 ──
    ## ✔ broom        1.0.7      ✔ rsample      1.2.1 
    ## ✔ dials        1.3.0      ✔ tune         1.2.1 
    ## ✔ infer        1.0.7      ✔ workflows    1.1.4 
    ## ✔ modeldata    1.4.0      ✔ workflowsets 1.1.0 
    ## ✔ parsnip      1.2.1      ✔ yardstick    1.3.1 
    ## ✔ recipes      1.0.10

    ## Warning: package 'broom' was built under R version 4.3.3

    ## Warning: package 'dials' was built under R version 4.3.3

    ## Warning: package 'scales' was built under R version 4.3.3

    ## Warning: package 'infer' was built under R version 4.3.3

    ## Warning: package 'modeldata' was built under R version 4.3.3

    ## Warning: package 'parsnip' was built under R version 4.3.3

    ## Warning: package 'recipes' was built under R version 4.3.3

    ## Warning: package 'rsample' was built under R version 4.3.3

    ## Warning: package 'tune' was built under R version 4.3.3

    ## Warning: package 'workflows' was built under R version 4.3.3

    ## Warning: package 'workflowsets' was built under R version 4.3.3

    ## Warning: package 'yardstick' was built under R version 4.3.3

    ## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
    ## ✖ scales::discard() masks purrr::discard()
    ## ✖ dplyr::filter()   masks stats::filter()
    ## ✖ recipes::fixed()  masks stringr::fixed()
    ## ✖ dplyr::lag()      masks stats::lag()
    ## ✖ yardstick::spec() masks readr::spec()
    ## ✖ recipes::step()   masks stats::step()
    ## • Use tidymodels_prefer() to resolve common conflicts.

``` r
library(skimr)
```

    ## Warning: package 'skimr' was built under R version 4.3.3

``` r
library(correlationfunnel)
```

    ## Warning: package 'correlationfunnel' was built under R version 4.3.3

    ## ══ correlationfunnel Tip #1 ════════════════════════════════════════════════════
    ## Make sure your data is not overly imbalanced prior to using `correlate()`.
    ## If less than 5% imbalance, consider sampling. :)

``` r
library(xgboost)
```

    ## Warning: package 'xgboost' was built under R version 4.3.3

    ## 
    ## Attaching package: 'xgboost'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     slice

``` r
library(vip)
```

    ## Warning: package 'vip' was built under R version 4.3.3

    ## 
    ## Attaching package: 'vip'
    ## 
    ## The following object is masked from 'package:utils':
    ## 
    ##     vi

``` r
# CSV into DF -- 

boatraw <- read_csv("boat_data.csv")
```

    ## Rows: 9888 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (6): Price, Boat Type, Manufacturer, Type, Material, Location
    ## dbl (4): Year Built, Length, Width, Number of views last 7 days
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Data introduction --

boatraw %>% skim()
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 9888       |
| Number of columns                                | 10         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 6          |
| numeric                                          | 4          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| Price         |         0 |          1.00 |   7 |  12 |     0 |     3182 |          0 |
| Boat Type     |         0 |          1.00 |   3 |  43 |     0 |      126 |          0 |
| Manufacturer  |      1338 |          0.86 |  14 |  42 |     0 |      910 |          0 |
| Type          |         6 |          1.00 |   6 |  28 |     0 |       24 |          0 |
| Material      |      1749 |          0.82 |   3 |  19 |     0 |       11 |          0 |
| Location      |        36 |          1.00 |   4 |  84 |     0 |     2837 |          0 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| Year Built | 0 | 1.00 | 1893.19 | 460.20 | 0.00 | 1996.00 | 2007.00 | 2017.00 | 2021.00 | ▁▁▁▁▇ |
| Length | 9 | 1.00 | 11.57 | 6.00 | 1.04 | 7.47 | 10.28 | 13.93 | 100.00 | ▇▁▁▁▁ |
| Width | 56 | 0.99 | 3.52 | 1.22 | 0.01 | 2.54 | 3.33 | 4.25 | 25.16 | ▇▁▁▁▁ |
| Number of views last 7 days | 0 | 1.00 | 149.16 | 151.82 | 13.00 | 70.00 | 108.00 | 172.00 | 3263.00 | ▇▁▁▁▁ |

``` r
boatraw %>% head()
```

    ## # A tibble: 6 × 10
    ##   Price     `Boat Type`    Manufacturer Type  `Year Built` Length Width Material
    ##   <chr>     <chr>          <chr>        <chr>        <dbl>  <dbl> <dbl> <chr>   
    ## 1 CHF 3337  Motor Yacht    Rigiflex po… new …         2017   4     1.9  <NA>    
    ## 2 EUR 3490  Center consol… Terhi power… new …         2020   4     1.5  Thermop…
    ## 3 CHF 3770  Sport Boat     Marine powe… new …            0   3.69  1.42 Alumini…
    ## 4 DKK 25900 Sport Boat     Pioner powe… new …         2020   3     1    <NA>    
    ## 5 EUR 3399  Fishing Boat   Linder powe… new …         2019   3.55  1.46 Alumini…
    ## 6 CHF 3650  Sport Boat     Linder powe… new …            0   4.03  1.56 Alumini…
    ## # ℹ 2 more variables: Location <chr>, `Number of views last 7 days` <dbl>

``` r
boatraw %>% tail()
```

    ## # A tibble: 6 × 10
    ##   Price    `Boat Type`  Manufacturer    Type  `Year Built` Length Width Material
    ##   <chr>    <chr>        <chr>           <chr>        <dbl>  <dbl> <dbl> <chr>   
    ## 1 CHF 4950 Fishing Boat Staempfli powe… Used…         1984   6     1.62 Plastic 
    ## 2 CHF 4900 Sport Boat   Sea Ray power … Used…         1987   6.3   2.44 <NA>    
    ## 3 EUR 4516 Sport Boat   <NA>            new …            0   4.17  1.68 GRP     
    ## 4 EUR 4499 Sport Boat   BlueCraft powe… new …         2020   4.4   1.8  GRP     
    ## 5 EUR 4300 Pontoon Boat Whaly power bo… new …         2018   4.37  1.89 <NA>    
    ## 6 CHF 3780 Fishing Boat DarekCo power … new …         2019   3.6   1.6  GRP     
    ## # ℹ 2 more variables: Location <chr>, `Number of views last 7 days` <dbl>

``` r
#     ---- DATA CLEANING ----

boat <- boatraw %>% clean_names()

# Split price column and creating a new converted EUR column -- 
# More efficient for proper price analysis -- 

boat <- boat %>% 
  mutate(Currency= str_split(price, " ", simplify = TRUE)[,1],
         price=str_remove_all(price,"[^0-9]"),
         price=as.numeric(price))

boat <- boat %>% 
  mutate(price_eur=case_when(Currency=="CHF" ~ price * 1.06,
                             Currency=="DKK" ~ price * 0.13,
                             Currency=="Â£" ~ price * 1.2,
                             TRUE ~ price)) %>% 
  select(-price,-Currency)


# Some boats has multiple types, lets split them -- 

boat <- boat %>% 
  separate_rows(boat_type,sep=",") 

#Manufacturer consists of 1405 NA:s , replace with unknown -- 

boat <- boat %>% 
  mutate(manufacturer=ifelse(is.na(manufacturer),"Unknown",manufacturer))

#        Cleaning column type --- 

boat <- boat %>% separate_rows(type,sep=",") %>% 
  mutate(type=ifelse(is.na(type),"Unknown",type))

#   12 NA:s in column length, mean imputation -- 
#   61 NA:s in column width, mean imputation -- 

boat <- boat %>% 
  mutate(length = ifelse(is.na(length), mean(length, na.rm = TRUE), length),
         width= ifelse(is.na(width), mean(width, na.rm = TRUE), width))

#  Material -- Handling NA:s --

boat <- boat %>% 
  mutate(material=ifelse(is.na(material),"Unknown",material))


# Cleaning location column -- Creating a country column --- 

boat <- boat %>% 
  mutate(country=case_when(
                           str_detect(location,"Netherlands")~"Netherlands",
                           str_detect(location,"Norway")~"Norway",
                           str_detect(location,"Philippines")~"Philippines",
                           str_detect(location,"Poland")~"Poland",
                           str_detect(location,"Portugal")~"Portugal",
                           str_detect(location,"Portugal")~"Portugal",
                           str_detect(location,"Russian")~"Russia",
                           str_detect(location,"Slovak Republic")~"Slovak Republic",
                           str_detect(location,"Slovenia")~"Slovenia",
                           str_detect(location,"Spain")~"Spain",
                           str_detect(location,"Sweden")~"Sweden",
                           str_detect(location,"Switzerland")~"Switzerland",
                           str_detect(location,"Thailand")~"Thailand",
                           str_detect(location,"Turkey")~"Turkey",
                           str_detect(location,"Ukraine")~"ukraine",
                           str_detect(location,"Arab Emirates")~"Arab Emirates",
                           str_detect(location,"United Kingdom")~"United Kingdom",
                           str_detect(location,"United States")~"United States",
                           str_detect(location,"Germany")~"Germany",
                           str_detect(location,"Greece")~"Greece",
                           str_detect(location,"Hungary")~"Hungary",
                           str_detect(location,"Ibiza")~"Spain",
                           str_detect(location,"Ireland")~"Ireland",
                           str_detect(location,"Italie")~"Italy",
                           str_detect(location,"Italien")~"Italy",
                           str_detect(location,"Italy")~"Italy",
                           str_detect(location,"Latvia")~"Latvia",
                           str_detect(location,"Lithuania")~"Lithuania",
                           str_detect(location,"Mallorca")~"Spain",
                           str_detect(location,"Malta")~"Malta",
                           str_detect(location,"Montenegro")~"Montenegro",
                           str_detect(location,"Australia")~"Australia",
                           str_detect(location,"Austria")~"Austria",
                           str_detect(location,"Belgium")~"Belgium",
                           str_detect(location,"Bulgaria")~"Bulgaria",
                           str_detect(location,"Croatia")~"Croatia",
                           str_detect(location,"Cyprus")~"Cyprus",
                           str_detect(location,"Czech Republic")~"Czech Republic",
                           str_detect(location,"Denmark")~"Denmark",
                           str_detect(location,"Egypt")~"Egypt",
                           str_detect(location,"Estonia")~"Estonia",
                           str_detect(location,"Finland")~"Finland",
                           str_detect(location,"France")~"France",
                                      TRUE ~ location))

boat <- boat %>% 
  mutate(country=case_when(
                           str_detect(location,"BÃ¼delsdorf")~"Germany",
                           str_detect(location,"Traunstein")~"Traunsteiny",
                           str_detect(location,"Adria")~"Italy",
                           str_detect(location,"Angera")~"Italy",
                           str_detect(location,"Avenches")~"Switzerland",
                           str_detect(location,"Barssel")~"Germany",
                           str_detect(location,"Beilngries")~"Germany",
                           str_detect(location,"BelgiÃ«")~"Belgium",
                           str_detect(location,"Berlin")~"Germany",
                           str_detect(location,"Bielefeld")~"Germany",
                           str_detect(location,"Brandenburg")~"Germany",
                           str_detect(location,"Bremen")~"Germany",
                           str_detect(location,"Dalmatien")~"Croatia",
                           str_detect(location,"Donau")~"Donau",
                           str_detect(location,"Faoug")~"Switzerland",
                           str_detect(location,"French")~"France",
                           str_detect(location,"FuÃ\u009fach")~"France",
                           str_detect(location,"Gibraltar")~"United Kingdom",
                           str_detect(location,"Greetsile")~"Germany",
                           str_detect(location,"Heilbronn")~"Germany",
                           str_detect(location,"Isle of Man")~"United Kingdom",
                           str_detect(location,"Izola")~"Slovenia",
                           str_detect(location,"Jersey")~"United Kingdom",
                           str_detect(location,"Juelsminde Havn")~"Denmark",
                           str_detect(location,"Katwijk")~"Netherlands",
                           str_detect(location,"Kroatien")~"Croatia",
                           str_detect(location,"Juelsminde Havn")~"Denmark",
                           str_detect(location,"Castelletto Ticino")~"Italy",
                           str_detect(location,"Minusio")~"Switzerland",
                           str_detect(location,"78337")~"Germany",
                           str_detect(location,"78343")~"Germany",
                           str_detect(location,"8253")~"Switzerland",
                           str_detect(location,"8560")~"Germany",
                           str_detect(location,"Altenrhein")~"Switzerland",
                           str_detect(location,"Andelfingen")~"Switzerland",
                           str_detect(location,"Sipplingen")~"Germany",
                           str_detect(location,"St.Gallen")~"Switzerland",
                           str_detect(location,"Founex")~"Switzerland",
                           str_detect(location,"Geneva")~"Switzerland",
                           str_detect(location,"GenÃ¨ve")~"Switzerland",
                           str_detect(location,"Lausanne")~"Switzerland",
                           str_detect(location,"Nyon")~"Switzerland",
                           str_detect(location,"versoix")~"Switzerland",
                           str_detect(location,"Lebanon")~"Lebanon",
                           str_detect(location,"Lommel")~"Belgium",
                           str_detect(location,"Marina Punat")~"Croatia",
                           str_detect(location,"Lebanon")~"Lebanon",
                           str_detect(location,"Martinique Â» Le Marin")~"France",
                           str_detect(location,"Monaco")~"Monaco",
                           str_detect(location,"Morocco")~"Morocco",
                           str_detect(location,"Neusiedl am See")~"Austria",
                           str_detect(location,"Neustadt in Holstein (Ostsee)")~"Germany",
                           str_detect(location,"Niederrhein")~"Germany",
                           str_detect(location,"NordseekÃ¼ste")~"Germany",
                           str_detect(location,"Novi Vinodolski")~"Croatia",
                           str_detect(location,"Wegorzewo")~"Poland",
                           str_detect(location,"Opwijk")~"Netherlands",
                           str_detect(location,"Ostsee Â» Naantali")~"Finland",
                           str_detect(location,"Ostsee Â» Zingst")~"Germany",
                           str_detect(location,"PT Ã¸stkysten ellers Esbjerg")~"Denmark",
                           str_detect(location,"Porto Rotondo")~"Italy",
                           str_detect(location,"Rheinfelden")~"Germany",
                           str_detect(location,"Rolle")~"Switzerland",
                           str_detect(location,"Romania")~"Romania",
                           str_detect(location,"Rostock")~"Germany",
                           str_detect(location,"Rovinij")~"Croatia",
                           str_detect(location,"RÃ¼gen")~"Germany",
                           str_detect(location,"Serbia")~"Serbia",
                           str_detect(location,"Seychelles")~"Seychelles",
                           str_detect(location,"Split")~"Croatia",
                           str_detect(location,"Steinwiesen")~"Germany",
                           str_detect(location,"Stralsund")~"Germany",
                           str_detect(location,"Taiwan")~"Taiwan",
                           str_detect(location,"Thalwil")~"Switzerland",
                           str_detect(location,"Thun")~"Switzerland",
                           str_detect(location,"Toscana")~"Italy",
                           str_detect(location,"TravemÃ¼nde")~"Germany",
                           str_detect(location,"Venezuela")~"Venezuela",
                           str_detect(location,"VierwaldstÃ¤ttersee - Buochs")~"Switzerland",
                           str_detect(location,"Welschenrohr")~"Switzerland",
                           str_detect(location,"Wijdenes")~"Netherlands",
                           str_detect(location,"Zevenbergen")~"Netherlands",
                           str_detect(location,"ZÃ¼richse, 8855 Wangen SZ")~"Switzerland",
                           str_detect(location,"annecy")~"France",
                           str_detect(location,"baden baden")~"Germany",
                           str_detect(location,"espa?a")~"Spain",
                           str_detect(location,"lago maggiore")~"Italy",
                           str_detect(location,"ukraine")~"Ukraine",
                           str_detect(location,"waren mÃ¼ritz")~"Germany",
                           str_detect(location,"Lago di Garda")~"Italy",
                           str_detect(location,"Steckborn")~"Switzerland",
                           str_detect(location,"(Ostsee)")~"Germany",
                           str_detect(location,"Tenero, lago Maggiore")~"Switzerland",
                           TRUE ~ country))

#   Replace NA:s with unknown -- Remove location -- 

boat <- boat %>% 
  mutate(country=ifelse(is.na(country),"Unknown",country)) %>% 
  select(-location)


#  Change variable type for modeling -- 

boat <- boat %>% 
  mutate_if(is.character,factor)

# Creating size var -- RM LENGTH / WIDTH -- 

boat <- boat %>% 
  mutate(size=length*width) %>% 
  select(-length,-width)
```

``` r
#        EXPLORATORY DATA ANALYSIS --- 

# Number of views will be treated as demand / popularity in this analysis. 
# Exploring relationships to that feature will be of interest. -- 

# Is there any relationship between price and number of views ? 

boat %>% 
  ggplot(aes(log(price_eur),number_of_views_last_7_days))+
  geom_point(shape=19,size=0.6,color="midnightblue",alpha=0.6)+
  labs(title="N of views vs. price",
       y="Number of views(last 7 days)",
       x="lg(price)")
```

![](boatproj_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# Is older boats more or less expensive ? 

boat %>% 
  filter(year_built>1900) %>% 
  ggplot(aes(year_built,price_eur))+
  geom_point(shape=19,size=3,alpha=0.1,color="midnightblue")+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Price by time",y=NULL,x=NULL)
```

![](boatproj_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
#Avg number of views per boat in Countries -- 

boat %>% 
  group_by(country) %>% 
  summarise(avg_views=mean(number_of_views_last_7_days)) %>% 
  ggplot(aes(avg_views,fct_reorder(country,avg_views),fill=country))+
  geom_col()+
  theme(legend.position="NONE")+
  labs(title="AVG views per boat",y=NULL,x=NULL)
```

![](boatproj_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
# N of views by type -- 
boat %>% 
  ggplot(aes(number_of_views_last_7_days,
             fct_reorder(type,number_of_views_last_7_days),
                         fill=type))+
  geom_boxplot()+
  theme(legend.position ="NONE")+
  labs(title="Views by type",x=NULL,y=NULL)
```

![](boatproj_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

``` r
#N of views by boat_type -- 

boat %>% 
  ggplot(aes(number_of_views_last_7_days,
             fct_reorder(boat_type,number_of_views_last_7_days),
                         fill=boat_type))+
  geom_boxplot()+
  theme(legend.position ="NONE")+
  labs(title="Views by boat_type",x=NULL,y=NULL)
```

![](boatproj_files/figure-gfm/unnamed-chunk-4-5.png)<!-- -->

``` r
#        CORRELATION ANALYSIS -- 

# Whats correlated with higher amount of views? ####

boat %>% 
  binarize() %>% 
  correlate(number_of_views_last_7_days__185_Inf) %>% 
  plot_correlation_funnel()
```

    ## Warning: ggrepel: 47 unlabeled data points (too many overlaps). Consider
    ## increasing max.overlaps

![](boatproj_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# Whats correlated with expensive boats? ###
boat %>% 
  binarize() %>% 
  correlate(price_eur__249000_Inf) %>% 
  plot_correlation_funnel()
```

    ## Warning: ggrepel: 49 unlabeled data points (too many overlaps). Consider
    ## increasing max.overlaps

![](boatproj_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
#              ----  MACHINE LEARNING --- 
#                        XG BOOST 

boat_ml<- boat %>% 
  select(-manufacturer)

# Split DF into training and testing set -- 

set.seed(123)
boat_split <- initial_split(boat_ml,prop=0.8)
boat_train <- training(boat_split)
boat_test <- testing(boat_split)


#       XGBOOST SPEC --

xgb_spec <-   boost_tree(trees=1000,
              tree_depth = tune(),min_n = tune(),
              loss_reduction = tune(),sample_size = tune(),
              mtry = tune(),learn_rate = tune()) %>% 
     set_engine("xgboost") %>% 
     set_mode("regression")

#      PARAMETER GRID --

xgb_grid <- grid_latin_hypercube(tree_depth(),
                     min_n(),
                     loss_reduction(),
                     sample_size=sample_prop(),
                     finalize(mtry(),boat_train),
                     learn_rate(),
                     size=10)
```

    ## Warning: `grid_latin_hypercube()` was deprecated in dials 1.3.0.
    ## ℹ Please use `grid_space_filling()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
xgb_grid
```

    ## # A tibble: 10 × 6
    ##    tree_depth min_n loss_reduction sample_size  mtry learn_rate
    ##         <int> <int>          <dbl>       <dbl> <int>      <dbl>
    ##  1         11    37       1.65e- 5       0.444     8   3.59e- 5
    ##  2          7     6       2.56e-10       0.691     1   3.85e- 9
    ##  3          8    17       2.16e- 7       0.334     5   1.42e- 6
    ##  4         13    33       2.98e- 7       0.898     7   2.51e- 8
    ##  5          8    13       4.34e- 2       0.160     3   1.54e- 5
    ##  6         15    21       1.30e- 3       0.462     2   3.23e- 4
    ##  7          3    30       2.17e- 4       0.732     6   5.35e- 2
    ##  8          1    27       1.15e- 8       0.634     4   5.88e- 8
    ##  9         10     4       3.13e+ 0       1.00      5   2.58e- 3
    ## 10          5    20       7.70e- 1       0.233     4   4.45e-10

``` r
#    Xgboost workflow and model formula -- 

xgb_wf <- workflow() %>% 
  add_formula(number_of_views_last_7_days~.,) %>% 
  add_model(xgb_spec)
```

``` r
#     CROSS VALIDATON FOLDS --

set.seed(123)
boat_folds <- vfold_cv(boat_train)


### Grid tuning ###

set.seed(234)
xgb_res <- tune_grid(xgb_wf,resamples=boat_folds,
          grid=xgb_grid)

#   Saving best PARAMETERS by RMSE and finalize xgboost workflow -- 

best_rmse <- select_best(xgb_res)
```

    ## Warning in select_best(xgb_res): No value of `metric` was given; "rmse" will be
    ## used.

``` r
final_xgb_wf <- finalize_workflow(xgb_wf,best_rmse)



#    XGBOOST VARIABLE IMPORTANCE -- 

final_xgb_wf %>% 
  fit(boat_train) %>% 
  pull_workflow_fit() %>% 
  vip(geom="point")
```

    ## Warning: `pull_workflow_fit()` was deprecated in workflows 0.2.3.
    ## ℹ Please use `extract_fit_parsnip()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](boatproj_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
#   Final evaluation ---

final_res <- last_fit(final_xgb_wf,boat_split)

final_res %>% collect_metrics()
```

    ## # A tibble: 2 × 4
    ##   .metric .estimator .estimate .config             
    ##   <chr>   <chr>          <dbl> <chr>               
    ## 1 rmse    standard     135.    Preprocessor1_Model1
    ## 2 rsq     standard       0.454 Preprocessor1_Model1
