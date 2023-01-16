
# alone <img src='dev/images/alone hex.png' align="right" height="240" />

A collection of datasets on the
[Alone](https://www.history.com/shows/alone) survival TV series in tidy
format. Included in the package are 4 datasets detailed below.

For non-Rstats users here is the link to the [Google sheets
doc](https://docs.google.com/spreadsheets/d/1-ZGasLGFVv6t50cOOhcA0SW68jdBIASTh3KFA2o1PQY/edit?usp=sharing).

# Installation

Install from Github:

``` r
devtools::install_github("doehm/alone")
```

# Datasets

## `survivors`

A data frame of all survivors across 9 seasons detailing:

1.  Name and demographics
2.  Location and profession
3.  Result
4.  Days lasted
5.  Reasons for tapping out (detailed and categorised)
6.  Page URL

``` r
survivors
```

    ## # A tibble: 94 × 16
    ##    season name     age gender city  state country result days_…¹ medic…² reaso…³
    ##     <dbl> <chr>  <dbl> <chr>  <chr> <chr> <chr>    <dbl>   <dbl> <lgl>   <chr>  
    ##  1      1 Alan …    40 Male   Blai… Geor… United…      1      56 FALSE   <NA>   
    ##  2      1 Sam L…    22 Male   Linc… Nebr… United…      2      55 FALSE   Lost t…
    ##  3      1 Mitch…    34 Male   Bell… Mass… United…      3      43 FALSE   Realiz…
    ##  4      1 Lucas…    32 Male   Quas… Iowa  United…      4      39 FALSE   Felt c…
    ##  5      1 Dusti…    37 Male   Pitt… Penn… United…      5       8 FALSE   Fear o…
    ##  6      1 Brant…    44 Male   Albe… Nort… United…      6       6 FALSE   Consum…
    ##  7      1 Wayne…    46 Male   Sain… New … Canada       7       4 FALSE   Fear o…
    ##  8      1 Joe R…    24 Male   Wind… Onta… Canada       8       4 FALSE   Loss o…
    ##  9      1 Chris…    41 Male   Umat… Flor… United…      9       1 FALSE   Fear o…
    ## 10      1 Josh …    31 Male   Jack… Ohio  United…     10       0 FALSE   Fear o…
    ## # … with 84 more rows, 5 more variables: reason_category <chr>, team <chr>,
    ## #   day_linked_up <dbl>, profession <chr>, url <chr>, and abbreviated variable
    ## #   names ¹​days_lasted, ²​medically_evacuated, ³​reason_tapped_out

## `loadouts`

Information on each survivor’s loadout of 10 items. It includes a
detailed item description and a simplified version for easier
aggregation and analysis.

``` r
loadouts
```

    ## # A tibble: 870 × 6
    ##    version season name     item_number item_detailed                       item 
    ##    <chr>    <dbl> <chr>          <dbl> <chr>                               <chr>
    ##  1 US           1 Alan Kay           1 Saw                                 Saw  
    ##  2 US           1 Alan Kay           2 Axe                                 Axe  
    ##  3 US           1 Alan Kay           3 Sleeping bag                        Slee…
    ##  4 US           1 Alan Kay           4 Large 2-quart pot                   Pot  
    ##  5 US           1 Alan Kay           5 Ferro rod                           Ferr…
    ##  6 US           1 Alan Kay           6 Water bottle/canteen                Cant…
    ##  7 US           1 Alan Kay           7 300 yards single filament line wit… Fish…
    ##  8 US           1 Alan Kay           8 Small gauge gill net                Gill…
    ##  9 US           1 Alan Kay           9 3.5lb wire                          Wire 
    ## 10 US           1 Alan Kay          10 Knife                               Knife
    ## # … with 860 more rows

## `episodes`

Contains details of each episode including the title, number of viewers,
beginning quote and IMDb rating

``` r
episodes
```

    ## # A tibble: 98 × 11
    ##    version season episode_n…¹ episode title air_d…² viewers quote author imdb_…³
    ##    <chr>    <dbl>       <dbl>   <dbl> <chr> <chr>     <dbl> <chr> <chr>    <dbl>
    ##  1 US           1           1       1 And … 2015-0…    1.58 I we… Henry…     7.5
    ##  2 US           1           2       2 Of W… 2015-0…    1.70 If y… Nikit…     7.7
    ##  3 US           1           3       3 The … 2015-0…    1.86 Exti… Carl …     7.7
    ##  4 US           1           4       4 Stal… 2015-0…    2.08 Hung… Alber…     7.7
    ##  5 US           1           5       5 Wind… 2015-0…    2.08 The … Micha…     7.6
    ##  6 US           1           6       6 Rain… 2015-0…    2.18 Extr… Rober…     7.6
    ##  7 US           1           7       7 The … 2015-0…    2.09 Huma… David…     7.7
    ##  8 US           1           8       8 Afte… 2015-0…   NA    This… Wayne      7.7
    ##  9 US           1           9       9 The … 2015-0…    1.80 If q… Sun T…     7.7
    ## 10 US           1          10      10 Brok… 2015-0…    1.94 Does… May S…     8.2
    ## # … with 88 more rows, 1 more variable: n_ratings <dbl>, and abbreviated
    ## #   variable names ¹​episode_number_overall, ²​air_date, ³​imdb_rating

## `seasons`

Season summary includes location and other season level information

``` r
seasons
```

    ## # A tibble: 9 × 8
    ##   version season location         country   n_survivors   lat    lon date_drop…¹
    ##   <chr>    <dbl> <chr>            <chr>           <dbl> <dbl>  <dbl> <chr>      
    ## 1 US           1 Quatsino         Canada             10  50.7 -127.  <NA>       
    ## 2 US           2 Quatsino         Canada             10  50.7 -127.  <NA>       
    ## 3 US           3 Patagonia        Argentina          10 -41    -68   <NA>       
    ## 4 US           4 Quatsino         Canada             14  50.7 -127.  <NA>       
    ## 5 US           5 Selenge Province Mongolia           10  49.8  106.  <NA>       
    ## 6 US           6 Great Slave Lake Canada             10  61.5 -114.  <NA>       
    ## 7 US           7 Great Slave Lake Canada             10  61.5 -114.  2019-09-18 
    ## 8 US           8 Chilko Lake      Canada             10  51.3 -124.  2020-09-18 
    ## 9 US           9 Nunatsiavut      Canada             10  59.7  -64.3 2021-09-18 
    ## # … with abbreviated variable name ¹​date_drop_off
