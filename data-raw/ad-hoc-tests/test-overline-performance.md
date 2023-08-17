
``` r
devtools::load_all()
```

    ## ℹ Loading stplanr

    ## Warning: Objects listed as exports, but not present in namespace:
    ## • overline2

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ readr::edition_get()   masks testthat::edition_get()
    ## ✖ dplyr::filter()        masks stats::filter()
    ## ✖ purrr::is_null()       masks testthat::is_null()
    ## ✖ dplyr::lag()           masks stats::lag()
    ## ✖ readr::local_edition() masks testthat::local_edition()
    ## ✖ dplyr::matches()       masks tidyr::matches(), testthat::matches()
    ## ✖ readr::parse_date()    masks stplanr::parse_date()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
if(!file.exists("routes.geojson")) {
  routes = pct::get_pct_routes_fast("isle-of-wight")
  routes = routes %>% 
    slice(1:1000)
  sf::write_sf(routes, "routes.geojson", delete_dsn = TRUE)
}

routes = geojsonsf::geojson_sf("routes.geojson")
nrow(routes)
```

    ## [1] 1000

``` r
res1 = overline_old(routes, attrib = "foot")
```

    ## 2023-08-17 09:40:59.482411 constructing segments

    ## 2023-08-17 09:41:00.722783 building geometry

    ## 2023-08-17 09:41:01.004696 simplifying geometry

    ## 2023-08-17 09:41:01.005065 aggregating flows

    ## 2023-08-17 09:41:01.237217 rejoining segments into linestrings

``` r
res2 = overline3(routes, attrib = "foot")
```

    ## 2023-08-17 09:41:01.377522 constructing segments

    ## 2023-08-17 09:41:02.752467 building geometry

    ## 2023-08-17 09:41:03.045405 simplifying geometry

    ## 2023-08-17 09:41:03.045752 aggregating flows

    ## 2023-08-17 09:41:03.28658 rejoining segments into linestrings

# Small test

``` r
res = bench::mark(time_unit = "s", check = FALSE,
  original = {overline_old(routes, attrib = "foot")},
  new = {overline3(routes, attrib = "foot")}
)
```

    ## 2023-08-17 09:41:03.547956 constructing segments

    ## 2023-08-17 09:41:04.903392 building geometry

    ## 2023-08-17 09:41:05.311159 simplifying geometry

    ## 2023-08-17 09:41:05.311571 aggregating flows

    ## 2023-08-17 09:41:05.578866 rejoining segments into linestrings

    ## 2023-08-17 09:41:10.37143 constructing segments

    ## 2023-08-17 09:41:11.527714 building geometry

    ## 2023-08-17 09:41:11.844704 simplifying geometry

    ## 2023-08-17 09:41:11.845035 aggregating flows

    ## 2023-08-17 09:41:12.235894 rejoining segments into linestrings

    ## 2023-08-17 09:41:15.413275 constructing segments

    ## 2023-08-17 09:41:16.371987 building geometry

    ## 2023-08-17 09:41:16.663718 simplifying geometry

    ## 2023-08-17 09:41:16.66405 aggregating flows

    ## 2023-08-17 09:41:16.876137 rejoining segments into linestrings

    ## 2023-08-17 09:41:17.162153 constructing segments

    ## 2023-08-17 09:41:18.370308 building geometry

    ## 2023-08-17 09:41:18.723326 simplifying geometry

    ## 2023-08-17 09:41:18.723716 aggregating flows

    ## 2023-08-17 09:41:19.020335 rejoining segments into linestrings

    ## Warning: Some expressions had a GC in every iteration; so filtering is
    ## disabled.

The results are as follows:

``` r
res |>
  dplyr::select(expression, median, mem_alloc) |>
  mutate(routes_per_second = nrow(routes) / median) |>
  knitr::kable()
```

| expression |   median | mem_alloc | routes_per_second |
|:-----------|---------:|----------:|------------------:|
| original   | 1.561284 |     213MB |          640.4983 |
| new        | 2.189923 |     192MB |          456.6370 |

# Large test

``` r
routes = readRDS("/tmp/uptake_commute_fastest.Rds")
r = routes |> 
  slice(seq(10000))
names(r)
```

    ##  [1] "route_number"              "name"                     
    ##  [3] "provisionName"             "distances"                
    ##  [5] "time"                      "quietness"                
    ##  [7] "gradient_smooth"           "geo_code1"                
    ##  [9] "geo_code2"                 "car"                      
    ## [11] "taxi"                      "foot"                     
    ## [13] "bicycle"                   "public_transport"         
    ## [15] "all"                       "dist_euclidean"           
    ## [17] "dist_euclidean_jittered"   "route_id"                 
    ## [19] "splittingID"               "geometry"                 
    ## [21] "route_hilliness"           "length_route"             
    ## [23] "pcycle_go_dutch"           "pcycle_ebike"             
    ## [25] "bicycle_go_dutch"          "bicycle_ebike"            
    ## [27] "mode_ratio_go_dutch"       "mode_ratio_ebike"         
    ## [29] "car_go_dutch"              "public_transport_go_dutch"
    ## [31] "foot_go_dutch"             "car_ebike"                
    ## [33] "public_transport_ebike"    "foot_ebike"

``` r
system.time({
rnet = overline_old(r,
                            attrib = c("bicycle","bicycle_go_dutch","bicycle_ebike","quietness","gradient_smooth"),
                            fun = list(sum = sum, max = first),
                            ncores = 1,
                            regionalise = 1e9)
})
```

    ## 2023-08-17 09:42:23.777888 constructing segments

    ## 2023-08-17 09:42:31.65472 building geometry

    ## 2023-08-17 09:42:32.809003 simplifying geometry

    ## 2023-08-17 09:42:32.809407 aggregating flows

    ## 2023-08-17 09:42:36.679727 rejoining segments into linestrings

    ##    user  system elapsed 
    ##  12.193   1.239  13.469

``` r
system.time({
rnet = overline3(r,
                            attrib = c("bicycle","bicycle_go_dutch","bicycle_ebike","quietness","gradient_smooth"),
                            fun = list(sum = sum, max = first),
                            ncores = 1,
                            regionalise = 1e9)
})
```

    ## 2023-08-17 09:42:39.289945 constructing segments

    ## 2023-08-17 09:42:46.551332 building geometry

    ## 2023-08-17 09:42:47.787907 simplifying geometry

    ## 2023-08-17 09:42:47.788377 aggregating flows

    ## 2023-08-17 09:42:49.824217 rejoining segments into linestrings

    ##    user  system elapsed 
    ##  11.511   0.044  11.107

``` r
bench::mark(check = FALSE, iterations = 1,
            old = {res1 <<- overline_old(r,
                            attrib = c("bicycle","bicycle_go_dutch","bicycle_ebike","quietness","gradient_smooth"),
                            fun = list(sum = sum, max = first),
                            ncores = 1,
                            regionalise = 1e9)
            },
new = {res2 <<- overline3(r,
                            attrib = c("bicycle","bicycle_go_dutch","bicycle_ebike","quietness","gradient_smooth"),
                            fun = list(sum = sum, max = first),
                            ncores = 1,
                            regionalise = 1e9)
}
            )
```

    ## 2023-08-17 09:42:50.545904 constructing segments

    ## 2023-08-17 09:42:57.975437 building geometry

    ## 2023-08-17 09:42:58.99299 simplifying geometry

    ## 2023-08-17 09:42:58.993323 aggregating flows

    ## 2023-08-17 09:43:02.448424 rejoining segments into linestrings

    ## 2023-08-17 09:43:29.905679 constructing segments

    ## 2023-08-17 09:43:36.984241 building geometry

    ## 2023-08-17 09:43:38.07569 simplifying geometry

    ## 2023-08-17 09:43:38.076099 aggregating flows

    ## 2023-08-17 09:43:40.249743 rejoining segments into linestrings

    ## 2023-08-17 09:43:49.839485 constructing segments

    ## 2023-08-17 09:43:56.549587 building geometry

    ## 2023-08-17 09:43:57.601956 simplifying geometry

    ## 2023-08-17 09:43:57.602298 aggregating flows

    ## 2023-08-17 09:44:02.195464 rejoining segments into linestrings

    ## 2023-08-17 09:44:02.867219 constructing segments

    ## 2023-08-17 09:44:11.384976 building geometry

    ## 2023-08-17 09:44:12.857531 simplifying geometry

    ## 2023-08-17 09:44:12.85796 aggregating flows

    ## 2023-08-17 09:44:15.095431 rejoining segments into linestrings

    ## Warning: Some expressions had a GC in every iteration; so filtering is
    ## disabled.

    ## # A tibble: 2 × 6
    ##   expression      min   median `itr/sec` mem_alloc `gc/sec`
    ##   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
    ## 1 old             13s      13s    0.0770    3.41GB    0.539
    ## 2 new           12.9s    12.9s    0.0773  223.49MB    0.386
