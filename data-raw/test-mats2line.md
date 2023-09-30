
This article was written following new functionality added to the
`{stplanr}` package by Josiah Parry, in [this pull request
(`#540`)`)](https://github.com/ropensci/stplanr/pull/540). As described issue [`\#539\`](https://github.com/ropensci/stplanr/issues/539),
the original implementation was slower than needed.

This short article describes what the function does, benchmarks
alternative implementation, and concludes with some thoughts about the
`{stplanr}` package. It was written straight after the new functionality
was implemented, so we’ll start by installing that version of the
package:

``` r
remotes::install_github(repo = "ropensci/stplanr", ref = "39858b6")
```

    ## Using github PAT from envvar GITHUB_PAT

    ## Skipping install of 'stplanr' from a github remote, the SHA1 (39858b69) has not changed since last install.
    ##   Use `force = TRUE` to force installation

Loading the package allows us to run the function.

``` r
library(stplanr)
```

`mats2line()` is a function that takes 2 sets of coordinates and
converts them into linestrings. A simple example is shown function’s
online
[documentation](https://docs.ropensci.org/stplanr/reference/mats2line.html)
hosted by community science support organsiation rOpenSci:

``` r
m1 <- matrix(c(1, 2, 1, 2), ncol = 2)
m2 <- matrix(c(9, 9, 9, 1), ncol = 2)
l <- mats2line(m1, m2)
class(l)
```

    ## [1] "sfc_LINESTRING" "sfc"

``` r
l
```

    ## Geometry set for 2 features 
    ## Geometry type: LINESTRING
    ## Dimension:     XY
    ## Bounding box:  xmin: 1 ymin: 1 xmax: 9 ymax: 9
    ## CRS:           NA

    ## LINESTRING (1 1, 9 9)

    ## LINESTRING (2 2, 9 1)

``` r
lsf <- sf::st_sf(l, crs = 4326)
class(lsf)
```

    ## [1] "sf"         "data.frame"

``` r
plot(lsf)
```

![](test-mats2line_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

The previous version of `mats2line()` as as follows, which shows how it
works (see the new implementation in [source
code](https://github.com/ropensci/stplanr/blob/39858b69ba1f4ea978a46aec3df0086b9304ce24/R/line_via.R#L19-L47)):

``` r
mats2line_old <- function(mat1, mat2, crs = NA) {
  l <- lapply(1:nrow(mat1), function(i) {
    mat_combined <- rbind(mat1[i, ], mat2[i, ])
    sf::st_linestring(mat_combined)
  })
  if(is.na(crs)) {
    sf::st_sfc(l)
  } else {
    sf::st_sfc(l, crs = crs)
  }
}
```

The new function is as follows:

``` r
mats2line
```

    ## function (mat1, mat2, crs = NA) 
    ## {
    ##     if (nrow(mat1) != nrow(mat2)) {
    ##         rlang::abort("`mat1` and `mat2` must have the same number of rows.")
    ##     }
    ##     else if (ncol(mat1) != 2 || ncol(mat2) != 2) {
    ##         rlang::abort("`mat1` and `mat2` must have exactly 2 columns")
    ##     }
    ##     m <- vctrs::vec_interleave(mat1, mat2)
    ##     m <- cbind(m, rep.int(1:nrow(mat1), rep(2, nrow(mat1))))
    ##     colnames(m) <- c("x", "y", "id")
    ##     res <- sfheaders::sfc_linestring(m, x = "x", y = "y", linestring_id = "id")
    ##     sf::st_set_crs(res, crs)
    ## }
    ## <bytecode: 0x55df6fb50b10>
    ## <environment: namespace:stplanr>

We can check the 2 results are similar as follows:

``` r
l_old <- mats2line_old(m1, m2)
waldo::compare(l_old, l)
```

    ## ✔ No differences

As shown, the 2 results are identical.

Let’s do a quick benchmark:

``` r
bench::mark(
  mats2line_old(m1, m2),
  mats2line(m1, m2)
)
```

    ## # A tibble: 2 × 6
    ##   expression                 min   median `itr/sec` mem_alloc `gc/sec`
    ##   <bch:expr>            <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
    ## 1 mats2line_old(m1, m2)    110µs    114µs     8574.  105.54KB     16.9
    ## 2 mats2line(m1, m2)        118µs    123µs     7910.    3.01KB     15.0

From that you may think there’s no benefit to speeding things up. But,
when you look at the benchmark results for larger matrices, you can see
the difference (as shown in the initial issue
[`#539`](https://github.com/ropensci/stplanr/issues/539):

``` r
m1 <- matrix(rnorm(20000), ncol = 2)
m2 <- matrix(runif(20000), ncol = 2)
bench::mark(
  mats2line_old(m1, m2),
  mats2line(m1, m2)
)
```

    ## Warning: Some expressions had a GC in every iteration; so filtering is
    ## disabled.

    ## # A tibble: 2 × 6
    ##   expression                 min   median `itr/sec` mem_alloc `gc/sec`
    ##   <bch:expr>            <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
    ## 1 mats2line_old(m1, m2)  217.4ms  217.6ms      4.50  518.21KB     25.5
    ## 2 mats2line(m1, m2)       14.1ms   17.6ms     41.5     4.01MB     15.8

The results show that the new implementation is more than 10x faster.
But that’s not the end of the story. There is another implementation, in
the package `{od}`, which works as follows:

``` r
odc <- cbind(m1[1:3, ], m2[1:3, ])
desire_lines <- od::odc_to_sfc(odc)
waldo::compare(desire_lines, mats2line(m1[1:3, ], m2[1:3, ]))
```

    ## ✔ No differences

``` r
bench::mark(
  mats2line_old(m1, m2),
  mats2line(m1, m2),
  od::odc_to_sfc(cbind(m1, m2))
)
```

    ## Warning: Some expressions had a GC in every iteration; so filtering is
    ## disabled.

    ## # A tibble: 3 × 6
    ##   expression                         min   median `itr/sec` mem_alloc `gc/sec`
    ##   <bch:expr>                    <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
    ## 1 mats2line_old(m1, m2)          215.7ms    228ms      4.44  518.21KB     17.7
    ## 2 mats2line(m1, m2)               14.2ms   18.9ms     50.9     4.01MB     17.6
    ## 3 od::odc_to_sfc(cbind(m1, m2))   15.7ms     19ms     49.3     5.01MB     11.8

Given the advantages of modularity, and that the purpose of the `{od}`
package is to work with origin-destination data, it makes sense to use
the `{od}` implementation.

The `{od}` implementation uses the following:

``` r
od_coordinates_ids = function(odc) {
  res = data.frame(id = rep(1:nrow(odc), each = 2), x = NA, y = NA)
  ids_odd = seq(1, nrow(res), by = 2)
  ids_even = ids_odd + 1
  res[ids_odd, c("x", "y")] = odc[, 1:2]
  res[ids_even, c("x", "y")] = odc[, 3:4]
  res
}
od_coordinates_ids(odc)
```

    ##   id           x          y
    ## 1  1  0.54634963 -1.9157423
    ## 2  1  0.38803029  0.9626673
    ## 3  2 -0.75469220  0.4153730
    ## 4  2  0.02065935  0.3727175
    ## 5  3 -1.03140234 -0.1922811
    ## 6  3  0.16871653  0.7634995

Let’s see if we can speed that up:

``` r
od_coordinates_ids2 = function(odc) {
  res = vctrs::vec_interleave(odc[, 1:2], odc[, 3:4])
  res = data.frame(id = rep(1:nrow(odc), each = 2), x = res[, 1], y = res[, 2])
  res
}
waldo::compare(
  od_coordinates_ids(odc),
  od_coordinates_ids2(odc)
)
```

    ## ✔ No differences

``` r
odc = cbind(m1, m2)
bench::mark(
  od = od_coordinates_ids(odc),
  vctrs = od_coordinates_ids2(odc)
)
```

    ## # A tibble: 2 × 6
    ##   expression      min   median `itr/sec` mem_alloc `gc/sec`
    ##   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
    ## 1 od           2.75ms   2.84ms      343.    3.94MB     33.9
    ## 2 vctrs      876.61µs 902.94µs     1092.    1.74MB     50.8

<!-- 
&#10;
```r
remotes::install_cran("rsgeo")
```
&#10;
&#10;```r
packageVersion("rsgeo")
```
&#10;
The new implementation uses the `{rsgeo}` package by default, which can be installed as follows:
&#10;If this is the first time installing the package and you have Rust installed, it may take some time.
This is because the package is compiled from source code, which is a bit slower than installing a pre-compiled binary.
&#10;You can check which version of the package you have installed as follows (`{rsgeo}` version 0.1.6 or later is required):
-->
