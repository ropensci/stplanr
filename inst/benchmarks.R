library(stplanr)
od = pct::get_od()
create_df <- function(rows, cols) {
  od[1:rows, 1:cols]
}
res = bench::press(
  rows = c(5000, 10000, 15000),
  cols = c(3, 4, 5),
  {
    d = create_df(rows, cols)
    bench::mark(check = FALSE,
      onewayid = onewayid(d, attrib = cols[-c(1:2)]),
      od_oneway = od_oneway(d, attrib = cols[-c(1:2)]),
      od_oneway_char = od_oneway(d, attrib = cols[-c(1:2)], stplanr.key = od_id_character(d[[1]], d[[2]])),
      od_oneway_szud = od_oneway(d, attrib = cols[-c(1:2)], stplanr.key = od_id_szudzik(d[[1]], d[[2]])),
      od_oneway_max = od_oneway(d, attrib = cols[-c(1:2)], stplanr.key = od_id_max_min(d[[1]], d[[2]]))
    )
  }
  )
ggplot2::autoplot(res)
