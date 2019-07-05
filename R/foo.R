t1list <- list()
t2list <- list()
for(i in 1:10){
  message(paste0(Sys.time(), i))
  ids <- as.character(runif(i * 100, 1e6, 1e7 - 1))
  x <- data.frame(id1 = rep(ids, times = i * 100),
                  id2 = rep(ids, each = i * 100),
                  val = 1,
                  stringsAsFactors = FALSE)
  t1list[[i]] <- system.time(onewayid(x, attrib = "val"))
  t2list[[i]] <- system.time(onewayid2.data.frame(x, attrib = "val"))

}

t1 <- sapply(t1list, `[[`, 3)
t2 <- sapply(t2list, `[[`, 3)
val <- 1:10 * 100

plot(val, t1, type = "l", main = "od_id_order vs szudzik_pairing", xlab = "Number of ids", ylab = "seconds", col = "red")
lines(val, t2, col = "blue")




region = "west-yorkshire"
u = paste0(
  "https://github.com/npct/pct-outputs-regional-notR/raw/master/commute/lsoa/",
   region,
  "/rf.geojson"
)
sl = sf::read_sf(u)

system.time({rnet1 = overline2(sl, "bicycle")})
system.time({rnet2 = overline3(sl, "bicycle")})
identical(rnet1, rnet2)









