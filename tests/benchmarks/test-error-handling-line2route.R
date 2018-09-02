devtools::install_github("nikolai-b/stplanr", ref = "add_error_handelling")
r <- line2route(flowlines, reporterror = TRUE)
r$error # shows error

# now switch off internet partway (manually)
r <- line2route(flowlines, reporterror = TRUE)

r2 <- stplanr:::line2routeRetry(flowlines, silent = F)
