stplanr 0.0.3
----------------------------------------------------------------

NEW FEATURES

* Addition of new class definition `SpatialLinesNetwork` and functions
  (including `plot.SpatialLinesNetwork`, `sum_network_routes`, `find_network_nodes`)
  allowing fastest route calculations via igraph and other network analysis
  functions.

* Functions for removing beginning and end of lines: `toptail` and
  `toptailgs` (@richardellison). Helper functions `buff_geo`,
  `crs_select_aeq` and `line2points` added.

* Functionality for reading in the UK's stats19 data: `read_stats19_*`
  functions download, unzip and re-categorise the data.

* `read_table` functions added for reading Australian OD data
  (@richardellison).

* `decode_gl` added to decode Google polylines (@richardellison)
  and other functions for querying and reading data from OSRM
  services.

* `gtfs2sldf` added to import GTFS routes as SpatialLinesDataFrames
  (@richardellison).

stplanr 0.0.2
----------------------------------------------------------------

* Published on CRAN