% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linefuns.R
\name{n_vertices}
\alias{n_vertices}
\title{Retrieve the number of vertices in sf objects}
\usage{
n_vertices(l)
}
\arguments{
\item{l}{An sf object with LINESTRING geometry}
}
\description{
Returns a vector of the same length as the number of sf objects.
}
\examples{
l <- routes_fast_sf
n_vertices(l)
n_vertices(zones_sf)
}
\seealso{
Other lines: 
\code{\link{angle_diff}()},
\code{\link{geo_toptail}()},
\code{\link{is_linepoint}()},
\code{\link{line2df}()},
\code{\link{line2points}()},
\code{\link{line_bearing}()},
\code{\link{line_breakup}()},
\code{\link{line_midpoint}()},
\code{\link{line_segment}()},
\code{\link{line_segment1}()},
\code{\link{line_via}()},
\code{\link{mats2line}()},
\code{\link{n_segments}()},
\code{\link{onewaygeo}()},
\code{\link{points2line}()},
\code{\link{toptail_buff}()}
}
\concept{lines}
