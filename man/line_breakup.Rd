% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/line_breakup.R
\name{line_breakup}
\alias{line_breakup}
\title{Break up line objects into shorter segments}
\usage{
line_breakup(l, z)
}
\arguments{
\item{l}{An sf object with LINESTRING geometry}

\item{z}{An sf object with \code{POLYGON} geometry or a number representing the
resolution of grid cells used to break up the linestring objects}
}
\value{
An sf object with LINESTRING geometry created after breaking up the
input object.
}
\description{
This function breaks up a LINESTRING geometries into smaller pieces.
}
\examples{
library(sf)
z <- zones_sf$geometry
l <- routes_fast_sf$geometry[2]
l_split <- line_breakup(l, z)
l
l_split
sf::st_length(l)
sum(sf::st_length(l_split))
plot(z)
plot(l, add = TRUE, lwd = 9, col = "grey")
plot(l_split, add = TRUE, col = 1:length(l_split))
}
\seealso{
Other lines: 
\code{\link{angle_diff}()},
\code{\link{geo_toptail}()},
\code{\link{is_linepoint}()},
\code{\link{line2df}()},
\code{\link{line2points}()},
\code{\link{line_bearing}()},
\code{\link{line_midpoint}()},
\code{\link{line_segment}()},
\code{\link{line_segment1}()},
\code{\link{line_via}()},
\code{\link{mats2line}()},
\code{\link{n_segments}()},
\code{\link{n_vertices}()},
\code{\link{onewaygeo}()},
\code{\link{points2line}()},
\code{\link{toptail_buff}()}
}
\concept{lines}
