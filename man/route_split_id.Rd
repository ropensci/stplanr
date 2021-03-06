% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/node-funs.R
\name{route_split_id}
\alias{route_split_id}
\title{Split route based on the id or coordinates of one of its vertices}
\usage{
route_split_id(r, id = NULL, p = NULL)
}
\arguments{
\item{r}{An \code{sf} object with one feature containing a linestring geometry to be split}

\item{id}{The index of the point on the number to be split}

\item{p}{A point represented by an \code{sf} object the will split the \code{route}}
}
\description{
Split route based on the id or coordinates of one of its vertices
}
\examples{
sample_routes <- routes_fast_sf[2:6, 3]
r <- sample_routes[2, ]
id <- round(n_vertices(r) / 2)
r_split <- route_split_id(r, id = id)
plot(r$geometry, lwd = 9, col = "grey")
plot(r_split, col = c("red", "blue"), add = TRUE)
}
