% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/madantextnetwork.r
\name{cluster.graph}
\alias{cluster.graph}
\title{Cluster a Graph and Extract Largest Component}
\usage{
cluster.graph(network)
}
\arguments{
\item{network}{A graph object.}
}
\value{
A list containing three elements: 'gr' with the largest connected
component of the graph, 'cl' with a data frame of nodes and their
cluster membership, and 'node.impo' with a data frame of node
importance measures like degree, closeness, and betweenness.
}
\description{
This function applies clustering to a graph and extracts the largest
connected component.
}
\examples{
\dontrun{
  # Assuming 'network' is a predefined graph object
  cluster.graph(network)
}
}
