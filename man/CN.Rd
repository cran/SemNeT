% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CN.R
\name{CN}
\alias{CN}
\title{Community Network Estimation}
\usage{
CN(data, window = 2, alpha = 0.05, enrich = FALSE)
}
\arguments{
\item{data}{Matrix or data frame.
A preprocessed verbal fluency matrix where
rows are participants and columns are verbal fluency
responses}

\item{window}{Numeric.
Size of window to look for co-occurences in.
Defaults to \code{2}}

\item{alpha}{Numeric.
Significance value.
Defaults to \code{.05}}

\item{enrich}{Boolean.
Should the network be enriched by connecting
all nodes in their respective modules?
Defaults to \code{FALSE}}
}
\value{
Returns a undirected semantic network
}
\description{
Estimates a semantic network using the Community Network
method described in Goni et al. (2011)
}
\examples{
# Get data
data <- open.clean

# Organize group data
## Get group data
group <- open.group

## Low and high openness to experience groups
low <- data[which(group == "Low"),]
high <- data[which(group == "High"),]

\dontrun{
# Compute networks
low.net <- CN(low)
high.net <- CN(high)
}

}
\references{
Goni, J., Arrondo, G., Sepulcre, J., Martincorena, I., de Mendizabal, N. V., Corominas-Murtra, B., ... & Villoslada, P. (2011).
The semantic organization of the animal category: Evidence from semantic verbal fluency and network theory.
\emph{Cognitive Processing}, \emph{12}, 183-196.
}
\author{
Alexander Christensen <alexpaulchristensen@gmail.com>
}
