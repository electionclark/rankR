\name{pca_scalr}
\alias{pca_scalr}
\title{Calculates ideal points for all voters in an RCV election.}
\usage{
pca_scalr()
}
\description{
Outputs a copy of the RCV dataset, with a new column featuring the first principle component. First, it takes the ranked choice dataset and breaks it into a binary choice metrhic (See Thurston 1927) before running a principle componenet analysis on the binary choices.
}
\examples{
pca_scalr(data, candidates)
}
