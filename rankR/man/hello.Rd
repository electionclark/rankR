\name{tabulatR}
\alias{tabulatR}
\title{tabulates RCV data to find the winner based off of Maine rules}
\usage{
tabulatR()
}
\description{
Outputs the winner and how many allocation rounds it took to find a winner.
}
\examples{
tabulatR(data, firstrank, finalrank, totalranks)
}

\name{rcv_approval}
\alias{rcv_approval}
\title{Calculates approval for a the winning candidate.}
\usage{
rcv_approval()
}
\description{
Outputs a vector of support; 1 if a voter ranked the winner, 0 if not. Requres the output from tabulatR function.
}
\examples{
rcv_approval(data)
}
