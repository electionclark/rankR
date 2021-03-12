#' ranked-chioce voting tabulation
#'
#' This function takes ranked-choice ballot images and tabulates winners and othermetrics
#' data, as acquired from Social Explorer.
#'
#' @param data The data frame for the rcv ballots.
#' @param firstrank The number of the column where the first ranked candidate for each voter is found
#' @param finalrank the number of the column where the last ranked candidate for each voter is found
#' @param totalranks The number of candidates on the ballot.
#' @param surname_field The column name for the surname that the wru package will use to merge on the surname probabilities. Note that they
#' must all be uppercase, and if not already, the column will be renamed to surname
#' @return A list of objects, including the tabulations.
#' }
#'
#' @export
#' @examples
#' maine_data<- tabulatR(data = maine, firstrank = 3, finalrank = 5, totalranks = 3)
#' approved<- approval(maine)
#'


tabulatR<- function(data, firstrank, finalrank, totalranks){
  data_facsim<- data ### make facsimile of data

  #### initial tabulation
  data_facsim[,firstrank]<-ifelse(data_facsim[,firstrank] == "undervote",  data_facsim[,(firstrank+1)], data_facsim[,firstrank])
  data_facsim<-data_facsim[!data_facsim[,firstrank] == "overvote",]
  data_facsim<-data_facsim[!data_facsim[,firstrank] == "undervote",]

  data_facsim$initialvote<-data_facsim[,firstrank]

  counts<- list()
  initialcount<- NULL
  winner<- NULL

  for(i in 1:(totalranks-1)){

    count<-as.data.frame(table(data_facsim[,firstrank]))
    count$Var1<- as.character(count$Var1)
    colnames(count)[1]<- "Candidate"
    colnames(count)[2]<- "votes"
    initialcount<- count

    cands<- unique(count$Candidate)
    cands<- cands[!cands %in% c("undervote", "overvote")]
    loser<- count$Candidate[which.min(count$votes[count$Candidate %in% cands])]
    data_facsim$loser<- ifelse(data_facsim[,firstrank] == loser, 1, 0)
    data_facsim[,firstrank]<- ifelse(data_facsim$loser == 1, data_facsim[,(firstrank +1)], data_facsim[,firstrank])

    for(j in 1:(totalranks-1)){
      #### loop through and reallocate here
      data_facsim[,(firstrank+ j)]<- ifelse(data_facsim$loser == 1, data_facsim[,(firstrank+ j + 1)], data_facsim[,(firstrank+ j)])
    }

    count<-as.data.frame(table(data_facsim[,firstrank]))
    count$Var1<- as.character(count$Var1)
    colnames(count)[1]<- "Candidate"
    colnames(count)[2]<- "votes"
    count$votes[count$Candidate == loser]<- 0
    total_roundi<- sum(count$votes[count$Candidate %in% cands])
    counts[[i]]<- count

    winner<<-(count$Candidate[(count$votes/ total_roundi) > .5])
    winner<- (count$Candidate[(count$votes/ total_roundi) > .5])
    tabulationround<- i
    output<- list(initialcount, counts, winner, tabulationround)
    finalcount<<- counts

    ifelse(length(count$Candidate[(count$votes/ total_roundi) > .5]) > 0, return(output), print("Next round!"))

  }
}

rcv_approval<- function(data){
  approval<- as.numeric(apply(data, 1, function(r) any(r %in% c(winner))))
  approval<<-as.numeric(apply(data, 1, function(r) any(r %in% c(winner))))
  return(approval)
}

pca_scalr <- function (voter_idealpoint, candidates ){
  print("Making the binary choice dataset. This may take several minutes.")
  unique_vec<- c(letters[1:candidates])
  col_namen<- c()
  for(i in 1:length(unique_vec)){
    df <- as.data.frame(permutations(n=length(unique_vec), r=2, v=unique_vec))
    col_namen<- c(col_namen, (paste0(df$V1, df$V2, i)))
  }


  binary_mat<- data.frame(matrix(0, nrow = nrow(voter_idealpoint), ncol = length(col_namen)))
  colnames(binary_mat)<- col_namen



  for(i in 1:nrow(voter_idealpoint)){
    for(j in 1:ncol(voter_idealpoint)){
      for(m in 1:ncol(binary_mat)){
        binary_mat[i,m]<- ifelse(voter_idealpoint[i, j] == substr(colnames(binary_mat)[m], 1, 1) & j == readr::parse_number(colnames(binary_mat)[m]), 1, binary_mat[i,m] )
      }
    }
  }


  #### run PCA
  #### add candidates, run pca again. See if paramteres recovered

  a1<- prcomp(binary_mat)
  voter_idealpoint_pca<- voter_idealpoint
  voter_idealpoint_pca$pr_1<- a1$x[,1]
  a1<<- a1
  voter_idealpoint_pca<<- voter_idealpoint_pca
  return(print("PCA Scaling Complete"))
}

