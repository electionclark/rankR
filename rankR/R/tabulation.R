### rank functions

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
  approval<- as.numeric(apply(maine, 1, function(r) any(r %in% c(winner))))
  approval<<-as.numeric(apply(maine, 1, function(r) any(r %in% c(winner))))
  return(approval)
}


