### rank functions

maine<- readRDS("maine_rcv.rData")

#rcv_tabulator<- function(data){

data_facsim<- maine ### make facsimile of maine

round<- as.data.frame(table(data_facsim$choice_1))
round$Var1<- as.character(round$Var1)
colnames(round)[1]<- "Candidate"
colnames(round)[2]<- "votes"
cands<- unique(round$Candidate)
cands<- cands[!cands %in% c("undervote", "overvote")]

round$initial_percent<- round$votes / nrow(maine)

data_facsim$choice_1<-ifelse(data_facsim$choice_1 == "undervote",  data_facsim$choice_2, data_facsim$choice_1)
data_facsim<-data_facsim[!data_facsim$choice_1 == "overvote",]
data_facsim<-data_facsim[!data_facsim$choice_1 == "undervote",]
round


data_facsim$initialvote<-data_facsim$choice_1

loser<- round$Candidate[which.min(round$votes[round$Candidate %in% cands])]
data_facsim$loser <- ifelse(data_facsim$initialvote == loser, 1, 0)
data_facsim$choice_1<- ifelse(data_facsim$loser == 1, data_facsim$choice_2, data_facsim$choice_1)
data_facsim$choice_2<- ifelse(data_facsim$loser == 1, data_facsim$choice_3, data_facsim$choice_2)
data_facsim$choice_3<- ifelse(data_facsim$loser == 1, "exhausted", data_facsim$choice_3)



i = 1
round2<- as.data.frame(table(data_facsim$choice_1))
alloc<- paste0("round", i)
colnames(round2)[2]<- alloc
round2$Var1 <- as.character(round2$Var1 )
round2$round1[round2$Var1 == "overvote"] <- round2$round1[round2$Var1 == loser] + round2$round1[round2$Var1 == "overvote"]
round2$round1[round2$Var1 == loser]<- 0


total_roundi<- sum(round2$round1[round2$Var1 %in% cands])

round2$Var1[(round2$round1 / total_roundi) > .5]

