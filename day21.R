
# Part A

pos<-c(9,3)-1
scores<-c(0,0)
die<-1:100
pl<-1
nbRoll<-0
while(all(scores<1000)){
  nbRoll<-nbRoll+3
  pos[pl]<-(pos[pl]+sum(die[1:3]))%%10
  die<-die[c(4:100,1:3)]
  scores[pl]<-scores[pl]+pos[pl]+1
  pl<-3-pl
}

nbRoll*min(scores)
# 1073709


# Part B

possibleUniv<-data.frame(x=0,y=0,move=1,posX=8,posY=2,w=1,end=FALSE)
addUnivs<-function(x,y,move,posX,posY,w){
  if(move==1){
    ny<-y
    npy<-posY
    npx<-(posX+(3:9)) %% 10
    nx<-x+npx+1
    nmove<-2
    nend<-nx>20
  }
  if(move==2){
    nx<-x
    npx<-posX
    npy<-(posY+(3:9)) %% 10
    ny<-y+npy+1
    nmove<-1
    nend<-ny>20
  }
  nw<-w*(c(1,3,6,7,6,3,1)) # possibilities to score 3:9 in three rolls
  data.frame(x=nx,y=ny,posX=npx,posY=npy,move=nmove,w=nw,end=nend,done=nend)
}

# store ends of game
finish<-possibleUniv %>% filter(end)
# Calc possible universes
while(!all(possibleUniv$end) & nrow(possibleUniv)<100000){
  print(nrow(possibleUniv))
  finish<-finish %>% bind_rows(possibleUniv %>% filter(end)) %>% group_by(x,y,posX,posY,move,end) %>% summarise(w=sum(w)) %>% ungroup
  possibleUniv<-possibleUniv %>% filter(!end) %>% select(-end)
  possibleUniv<-pmap_dfr(possibleUniv,addUnivs) %>% group_by(x,y,posX,posY,move,end) %>% summarise(w=sum(w)) %>% ungroup
}
# add the last results
finish<-finish %>% bind_rows(possibleUniv %>% filter(end)) %>% group_by(x,y,posX,posY,move,end) %>% summarise(w=sum(w)) %>% ungroup

finish$w %>% max
# nb winning universes for each player
finish %>% mutate(w=as.double(w),xWins=(x>20)) %>% 
  group_by(xWins) %>% summarise(sum(w)) ->res
# for displaying result:
res-140000000000000
paste0(round(max(res),-11)%/%10^11,max(res)-round(max(res),-11))
# 148747830493442

