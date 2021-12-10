
day10A <- read_delim("day10A.txt", col_names = FALSE, delim=' ')
test <- read_delim("day10test.txt", col_names = FALSE, delim=' ')

# A

op<-c("(","[","{","<")
cl<-c(")","]","}",">")

calcCor<-function(s) {
  vect<-s %>% stringToVector
  Opens<-c()
  for(l in vect){
    if(l %in% op) Opens<-c(Opens,l)
    if(l %in% cl) {
      lo<-Opens[length(Opens)]  
      if(which(op==lo)!=which(cl==l)) return(l)
      if(which(op==lo)==which(cl==l)) Opens<-Opens[1:(length(Opens)-1)] 
    }
  }
  return("ok")
}

day10A %>% rowwise() %>% mutate(reps=calcCor(X1)) %>% group_by(reps) %>% count %>% ungroup %>% 
  bind_cols(val=c(3,57,1197,25137,0)) %>%  summarise(A=n*val) %>% summarise(sum(A))

# 392043


# B

score<-function(Opens){
  v<-rev(Opens)
  sc<-0
  for(c in v){
    sc<-(5*sc)+which(op==c)
  }
  return(sc)
}

calcScore<-function(s) {
  vect<-s %>% stringToVector
  Opens<-c()
  for(l in vect){
    if(l %in% op) Opens<-c(Opens,l)
    if(l %in% cl) {
      lo<-Opens[length(Opens)]  ; 
      if(which(op==lo)!=which(cl==l)) return(0)
      if(which(op==lo)==which(cl==l)) Opens<-Opens[-(length(Opens))] 
    }
  }
  return(score(Opens))
}

day10A %>% rowwise() %>% mutate(reps=calcScore(X1)) %>% ungroup %>% filter(reps!=0) %>% 
  summarise(median(reps))

# 1605968119



########################################################################################################

################################################
# faster by reducing strings with gsub()
################################################

day10A <- read_delim("day10A.txt", col_names = FALSE, delim=' ')
test <- read_delim("day10test.txt", col_names = FALSE, delim=' ')

# A

op<-c("(","[","{","<")
cl<-c(")","]","}",">")

redString<-function(s){
  n<-nchar(s)+1
  while(nchar(s)<n){
    n<-nchar(s)
    gsub("[(][)]|[[][]]|[{][}]|[<][>]","",s)->s
  }
  s
}
redString(test[1,])

isCor<-function(s) {
  s %>% redString %>% stringToVector ->s
  ifelse(any(s %in% cl),(s[s %in% cl])[1],"ok")
}

day10A %>% rowwise() %>% mutate(cor=isCor(X1)) %>% group_by(cor) %>% count %>% ungroup %>% 
  bind_cols(val=c(3,57,1197,25137,0)) %>%  summarise(A=n*val) %>% summarise(sum(A))

# 392043

# B

score<-function(Opens){
  v<-rev(Opens)
  sc<-0
  for(c in v){
    sc<-(5*sc)+which(op==c)
  }
  return(sc)
}

day10A %>% rowwise() %>% mutate(s=redString(X1)) %>%
  filter(!any((s %>% stringToVector) %in% cl)) %>% mutate(score=score(s%>% stringToVector)) %>% ungroup %>% 
  summarise(median(score))

# 1605968119




