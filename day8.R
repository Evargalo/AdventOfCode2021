day8A <- read_table("day8A.txt", col_names = FALSE)
longtest <- read_table("day8test.txt", col_names = FALSE)

# A

day8A %>% select(12:15) %>% unlist %>% nchar ->l
sum(l %in% c(2:4,7))

# 355

# B

# Matrix of correspondance between segments 1:7 and letters a:g
corresp <- matrix(1,ncol = 7,nrow = 7) %>% as.data.frame()
colnames(corresp)<-letters[1:7]

# Update corresp given a hint s : Put 0's for impossibilities
use<-function(s){
  # if(s=="|") break
  lettres<-stringToVector(s)
  which(letters %in% lettres) -> nombres
  which( ! letters[1:7] %in% lettres) -> autresNombres
  n<-nchar(s)
  if(n==2) {corresp[c(1,2,4,5,7),nombres]<-0 ; corresp[c(3,6),autresNombres]<-0 }
  if(n==3) {corresp[c(2,4,5,7),nombres]<-0 ; corresp[c(1,3,6),autresNombres]<-0 }
  if(n==4) {corresp[c(1,5,7),nombres]<-0 ; corresp[c(2,4,3,6),autresNombres]<-0 }
  if(n==5) {corresp[c(1,4,7),autresNombres]<-0 }
  if(n==6) {corresp[c(1,2,6,7),autresNombres]<-0 }
  corresp<<-corresp
}

# transform the message s into a number according to corresp
interpret<-function(s,corresp){
  n<-nchar(s)
  if(n==2) return(1)
  if(n==3) return(7)
  if(n==4) return(4)
  if(n==7) return(8)
  lettres<-stringToVector(s)
  which(letters %in% lettres) -> nombres
  corresp[,nombres] %>% t() %>% as.data.frame() %>% summarise_all(sum) %>% unlist -> nombres
  if(n==6 & nombres[4]==0) return(0)
  if(n==6 & nombres[5]==0) return(9)
  if(n==6 & nombres[3]==0) return(6)
  if(n==5 & nombres[2]==1) return(5)
  if(n==5 & nombres[5]==1) return(2)
  return(3)
}

# use a line of the input to produce a four-digits number 
decode<-function(tab){
  corresp <<- matrix(1,ncol = 7,nrow = 7) %>% as.data.frame()
  colnames(corresp)<<-letters[1:7]
  sapply(tab[,1:10],use)
  for(j in 1:7){
    if(sum(corresp[,j])==1) {
      i<-which(corresp[,j]==1)
      corresp[i,]<-0
      corresp[i,j]<-1
    }
  }
  for(i in 1:7){
    if(sum(corresp[i,])==1) {
      j<-which(corresp[i,]==1)
      corresp[,j]<-0
      corresp[i,j]<-1
    }
  }
  (sapply(tab[12:15],interpret,corresp) * 10^(3:0) ) %>% sum
}

v<-0
for(i in 1:nrow(day8A)){
  d<-decode(day8A[i,])
  v<-v+d
}
v
