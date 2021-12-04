day4A <- read_table("day4A.txt", col_names = FALSE)

numbers<-c(93,35,66,15,6,51,49,67,16,77,80,8,1,57,99,92,14,9,13,23,33,11,43,50,60,96,40,25,22,39,56,18,2,7,34,68,26,90,75,41,4,95,71,30,42,5,46,55,27,98,79,12,65,73,29,28,17,48,81,32,59,63,85,91,52,21,38,31,61,83,97,62,44,70,19,69,36,47,74,58,78,24,72,0,10,88,37,87,3,45,82,76,54,84,20,94,86,53,64,89)

# A

check_win<-function(i) {
  grid<-found [(5*i-4):(5*i),]
  if(any(colSums(grid)==5 | any(rowSums(grid)==5))) {
    board<- day4A[(5*i-4):(5*i),]
    return (sum(board[!grid]) )}
  0
}

wins<-c(grid=0,value=0,s=0)
found<-matrix(FALSE,nrow = 500, ncol = 5)

stop<-FALSE
for(n in numbers){
  found[day4A==n]<-TRUE
  for(i in 1:100){
    c<-check_win(i)
    if(c>0) {
      wins<-c(i,n,c)
      stop<-TRUE
    }
  }
  if(stop) break
}
wins[2]*wins[3]

# 27027



# B


wins<-c(grid=0,value=0,s=0)
found<-matrix(FALSE,nrow = 500, ncol = 5)
winners<-1:100

for(n in numbers){
  found[day4A==n]<-TRUE
  for(i in winners){
    c<-check_win(i)
    if(c>0) {
      wins<-c(i,n,c)
      winners<-winners[winners!=i]
    }
  }
  if(length(winners)==0) break
}
wins[2]*wins[3]

# 36975

