day5B <- read_csv("day5B.txt", col_names = FALSE)
day5B <-day5B +1

Xmax<-max(day5B$X1,day5B$X3)
Ymax<-max(day5B$X2,day5B$X4)

# A

grid<-matrix(0,nrow = Xmax,ncol = Ymax)

add_dot<-function(X1,X2,X3,X4){
  grid[X1:X3,X2:X4]<<-grid[X1:X3,X2:X4]+1
}
day5B %>% filter(X1==X3 | X2==X4) %>% pmap(add_dot) 
sum(grid>1)
# 5197

# B

grid<-matrix(0,nrow = Xmax,ncol = Ymax)

add_dot<-function(X1,X2,X3,X4){
  if (X1==X3 | X2==X4)  {
    grid[X1:X3,X2:X4]<<-grid[X1:X3,X2:X4]+1
  } else{
    s<-sign(X3-X1)*sign(X4-X2)
    for(k in 0:(X3-X1)){
      grid[X1+k,X2+k*s]<<-grid[X1+k,X2+k*s]+1
    }
  }
}
day5B %>% pmap(add_dot) 
sum(grid>1)
# 18605
