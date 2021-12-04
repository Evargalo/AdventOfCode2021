day2A <- read_table("day2A.txt", col_names = FALSE)

# A
day2A %>% filter(X1=="forward") %>% summarise(sum(X2))->f
day2A %>% filter(X1=="up") %>% summarise(sum(X2))->u
day2A %>% filter(X1=="down") %>% summarise(sum(X2))->d
f*(d-u)
# 2147104

# B
pos<-c(0,0,0) # horizontal pos, depth, aim
move<-function(X1,X2){
  pos<<-case_when(
    X1=="up" ~ pos - c(0,0,X2),
    X1=="down" ~ pos + c(0,0,X2),
    X1=="forward" ~ pos + c(X2,X2*pos[3],0)
    )
}
pmap(day2A,move)
pos[1]*pos[2]
# 2044620088
