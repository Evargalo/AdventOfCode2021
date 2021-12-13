setwd("C:/Users/mmajr1/Documents/Perso/AdventOfCode/AoC_2021")

instr <- read_delim("day13test.txt", col_names = FALSE, delim='=')
test <- read_delim("6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0", col_names = FALSE, delim=',')

d <- read_delim("day13A.txt", col_names = FALSE, delim=',')

# A

# x=655

ymax<-max(d$X2)
xmax<-max(d$X1)
n<-nrow(d)

test %>% mutate(y=if_else(X2>7,7*2-X2,X2),z=100*X1+y) %>% filter(!duplicated(z)) %>% nrow
test %>% mutate(y=if_else(X2>7,7*2-X2,X2),z=100*X1+y) %>% filter(!duplicated(z)) %>% gf_tile((-y)~+X1)

names(d)<-c("x","y")
d %>% mutate(xx=x,x=if_else(x>655,655*2-x,x),z=10000*x+y) %>% summarise(sum(!duplicated(z)))

# 693

# B

fold<-function(X1,X2){
  if(X1=="fold along x") d<<-d %>% 
      mutate(xx=x,x=if_else(x>X2,X2*2-x,x),z=10000*x+y) %>% filter(!duplicated(z)) 
  if(X1=="fold along y") d<<-d %>% 
      mutate(yy=y,y=if_else(y>X2,X2*2-y,y),z=10000*x+y) %>% filter(!duplicated(z)) 
}
pmap(instr,fold)

d %>% gf_tile((-y)~x)

# UCLZRAZU

