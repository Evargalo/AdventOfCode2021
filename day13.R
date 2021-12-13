d <- read_delim("day13A.txt", col_names = FALSE, delim=',')
instr <- read_delim("day13instr.txt", col_names = FALSE, delim='=')

# A

# fold along x=655

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

