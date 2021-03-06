d <- read_delim("day13A.txt", col_names = FALSE, delim=',')
instr <- read_delim("day13instr.txt", col_names = FALSE, delim='=')

# A

# fold along x=655

names(d)<-c("x","y")
d %>% mutate(x=if_else(x>655,655*2-x,x),z=10000*x+y) %>% summarise(sum(!duplicated(z)))

# 693

# B

fold<-function(X1,X2){
  if(X1=="fold along x") d<<-d %>% 
      mutate(x=if_else(x>X2,X2*2-x,x),z=10000*x+y) %>% filter(!duplicated(z)) 
  if(X1=="fold along y") d<<-d %>% 
      mutate(y=if_else(y>X2,X2*2-y,y),z=10000*x+y) %>% filter(!duplicated(z)) 
}
pmap(instr,fold)

d %>% gf_tile((-y)~x)

# UCLZRAZU

# Nicer

d %>% gf_tile((-y)~x,fill = ~x,show.legend = FALSE)+
  gf_theme(theme_void())+gf_refine(viridis::scale_fill_viridis(discrete = FALSE))
