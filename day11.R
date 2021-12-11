# test <- read_delim("day11test.txt", col_names = FALSE, delim=' ')
# df<-df_from_vector_of_strings(test$X1) %>% mutate_all(as.numeric)

day11A <- read_delim("day11A.txt", col_names = FALSE, delim=' ')
df<-df_from_vector_of_strings(day11A$X1) %>% mutate_all(as.numeric)

d<-expand(tibble(x=1:10,y=1:10),x,y) %>% rowwise %>%
  mutate(fish=df[x,y]) %>% ungroup %>% 
  mutate(xy=1000*x+y)

# A

incNeighbours<-function(x,y,fish,xy){
  z<-xy+c(1000,1001,1,-999,-1000,-1001,-1,999)
  d %>% rowwise %>% mutate(fish=if_else(xy %in% z,
                                        fish+1,
                                        fish)) %>% 
    ungroup ->> d
}

step<-function(){
  d<<-d %>% mutate(fish=fish+1)
  totalFlashes<-tibble(x=numeric(),y=numeric(),fish=numeric(),xy=numeric())
  flash<-d %>% filter(fish>9 & !(xy %in% totalFlashes$xy))
  while(nrow(flash)>0) {
    totalFlashes<-totalFlashes %>% bind_rows(flash)
    pmap(flash,incNeighbours)  
    flash<-d %>% filter(fish>9 & !(xy %in% totalFlashes$xy))
  }
  d$fish[d$xy %in% totalFlashes$xy]<<-0
  return(nrow(totalFlashes))
}

totalFlashes<-0
for(i in 1:100) {
  k<-step()
  if(k==100) print(i) # in case, for step B
  totalFlashes<-totalFlashes+k
}
totalFlashes

# 1679

# B

d<-expand(tibble(x=1:10,y=1:10),x,y) %>% rowwise %>%
  mutate(fish=df[x,y]) %>% ungroup %>% 
  mutate(xy=1000*x+y)

i<-1 ; while(step()<100) {i<-i+1} ; i

# 519
