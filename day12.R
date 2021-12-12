setwd("C:/Users/mmajr1/Documents/Perso/AdventOfCode/AoC_2021")

day12A <- read_delim("day12testSimple.txt", col_names = FALSE, delim='-')
day12A <- read_delim("day12test.txt", col_names = FALSE, delim='-')
day12A <- read_delim("day12A.txt", col_names = FALSE, delim='-')

# A
day12A %>% filter(X1=="start" | X2=="start")
day12A %>% filter(X1=="end" | X2=="end")

path<-c("start")
cave<-c("start")
paths<-data.frame(path=path,cave=path)

maj<-c("A")
maj<-c("HN","LN")
maj<-c("BN","LP","PK","EP")

continue<-function(path,cave){
  unlist(str_split(path,'-'))->prevCaves
  newPaths<-data.frame(path=character(),cave=character())
  d$X2[day12A$X1==cave] -> nextCaves
  for(c in nextCaves) {
    if(c %in% maj | !(c %in% prevCaves)) 
      newPaths<-newPaths %>% add_row(path=paste(path,c,sep ='-'),cave=c)
  }
  d$X1[day12A$X2==cave] -> nextCaves
  for(c in nextCaves) {
    if(c %in% maj | !(c %in% prevCaves)) 
      newPaths<-newPaths %>% add_row(path=paste(path,c,sep ='-'),cave=c)
  }
  return(newPaths)
}
continue(path,cave)

solutions<-data.frame(path=character(),cave=character())

while(nrow(paths)>0){
  newPaths<-pmap_df(paths,continue) %>% filter(!is.na(cave))
  solutions<-solutions %>% bind_rows(newPaths %>% filter(cave=="end"))
  paths<-newPaths %>% filter(cave!="end")
}

solutions %>% nrow

# 3298

# B

continue2<-function(path,cave){
  unlist(str_split(path,'-'))->prevCaves
  prevCaves<-prevCaves[which(!prevCaves %in% maj)]
  newPaths<-data.frame(path=character(),cave=character())
  day12A$X2[day12A$X1==cave] -> nextCaves
  for(c in nextCaves) {
    if(c %in% maj | !(c %in% prevCaves) | (!(anyDuplicated(prevCaves)) & c!="start")) 
      newPaths<-newPaths %>% add_row(path=paste(path,c,sep ='-'),cave=c)
  }
  day12A$X1[day12A$X2==cave] -> nextCaves
  for(c in nextCaves) {
    if(c %in% maj | !(c %in% prevCaves) | (!(anyDuplicated(prevCaves)) & c!="start")) 
      newPaths<-newPaths %>% add_row(path=paste(path,c,sep ='-'),cave=c)
  }
  return(newPaths)
}

path<-c("start")
cave<-c("start")
paths<-data.frame(path=path,cave=path)


solutions<-data.frame(path=character(),cave=character())

while(nrow(paths)>0){
  newPaths<-pmap_df(paths,continue2) %>% filter(!is.na(cave))
  solutions<-solutions %>% bind_rows(newPaths %>% filter(cave=="end"))
  paths<-newPaths %>% filter(cave!="end")
}

solutions %>% nrow
# 93572

testSol<-function(path){
  unlist(str_split(path,'-'))->prevCaves
  sum(duplicated(prevCaves) & !(prevCaves %in% maj))<2
}
solutions %>% rowwise() %>% mutate(test=testSol(path)) %>% ungroup %>% summarise(sum(test))

# 93572







