
# Part A

test <- read_delim("day25test.txt", col_names = FALSE, delim=' ')
d <- read_delim("day25.txt", col_names = FALSE, delim=' ')

# Strategy
# duplicate first and last columns at the other end
# use string substitutions to move the sea cucumbers
# remove the duplicated first and last columns
# switch to tibble in order to transpose the data
# collapse back to a vector and repeat for the other kind of sea cucumber
moveV<-function(v){
  v<-paste0(substr(v,nchar(v),nchar(v)),v,substr(v,1,1)) %>% gsub(pattern = '>\\.',replacement = '\\.>')
 substr(v,2,nchar(v)-1) %>% fileToDF() %>% t %>%  as_tibble() -> df
  (df %>% unite_("X1",colnames(df),sep=''))$X1 ->v
  v<-paste0(substr(v,nchar(v),nchar(v)),v,substr(v,1,1)) %>% 
    gsub(pattern = 'v\\.',replacement = '\\.v') 
  substr(v,2,nchar(v)-1) %>% fileToDF() %>% t %>%  as_tibble() -> df
  (df %>% unite_("X1",colnames(df),sep=''))$X1
}

v<-test$X1
# test ok

v<-d$X1
w<-moveV(v)
i<-0
while(any(v!=w)) {
  i<-i+1 
  if(i%%10==0) print(i)
  if(i%%100==0) assign(paste0("v",i),v)
  v<-w
  w<-moveV(v)
}
i+1
# 351

# Part B
# Merry Christmas !


############################################
# A slower way, without string subtitution

M<-buildMaze(d$X1)
# M<-buildMaze(test$X1)

M$Maze->maze
maze %>% group_by(t) %>% count
maze %>% summarise(max(x),max(y))
max(maze$x)->xMax
max(maze$y)->yMax

# Part A
step<-function(){
  mat2<-matrix(data = maze$t,nrow=xMax,byrow = TRUE)
  maze %>% rowwise %>% mutate(t=case_when(
    mat2[x,y]=='.' & mat2[x,(y-2)%%yMax+1]=='>' ~ '>',
    mat2[x,y]=='>' & mat2[x,y%%yMax+1]=='.' ~ '.',
    TRUE ~ t
  )) %>% ungroup->>maze
  mat<-matrix(data = maze$t,nrow=xMax,byrow = TRUE)
  maze %>% rowwise %>% mutate(t=case_when(
    mat[x,y]=='.' & mat[(x-2)%%xMax+1,y]=='v' ~ 'v',
    mat[x,y]=='v' & mat[x%%xMax+1,y]=='.' ~ '.',
    TRUE ~ t
  )) %>% ungroup->>maze
  mat3<-matrix(data = maze$t,nrow=xMax,byrow = TRUE)
  print(sum(mat3!=mat2))
  if(any(mat3!=mat2)) return (FALSE)
  return(TRUE)
}
# step()
i<-0
while(!step()) {
  i<-i+1 
  print(i)
  if(i%%100==0) assign(paste0("maze",i),maze)
}
i+1
# Test ok, real quite slow ~ 30 minutes
# 351
