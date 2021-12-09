day9A <- read_delim("day9A.txt", col_names = FALSE, delim=' ')

n<-nchar(day9A[1,1])
day9A %>% separate(X1,into=paste0('x',1:(n+1)),sep='')->df
df %>% mutate_all(as.numeric) %>% select(-x1)->df

# A

isMin<-function(xx,yy){
  if(xx>1) if (df[xx,yy]>=df[xx-1,yy]) return(FALSE)
  if(yy>1) if (df[xx,yy]>=df[xx,yy-1]) return(FALSE)
  if(xx<n) if (df[xx,yy]>=df[xx+1,yy]) return(FALSE)
  if(yy<n) if (df[xx,yy]>=df[xx,yy+1]) return(FALSE)
  TRUE
}

valMin<-function(x,y){ifelse(isMin(x,y),df[x,y]+1,0) %>% unlist}
grid<-expand(tibble(x=1:n,y=1:n),x,y) %>% rowwise() %>% mutate(lp=valMin(x,y)) %>% ungroup
grid %>% summarise(sumLP=sum(lp),nbLP=sum(lp!=0))

# 575

# B

d<-df  
  
# Go down until a min - possible because it is given that every non-9 point is part of a single bassin : no collar point !
findBassin<-function(x,y){
  v<-d[x,y]
    if(v==9) return(0)
    if(isMin(x,y)) return(x*1000+y)
    if(x>1) if (v>d[x-1,y]) return(findBassin(x-1,y))
    if(y>1) if (v>d[x,y-1]) return(findBassin(x,y-1))
    if(x<n) if (v>d[x+1,y]) return(findBassin(x+1,y))
    if(y<n) if (v>d[x,y+1]) return(findBassin(x,y+1))
  }

monBassin<-expand(tibble(x=1:n ,y=1:n),x,y)
monBassin %>% rowwise() %>% mutate(bassin=findBassin(x,y)) %>% group_by(bassin) %>% count %>% arrange(desc(n))
99 * 100 * 103 

# 1019700


