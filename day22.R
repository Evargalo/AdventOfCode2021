test <- read_delim("day22test.txt", col_names = FALSE, delim=' ')
d <- read_delim("day22.txt", col_names = FALSE, delim=' ')

# Prep test
test$X2 %>% gsub(pattern='[^0-9\n-]',replacement = ' ') %>% trim %>%  str_split(' ') %>% unlist -> vals
vals[vals!=""] %>% as.numeric -> vals
length(vals)/6==nrow(test)
test$xMin<-vals[6*(1:nrow(test))-5]
test$xMax<-vals[6*(1:nrow(test))-4]
test$yMin<-vals[6*(1:nrow(test))-3]
test$yMax<-vals[6*(1:nrow(test))-2]
test$zMin<-vals[6*(1:nrow(test))-1]
test$zMax<-vals[6*(1:nrow(test))]

# Prep real
d$X2 %>% gsub(pattern='[^0-9\n-]',replacement = ' ') %>% trim %>%  str_split(' ') %>% unlist -> vals
vals[vals!=""] %>% as.numeric -> vals
length(vals)/6==nrow(d)
d$xMin<-vals[6*(1:nrow(d))-5]
d$xMax<-vals[6*(1:nrow(d))-4]
d$yMin<-vals[6*(1:nrow(d))-3]
d$yMax<-vals[6*(1:nrow(d))-2]
d$zMin<-vals[6*(1:nrow(d))-1]
d$zMax<-vals[6*(1:nrow(d))]

# Part A
grid<-expand(data.frame(x=(-50):50,y=(-50):50,z=(-50):50),x,y,z)

unique(c(dd$xMax,dd$xMin)) %>% min
unique(c(dd$xMax,dd$xMin)) %>% max
unique(c(dd$yMax,dd$yMin)) %>% min
unique(c(dd$yMax,dd$yMin)) %>% max
unique(c(dd$zMax,dd$zMin)) %>% min
unique(c(dd$zMax,dd$zMin)) %>% max
grid2<-grid %>% filter(x>-50,x<50,y>-49,y<50,z>-48,z<49)

# Start from the last instructions
ddd<-d d %>% mutate(X1=(X1=='on')) %>% select(-X2) %>% arrange(-(row_number()))

griOn<-grid2 %>% filter(FALSE) # grid of cubes that are on at the end
gri<-grid2 # the grid will be reduced each time we are sure of its final status

doLine<-function(X1,xMin,xMax,yMin,yMax,zMin,zMax){
  if(X1){    # keep the cubes taht are turned on
    griOn<<-griOn %>% bind_rows(gri %>% filter(x>=xMin,x<=xMax,y>=yMin,y<=yMax,z>=zMin,z<=zMax))
  }
  # reduce the grid
  gri<<-gri %>% filter(!(x>=xMin & x<=xMax & y>=yMin & y<=yMax & z>=zMin & z<=zMax))
}
pmap(ddd,doLine)
nrow(griOn)

# 612714

# Part B

dd<-test %>% mutate(X1=(X1=='on')) %>% select(-X2) %>% arrange(-(row_number()))
dd<-d %>% mutate(X1=(X1=='on')) %>% select(-X2) %>% arrange(-(row_number()))

doLineZ<-function(X1,mi,ma){
  if(length(zz)==0) return(ans)
  if(X1){
    ans<<-ans+sum(zz>=mi & zz<=ma)
  }
  zz<<-zz[zz<mi | zz>ma]
  ans
}

countZ<-function(x,y){
  ddd<-dd %>% filter(x>=xMin,x<=xMax,y>=yMin,y<=yMax) %>% 
    select(X1,mi=zMin,ma=zMax)
  if(nrow(ddd)==0) {return(0)}
  ans<<-0
  zz<<-(min(ddd$mi):max(ddd$ma))
  pmap(ddd,doLineZ)
  ans
}

countY<-function(x){
  dx<<-dd %>% filter(x>=xMin,x<=xMax)
  limsY<-c(dx$yMin,dx$yMax) %>% unique() %>% sort
  gapsY<-(limsY-lag(limsY))[-1]
  bigGapsY<-limsY[which(gapsY>1)]+1
  parangonsY<-c(limsY,bigGapsY)
  nbY<-c(rep(1,length(limsY)),gapsY[gapsY>1]-1)
  pmap_dbl(.l=list(y=parangonsY),.f=countZ,x=x)->zScores
  sum(zScores*nbY)
}

lims<-c(dd$xMin,dd$xMax) %>% unique() %>% sort
gaps<-(lims-lag(lims))[-1]
bigGaps<-lims[which(gaps>1)]+1
parangons<-c(lims,bigGaps)
nbX<-c(rep(1,length(lims)),gaps[gaps>1]-1)
#check
sum(nbX)==max(Xcuts)-min(Xcuts)+1

pmap_dbl(.l=list(x=parangons),.f=countY)->yScores
sum(yScores*nbX)

# 1311612259117092
##########

