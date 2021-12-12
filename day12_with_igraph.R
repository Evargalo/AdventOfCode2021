
require(igraph)

t <- read_delim("day12test.txt", col_names = FALSE, delim='-')
day12A <- read_delim("day12A.txt", col_names = FALSE, delim='-')

# A

g <- graph_from_data_frame(t,directed = FALSE)
g <- graph_from_data_frame(day12A,directed = FALSE)

g %>% plot

maj<-c("HN","LN")
maj<-c("BN","LP","PK","EP")

c(day12A$X1[substr(day12A$X1,1,1)%in%LETTERS],
  day12A$X2[substr(day12A$X2,1,1)%in%LETTERS])->maj

nbPaths<-function(g,start,end){
  if(!start %in% get.vertex.attribute(g,"name")) return(0)
  if(!end %in% get.vertex.attribute(g,"name")) return(0)
  if(start==end) return(1)
  nei<-neighbors(g,start)
  nei<-vertex_attr(g, "name", index = nei)
  if(length(nei)==0) return(0)
  if(!start %in% maj){g<-delete_vertices(g,start)}
  ans<-0
  for(ne in nei){ans<-ans+nbPaths(g,ne,end)}
  ans
}

nbPaths(g,"start","end")
# 3298  


# B

g <- graph_from_data_frame(t,directed = FALSE)
g <- graph_from_data_frame(day12A,directed = FALSE)

maj<-c("HN","LN")
maj<-c("BN","LP","PK","EP")

nbPaths2<-function(g,start,end,prev){
  get.vertex.attribute(g,"name")->vertices
  
  if(!start %in% get.vertex.attribute(g,"name")) return(0)
  if(!end %in% get.vertex.attribute(g,"name")) return(0)
  if(start==end) return(1)
  nei<-neighbors(g,start)
  nei<-vertex_attr(g, "name", index = nei)
  
  if(length(nei)==0) return(0)
  if(!start %in% maj) {
    prev<-c(prev,start)
    }
  if(start == "start"){g<-delete_vertices(g,start)}
  if(sum(duplicated(prev))>1) return(0)
  if(sum(duplicated(prev))==1) {
    g<-delete_vertices(g,prev[prev %in% vertices])
  }
  ans<-0
  for(ne in nei){
    ans<-ans+nbPaths2(g,ne,end,prev)
    }
  ans
}

nbPaths2(g,"start","end",c())
# 93572  

