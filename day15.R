setwd("C:/Users/mmajr1/Documents/Perso/AdventOfCode/AoC_2021")
require(igraph)

# For Test
test<-read_csv("day15test.txt", col_names = FALSE, 
               col_types = cols(X1 = col_character()))
n<-nrow(test)
buildMaze(test$X1)->maze

# For Real
d <- read_csv("day15.txt", col_names = FALSE, 
              col_types = cols(X1 = col_character()))
n<-nrow(d)
# grid 100x100
buildMaze(d$X1)->maze

maze$Maze -> M
M$t<-as.integer(M$t)

# A

drawMaze(M)

M %>% gf_tile(y~x,fill=~t)+
  gf_theme(theme_void())+gf_refine(viridis::scale_fill_viridis(discrete = FALSE))

h <- make_lattice(length = n, dim = 2,directed = TRUE,mutual = TRUE) 
plot(h,layout=layout_on_grid)

M %>% mutate(z=x+n*(y-1)) -> M

# checks
sum(duplicated(M$z)) 
sum(head_of(h,E(h)) %in% M$z)

data.frame(noeud=head_of(h,E(h)) %>% as.vector) %>%
  left_join(M %>% select(z,t),by=c("noeud"="z"))->vals
h %>% set.edge.attribute(index = E(h), name = "weight", value = vals$t) ->h
  
h %>% distances(v=1,to=n*n,mode = 'out') 

# 739


# B

# bigger grid
h <- make_lattice(length = 5*n, dim = 2,directed = TRUE,mutual = TRUE) 

tidyr::expand(data.frame(X=1:5,Y=1:5),X,Y)->bigGrid

M %>% mutate(b=1) %>% full_join(bigGrid %>% mutate(b=1)) -> M

M%>% mutate(z=x+(X-1)*n+5*n*(y+(Y-1)*n-1)) -> M

M$t<-(M$t+M$X+M$Y-2-1) %% 9 +1

# checks
sum(duplicated(M$z)) 
sum(head_of(h,E(h) %>% as.vector) %in% M$z)

# add weight on the edges
data.frame(noeud=head_of(h,E(h)) %>% as.vector) %>%
  left_join(M %>% select(z,t),by=c("noeud"="z"))->vals
h %>% set.edge.attribute(index = E(h), name = "weight", value = vals$t) ->h

h %>% distances(v=1,to=5*n*5*n,mode = 'out') 

# 3040



