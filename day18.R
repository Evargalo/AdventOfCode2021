
test <- read_delim("day18test.txt", col_names = FALSE, delim=' ')
test2 <- read_delim("day18test2.txt", col_names = FALSE, delim=' ')
d <- read_delim("day18.txt", col_names = FALSE, delim=' ')

# A

s<-"[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"

# Code a snailfish number in a vector of integers
prepString<-function(s){
  v<-s %>% strsplit('') %>% unlist
  w<-replace(v,which(v=="["),"-3")
  w<-replace(w,which(w=="]"),"-2")
  w<-replace(w,which(w==","),"-1") %>% as.numeric
  w
}
prepString(s)
# For printing
unPrepString<-function(v){
  v<-v %>% as.character
  w<-replace(v,which(v=="-3"),"[")
  w<-replace(w,which(w=="-2"),"]")
  w<-replace(w,which(w=="-1"),",") 
  paste0(w,collapse='')
}
prepString(s) %>% unPrepString()

reduceNumber<-function(w){
  action<-TRUE
  while(action){
    if(anyNA(w)) break
    action<-FALSE
    l<-length(w)
    depth<-cumsum(w==-3)-cumsum(w==-2)
    if(any(depth==5)){
      # explode
      k<-which(depth==5)[1]
      # print("explode:")
      # print(k)
      # print(w %>% unPrepString())
      # print(paste0(depth,collapse=''))
      prev<-w[1:(k-1)]
      a<-k
      if(any(prev>=0)) {
        a<-max(which(prev>=0))
        prev[a]<-prev[a]+w[k+1]
      }
      end<-w[(k+5):l]
      if(any(end>=0)) {
        b<-min(which(end>=0))
        end[b]<-end[b]+w[k+3]
      }
      w<-c(prev,0,end)
      action<-TRUE
    }else  {
      if(any(w>9)){ 
        # split
        k<-which(w>9)[1]
        # print("split:")
        # print(k)
        # print(w %>% unPrepString())
        newPair<-c(-3,w[k]%/%2,-1,(w[k]+1)%/%2,-2)
        w<-c(w[1:k-1],newPair,w[(k+1):l])
        action<-TRUE
      }
    }
  }
  return(w)
}

# Test ok
w <- prepString(s) 
reduceNumber(w)

addNumbers<-function(x,y){
  reduceNumber(c(-3,x,-1,y,-2))
}

# Test ok
addNumbers(test$X1[1] %>% prepString(),
           test$X1[2]%>% prepString()) %>% 
  unPrepString()

addString<-function(s){
  v<-prepString(s)
  w<<-addNumbers(w,v)
}

# Test ok
w<-prepString("[[[[1,1],[2,2]],[3,3]],[4,4]]")
addString("[5,5]")
w %>% unPrepString()
addString("[6,6]")
w %>% unPrepString()

calcMagnitude<-function(w){
  depth<-cumsum(w==-3)-cumsum(w==-2)
  # if(max(depth)==1) return(3*w[2]+2*w[4])
  splits<-which(depth==1)
  if(splits[3]==3) {left<-w[2]} else {
    left<-calcMagnitude(w[2:(splits[3]-1)])}
  if(splits[4]==splits[3]+1) {right<-w[splits[4]]} else {
    right<-calcMagnitude(w[(splits[3]+1):splits[4]])}
  return(3*left+2*right)
}

# Test ok
calcMagnitude(c(-3,9,-1,1,-2))
s<-"[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
s %>% prepString() %>% calcMagnitude()
# 3488 ok

# Test ok
s<-test2$X1[1]
w<-prepString(s)
sapply(test2$X1[2:nrow(test2)],addString)
calcMagnitude(w)
# 4140 ok

# Part A
input<-d$X1
s<-input[1]
w<-prepString(s)
sapply(input[2:nrow(d)],addString)
calcMagnitude(w)

# 4435


# Part  B

res<-expand(data.frame(x=1:nrow(d),y=1:nrow(d)),x,y)
res<- res %>% rowwise %>%  mutate(s=addNumbers(input[x] %>% prepString(),input[y] %>% prepString()) %>% calcMagnitude)
max(res$s)

# 4802



