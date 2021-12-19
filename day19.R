
# Rotation matrices
expand(data.frame(x=c(1,2,3),y=c(1,2,3)),x,y) %>% filter(x!=y) %>% 
  mutate(z=6/x/y) ->A
A %>% mutate(z=-z) %>% bind_rows(A) -> B
B %>% mutate(y=-y) %>% bind_rows(B) -> C
C %>% bind_rows(C*(-1)) -> D

buildMat<-function(x,y,z){
  v<-rep(0,9)
  v[abs(x)]<-sign(x)
  v[abs(y)+3]<-sign(y)
  v[abs(z)+6]<-sign(z)
  matrix(v,ncol=3,byrow = FALSE)
}

validMat<-function(x,y,z) (buildMat(x,y,z) %>% det )==1

D %>% rowwise %>%   mutate(valid=validMat(x,y,z)) %>%
  ungroup %>% filter(valid) %>% select(-valid)->E

for(numRot in 1:24){
  assign(paste0("M",numRot),
         pmap(E[numRot,],buildMat)[[1]])
}

M6 %>% det


# A
currentScanName<-"azerty"
currentScanner<-0

nbScans<-0
# for(l in read_lines("day19test.txt")){
for(l in read_lines("day19.txt")){
  if(l=="") next
  if(substr(l,1,3)=="---"){
    assign(currentScanName,currentScanner)
    nbScans<-nbScans+1
    currentScanName<-paste0("scan",nbScans)
    assign(currentScanName,
           data.frame(x=numeric(),y=numeric(),z=numeric()))
    currentScanner<-get(currentScanName)
  }else{
    currentScanner<-currentScanner %>% bind_rows(data.frame(p=l) %>% separate(col=p,sep=",",into=c("x","y","z")) %>% mutate_all(as.numeric))
  }
  assign(currentScanName,currentScanner)
}

numScan<-0;numRot<-1
rotateScanner<-function(numScan,numRot){
  newScan<-data.frame(x=numeric(),y=numeric(),z=numeric())
  scan<-get(paste0("scan",numScan))
  M<-get(paste0("M",numRot))
  for (k in 1: nrow(scan)){
    M %*% matrix(scan[k,] %>% as.numeric(),ncol=1) -> newVals
    newScan<-newScan %>% add_row(x=newVals[1,1],y=newVals[2,1],z=newVals[3,1])
  }
  newScan
}
scan2 %>% head
rotateScanner(2,3) %>% head
s1<-scan0; s2<-rotateScanner(2,3); j<-1; k<-1

findMatchingBeacons<-function(s1,s2){
  for(j in 1:(nrow(s1)-12)){
    if(j%%5 != 0) next
    beacons1<-s1[j,]
    for(k in 1:(nrow(s2)-12)){
      beacons2<-s2[k,]
      relPos<-beacons2-beacons1
      s3<-s2 %>% rowwise() %>%  mutate(x=x-relPos$x,y=y-relPos$y,z=z-relPos$z) %>% ungroup()
      if( s3 %>% inner_join(s1, by = c("x", "y", "z")) %>% nrow >11) {
        return(relPos)
      }
    }
  }
  return(c(0,0,0))
}
findMatchingBeacons(scan0,scan1)

allBeacons<-scan0
scanList<-1:(nbScans-1)

addScan<-function(numScan){
  found<-FALSE
  numRot<-1
  while(!found & numRot<=24){
    print(numScan);print(numRot)
    s<-rotateScanner(numScan,numRot)
    findMatchingBeacons(allBeacons,s)->relPos
    if(any(relPos!=0)){
      found<-TRUE
      s3<-s %>% rowwise() %>%  mutate(x=x-relPos$x,y=y-relPos$y,z=z-relPos$z) %>% ungroup()
      allBeacons<<-s3 %>% bind_rows(allBeacons) %>% unique()
    }
    numRot<-numRot+1  
  }
  return(found)
}


while(length(scanList)>0){
  for(k in scanList){
    addScan(k)->tryAdd
    if(tryAdd){
      scanList<-scanList[scanList!=k]
      print("scannerAdded");print(k);print(scanList);print(nrow(allBeacons))
    }
  }
}
# 
scanList
allBeacons %>% nrow

# 306

# B

allScan <-data.frame(num=0,x=0,y=0,z=0)

for(numScan in 1:(nbScans-1)){
  found<-FALSE
  numRot<-1
  while(!found & numRot<=24){
    print(numScan);print(numRot)
    s<-rotateScanner(numScan,numRot)
    findMatchingBeacons(allBeacons,s)->relPos
    if(any(relPos!=0)){
      found<-TRUE
     allScan<-allScan %>% add_row(num=numScan,x=relPos$x,y=relPos$y,z=relPos$z)
    }
    numRot<-numRot+1  
  }
}

allScan %>% mutate(one=1) %>% full_join(allScan %>% mutate(one=1),by="one") %>% mutate(manDist=abs(x.x-x.y)+abs(y.x-y.y)+abs(z.x-z.y)) %>% 
  summarise(max(manDist))

# 9764

