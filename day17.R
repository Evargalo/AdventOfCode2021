setwd("C:/Users/mmajr1/Documents/Perso/AdventOfCode/AoC_2021")

# target area: x=153..199, y=-114..-75

xMin<-153
xMax<-199
yMin<-(-114)
yMax<-(-75)

# longitudes of a trajectory with initial velocity vx0 :
calcTrajX<-function(vx0){
  xVel<-c(vx0:0)
  xTraj<-cumsum(xVel)
  # if the last longitude is within the target, we stay there forever; if not, don't bother
  lastPos<-xTraj[length(xTraj)]
  if(lastPos>=xMin & lastPos<=xMax ){
    xTraj<-c(xTraj,rep(lastPos,1000))
  }
  return(xTraj)
}

# Tests
calcTrajX(9)
calcTrajX(19)
calcTrajX(25)

# nrep first altitudes of a path starting with vertical velocity vy0 :
calcTrajY<-function(vy0,nrep){
  yVel<-vy0:(vy0-nrep+1)
  yTraj<-cumsum(yVel)
  return(yTraj)
}
# Test
calcTrajY(15,50)


# A

Ys<-c()
highYs<-c()

# After at least how many steps is x in the target forever
minStep<-which(calcTrajX(19)>=xMin)[1] 

vy<-10

for (vy in 1:1000){
  print("vy=");print(vy)
  yTraj<-calcTrajY(vy,1000)
  highY<-max(yTraj)
  yTraj<-yTraj[-(1:(minStep-1))]
  if (any(yMin <= yTraj & yTraj < yMax)) {
    highYs<-c(highYs,highY)
    Ys<-c(Ys,vy)
  }
}
max(highYs)

# 6441




# B
max(Ys)


nbPaths<-0
for (x in 1:xMax){
  print("x=");print(x)
  xTraj<-calcTrajX(x)
  possibleSteps<-(which(xMin <= xTraj & xTraj <= xMax))
  if(length(possibleSteps)>0){
    for (vy in yMin:150){
      trajY<-calcTrajY(vy,length(xTraj))
      yTraj<-trajY[possibleSteps]
      yTraj<-yTraj[yTraj>=yMin & yTraj<=yMax]
      nbPaths<-nbPaths+as.integer(length(yTraj)>0)
    }
  }
}
nbPaths

# 3186


# test
# target area: x=20..30, y=-10..-5
# xMin<-20;xMax<-30;yMin<-(-10);yMax<-(-5)
# nbPaths = 112 ok
