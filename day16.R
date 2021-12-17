
day16test <- read_csv("day16test.txt", col_names = FALSE, 
                      col_types = cols(X1 = col_character()))
day16test2 <- read_csv("day16test2.txt", col_names = FALSE, 
                      col_types = cols(X1 = col_character()))
d <- read_csv("day16B.txt", col_names = FALSE, 
                      col_types = cols(X1 = col_character()))


bin<-day16test2$X1 %>% as.character()
bin<-d$X1 %>% as.character()
nchar(bin)

# A

packetVersions<-c()

readPacket<-function(x){
 print("call readPacket")
 print(x)
  if(length(x)>1){print ("lenght(x)>2"); print(x); return("")}
  l<-nchar(x)
  if(l==0)(return(""))
  if(l>nchar(bin)){return("")}
  substr(x,1,3) %>% BinToDec()->packetVersion
#  print(packetVersion)
  packetVersions<<-c(packetVersions,packetVersion)
  substr(x,4,6) %>% BinToDec()->packetTypeID # 4->literal ; other -> operator
  if(packetTypeID==4){
    readLiteral(substr(x,7,l))[2]->x
    if(length(x)>1){print ("lenght(x)>2 after call readLiteral"); return("")}
  } else {
    readOperator(substr(x,7,l))->x
    if(length(x)>1){print ("lenght(x)>2 after call readOperator"); return("")}
  }
  if(length(x)>1){print ("lenght(x)>2 before end of readPacket"); print(x); return("")}
  return(x)
}

readLiteral<-function(x){
  print("call readLiteral")
  print(x)
  l<-nchar(x)
  nbSigns<-6
  ans<-""
  while(substr(x,1,1)=="1"){
    ans<-paste0(ans,substr(x,2,5))
    x<-substr(x,6,l)
    nbSigns<-nbSigns+5
  }
  ans<-paste0(ans,substr(x,2,5))
  nbSigns<-nbSigns+5
  x<-substr(x,6,l)
  #x<-substr(x,6+(15-((nbSigns-1)%%16)),l)
  print("literalValueRead=");print(BinToDec(ans,charactersForOnes = "1"))
  return(c(BinToDec(ans,charactersForOnes = "1"),x))
}

# readLiteral("101111111000101000")

readOperator<-function(x){
  print("call readOperator")
  print(x)
  l<-nchar(x)  
  if(l==0)(return(""))
  if(x==paste0(rep("0",l),collapse=''))(return(""))
  substr(x,1,1)->lengthTypeId
  if(lengthTypeId=="0") { 
    ll<-BinToDec(x = substr(x,2,16))
  #  print("ll=");print(ll)
    readSubPacketsByLength(substr(x,17,17+ll-1),length=ll)
    substr(x,17+ll,l)->s
  } else {
    readSubPacketsByQuantity(substr(x,13,l),quantity=BinToDec(x = substr(x,2,12)))->s
  }
  readPacket(s)
  #ope<<-ope
}

readSubPacketsByLength<-function(x,length){
  print("call readSubPacketsByLength")
  print(x)
  print("Length=");print(length)
  l<-nchar(x)
  while(x!=""){
    x<-readPacket(x)
   # ope<<-ope
  }
  packetVersions<<-packetVersions
  return(x)
}

readSubPacketsByQuantity<-function(x,quantity){ 
  print("call readSubPacketsByQuantity")
  print(x)
  print("quantity=");print(quantity)
  l<-nchar(x)
  for(i in 1:l){
    x<-readPacket(x)
   # ope<<-ope
  }
  packetVersions<<-packetVersions
  return(x)
}

#######
# Tests ----

# 8A004A801A8002F478
packetVersions<-c()
readPacket("100010100000000001001010100000000001101010000000000000101111010001111000")
packetVersions
sum(packetVersions)
#ok : 16

#620080001611562C8802118E34
packetVersions<-c()
readPacket("01100010000000001000000000000000000101100001000101010110001011001000100000000010000100011000111000110100")
packetVersions
sum(packetVersions)
# Ok : 12

# C0015000016115A2E0802F182340
packetVersions<-c()
readPacket("1100000000000001010100000000000000000001011000010001010110100010111000001000000000101111000110000010001101000000")
packetVersions
sum(packetVersions)
# ok :23

# A0016C880162017C3686B18A3D4780
packetVersions<-c()
readPacket("101000000000000101101100100010000000000101100010000000010111110000110110100001101011000110001010001111010100011110000000")
packetVersions
sum(packetVersions)
# ok :31


#######
# Part A solution ----

packetVersions<-c()
readPacket(bin)
packetVersions
sum(packetVersions)

# 871



# B

ope<-character(0)

readPacket2<-function(x){
  # print("call evalPacket")
  # print(x)
  # print(ope)
  l<-nchar(x)
  if(l==0)(return(""))
  if(l>nchar(bin)){return("")}
  if(x==paste0(rep("0",l),collapse=''))(return(""))
  substr(x,1,3) %>% BinToDec()->packetVersion
  packetVersions<<-c(packetVersions,packetVersion)
  substr(x,4,6) %>% BinToDec()->packetTypeID # 4->literal ; other -> operator
  if(packetTypeID==4){
    readLiteral2(substr(x,7,l))->x
    ope<<-ope
  } else {
    ope<<-c(ope,paste0("op",packetTypeID))
    readOperator2(substr(x,7,l))->x
  }
  ope<<-ope
  readPacket2(x)
  
}

readOnePacket2<-function(x){
  l<-nchar(x)
  if(l==0)(return(""))
  if(l>nchar(bin)){return("")}
  if(x==paste0(rep("0",l),collapse=''))(return(""))
  substr(x,1,3) %>% BinToDec()->packetVersion
  packetVersions<<-c(packetVersions,packetVersion)
  substr(x,4,6) %>% BinToDec()->packetTypeID # 4->literal ; other -> operator
  if(packetTypeID==4){
    readLiteral2(substr(x,7,l))->x
    ope<<-ope
  } else {
    ope<<-c(ope,paste0("op",packetTypeID))
    readOperator2(substr(x,7,l))->x
  }
  ope<<-ope
  x
}

readLiteral2<-function(x){
  l<-nchar(x)
  ans<-""
  while(substr(x,1,1)=="1"){
    ans<-paste0(ans,substr(x,2,5))
    x<-substr(x,6,l)
  }
  ans<-paste0(ans,substr(x,2,5))
  x<-substr(x,6,l)
  
  literalValue<-BinToDec(ans,charactersForOnes = "1")
  ope<<-c(ope,as.character(literalValue))

  return(x)
}

readOperator2<-function(x){
  l<-nchar(x)  
  if(l==0)(return(""))
  if(x==paste0(rep("0",l),collapse=''))(return(""))
  substr(x,1,1)->lengthTypeId
  if(lengthTypeId=="0") { 
    ll<-BinToDec(x = substr(x,2,16))
    readSubPacketsByLength2(substr(x,17,17+ll-1),length=ll)
    substr(x,17+ll,l)->s
  } else {
    readSubPacketsByQuantity2(substr(x,13,l),quantity=BinToDec(x = substr(x,2,12)))->s
  }
  ope<<-ope
  return(s)
}

readSubPacketsByLength2<-function(x,length){
  l<-nchar(x)
  while(x!=""){
    x<-readOnePacket2(x)
    ope<<-ope
  }
  packetVersions<<-packetVersions
  ope<<-c(ope,"finOp")
  return(x)
}
readSubPacketsByQuantity2<-function(x,quantity){ 
  for(i in 1:quantity){
    x<-readOnePacket2(x)
    ope<<-ope
  }
  packetVersions<<-packetVersions
  ope<<-c(ope,"finOp")
  return(x)
}

calcOpe<-function(v){
  l<-length(v)
  print(l)
  if(anyNA(v)){print(v);return(0)}
  if(l<15) {print(v)}
  if (l==1) return (as.numeric(v))
  v2<-which(v=="finOp")[1]
  x<-which(substr(v,1,2)=="op")
  v1<-max(x[x<v2])
  op<-v[v1]
  if(op=="op0") val<-sum(as.numeric(v[(v1+1):(v2-1)]))
  if(op=="op1") val<-prod(as.numeric(v[(v1+1):(v2-1)]))
  if(op=="op2") val<-min(as.numeric(v[(v1+1):(v2-1)]))
  if(op=="op3") val<-max(as.numeric(v[(v1+1):(v2-1)]))
  if(op=="op5") val<-as.numeric(as.numeric(v[v1+1])>as.numeric(v[v1+2]))
  if(op=="op6") val<-as.numeric(as.numeric(v[v1+1])<as.numeric(v[v1+2]))
  if(op=="op7") val<-as.numeric(as.numeric(v[v1+1])==as.numeric(v[v1+2]))
  if(v2==l){fin<-c()}else fin<-v[(v2+1):l]
  if(v1==1){deb<-c()}else deb<-v[1:(v1-1)]
  calcOpe(c(deb,as.character(val),fin))
}


####
# Tests ----

calcOpe(c( "op1",   "2"  ,   "2"  ,   "finOp"))


# C200B40A82
ope<-c()
readPacket2("1100001000000000101101000000101010000010")
ope
# ok: 10 1 2 20
calcOpe(ope)

# # C200B40A82
ope<-c()
readPacket2("000001000000000001011010110000110011100010010000")
ope
# # ok "op1"   "6"     "9"     "finOp"
calcOpe(ope)


# C200B40A82
ope<-c()
readPacket2("11001110000000001100010000111101100010000001000100100000")
ope
# ok 13  7  8  9 20
calcOpe(ope)


# C200B40A82
ope<-c()
readPacket2("1100001000000000101101000000101010000010")
ope
# ok: 10 1 2 20
calcOpe(ope)



# C200B40A82
ope<-c()
readPacket2("10011100000000010100000100001000000000100101000000110010000011110001100000000010000100000100101000001000")
ope
#  ok "op7"   "op0"   "1"     "3"     "finOp" "op1"   "2"     "2"     "finOp" "finOp"
#
calcOpe(ope)
# ok


# D8005AC2A8F0
ope<-c()
readPacket2("110110000000000001011010110000101010100011110000")
ope
#  ok "op7"   "op0"   "1"     "3"     "finOp" "op1"   "2"     "2"     "finOp" "finOp"
#
calcOpe(ope)
# ok

# 9C005AC2F8F0
ope<-c()
readPacket2("100111000000000001011010110000101111100011110000")
ope
#  ok ""op7"   "5"     "15"    "finOp"
#
calcOpe(ope)
# ok

####

# Part B solution ----

ope<-c()
readPacket2(bin)
ope
calcOpe(ope)

# 68703010504 correct !


# 1336716 too low
# 117565369146 too high
# 117563685580 too high

