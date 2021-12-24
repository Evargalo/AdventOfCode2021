d <- read_delim("day24.txt", col_names = FALSE, delim=' ')

inps<-which(d$X1=="inp")
inps
# The algorithm is made of 14 blocks of 18 lines
d[2:19,] %>% bind_cols(d[2:19+18*5,]) %>% bind_cols(d[2:19+18*9,])%>% bind_cols(d[2:19+18*13,])
d[2:19+18*2,]

# Which kind of block
lignes<-(0:13)*18+6
d[lignes,]$X3==26->checkmods
# sixth X3 value of the block
lignes<-(0:13)*18+7
d[lignes,]$X3->p
# sixteenth X3 value of the block
lignes<-(0:13)*18+17
d[lignes,]$X3->q

p %>% as.integer -> p
q %>% as.integer -> q

# when (!checkmods) the algorithm read a value v, add it to q, store it z in base 26: z<-26*z+v+q
# when (checkmods) the algorithm compares the last v+q stored with v. If it is not 0, it keeps storing values and z will never get back to zero.
# So we can get the constraint : if (checkmods[k]) and j is the last non-used value for which checkmods[j]==FALSE, then v[k]=v[j]+q[j]+p[k]

q[3]+p[4]
# v4=v3+7
q[2]+p[5]
# v5=v2+8
q[7]+p[8]
# v8=v7-5
q[10]+p[11]
# v11=v10+2
q[9]+p[12]
# v12=v9+1
q[6]+p[13]
# v13=v6-4
q[1]+p[14]
# v14=v1+5

# Part A

# 41299995879939 too high
# 41299994879939 too low
# 41299994879959 Correct !
  
# Part B

# 11189561113216 Correct !
