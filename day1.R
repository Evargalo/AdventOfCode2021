require(dplyr)
val<-read.delim2("day1A.txt",header = FALSE)$V1

# A
sum(val>lag(val),na.rm = TRUE)
# 1393

# B
sum(val>lag(val,3),na.rm = TRUE)
# 1359
