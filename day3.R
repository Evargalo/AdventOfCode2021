day3A <- read_table("day3A.txt", col_names = FALSE)
test <- read_table("00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010", col_names = FALSE)

day3A %>%  separate(X1, paste0("x",as.character(0:12)), "") %>% select(-x0) ->df
# test %>%  separate(X1, paste0("x",as.character(0:5)), "") %>% select(-x0) ->df

# A
getmode <- function(v) {
  uniqv <- c("1","0")
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(c(0,10,1,0,1))

df %>% summarise_all(getmode) %>% unlist %>% BinToDec(charactersForOnes = "1") ->gamma 
df %>% summarise_all(getmode) %>% unlist %>% BinToDec(charactersForOnes = "0") ->epsilon 
epsilon*gamma

# 845186



# B

# oxygen
d<-df %>% mutate(n=row_number()) 
while(nrow(d)>1){
  names(d)[1]<-"x1"
  modus<-d %>% summarise(getmode(x1)) %>% unlist
  d %>% filter(x1==modus) %>% select(-x1)->d
}
df %>% filter(row_number()==d$n) %>% unlist %>% BinToDec("1")->oxygen

# CO2
df %>% mutate(n=row_number()) -> d
while(nrow(d)>1){
  names(d)[1]<-"x1"
  modus<-d %>% summarise(getmode(x1)) %>% unlist
  d %>% filter(x1!=modus) %>% select(-x1)->d
}
df %>% filter(row_number()==d$n) %>% unlist %>% BinToDec("1")->CO2

oxygen*CO2
# 4636702
