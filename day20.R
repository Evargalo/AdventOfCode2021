
test <- read_delim("day20test.txt", col_names = FALSE, delim=' ')
d <- read_delim("day20.txt", col_names = FALSE, delim=' ')

algoTest<-"..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
table(str_split(algoTest,''))
algo<-"##.##.#.####.#.###.#...##.#..####.##.##..###.####.###.#...###..##.####.##.#..#........####..###....#.#..###.##.######..#####.#.##...#.#.#.####.#.##.#.#.#.#.#....#.###.##...##.####.#.#...#..#.##...##.##....##...##...######.####.#.....####..#.#..#..###..####..###...#.##..#.....###..#.##..#....#.#.#..#.####..#..##...#.##.#......#.###..##...#...##.##...#.##...##.....####.#...........#.#.#..####...###...#.....#.#.#.....###.....###.#.#..##..#.##...####.##..#..#...#.##..#######.#.###..#..##...#.###.#.###.##.##.#.."
table(str_split(algo,''))

calcVal<-function(xy){
  xxyy<-xy
  largerMaze %>% mutate(val=case_when(
    xy==xxyy-1001 ~ val+1,
    xy==xxyy-1000  ~ val+2,
    xy==xxyy-999 ~ val+4,
    xy==xxyy-1 ~ val+8,
    xy==xxyy ~ val+16,
    xy==xxyy+1 ~ val+32,
    xy==xxyy+999 ~ val+64,
    xy==xxyy+1000 ~ val+128,
    xy==xxyy+1001 ~ val+256,
    TRUE~val
  ))->>largerMaze
}

expMaze<-function(k){
  maze<<-maze %>% mutate(xy=1000*x+y,val=0)
  for(j in 1:k){
    print(j)
    print(sum(maze$t=="#"))
    xMin<<-min(maze$x)
    xMax<<-max(maze$x)
    newMaze<<-expand(data.frame(x=(xMin-2):(xMax+2),y=(xMin-2):(xMax+2)),x,y) %>%
      filter(x<xMin | x>xMax | y<xMin | y>xMax) %>% mutate(xy=1000*x+y,val=0)
    if(j%%2==1){
      largerMaze<<-maze %>% bind_rows(newMaze %>% mutate(t="."))
    } else {
      largerMaze<<-maze %>% bind_rows(newMaze %>% mutate(t="#"))    
    }
    print(sum(largerMaze$t=="#"))
    largerMaze$val<<-0
    pmap(largerMaze %>% filter(t=="#") %>% select(xy),calcVal)
    maze<<-largerMaze %>%
      filter(x %in% (xMin-1):(xMax+1) & y %in% (xMin-1):(xMax+1)) %>%
      rowwise %>% mutate(t=readChar(val)) %>% ungroup
  }
}

# Test
readChar<-function(n) substr(algoTest,n+1,n+1)
buildMaze(test$X1)->M
M$Maze->maze
for(i in 1:50) expMaze(1)
sum(maze$t=="#")
# 3351 ok

readChar<-function(n) substr(algo,n+1,n+1)
buildMaze(d$X1)->M
M$Maze->maze

# Part A
expMaze(2)
sum(maze$t=="#")
# 5391 OK

# Part B
expMaze(48)
sum(maze$t=="#")
# 16383
