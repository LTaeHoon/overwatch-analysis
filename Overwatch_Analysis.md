---
title: "Overwatch Analysis"
author: "taehoon"
date: "2017년 3월 14일"
output: html_document
---



## 데이터 분석 목적

오버워치를 즐기는 친구들에게 각종 스탯을 제공하고 게임 진행 시 적절하게 팀을 나누기 위해 군집분석을 실시하였다.

## 데이터 수집

데이터 수집은 비공식 오버워치 API를 통해 수집하였다. 블리자드에서 아직 공식적으로 오버워치 데이터를 제공하지 않고 있다.
친구들의 아이디를 통해 API를 호출하였고 MariaDB(My-SQL)에 저장하여 데이터 분석 시 사용 하였다.

필요 라이브러리

```r
library(RJSONIO) # JSON 데이터 입출력
library(dplyr) #데이터 전처리
library(jsonlite) #json 데이터 처리
library(plyr) 
library(RMySQL) # DB 관련 
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_111') 
library(RJDBC) # DB 관련
library(DBI) # DB 관련
library(rJava) # java vm 인터페이스 
```

### 데이터 수집 과정

데이터 수집은 API 호출을 통해 json 데이터를 받아 처리하여 DB로 수집하였다.
오버워치 배틀 태그를 통해 API를 호출하고 가져온 json 데이터를 readLines로 읽어 json 데이터를 list 형태로 변환하고 원하는 데이터만 데이터 프레임 형태로 만들었다.

```r
userlist <-c("RandomTH-3179","머거본놈-31139","ksa-3943","wlsrud531-3918","yangrae-3284","틴팬강원래-3198","농촌계모임-3391","마키아울프-3387","쉑쉑감자-3831","더네임-3971","HotDokk-1561","할만한데-3983")
df_avg <- data.frame(NULL) #경쟁전 평균 스탯
df_game <- data.frame(NULL) # 경쟁전 게임 스탯
df_over <- data.frame(NULL) # 경쟁전 누적 스탯
df_comp <- data.frame(NULL)
userlist <-iconv(userlist, from="cp949", to="UTF-8") 
for(i in 1:length(userlist)){
  print(i)
 
  user <- URLencode(userlist[i])
  url <- paste0("https://owapi.net/api/v3/u/",user,"/stats")
 
  raw.data <- readLines(url, warn="F", encoding = "UTF-8")
  dat <- fromJSON(raw.data) #데이터 변환(list)
  
  #경쟁전 avg 데이터 추출
  data_avg <- as.data.frame(dat$kr$stats$competitive$average_stats) #list -> data.frame
  #경쟁전 game_stats 데이터 추출
  data_game <- as.data.frame(dat$kr$stats$competitive$game_stats) #list -> data.frame
  #경쟁전 overall_stats 데이터 추출
  data_over <- as.data.frame(dat$kr$stats$competitive$overall_stats) #list -> data.frame
  
  #row 누적 각 데이터가 있는 경
  if(nrow(data_avg)){
  df_avg <- rbind.fill(df_avg,data_avg)
  }
  if(nrow(data_game)){
  df_game <- rbind.fill(df_game,data_game)
  }
  if(nrow(data_over)){
  df_over <- rbind.fill(df_over,data_over)
  }
  
}
df_avg <- cbind(userlist,df_avg)
df_game <- cbind(userlist,df_game)
df_over <- cbind(userlist,df_over)
```

추가로 각 데이터 프레임 첫 열에 배틀태그를 삽입하여 데이터를 식별하였다.

```r
names(df_avg)[1]<- c('battletag')
names(df_game)[1]<- c('battletag')
names(df_over)[1]<- c('battletag')
```

DB 연결 및 테이블 저장

```r
#db 연결 과정
drv <- JDBC(driverClass="com.mysql.jdbc.Driver", 
            classPath="E:\\NCS\\python\\util\\mysql-connector-java-5.1.40\\mysql-connector-java-5.1.40\\mysql-connector-java-5.1.40-bin.jar")
con <- dbConnect(drv, "jdbc:mysql://127.0.0.1:3306/overwatch", "scott", "tiger")
```

```r
#테이블 저장
dbWriteTable(con, "user_compe_avg", df_avg, append=TRUE, row.names =F)
dbWriteTable(con, "user_compe_game", df_game, append=TRUE, row.names =F)
dbWriteTable(con, "user_compe_over", df_over, append=TRUE, row.names =F)
```
## 데이터 분석

### 1. 경쟁전 데이터 랭킹 시각화

경쟁전 데이터를 토대로 각 배틀태그의 경쟁전 점수를 시각화 하였다.


```r
library(ggplot2)
data_overall <- dbGetQuery(con,"select * from user_compe_over")
head(data_overall)
```

```
##         battletag games ties
## 1   RandomTH-3179    69    4
## 2  머거본놈-31139    77    5
## 3        ksa-3943    39    0
## 4  wlsrud531-3918   113   12
## 5    yangrae-3284    70    5
## 6 틴팬강원래-3198   164   11
##                                                                   avatar
## 1 https://blzgdapipro-a.akamaihd.net/game/unlocks/0x0250000000000D66.png
## 2 https://blzgdapipro-a.akamaihd.net/game/unlocks/0x025000000000030D.png
## 3 https://blzgdapipro-a.akamaihd.net/game/unlocks/0x0250000000000D73.png
## 4 https://blzgdapipro-a.akamaihd.net/game/unlocks/0x0250000000000800.png
## 5 https://blzgdapipro-a.akamaihd.net/game/unlocks/0x0250000000000D60.png
## 6 https://blzgdapipro-a.akamaihd.net/game/unlocks/0x02500000000008B9.png
##   wins prestige     tier level losses comprank win_rate
## 1   29        0   silver    97     36     1690       44
## 2   31        1   silver    71     41     1490       43
## 3   17        0   silver    40     22     1821       43
## 4   53        1 platinum    60     48     2388       52
## 5   25        1   silver    20     40     1522       38
## 6   80        1     gold    51     73     2102       52
```

```r
p<-ggplot(data=data_overall,aes(x=battletag,y=comprank,fill=battletag))+
  geom_bar(stat="identity", position="identity", colour="white", width=0.5)+
  ggtitle("경쟁전 랭킹")
p+
  geom_text(aes(label=comprank))+
  theme(axis.text.x=element_text(angle=-90))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

### 2.경쟁전 주요 스탯 시각화

```r
#경쟁전 주요 스탯 데이터 가져오기
data_comp_avg <- dbGetQuery(con,"select battletag 
                            ,eliminations_avg
                            ,damage_done_avg
                            ,deaths_avg
                            ,final_blows_avg
                            ,healing_done_avg
                            ,objective_kills_avg
                            ,objective_time_avg
                            ,solo_kills_avg
                            from user_compe_avg")
```

### 공격부분 데이터


```r
#공격부분 - 딜/ 처지 그래프
# 데이터 추출
data_avg_attack <- subset(data_comp_avg, select=c(battletag,eliminations_avg,damage_done_avg))

g <- ggplot(data=data_avg_attack,aes(x=damage_done_avg,y=eliminations_avg,fill=battletag))
g+geom_point(aes(color=battletag))+geom_text(aes(label=battletag,color=battletag),hjust=0,vjust=-1)
```

![plot of chunk attack](figure/attack-1.png)

### 수비부분 데이터


```r
data_avg_defence <- subset(data_comp_avg, select=c(battletag,deaths_avg,healing_done_avg))
g <- ggplot(data=data_avg_defence,aes(y=healing_done_avg,x=deaths_avg,fill=battletag))
g+geom_point(aes(color=battletag))+geom_text(aes(label=battletag,color=battletag),hjust=0,vjust=0)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

### 군집분석

### 1. kmean clustering

```r
#kmeans clustering
#처치 평균과 죽음 평균을 이용한 군집분석
data1 <- subset(data_comp_avg,select=c(battletag,eliminations_avg,deaths_avg))

data1_km <-kmeans(data1[,2:3],2)
par(mfrow=c(1,1))
plot(data1$deaths_avg,data1$eliminations_avg,type="n",xlab="죽음 평균",ylab="처치 평균")
text(data1$deaths_avg,data1$eliminations_avg,labels=data1$battletag,cex=0.8,col=data1_km$cluster)
points(x=data1_km$centers[,2],y=data1_km$centers[,1], col="blue",pch=4,cex=2)
```

![plot of chunk kmean](figure/kmean-1.png)
### 2.Hierarchical Clustering

```r
#Hierarchical agglomerative clustering
#데이터 표준화
data_comp_avg_z <- sapply(data_comp_avg[,2:9],scale)
data_comp_avg_z<-as.data.frame(data_comp_avg_z)
data_comp_avg_z <- cbind(data_comp_avg$battletag,data_comp_avg_z)
#outlier
boxplot(data_comp_avg_z[2:9])
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
#거리 구하기
d <- dist(data_comp_avg_z[2:9])
d
```

```
##           1        2        3        4        5        6        7        8
## 2  2.189149                                                               
## 3  2.398927 2.372253                                                      
## 4  4.510506 2.992277 3.555019                                             
## 5  3.782694 4.071020 1.864745 4.447451                                    
## 6  3.930976 2.834634 2.102250 2.645159 2.661838                           
## 7  5.059151 3.102573 4.156089 3.046314 5.181833 3.113180                  
## 8  2.520324 2.381624 2.583142 2.593577 3.644161 3.247010 4.497081         
## 9  4.634420 6.167668 4.720521 7.698972 4.051282 5.992142 8.028185 5.956258
## 10 4.927779 2.975014 3.994639 1.384225 5.042174 2.763113 2.133937 3.506100
## 11 3.191080 2.029564 2.114038 1.782524 3.278832 2.069863 2.941302 1.948495
##           9       10
## 2                   
## 3                   
## 4                   
## 5                   
## 6                   
## 7                   
## 8                   
## 9                   
## 10 8.206055         
## 11 6.423103 2.389746
```

```r
hClustdat<-hclust(d,method="average")
names(hClustdat)
```

```
## [1] "merge"       "height"      "order"       "labels"      "method"     
## [6] "call"        "dist.method"
```

```r
plot(hClustdat,hang=-1,cex=.8,main="Average Linkage Clustering",labels=data_comp_avg$battletag)
rect.hclust(hClustdat, h = 3)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-2.png)

```r
cutree(hClustdat,h=3)
```

```
##  [1] 1 1 2 3 2 2 3 1 4 3 1
```

```r
data <-data.frame(BT = data_comp_avg$battletag,cluster = cutree(hClustdat,h=3))
# data[data$cluster==1,]
# data[data$cluster==2,]
# data[data$cluster==3,]
# data[data$cluster==4,]
```
