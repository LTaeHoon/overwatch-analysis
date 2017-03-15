#overwatch competitive stats data collection script
install.packages("RJSONIO")
library(RJSONIO) # JSON 데이터 입출력
library(dplyr) #데이터 전처리
library(jsonlite) #json 데이터 처리
library(plyr)
library(RMySQL) # DB 관련 
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_111') 
library(RJDBC) # DB 관련
library(DBI) # DB 관련
library(rJava) # java vm 인터페이스 

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

names(df_avg)[1]<- c('battletag')
names(df_game)[1]<- c('battletag')
names(df_over)[1]<- c('battletag')


#db 연결 과정
drv <- JDBC(driverClass="com.mysql.jdbc.Driver", 
            classPath="C:\\NCS\\python\\util\\mysql-connector-java-5.1.40\\mysql-connector-java-5.1.40\\mysql-connector-java-5.1.40-bin.jar")
con <- dbConnect(drv, "jdbc:mysql://127.0.0.1:3306/overwatch", "scott", "tiger")

#테이블 저장
dbWriteTable(con, "user_compe_avg", df_avg, append=TRUE, row.names =F)
dbWriteTable(con, "user_compe_game", df_game, append=TRUE, row.names =F)
dbWriteTable(con, "user_compe_over", df_over, append=TRUE, row.names =F)


# query = "select * from user_compe_avg_stat"
# result <- dbGetQuery(con, query)
# result

#1. 경쟁전 랭킹 시각화 
# 데이터 가져오기
library(ggplot2)
data_overall <- dbGetQuery(con,"select * from user_compe_over")
head(data_overall)

# pal <- colorRamp(c("red","blue"))
# palcol<-pal(seq(0,1,len=10))

p<-ggplot(data=data_overall,aes(x=battletag,y=comprank,fill=battletag))+
  geom_bar(stat="identity", position="identity", colour="white", width=0.5)+
  ggtitle("경쟁전 랭킹")
p+geom_text(aes(label=comprank))

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
#kmeans clustering
#처치 평균과 죽음 평균을 이용한 군집분석
data1 <- subset(data_comp_avg,select=c(battletag,eliminations_avg,deaths_avg))

data1_km <-kmeans(data1[,2:3],2)
par(mfrow=c(1,1))
plot(data1$deaths_avg,data1$eliminations_avg,type="n",xlab="죽음 평균",ylab="처치 평균")
text(data1$deaths_avg,data1$eliminations_avg,labels=data1$battletag,cex=0.8,col=data1_km$cluster)
points(x=data1_km$centers[,2],y=data1_km$centers[,1], col="blue",pch=4,cex=2)
?points
#Hierarchical agglomerative clustering
#데이터 표준화
data_comp_avg_z <- sapply(data_comp_avg[,2:9],scale)
data_comp_avg_z<-as.data.frame(data_comp_avg_z)
data_comp_avg_z <- cbind(data_comp_avg$battletag,data_comp_avg_z)
#outlier
boxplot(data_comp_avg_z[2:9])
#거리 구하기
d <- dist(data_comp_avg_z[2:9])
d
hClustdat<-hclust(d,method="average")
names(hClustdat)
plot(hClustdat,hang=-1,cex=.8,main="Average Linkage Clustering",labels=data_comp_avg$battletag)
rect.hclust(hClustdat, h = 3)
cutree(hClustdat,h=3)
data <-data.frame(BT = data_comp_avg$battletag,cluster = cutree(hClustdat,h=3))
data[data$cluster==1,]
data[data$cluster==2,]
data[data$cluster==3,]
data[data$cluster==4,]

# install.packages("NbClust")
# library(NbClust)
# nc <- NbClust(data_comp_avg_z[,2:9],distance="euclidean",min.nc=2,max.nc=11,method="average")
# 
# nc$Best.nc

#공격부분 - 딜/ 처지 그래프
# 데이터 추출
data_avg_attack <- subset(data_comp_avg, select=c(battletag,eliminations_avg,damage_done_avg))

g <- ggplot(data=data_avg_attack,aes(x=damage_done_avg,y=eliminations_avg,fill=battletag))
g+geom_point(aes(color=battletag))+geom_text(aes(label=battletag,color=battletag),hjust=0,vjust=-1)

#3. 수비부분 - 힐/ 죽음
data_avg_defence <- subset(data_comp_avg, select=c(battletag,deaths_avg,healing_done_avg))
g <- ggplot(data=data_avg_defence,aes(y=healing_done_avg,x=deaths_avg,fill=battletag))
g+geom_point(aes(color=battletag))+geom_text(aes(label=battletag,color=battletag),hjust=0,vjust=0)

#db disconnect
dbDisconnect(con)


