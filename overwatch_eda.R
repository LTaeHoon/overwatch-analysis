#overwatch_eda 탐색적 데이터 분석
#goals
#1. 경쟁전 랭킹 시각화 
#2. 공격부분 - 딜/ 처지 그래프
#3. 수비부분 - 힐/ 죽음
#2. kmeans clustering 을 이용한 그룹 분류 (avg 스탯 사용), heatmap 만들
#3. 상관관계 테이블 생성(설명변수, 반응변수:comprank)

#library
library(DBI) #db interface
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_101')
library(rJava)
library(RJDBC) # rJava에 의존적이다.

library(ggplot2)
library(plyr)
library(reshape2)

#read data
#db 연결
drv <- JDBC(driverClass="com.mysql.jdbc.Driver", 
            classPath="E:\\NCS\\python\\util\\mysql-connector-java-5.1.40\\mysql-connector-java-5.1.40\\mysql-connector-java-5.1.40-bin.jar")
conn <- dbConnect(drv, "jdbc:mysql://127.0.0.1:3306/overwatch", "scott", "tiger")

#1. 경쟁전 랭킹 시각화 
# 데이터 가져오기
data_overall <- dbGetQuery(conn,"select * from user_compe_over")
head(data_overall)

# pal <- colorRamp(c("red","blue"))
# palcol<-pal(seq(0,1,len=10))

p<-ggplot(data=data_overall,aes(x=battletag,y=comprank,fill=battletag))+
  geom_bar(stat="identity", position="identity", colour="white", width=0.5)+
  ggtitle("경쟁전 랭킹")
p+geom_text(aes(label=comprank))

#경쟁전 주요 스탯 데이터 가져오기
data_comp_avg <- dbGetQuery(conn,"select battletag 
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

data1_km <-kmeans(data1[,2:3],3)
par(mfrow=c(1,1))
plot(data1$deaths_avg,data1$eliminations_avg,type="n",xlab="죽음 평균",ylab="처치 평균")
text(data1$deaths_avg,data1$eliminations_avg,labels=data1$battletag,cex=0.8,col=data1_km$cluster)
points(data1_km$centers[,1:2], col="blue",pch=4,cex=2)

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
rect.hclust(hClustdat, k = 5)
install.packages("NbClust")
library(NbClust)
nc <- NbClust(data_comp_avg_z[,2:9],distance="euclidean",min.nc=2,max.nc=15,method="average")

nc$Best.nc

#공격부분 - 딜/ 처지 그래프
# 데이터 추출
data_avg_attack <- subset(data_comp_avg, select=c(battletag,eliminations_avg,damage_done_avg))

g <- ggplot(data=data_avg_attack,aes(x=damage_done_avg,y=eliminations_avg,fill=battletag))
g+geom_point(aes(color=battletag))+geom_text(aes(label=battletag,color=battletag),hjust=0,vjust=-1)

#3. 수비부분 - 힐/ 죽음
data_avg_defence <- subset(data_comp_avg, select=c(battletag,deaths_avg,healing_done_avg))
g <- ggplot(data=data_avg_defence,aes(y=healing_done_avg,x=deaths_avg,fill=battletag))
g+geom_point(aes(color=battletag))+geom_text(aes(label=battletag,color=battletag),hjust=0,vjust=0)


#상관관계 테이블 생성
?pairs
pairs(data_comp_avg[2:9])
