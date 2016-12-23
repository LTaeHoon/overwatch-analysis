#overwatch_eda 탐색적 데이터 분석
#goals
#1. 경쟁전 랭킹 시각화 및 각종 데이터 이해(scatter plot)를 위한 plot 그리기
#2. kmeans clustering 을 이용한 그룹 분류 (avg 스탯 사용), heatmap 만들
#3. 상관관계 테이블 생성(설명변수, 반응변수:comprank)

#library
library(DBI) #db interface
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_111')
library(rJava)
library(RJDBC) # rJava에 의존적이다.

library(ggplot2)
library(plyr)
library(reshape2)

#read data
#db 연결
drv <- JDBC(driverClass="com.mysql.jdbc.Driver", 
            classPath="C:\\NCS\\python\\util\\mysql-connector-java-5.1.40\\mysql-connector-java-5.1.40\\mysql-connector-java-5.1.40-bin.jar")
conn <- dbConnect(drv, "jdbc:mysql://127.0.0.1:3306/overwatch", "scott", "tiger")

data_overall <- dbGetQuery(conn,"select * from user_compe_over")
head(data_overall)

# pal <- colorRamp(c("red","blue"))
# palcol<-pal(seq(0,1,len=10))

ggplot(data=data_overall,aes(x=battletag,y=comprank,fill=battletag))+
  geom_bar(stat="identity", position="identity", colour="white", width=0.5)+
  ggtitle("경쟁전 랭킹")

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

data1 <- subset(data_comp_avg,select=c(battletag,eliminations_avg,deaths_avg))

data1_z <- sapply(data1[,2:3],scale)
data1_z <- cbind(data1$battletag,data1_z)
colnames(data1_z)[1] <-c("battletag")
data1_z <- as.data.frame(data1_z)
data1_km <-kmeans(data1_z,3)
plot(data1_z$deaths_avg,data1_z$eliminations_avg,type="n",xlab="죽음 평균",ylab="처치 평균")
text(data1_z$deaths_avg,data1_z$eliminations_avg,labels=rownames(data1_z$battletag),cex=0.8,col=data1_km$cluster)
