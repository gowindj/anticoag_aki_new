getwd()
rm(list=ls())
set.seed(8888)
library(RPostgreSQL)
#如果有antidata3in1总数据就直接读取，没有就从postgresq抓取
if(file.exists("rawdata/antidata3in1.csv")==F){
  coneicu <- dbConnect( PostgreSQL(), host="localhost", user= "postgres", password="postgres", dbname="eicu")
  eicu<- dbGetQuery(coneicu, statement = "select * from project_anti_obesity_aki")
  write.csv(eicu,"rawdata/eicu.csv") 
  
  conmimic3 <- dbConnect( PostgreSQL(), host="localhost", user= "postgres", password="postgres", dbname="mimic3")
  mimic3<- dbGetQuery(conmimic3, statement = "select * from project_anti_obesity_aki")
  write.csv(eicu,"rawdata/mimic3.csv") 
  
  conmimic4 <- dbConnect( PostgreSQL(), host="localhost", user= "postgres", password="postgres", dbname="mimic4")
  mimic4<- dbGetQuery(conmimic4, statement = "select * from project_anti_obesity_aki")
  write.csv(mimic4,"rawdata/mimic4.csv") 
  antidata<-rbind.data.frame(eicu,mimic3,mimic4)
  rm(list = c("eicu","mimic3","mimic4"))
  write.csv(antidata,"rawdata/antidata3in1.csv")
}else{
  rm(list = ls())
  antidata<-read.csv("rawdata/antidata3in1.csv")
}


View(antidata)

summary(antidata)
length(antidata$icustay_id)# 338450 338535 285125
#查看各数据库的人数
table(antidata$database)
length(antidata$icustay_id)#  295125各患者



#标记一下需要排除的病人，主要是出血的患者
antidata$bleed_exclud<-0
antidata$bleed_exclud[grep("hemorrhage",antidata$diagnosis,ignore.case = T)]<-1
antidata$bleed_exclud[grep("bleed",antidata$diagnosis,ignore.case = T)]<-1
antidata$bleed_exclud[grep("hemato",antidata$diagnosis,ignore.case = T)]<-1

#年龄仍有异常值，此下代码修改
antidata$age<-as.numeric(antidata$age)
hist(as.numeric(antidata$age),breaks = 100)

library(dplyr)
antidata<-mutate(antidata,age=case_when(
  age>250~91.4,
  TRUE~age
))
summary(antidata$age)
length(antidata$age) 

#去掉那些年龄小于18的人
data_upon18<-subset(antidata,age>=18)
ageunder18<-length(antidata$age)-length(data_upon18$age)

#去掉出血性疾病
data_bleed<-subset(data_upon18,bleed_exclud==1)
#剩下我们需要的data
data<-subset(data_upon18,bleed_exclud==0)

num_bleed_exclud<-length(data_bleed$age)

#盖帽法修改身高体重异常值
summary(data$weight)
boxplot(data$weight)
hist(data$weight,breaks=100)
q1<-quantile(data$weight,0.001)
q99<-quantile(data$weight,0.999)
data[data$weight<q1,]$weight<-q1
data[data$weight>q99,]$weight<-q99
summary(data$weight)
boxplot(data$weight)
hist(data$weight)

summary(data$height)
boxplot(data$height)
hist(data$height,breaks = 100)
q1<-quantile(data$height,0.005)
q99<-quantile(data$height,0.995)
data[data$height<q1,]$height<-q1
data[data$height>q99,]$height<-q99
summary(data$height)
boxplot(data$height)
hist(data$height,breaks = 100)


#计算28天住院死亡hospital_expire_flag28
data$hospital_expire_flag[is.na(data$hospital_expire_flag)]<-0
table(data$hospital_expire_flag)
data$hospital_expire_flag<-as.numeric(data$hospital_expire_flag)
data<-mutate(data,hospital_expire_flag7=if_else(los_hospital>7,0,hospital_expire_flag),
             hospital_expire_flag14=if_else(los_hospital>14, 0,hospital_expire_flag),
             hospital_expire_flag28=if_else(los_hospital>28, 0,hospital_expire_flag),
             hospital_expire_flag60=if_else(los_hospital>60,0,hospital_expire_flag),
             hospital_expire_flag90=if_else(los_hospital>90, 0,hospital_expire_flag),
             hospital_expire_flag365=if_else(los_hospital>365, 0,hospital_expire_flag))%>%
  mutate(los_hospital7=case_when(
    los_hospital>7~7,
    los_hospital<=7&hospital_expire_flag==0~los_hospital,
    los_hospital<=7&hospital_expire_flag==1~los_hospital
  ),
  los_hospital14=case_when(
    los_hospital>14~14,
    los_hospital<=14&hospital_expire_flag==0~los_hospital,
    los_hospital<=14&hospital_expire_flag==1~los_hospital
  ),
  los_hospital28=case_when(
    los_hospital>28~28,
    los_hospital<=28&hospital_expire_flag==0~los_hospital,
    los_hospital<=28&hospital_expire_flag==1~los_hospital
  ),
  los_hospital60sim=case_when(
    los_hospital>60~60,
    los_hospital<=60&hospital_expire_flag==0~los_hospital,
    los_hospital<=60&hospital_expire_flag==1~los_hospital
  ),
  los_hospital90=case_when(
    los_hospital>90~90,
    los_hospital<=90&hospital_expire_flag==0~los_hospital,
    los_hospital<=90&hospital_expire_flag==1~los_hospital
  ),
  los_hospital365=case_when(
    los_hospital>365~365,
    los_hospital<=365&hospital_expire_flag==0~los_hospital,
    los_hospital<=365&hospital_expire_flag==1~los_hospital
  ))

table(data$hospital_expire_flag28)



#整理aki的数据
#首先提出患者发生aki的最短时间
aki_offset<-dplyr::select(data,aki1_offset,aki2_offset,aki3_offset)%>%
  apply(1,min ,na.rm=T)
#由于offset时间是相对于进入icu的时间，因此需要换算为相对于入院的时间，加1是为了避免0或者负数
data$aki_offset_day<-(aki_offset-data$hospitaladmitoffset)/60/24
data$aki_offset_day[data$aki_offset_day<0]<-0.1 #把那些在入院前就已经aki的患者的aki时间设置到0.1
hist(data$aki_offset_day,breaks = 100)
summary(data$aki_offset_day)
#我们只看28天内发生aki的时间
data<-data%>%
  mutate(aki_los28=case_when(aki_offset_day==Inf&los_hospital>=28~28,
                             aki_offset_day==Inf&los_hospital<28~los_hospital,
                             aki_offset_day>=28~28,
                             aki_offset_day<28~aki_offset_day
  ))

# data<-data%>%
#   mutate(aki_los28=case_when(is.na(aki_offset_day)&los_hospital>=28~28,
#                              is.na(aki_offset_day)&los_hospital<28~los_hospital,
#                              aki_offset_day>=28~28,
#                              aki_offset_day<28~aki_offset_day))



#28天内的aki_flag28标记，大于28天才发生的将被剔除
data<-mutate(data,aki_flag28=case_when(aki_offset_day==Inf~0, #na说明没有aki
                                       aki_offset_day>28~0,       #大于28天也没有aki
                                       aki_offset_day<=28~as.numeric(aki_flag)
))
hist(data$aki_offset_day,breaks = 1000,xlim = c(0,22))

data$aki_offset_day
data$aki_flag


#计算CRRT_days时间/crrt_free28时间,#28天无ICU住院时间

data$rrt_days[is.na(data$rrt_days)]<-0
# data$CRRT_days<-as.numeric(data$CRRT_days)
data<-mutate(data,crrt_free28=case_when(rrt_days>28~0, # 28天无CRRT时间
                                        hospital_expire_flag==1~0,
                                        TRUE~28-rrt_days))

data<-mutate(data,icufree28=case_when(los_icu>28~0,   
                                      hospital_expire_flag==1~0,
                                      TRUE~28-los_icu))

data$norepi_los[is.na(data$norepi_los)]<-0
data<-mutate(data,norepifree28=case_when(norepi_los>28*24*60~0,   
                                         hospital_expire_flag==1~0,
                                         TRUE~28*24*60-los_icu))

data$dopamine_los[is.na(data$dopamine_los)]<-0
data<-mutate(data,dopaminefree28=case_when(dopamine_los>28*24*60~0,   
                                           hospital_expire_flag==1~0,
                                           TRUE~28*24*60-los_icu))

#为了进行竞争风险模型操作，我们生成一个新的event，竞争事件死亡event=2，主要观察结局aki_flag28=1,aki+event=1，event_time的也是一样

data<-data%>%
  mutate(event28=case_when(
    hospital_expire_flag28==0&aki_flag28==0~0,
    hospital_expire_flag28==1&aki_flag28==0~2,
    hospital_expire_flag28==0&aki_flag28==1~1,
    hospital_expire_flag28==1&aki_flag28==1~1
  ))
data<-data%>%
  mutate(event_time28=case_when(
    hospital_expire_flag28==0&aki_flag28==0~los_hospital28,
    hospital_expire_flag28==1&aki_flag28==0~los_hospital28,
    hospital_expire_flag28==0&aki_flag28==1~aki_los28,
    hospital_expire_flag28==1&aki_flag28==1~aki_los28
  ))
#名称缩短
data$LMH<-data$low_molecular_heparin
data$CCI<-data$charlson_comorbidity_index

write.csv(data,"data/antiwashed.csv")

length(data$X)
