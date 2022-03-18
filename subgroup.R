rm(list = ls())
data<-read.csv("data/antiwashed.csv")
datamch<-read.csv("data/datamch.csv")
data$LMH<-data$low_molecular_heparin
datamch$LMH<-datamch$low_molecular_heparin
library(survival)
library(tableone)
library(broom)
####################################################
##########                               ###########
##########           未匹配亚组分析      ###########
##########                               ###########
####################################################
#==broom::tidy(model,exponational=T,conf.int=T)可以显示
#对年龄来进行亚组分析
svakisub65more<-coxph(Surv(data$los_hospital28,data$hospital_expire_flag28)~low_molecular_heparin,subset=age>=65,data=data)
svakisub65less<-coxph(Surv(data$los_hospital28,data$hospital_expire_flag28)~low_molecular_heparin,subset=age<65,data=data)
data$age_group<-cut(data$age,breaks=c(0,65,300),right=F,labels = c("65_younger", "65_oler"))
svakisub65intact<-coxph(Surv(data$los_hospital28,data$hospital_expire_flag28)~low_molecular_heparin*age_group,data=data)

library(broom)
agesub<-cbind(rbind(tidy(svakisub65intact,exponentiate = T,conf.int = T),
      tidy(svakisub65more,exponentiate = T,conf.int = T),
      tidy(svakisub65less,exponentiate = T,conf.int = T)),
rbind(ShowRegTable(svakisub65intact),
              ShowRegTable(svakisub65more),
              ShowRegTable(svakisub65less))
)

agesub

#对database进行亚组分析
svakisubdatabaseintact<-coxph(Surv(data$los_hospital28,data$hospital_expire_flag28)~low_molecular_heparin*database,data=data)
svakisub3<-coxph(Surv(data$los_hospital28,data$hospital_expire_flag28)~low_molecular_heparin,subset=database=="mimic3",data=data)
svakisub4<-coxph(Surv(data$los_hospital28,data$hospital_expire_flag28)~low_molecular_heparin,subset=database=="mimic4",data=data)
svakisubeicu<-coxph(Surv(data$los_hospital28,data$hospital_expire_flag28)~low_molecular_heparin,subset=database=="eicu",data=data)
datasetsub<-cbind(rbind(tidy(svakisubdatabaseintact,exponentiate = T,conf.int = T),
      tidy(svakisub3,exponentiate = T,conf.int = T),
      tidy(svakisub4,exponentiate = T,conf.int = T),
      tidy(svakisubeicu,exponentiate = T,conf.int = T)
      ),
rbind(ShowRegTable(svakisubdatabaseintact),
                  ShowRegTable(svakisub3),
                  ShowRegTable(svakisub4),
                  ShowRegTable(svakisubeicu))
)
datasetsub
tidy(svakisubdatabaseintact,exponentiate = T,conf.int = T)
#有无糖尿病进行亚组分析
data$diabetes<-0
data$diabetes[datamch$diabetes_with_cc==1|datamch$diabetes_without_cc==1]<-1


svakisubdm<-coxph(Surv(data$los_hospital28,data$hospital_expire_flag28)~low_molecular_heparin,subset=diabetes==1,data=data)
svakisubnodm<-coxph(Surv(data$los_hospital28,data$hospital_expire_flag28)~low_molecular_heparin,subset=diabetes==0,data=data)
svakisubdmintact<-coxph(Surv(data$los_hospital28,data$hospital_expire_flag28)~low_molecular_heparin*diabetes,data=data)

dmsub<-
 cbind(rbind(
    tidy(svakisubdmintact,exponentiate = T,conf.int = T),
    tidy(svakisubdm,exponentiate = T,conf.int = T),
    tidy(svakisubnodm,exponentiate = T,conf.int = T)
  ),
  
  
  rbind(ShowRegTable(svakisubdmintact),
             ShowRegTable(svakisubdm),
             ShowRegTable(svakisubnodm))
 )
dmsub
#对有无脓毒症进行亚组分析

svakisubsepsis<-coxph(Surv(data$los_hospital28,data$hospital_expire_flag28)~low_molecular_heparin,subset=sepsis==1,data=data)
svakisubnosepsis<-coxph(Surv(data$los_hospital28,data$hospital_expire_flag28)~low_molecular_heparin,subset=sepsis==0,data=data)
svakisubsepsisintact<-coxph(Surv(data$los_hospital28,data$hospital_expire_flag28)~low_molecular_heparin*sepsis,data=data)

sepsissub<- cbind(
  rbind(tidy(svakisubsepsisintact,exponentiate = T,conf.int = T),
         tidy(svakisubsepsis,exponentiate = T,conf.int = T),
         tidy(svakisubnosepsis,exponentiate = T,conf.int = T)),
  rbind(ShowRegTable(svakisubsepsisintact),
             ShowRegTable(svakisubsepsis),
             ShowRegTable(svakisubnosepsis))
  )
sepsissub


write.csv(rbind(agesub,dmsub,datasetsub,sepsissub),"data/subgroupdata.csv")
#==================未匹配数据画图====================

###=============================================================================
#   方法一 我个人认为最好的方法
# 直接用forestploter包()画图，
#但是这个方法可以使用ggplot的函数，可以多图输出
#需要使用ggforestplot 包
###=============================================================================
library(forestploter)
forestdata<-read.csv("data/subgroupdata_modify.csv",header = T)
#NA to blank or NA will be transformed to carachter.
#forestdata$se <- (log(forestdata$hi) - log(forestdata$est))/1.96
#forestdata$est<-forestdata$HR #把HR输入到est中，防止在转NA为空值的时候num变string
#forestdata$num<-forestdata$N
#forestdata$N<- ifelse(is.na(forestdata$N), "", forestdata$N)
#forestdata$HR <- ifelse(is.na(forestdata$HR), "", forestdata$HR)

#forestdata这里必须要装载一个空的列，用来存放ci_column, 字符宽度就是图片宽度也即是20 其中的20代表图的宽度
forestdata$` ` <- paste(rep(" ", 20), collapse = " ")

forestdata

tm <- forest_theme(base_size = 10,
                   refline_col = "red",
                   footnote_col = "#636363",
                   footnote_fontface = "italic",
                   xaxis_lwd = 2)
head(forestdata)
p <- forest(forestdata[,c(1,8,10,9)], #用来展示的数据，这些数据尽量处理成为string
            est = forestdata$estimate,
            lower = forestdata$conf.low, 
            upper = forestdata$conf.high,
            sizes = forestdata$std.error,
            ci_column = 3,
            ref_line = 1,
            arrow_lab = c("LMH better", "LMH worse"),
            xlim = c(0.4, 1.1),
            xaxis = set_xaxis(break_at = c(0.4, 0.5,0.75,1)),
            footnote = "Subgroup analysis in the unmatched cohort.",
            theme = tm)

p
####################################################
##########                               ###########
##########         配对组画图            ###########
##########                               ###########
####################################################
#对年龄来进行亚组分析

#==broom::tidy(model,exponational=T,conf.int=T)可以显示
#对年龄来进行亚组分析
svakisub65more<-coxph(Surv(datamch$los_hospital28,datamch$hospital_expire_flag28)~low_molecular_heparin,subset=age>=65,data=datamch)
svakisub65less<-coxph(Surv(datamch$los_hospital28,datamch$hospital_expire_flag28)~low_molecular_heparin,subset=age<65,data=datamch)
datamch$age_group<-cut(datamch$age,breaks=c(0,65,300),right=F,labels = c("65_younger", "65_oler"))
svakisub65intact<-coxph(Surv(datamch$los_hospital28,datamch$hospital_expire_flag28)~low_molecular_heparin*age_group,data=datamch)

library(broom)
agesub<-cbind(rbind(tidy(svakisub65intact,exponentiate = T,conf.int = T),
                    tidy(svakisub65more,exponentiate = T,conf.int = T),
                    tidy(svakisub65less,exponentiate = T,conf.int = T)),
              rbind(ShowRegTable(svakisub65intact),
                    ShowRegTable(svakisub65more),
                    ShowRegTable(svakisub65less))
)

agesub

#对database进行亚组分析
svakisubdatabaseintact<-coxph(Surv(datamch$los_hospital28,datamch$hospital_expire_flag28)~low_molecular_heparin*database,data=datamch)
svakisub3<-coxph(Surv(datamch$los_hospital28,datamch$hospital_expire_flag28)~low_molecular_heparin,subset=database=="mimic3",data=datamch)
svakisub4<-coxph(Surv(datamch$los_hospital28,datamch$hospital_expire_flag28)~low_molecular_heparin,subset=database=="mimic4",data=datamch)
svakisubeicu<-coxph(Surv(datamch$los_hospital28,datamch$hospital_expire_flag28)~low_molecular_heparin,subset=database=="eicu",data=datamch)
datasetsub<-cbind(rbind(tidy(svakisubdatabaseintact,exponentiate = T,conf.int = T),
                        tidy(svakisub3,exponentiate = T,conf.int = T),
                        tidy(svakisub4,exponentiate = T,conf.int = T),
                        tidy(svakisubeicu,exponentiate = T,conf.int = T)
),
rbind(ShowRegTable(svakisubdatabaseintact),
      ShowRegTable(svakisub3),
      ShowRegTable(svakisub4),
      ShowRegTable(svakisubeicu))
)
datasetsub
tidy(svakisubdatabaseintact,exponentiate = T,conf.int = T)
#有无糖尿病进行亚组分析
datamch$diabetes<-0
datamch$diabetes[datamch$diabetes_with_cc==1|datamch$diabetes_without_cc==1]<-1


svakisubdm<-coxph(Surv(datamch$los_hospital28,datamch$hospital_expire_flag28)~low_molecular_heparin,subset=diabetes==1,data=datamch)
svakisubnodm<-coxph(Surv(datamch$los_hospital28,datamch$hospital_expire_flag28)~low_molecular_heparin,subset=diabetes==0,data=datamch)
svakisubdmintact<-coxph(Surv(datamch$los_hospital28,datamch$hospital_expire_flag28)~low_molecular_heparin*diabetes,data=datamch)

dmsub<-
  cbind(rbind(
    tidy(svakisubdmintact,exponentiate = T,conf.int = T),
    tidy(svakisubdm,exponentiate = T,conf.int = T),
    tidy(svakisubnodm,exponentiate = T,conf.int = T)
  ),
  
  
  rbind(ShowRegTable(svakisubdmintact),
        ShowRegTable(svakisubdm),
        ShowRegTable(svakisubnodm))
  )
dmsub
#对有无脓毒症进行亚组分析

svakisubsepsis<-coxph(Surv(datamch$los_hospital28,datamch$hospital_expire_flag28)~low_molecular_heparin,subset=sepsis==1,data=datamch)
svakisubnosepsis<-coxph(Surv(datamch$los_hospital28,datamch$hospital_expire_flag28)~low_molecular_heparin,subset=sepsis==0,data=datamch)
svakisubsepsisintact<-coxph(Surv(datamch$los_hospital28,datamch$hospital_expire_flag28)~low_molecular_heparin*sepsis,data=datamch)

sepsissub<- cbind(
  rbind(tidy(svakisubsepsisintact,exponentiate = T,conf.int = T),
        tidy(svakisubsepsis,exponentiate = T,conf.int = T),
        tidy(svakisubnosepsis,exponentiate = T,conf.int = T)),
  rbind(ShowRegTable(svakisubsepsisintact),
        ShowRegTable(svakisubsepsis),
        ShowRegTable(svakisubnosepsis))
)
sepsissub


write.csv(rbind(agesub,dmsub,datasetsub,sepsissub),"data/subgroupdatamch.csv")
#==================匹配数据画图====================

###=============================================================================
#   方法一 我个人认为最好的方法
# 直接用forestploter包()画图，
#但是这个方法可以使用ggplot的函数，可以多图输出
#需要使用ggforestplot 包
###=============================================================================
library(forestploter)
forestdata<-read.csv("data/subgroupdatamch_modify.csv",header = T)
#NA to blank or NA will be transformed to carachter.
#forestdata$se <- (log(forestdata$hi) - log(forestdata$est))/1.96
#forestdata$est<-forestdata$HR #把HR输入到est中，防止在转NA为空值的时候num变string


#forestdata这里必须要装载一个空的列，用来存放ci_column, 字符宽度就是图片宽度也即是20 其中的20代表图的宽度
forestdata$` ` <- paste(rep(" ", 20), collapse = " ")

forestdata

#forestdata$p.inte<-as.character(forestdata$p.inte)

tm <- forest_theme(base_size = 10,
                   refline_col = "red",
                   footnote_col = "#636363",
                   footnote_fontface = "italic",
                   xaxis_lwd = 2)
head(forestdata)
p2 <- forest(forestdata[,c(1,8,10,9)], #用来展示的数据，这些数据尽量处理成为string
            est = forestdata$estimate,
            lower = forestdata$conf.low, 
            upper = forestdata$conf.high,
            sizes = forestdata$std.error,
            ci_column = 3,
            ref_line = 1,
            arrow_lab = c("LMH better", "LMH worse"),
            xlim = c(0.4, 1.1),
            xaxis = set_xaxis(break_at = c(0.4, 0.5,0.75,1.1)),
            footnote = "Subgroup analysis in the unmatched cohort.",
            theme = tm)

p2

ggarrange(p,p2,ncol = 2,nrow = 1,labels=c("A","B"))%>%
  ggsave(filename = "fig_table/subgroup.pdf",width=10,height=5,units = "in",dpi=300)

####################################################
##########                               ###########
##########           LMH*age交互作用     ###########
##########                               ###########
####################################################
surv28<-Surv(data$los_hospital28,data$hospital_expire_flag28)
data$`LMH:age`=data$LMH*data$age
data$CCI<-data$charlson_comorbidity_index
model_inter<-coxph(surv28~LMH+age+`LMH:age`+sepsis+sapsii+CCI+heartrate_max+resprate_max+spo2_min+creatinine_max+wbc_max,data=data)
ShowRegTable(model_inter,digits = 4)
#无法显示交互项
ggforest(model_inter,data=data,noDigits = 3)

survmch28<-Surv(datamch$los_hospital28,datamch$hospital_expire_flag28)
datamch$`LMH:age`=datamch$low_molecular_heparin*datamch$age
datamch$CCI<-datamch$charlson_comorbidity_index
model_inter_mch<-coxph(survmch28~LMH+age+`LMH:age`+sepsis+sapsii+CCI+heartrate_max+resprate_max+spo2_min+creatinine_max+wbc_max,data=datamch)
ggforest(model_inter_mch,data=data,noDigits = 3)

ggarrange(ggforest(model_inter,data=data,noDigits = 3),
          ggforest(model_inter_mch,data=data,noDigits = 3),ncol = 2,nrow = 1,labels = c("A","B")
          )%>%
  ggsave(filename = "fig_table/plot_inter.pdf",width = 12,height = 6,units = "in",onefile=F)

####################################################
##########                               ###########
##########        LMH*sepsis交互作用     ###########
##########                               ###########
####################################################
surv28<-Surv(data$los_hospital28,data$hospital_expire_flag28)
data$`LMH:sepsis`=data$low_molecular_heparin*data$sepsis
model_inter<-coxph(surv28~LMH+age+`LMH:sepsis`+sepsis+sapsii+CCI+heartrate_max+resprate_max+spo2_min+creatinine_max+wbc_max,data=data)
ShowRegTable(model_inter,digits = 4)
#无法显示交互项
ggforest(model_inter,data=data,noDigits = 3)

survmch28<-Surv(datamch$los_hospital28,datamch$hospital_expire_flag28)
datamch$`LMH:sepsis`=datamch$low_molecular_heparin*datamch$sepsis
model_inter_mch<-coxph(survmch28~LMH+age+`LMH:sepsis`+sepsis+sapsii+CCI+heartrate_max+resprate_max+spo2_min+creatinine_max+wbc_max,data=datamch)
ggforest(model_inter_mch,data=data,noDigits = 3)

ggarrange(ggforest(model_inter,data=data,noDigits = 3),
          ggforest(model_inter_mch,data=data,noDigits = 3),ncol = 2,nrow = 1,labels = c("A","B")
)%>%
  ggsave(filename = "fig_table/plot_lmh_sepsis.pdf",width = 12,height = 6,units = "in",onefile=F)

