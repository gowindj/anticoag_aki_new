rm(list = ls())
data<-read.csv("data/antiwashed.csv")
datamch<-read.csv("data/datamch.csv")


# primary outcome: kaplan-meier plot
library(survival)
library(survminer)
library(tableone)
library(xtable)
library(flextable)
library(officer)
library(dplyr)


#28天发生死亡的绝对值
CreateTableOne(vars = c("hospital_expire_flag28"),strata = "low_molecular_heparin", data = data,factorVars = "hospital_expire_flag28")
CreateTableOne(vars = c("hospital_expire_flag28"),strata = "low_molecular_heparin", data = datamch,factorVars = "hospital_expire_flag28")




#===对计量进行修正，NA的给标记为0
data$lmh_daily_dose[is.na(data$lmh_daily_dose)]<-0
data$lmh_total_dose[is.na(data$lmh_total_dose)]<-0

datamch$lmh_daily_dose[is.na(datamch$lmh_daily_dose)]<-0
datamch$lmh_total_dose[is.na(datamch$lmh_total_dose)]<-0


#28天生存分析
surv28<-Surv(data$los_hospital28,data$hospital_expire_flag28)
survmch28<-Surv(datamch$los_hospital28,datamch$hospital_expire_flag28)

#lmh单因素分析
ShowRegTable(coxph(surv28~low_molecular_heparin,data = data))
ShowRegTable(coxph(survmch28~low_molecular_heparin,data = datamch))
#aki~dose单因素分析
coxph(surv28~lmh_daily_dose,data = data)
coxph(surv28~lmh_total_dose,data = data)

coxph(survmch28~lmh_daily_dose,data = datamch)
coxph(survmch28~lmh_total_dose,data = datamch)
#survplot2in3为匹配后和非匹配的28天AKI发生率的生存分析

#aki~lmh多因素分析

model_surv<-coxph(surv28~low_molecular_heparin+age+gender+sapsii+charlson_comorbidity_index  +heartrate_max+resprate_max+spo2_min+creatinine_max+wbc_max,data = data)
#~lmh 配对cohort多因素分析

model_surv_mch<-coxph(survmch28~low_molecular_heparin+age+gender+sapsii+charlson_comorbidity_index  +heartrate_max+resprate_max+spo2_min+creatinine_max+wbc_max,data = datamch)
ShowRegTable(model_surv,digits = 3)
ShowRegTable(model_surv_mch,digits = 3)

#~dose多因素分析
data$daily_dose<-data$lmh_daily_dose/10
data$total_dose <- data$lmh_total_dose/10
model_surv_dailydose<-coxph(surv28~daily_dose+age+gender+sapsii+heartrate_max+resprate_max+spo2_min+creatinine_max+wbc_max,data = data)
model_surv_totaldose<-coxph(surv28~total_dose+age+gender+sapsii +heartrate_max+resprate_max+spo2_min+creatinine_max+wbc_max,data = data)
ShowRegTable(model_surv_dailydose,digits = 4)
ShowRegTable(model_surv_totaldose,digits = 4)

#dose多因素分析,配对
datamch$daily_dose<-datamch$lmh_daily_dose/10
datamch$total_dose <- datamch$lmh_total_dose/10
modelmch_surv_dailydose<-coxph(survmch28~daily_dose+age+gender+sapsii+heartrate_max+resprate_max+spo2_min+creatinine_max+wbc_max,data = datamch)
modelmch_surv_totaldose<-coxph(survmch28~total_dose+age+gender+sapsii+heartrate_max+resprate_max+spo2_min+creatinine_max+wbc_max,data = datamch)
ShowRegTable(modelmch_surv_dailydose,digits = 4)
ShowRegTable(modelmch_surv_totaldose,digits = 4)
ggsave(
  ggarrange(
    ggforest(model_surv_dailydose,data=data,noDigits = 3,main = "HR in unmatched cohort"),
    ggforest(model_surv_totaldose,data=data,noDigits=3,main = "HR in unmatched cohort"),
    ggforest(modelmch_surv_dailydose,data=datamch,noDigits = 3,main = "HR in matched cohort"),
    ggforest(modelmch_surv_totaldose,data=datamch,noDigits = 3,main = "HR in matched cohort"),
    nrow = 2,ncol = 2,labels = c("A","B","C","D")
  ),
  filename = "fig_table/ggforest_dose_4in1.pdf",
  width = 14,height = 10,units = "in",
  onefile=FALSE
)
#aki~obesity双重稳健分析
library(gbm)
library(twang)
ps_gbm<-readRDS("rds/ps_gbm.RData", refhook = NULL)
#caculate the weight from GBM model
data$weight_gbm = get.weights(ps_gbm, stop.method = "es.mean")
#加权分析
model_surv_weighted<-coxph(surv28~low_molecular_heparin+age+gender+sapsii+charlson_comorbidity_index  +heartrate_max+resprate_max+spo2_min+creatinine_max+wbc_max,data = data,weights = weight_gbm)
ShowRegTable(model_surv_weighted,digits=3)
saveRDS(model_surv_weighted,file = "rds/model_surv_weighted.rds")


regtable3in1<-rbind(ShowRegTable(model_surv),ShowRegTable(model_surv_mch),ShowRegTable(model_surv_weighted))
colnames(regtable3in1)<-c("HRs in nonmatched cohort[confint]","pval1","HRs in matched cohort[confint]","pval2","HRs in doubly robust model","paval3")

#输出三线表到 fig_table/regtable.doc 其中的
read_docx()%>%
  body_add_par(value = "Table2 Multivariate COX model",style = "Normal")%>%
  body_add_flextable(xtable_to_flextable(xtable(regtable3in1)))%>%
  body_add_par(value = "SAPSII: Simplified Acute Physiology Score II.",style = "Normal")%>%
  print("fig_table/regtable3in1.docx")
#直接输出三个回归分析的森林图到regforest3in1图片中 
ggarrange( ggforest(model_surv,data=data,cpositions = c(0,0.25,0.41),fontsize = 0.48,noDigits = 3,ggtitle("COX regression in unmatched cohort"))
           ,ggforest(model_surv_mch,data=datamch,cpositions = c(0,0.25,0.41),fontsize = 0.48,noDigits = 3,ggtitle("COX regression in matched cohort"))
           ,ggforest(model_surv_weighted,data=datamch,cpositions = c(0,0.25,0.41),fontsize = 0.48,noDigits = 3,ggtitle("Doubly robust regression"))
           ,nrow = 3,ncol = 1,labels = c("A","B","C"))%>%
  ggsave(filename = "fig_table/regforest3in1.pdf",width = 6,height = 10,units = "in",onefile=F)




#survplot2in3为匹配后和非匹配的28天AKI发生率的生存分析
#ggforest(coxph(surv28~obesity,data = data),data=data)

svplot28unmatch<-ggsurvplot(survfit(surv28~low_molecular_heparin,data = data),data=data,
                            pval = TRUE, conf.int = TRUE,
                            pval.method=TRUE,
                            pval.coord=c(7,0.3),
                            pval.method.cood=c(3,0.1),
                            
                            risk.table = TRUE, # Add risk table
                            fontsize=3,
                            risk.table.col = "strata", # Change risk table color by groups
                            # linetype = "strata", # Change line type by groups 
                            #urv.median.line = "hv", # Specify median survival
                            ggtheme = theme_classic(),   #, # Change ggplot2 theme
                            #palette = c("#E7B800", "#2E9FDF"),
                            palette=c("grey60",	"grey0") ,
                            censor.shape="",
                            #fun = "cumhaz",
                            #legend=c(0.7,0.9),
                            legend.labs =    c("no-LMH", "LMH")  ,
                            xlab="Day",
                            ylab="Survive probability",
                            title="Nnmatched cohort",
                            font.mian=c(12,"black"),
                            font.x=c(12,"black"),
                            font.y=c(12,"black"),
                            font.tickslab=c(12,"plain","black"),
                            
                            title.position=c(0.5,0.5))
#========生存分析画图==========
svplot28match<-ggsurvplot(survfit(survmch28~low_molecular_heparin,data = datamch),data=datamch,
                          pval = TRUE, conf.int = TRUE,
                          pval.method=TRUE,
                          pval.coord=c(7,0.3),
                          pval.method.cood=c(3,0.1),
                          
                          risk.table = TRUE, # Add risk table
                          fontsize=3,
                          risk.table.col = "strata", # Change risk table color by groups
                          # linetype = "strata", # Change line type by groups 
                          #urv.median.line = "hv", # Specify median survival
                          ggtheme = theme_classic(),   #, # Change ggplot2 theme
                          #palette = c("#E7B800", "#2E9FDF"),
                          palette=c("grey60",	"grey0") ,
                          censor.shape="",
                          #fun = "cumhaz",
                          #legend=c(0.7,0.9),
                          legend.labs =    c("no-LMH", "LMH")  ,
                          xlab="Day",
                          ylab="Survive probability",
                          title="Matched cohort",
                          font.mian=c(12,"black"),
                          font.x=c(12,"black"),
                          font.y=c(12,"black"),
                          font.tickslab=c(12,"plain","black"),
                          title.position=c(0.5,0.5))
svplot28match


svplotaki_moratlity2in1<-ggarrange(
                                   svplot28unmatch$plot,svplot28match$plot,
                                   svplot28unmatch$table,svplot28match$table,
                                   heights = c(2,1,2,1),widths = c(1,1) ,
                                   ncol = 2, nrow = 2,align="hv", plotlist = NULL, 
                                   labels = c("A","B","",""),  
                                   # label.x = 0, label.y = 1,
                                   hjust = -0.5, vjust = 1.5, 
                                   font.label = list(size = 14, 
                                                     color = "black", face = "bold", family = NULL),  
                                   legend = NULL, common.legend = TRUE) 

ggsave("fig_table/svplotaki_moratlity2in1.pdf",plot=svplotaki_moratlity2in1,width = 10,height =5 )




