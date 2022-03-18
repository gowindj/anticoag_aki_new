rm(list = ls())
data<-read.csv("data/antiwashed.csv")
datamch<-read.csv("data/datamch.csv")

#将民族进行汇总计算
View(table(data$ethnicity))
data$ethnicity[grep("african american",data$ethnicity,ignore.case = T)]<-"BLACK"
data$ethnicity[grep("AMERICAN INDIAN",data$ethnicity,ignore.case = T)]<-"AMERICAN INDIAN"
data$ethnicity[grep("Caucasian",data$ethnicity,ignore.case = T)]<-"WHITE"
data$ethnicity[grep("asian",data$ethnicity,ignore.case = T)]<-"ASIAN"
data$ethnicity[grep("BLACK",data$ethnicity,ignore.case = T)]<-"BLACK"
data$ethnicity[grep("CARIBBEAN ISLAND",data$ethnicity,ignore.case = T)]<-"OTHER"
data$ethnicity[grep("Hispanic",data$ethnicity,ignore.case = T)]<-"HISPANIC"
data$ethnicity[grep("MIDDLE EASTERN",data$ethnicity,ignore.case = T)]<-"OTHER"
data$ethnicity[grep("MULTI RACE ETHNICITY",data$ethnicity,ignore.case = T)]<-"OTHER"
data$ethnicity[grep("NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",data$ethnicity,ignore.case = T)]<-"OTHER"
data$ethnicity[grep("Native American",data$ethnicity,ignore.case = T)]<-"AMERICAN INDIAN"
data$ethnicity[grep("OTHER",data$ethnicity,ignore.case = T)]<-"OTHER"
data$ethnicity[grep("PATIENT DECLINED TO ANSWER",data$ethnicity,ignore.case = T)]<-"OTHER"
data$ethnicity[grep("PORTUGUESE",data$ethnicity,ignore.case = T)]<-"WHITE"
data$ethnicity[grep("SOUTH AMERICAN",data$ethnicity,ignore.case = T)]<-"OTHER"
data$ethnicity[grep("UNABLE TO OBTAIN",data$ethnicity,ignore.case = T)]<-"OTHER"
data$ethnicity[grep("UNKNOWN",data$ethnicity,ignore.case = T)]<-"OTHER"
data$ethnicity[grep("WHITE",data$ethnicity,ignore.case = T)]<-"WHITE"
data$ethnicity[grep("PORTUGUESE",data$ethnicity,ignore.case = T)]<-"WHITE"
data$ethnicity[data$ethnicity==""]<-"OTHER"
#将datamch民族进行汇总计算
View(table(datamch$ethnicity))
datamch$ethnicity[grep("african american",datamch$ethnicity,ignore.case = T)]<-"BLACK"
datamch$ethnicity[grep("Caucasian",datamch$ethnicity,ignore.case = T)]<-"WHITE"
datamch$ethnicity[grep("AMERICAN INDIAN",datamch$ethnicity,ignore.case = T)]<-"AMERICAN INDIAN"
datamch$ethnicity[grep("asian",datamch$ethnicity,ignore.case = T)]<-"ASIAN"
datamch$ethnicity[grep("BLACK",datamch$ethnicity,ignore.case = T)]<-"BLACK"
datamch$ethnicity[grep("CARIBBEAN ISLAND",datamch$ethnicity,ignore.case = T)]<-"OTHER"
datamch$ethnicity[grep("Hispanic",datamch$ethnicity,ignore.case = T)]<-"HISPANIC"
datamch$ethnicity[grep("MIDDLE EASTERN",datamch$ethnicity,ignore.case = T)]<-"OTHER"
datamch$ethnicity[grep("MULTI RACE ETHNICITY",datamch$ethnicity,ignore.case = T)]<-"OTHER"
datamch$ethnicity[grep("NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",datamch$ethnicity,ignore.case = T)]<-"OTHER"
datamch$ethnicity[grep("Native American",datamch$ethnicity,ignore.case = T)]<-"AMERICAN INDIAN"
datamch$ethnicity[grep("OTHER",datamch$ethnicity,ignore.case = T)]<-"OTHER"
datamch$ethnicity[grep("PATIENT DECLINED TO ANSWER",datamch$ethnicity,ignore.case = T)]<-"OTHER"
datamch$ethnicity[grep("PORTUGUESE",datamch$ethnicity,ignore.case = T)]<-"WHITE"
datamch$ethnicity[grep("SOUTH AMERICAN",datamch$ethnicity,ignore.case = T)]<-"OTHER"
datamch$ethnicity[grep("UNABLE TO OBTAIN",datamch$ethnicity,ignore.case = T)]<-"OTHER"
datamch$ethnicity[grep("UNKNOWN",datamch$ethnicity,ignore.case = T)]<-"OTHER"
datamch$ethnicity[grep("WHITE",datamch$ethnicity,ignore.case = T)]<-"WHITE"
datamch$ethnicity[grep("PORTUGUESE",datamch$ethnicity,ignore.case = T)]<-"WHITE"
datamch$ethnicity[datamch$ethnicity==""]<-"OTHER"

#对诊断进行汇总
View(table(data$diagnosis))
data$diagnosis_agg<-"OTHER"
#sepsis
data$diagnosis_agg[grep("sepsis",data$diagnosis,ignore.case = T)]<-"Sepsis"
data$diagnosis_agg[grep("septic",data$diagnosis,ignore.case = T)]<-"Sepsis"
data$diagnosis_agg[grep("PANCRI",data$diagnosis,ignore.case = T)]<-"Sepsis"
data$diagnosis_agg[grep("CELLULITIS",data$diagnosis,ignore.case = T)]<-"Sepsis"
data$diagnosis_agg[grep("sepsis",data$diagnosis,ignore.case = T)]<-"Sepsis"
data$diagnosis_agg[grep("sepsis",data$diagnosis,ignore.case = T)]<-"Sepsis"
data$diagnosis_agg[grep("sepsis",data$diagnosis,ignore.case = T)]<-"Sepsis"


grep("CARD",data$diagnosis,value=T,ignore.case = T)

#Cardiovascular disease
data$diagnosis_agg[grep("ami",data$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
data$diagnosis_agg[grep("myocard",data$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
data$diagnosis_agg[grep("cardio",data$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
#Congestive heart failure
data$diagnosis_agg[grep("chf",data$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
data$diagnosis_agg[grep("congestive",data$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
data$diagnosis_agg[grep("S-CABG",data$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
data$diagnosis_agg[grep("S-VALVAO",data$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
data$diagnosis_agg[grep("HYPERTENS",data$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
data$diagnosis_agg[grep("CARD",data$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
data$diagnosis_agg[grep("CORONARY",data$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
data$diagnosis_agg[grep("CHEST PAIN",data$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
data$diagnosis_agg[grep("heart",data$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
#CARDIAC ARREST
data$diagnosis_agg[grep("arrest",data$diagnosis,ignore.case = T)]<-"Cardiovascular disease"




#Neurological 
data$diagnosis_agg[grep("stroke",data$diagnosis,ignore.case = T)]<-"Neurological condition"
#cerebral hemorrhage
data$diagnosis_agg[grep("cerebral",data$diagnosis,ignore.case = T)]<-"Neurological condition"
data$diagnosis_agg[grep("cranial hemorrhage",data$diagnosis,ignore.case = T)]<-"Neurological condition"
data$diagnosis_agg[grep("SEIZURES",data$diagnosis,ignore.case = T)]<-"Neurological condition"
data$diagnosis_agg[grep("COMA",data$diagnosis,ignore.case = T)]<-"Neurological condition"
data$diagnosis_agg[grep("ICH",data$diagnosis,ignore.case = T)]<-"Neurological condition"
data$diagnosis_agg[grep("CRANNEO",data$diagnosis,ignore.case = T)]<-"Neurological condition"
data$diagnosis_agg[grep("NEURO",data$diagnosis,ignore.case = T)]<-"Neurological condition"
data$diagnosis_agg[grep("SAH",data$diagnosis,ignore.case = T)]<-"Neurological condition"
data$diagnosis_agg[grep("MENTAL",data$diagnosis,ignore.case = T)]<-"Neurological condition"
data$diagnosis_agg[grep("stroke",data$diagnosis,ignore.case = T)]<-"Neurological condition"
data$diagnosis_agg[grep("stroke",data$diagnosis,ignore.case = T)]<-"Neurological condition"
data$diagnosis_agg[grep("stroke",data$diagnosis,ignore.case = T)]<-"Neurological condition"

#respiratory ASTHMA
#pneumonia
data$diagnosis_agg[grep("pneumonia",data$diagnosis,ignore.case = T)]<-"Sepsis"
data$diagnosis_agg[grep("pneumB",data$diagnosis,ignore.case = T)]<-"Sepsis"
data$diagnosis_agg[grep("pneumv",data$diagnosis,ignore.case = T)]<-"Sepsis"
#COPD
data$diagnosis_agg[grep("emphys",data$diagnosis,ignore.case = T)]<-"Respiratory condition"
data$diagnosis_agg[grep("COPD",data$diagnosis,ignore.case = T)]<-"Respiratory condition"
data$diagnosis_agg[grep("chronic obstructive",data$diagnosis,ignore.case = T)]<-"Respiratory condition"
data$diagnosis_agg[grep("resp",data$diagnosis,ignore.case = T)]<-"Respiratory condition"
data$diagnosis_agg[grep("RESOTHER",data$diagnosis,ignore.case = T)]<-"Respiratory condition"
data$diagnosis_agg[grep("pulm",data$diagnosis,ignore.case = T)]<-"Respiratory condition"
data$diagnosis_agg[grep("ASTHMA",data$diagnosis,ignore.case = T)]<-"Respiratory condition"
data$diagnosis_agg[grep("ARDS",data$diagnosis,ignore.case = T)]<-"Respiratory condition"
data$diagnosis_agg[grep("LUNG",data$diagnosis,ignore.case = T)]<-"Respiratory condition"

#
#对datamch的诊断进行汇总
datamch$diagnosis_agg<-"OTHER"
#sepsis
datamch$diagnosis_agg[grep("sepsis",datamch$diagnosis,ignore.case = T)]<-"Sepsis"
datamch$diagnosis_agg[grep("septic",datamch$diagnosis,ignore.case = T)]<-"Sepsis"
datamch$diagnosis_agg[grep("PANCRI",datamch$diagnosis,ignore.case = T)]<-"Sepsis"
datamch$diagnosis_agg[grep("CELLULITIS",datamch$diagnosis,ignore.case = T)]<-"Sepsis"
datamch$diagnosis_agg[grep("sepsis",datamch$diagnosis,ignore.case = T)]<-"Sepsis"
datamch$diagnosis_agg[grep("sepsis",datamch$diagnosis,ignore.case = T)]<-"Sepsis"
datamch$diagnosis_agg[grep("sepsis",datamch$diagnosis,ignore.case = T)]<-"Sepsis"


grep("CARD",datamch$diagnosis,value=T,ignore.case = T)

#Cardiovascular disease
datamch$diagnosis_agg[grep("ami",datamch$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
datamch$diagnosis_agg[grep("myocard",datamch$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
datamch$diagnosis_agg[grep("cardio",datamch$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
#Congestive heart failure
datamch$diagnosis_agg[grep("chf",datamch$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
datamch$diagnosis_agg[grep("congestive",datamch$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
datamch$diagnosis_agg[grep("S-CABG",datamch$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
datamch$diagnosis_agg[grep("S-VALVAO",datamch$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
datamch$diagnosis_agg[grep("HYPERTENS",datamch$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
datamch$diagnosis_agg[grep("CARD",datamch$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
datamch$diagnosis_agg[grep("CORONARY",datamch$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
datamch$diagnosis_agg[grep("CHEST PAIN",datamch$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
datamch$diagnosis_agg[grep("heart",datamch$diagnosis,ignore.case = T)]<-"Cardiovascular disease"
#CARDIAC ARREST
datamch$diagnosis_agg[grep("arrest",datamch$diagnosis,ignore.case = T)]<-"Cardiovascular disease"




#Neurological 
datamch$diagnosis_agg[grep("stroke",datamch$diagnosis,ignore.case = T)]<-"Neurological condition"
#cerebral hemorrhage
datamch$diagnosis_agg[grep("cerebral",datamch$diagnosis,ignore.case = T)]<-"Neurological condition"
datamch$diagnosis_agg[grep("cranial hemorrhage",datamch$diagnosis,ignore.case = T)]<-"Neurological condition"
datamch$diagnosis_agg[grep("SEIZURES",datamch$diagnosis,ignore.case = T)]<-"Neurological condition"
datamch$diagnosis_agg[grep("COMA",datamch$diagnosis,ignore.case = T)]<-"Neurological condition"
datamch$diagnosis_agg[grep("ICH",datamch$diagnosis,ignore.case = T)]<-"Neurological condition"
datamch$diagnosis_agg[grep("CRANNEO",datamch$diagnosis,ignore.case = T)]<-"Neurological condition"
datamch$diagnosis_agg[grep("NEURO",datamch$diagnosis,ignore.case = T)]<-"Neurological condition"
datamch$diagnosis_agg[grep("SAH",datamch$diagnosis,ignore.case = T)]<-"Neurological condition"
datamch$diagnosis_agg[grep("MENTAL",datamch$diagnosis,ignore.case = T)]<-"Neurological condition"
datamch$diagnosis_agg[grep("stroke",datamch$diagnosis,ignore.case = T)]<-"Neurological condition"
datamch$diagnosis_agg[grep("stroke",datamch$diagnosis,ignore.case = T)]<-"Neurological condition"
datamch$diagnosis_agg[grep("stroke",datamch$diagnosis,ignore.case = T)]<-"Neurological condition"

#respiratory ASTHMA
#pneumonia
datamch$diagnosis_agg[grep("pneumonia",datamch$diagnosis,ignore.case = T)]<-"Sepsis"
datamch$diagnosis_agg[grep("pneumB",datamch$diagnosis,ignore.case = T)]<-"Sepsis"
datamch$diagnosis_agg[grep("pneumv",datamch$diagnosis,ignore.case = T)]<-"Sepsis"
#COPD
datamch$diagnosis_agg[grep("emphys",datamch$diagnosis,ignore.case = T)]<-"Respiratory condition"
datamch$diagnosis_agg[grep("COPD",datamch$diagnosis,ignore.case = T)]<-"Respiratory condition"
datamch$diagnosis_agg[grep("chronic obstructive",datamch$diagnosis,ignore.case = T)]<-"Respiratory condition"
datamch$diagnosis_agg[grep("resp",datamch$diagnosis,ignore.case = T)]<-"Respiratory condition"
datamch$diagnosis_agg[grep("RESOTHER",datamch$diagnosis,ignore.case = T)]<-"Respiratory condition"
datamch$diagnosis_agg[grep("pulm",datamch$diagnosis,ignore.case = T)]<-"Respiratory condition"
datamch$diagnosis_agg[grep("ASTHMA",datamch$diagnosis,ignore.case = T)]<-"Respiratory condition"
datamch$diagnosis_agg[grep("ARDS",datamch$diagnosis,ignore.case = T)]<-"Respiratory condition"
datamch$diagnosis_agg[grep("LUNG",datamch$diagnosis,ignore.case = T)]<-"Respiratory condition"

#bleed related
datamch$diagnosis_agg[grep("hemorrhage",datamch$diagnosis,ignore.case = T)]<-"Bleed"
datamch$diagnosis_agg[grep("bleed",datamch$diagnosis,ignore.case = T)]<-"Bleed"
datamch$diagnosis_agg[grep("hemato",datamch$diagnosis,ignore.case = T)]<-"Bleed"

table(datamch$diagnosis_agg)


#bleeding related 
c("AORTIC DISSECTION","ANEURYSM","BLEE","hemat","DISSECTION","hemorrhage"," kidney failure","renal failure","hepatic failure","HEMATOMA")


#人口学参数
tablevar<-c("age","gender","sapsii","height","weight","BMI","diagnosis_agg","peripheral_vascular_disease","cerebrovascular_disease","chronic_pulmonary_disease","diabetes_with_cc","diabetes_without_cc","renal_disease","severe_liver_disease","charlson_comorbidity_index","heartrate_max","sysbp_min","resprate_max","tempc_max","spo2_min",    "bun_max","creatinine_max","glucose_max","lactate_max","platelet_min","pt_max","ptt_max","ph_min","wbc_max","ast_max")

facvar<-c("gender","myocardial_infarct"     ,"congestive_heart_failure","peripheral_vascular_disease","cerebrovascular_disease","dementia"     ,"chronic_pulmonary_disease","rheumatic_disease","peptic_ulcer_disease","mild_liver_disease","diabetes_with_cc","diabetes_without_cc","paraplegia","renal_disease","malignant_cancer","severe_liver_disease","metastatic_solid_tumor","aids")

library(tableone)
table1.1<-CreateTableOne(vars = tablevar,strata = "low_molecular_heparin",data = data,factorVars = facvar)
table1.2<-CreateTableOne(vars = tablevar,strata = "low_molecular_heparin",data = datamch,factorVars = facvar)
table1.1<-print(table1.1,smd=TRUE)
table1.2<-print(table1.2,smd=TRUE)
table1<-cbind(print(table1.1,smd=TRUE),print(table1.2,smd=TRUE))
# write.csv(print(table1.1),"fig_table/table1.1.csv")
# 
# write.csv(print(table1.2),"fig_table/table1.2.csv")

#write.csv(print(table1),"fig_table/table1.csv")
colnames(table1)<-c("non-LWM(unmatched)","LWM(unmatched)","pvalue","test1","SMD1","non-LWM(matched)","LWM(matched)","pavlue2","test2","SMD2")
table1<-table1[,c(1,2,3,5,6,7,8,10)]
head(table1)
#如何将表格美化后输出到doc文档
library(xtable)
library(flextable)
library(officer)
library(dplyr)
#输出三线表到 fig_table/tableone.doc 其中的
read_docx()%>%
  body_add_par(value = "Table1 Demographic data",style = "Normal")%>%
  body_add_flextable(xtable_to_flextable(xtable(table1)))%>%
  body_add_par(value = " Continuous variables are presented as mean (standard deviation), categorical as frequency (percentage). T-test was used to compare obes vs lean patients for continuous variables, Chi-square test for categorical variables. Standardized differences (SD) are defined as the difference in means, proportions or ranks divided by the mutual standard deviation. (*)variables were used for the calculation of propensity scores. Abbreviations: SAPSII, Simplified Acute Physiology Score II.",style = "Normal")%>%
  print("fig_table/tableone.docx")



#将用药数据汇总一下
table.dose<-cbind(
  print(CreateTableOne(vars = c("low_molecular_heparin","lmh_daily_dose","lmh_total_dose"),strata = "low_molecular_heparin",data = data,factorVars = "low_molecular_heparin")),
 print(CreateTableOne(vars = c("low_molecular_heparin","lmh_daily_dose","lmh_total_dose"),strata = "low_molecular_heparin",data = datamch,factorVars = "low_molecular_heparin"))
)
colnames(table.dose)<-c("non_LMH","LMH","p-value1","test1","non_LMH2","LMH2","p-value2","test2")
#输出三线表到 fig_table/tableone.doc 其中的
read_docx()%>%
  body_add_par(value = "Table2 LMH dose",style = "Normal")%>%
  body_add_flextable(xtable_to_flextable(xtable(table.dose)))%>%
  body_add_par(value = " Continuous variables are presented as mean (standard deviation). T-test was used to compare variables for continuous variables. ",style = "Normal")%>%
  print("fig_table/tabledose.docx")

#我们将次要结局的结果输出三线表到tablesecond.doc
second_oc_var<-c("icufree28","crrt_free28","dopaminefree28","norepifree28")
table_second<-CreateTableOne(vars = second_oc_var,strata = "low_molecular_heparin",data = data)
table_second_mch<-CreateTableOne(vars = second_oc_var,strata = "low_molecular_heparin",data = datamch)
table_secondoc<-cbind(print(table_second),print(table_second_mch))
colnames(table_secondoc)<-c("non-LMH in unmatched cohort","LMH in unmatched","pval1","test1","non-LWH in matched cohort","LWH in matched","pval2","test2")
#输出三线表到 fig_table/table_second.doc 其中的
read_docx()%>%
  body_add_par(value = "Table3 Secondary outcome",style = "Normal")%>%
  body_add_flextable(xtable_to_flextable(xtable(table_secondoc)))%>%
  body_add_par(value = " CRRT: Continuous renal replacement therapy",style = "Normal")%>%
  print("fig_table/table_second.docx")
