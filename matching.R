rm(list=ls())
data<-read.csv("data/antiwashed.csv")
#matching的运行必须依靠datawash这个script
set.seed(8888)
#matching
head(data)
library(dplyr)
library(DataExplorer)
plot_missing(data)
#data$ethnicity<-factor(data$ethnicity)
library(MatchIt)

#设置一个参数newmatching，如果我们需要重新跑matching，那么设置为T，如果没有重新跑就设置为F
newmatching<-F



row.names(data)<-c(1:length(data$hadm_id))
matchavar<- c("age", "gender", "sapsii","charlson_comorbidity_index"     ,"heartrate_max","resprate_max","spo2_min",  "creatinine_max","wbc_max","low_molecular_heparin")
matchdata<-subset(data,select=matchavar)

plot_missing(matchdata)


library(lattice) #调入函数包
library(MASS)
library(nnet)
library(mice) #前三个包是mice的基础

if(newmatching==T){
  imp=mice(matchdata,m=4)
  saveRDS(imp,"rds/imp.RData",ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)
}else{
  imp<-readRDS("rds/imp.RData", refhook = NULL)
}

#imp=mice(matchdata,m=4) #4重插补，即生成4个无缺失数据集
fit=with(imp,lm(low_molecular_heparin~age+gender+sapsii+charlson_comorbidity_index  +heartrate_max+resprate_max+spo2_min+creatinine_max+wbc_max,data=matchdata))#选择插补模型
pooled=pool(fit)
summary(pooled)
result4=complete(imp,action=3)#选择第三个插补数据集作为结果
#insert the data from result4 into data
plot_missing(result4)

data$heartrate_max<-result4$heartrate_max
data$creatinine_max<-result4$creatinine_max
data$spo2_min<-result4$spo2_min
data$charlson_comorbidity_index<-result4$charlson_comorbidity_index
data$sapsii<-result4$sapsii
data$wbc_max<-result4$wbc_max
data$resprate_max<-result4$resprate_max
data$gender<-result4$gender

table(data$low_molecular_heparin)#(0:2504,1:1280)






#下面我们使用 gradient boosted model (GBM)，必须使用gbm包和twang包计算weight
library(gbm)
library(twang)
data$gender<-factor(data$gender)
data$diagnosis<-factor(data$diagnosis) #必须是factor,numeric才能运行ps()

fml1<-"low_molecular_heparin~age+gender+sapsii+charlson_comorbidity_index +heartrate_max+resprate_max+spo2_min+creatinine_max+wbc_max"
fml<-"low_molecular_heparin~age+gender+sapsii+charlson_comorbidity_index +heartrate_max+resprate_max+spo2_min+creatinine_max+wbc_max"
# this gbm model need lots of time , so we only run once and save it as Rdata;
#以下代码无法运行
if(newmatching==T){
  ps_gbm<- ps(as.formula(fml),
              data = data, interaction.depth = 2,shrinkage = 0.01,
              perm.test.iters = 0, estimand = "ATE", verbose = FALSE,
              stop.method = c("es.mean", "ks.mean"),
              n.trees = 2000,
              train.fraction = 0.8,
              cv.folds = 3,
              n.cores = 8)
  saveRDS(ps_gbm,"rds/ps_gbm.RData",ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)
}else{
  ps_gbm<-readRDS("rds/ps_gbm.RData", refhook = NULL)
}
plot(ps_gbm,plots = 3)
#这个程序用来测试love.plot
if(newmatching==T){
ps_gbm_test<- ps(as.formula(fml),
            data = data, interaction.depth = 2,shrinkage = 0.01,
            perm.test.iters = 0, estimand = "ATE", verbose = FALSE,
            stop.method = c( "es.mean"),
            n.trees = 2000,
            train.fraction = 0.8,
            cv.folds = 3,
            n.cores = 8)
saveRDS(ps_gbm_test,"rds/ps_gbm_test.RData",ascii=FALSE,version=Null, compress = TRUE, refhook = Null)
}else{
  ps_gbm_test<-readRDS("rds/ps_gbm_test.RData", refhook = NULL)
}
#如果您想要更漂亮的平衡表显示，我建议使用cobalt包(为此目的我为此编写了包)。 运行library(cobalt)之后，运行love.plot(bal.tab(mnps.newtest1.ATE))，这将为绘图提供相同的信息(请参见下面的示例以及一些其他选项)。
#install.packages("cobalt")
library("cobalt")
loveplot<-love.plot(bal.tab(ps_gbm_test),
          line = T ,
          #themes = theme_bw(),
         drop.missing = F,
          stars ="std",
          var.order = c("unadjusted"),
          colors = c("grey20","grey60"),
          shapes = c("circle", "triangle")
          )

loveplot



ft_importance <- summary(ps_gbm$gbm.obj,
                         n.trees = ps_gbm$desc$es.mean.ATE$n.trees,
                         plot = TRUE)
# the importane plot of all the variables,reorder by descending order of rel.inf, we need just a negative before rel.inf
library(ggplot2)
plot.importance<-ft_importance %>%
  ggplot() +
  geom_col(aes(x = reorder(var,-rel.inf,identity), y = rel.inf), width = .5,
           #  fill = rgb(66, 139, 202, maxColorValue = 255)) +
           fill = "grey40") +
  scale_x_discrete(limits = ft_importance$cov) +
  scale_y_continuous(breaks = seq(0, 18, 2)) +
  labs(x = "", y = "",title = "Covariate influence") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = .5),
        panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())
plot.importance
library(ggpubr)
ggsave(
  ggarrange(plot.importance,loveplot,ncol = 2,nrow = 1,labels = c("A","B")),
  filename = "fig_table/fig_iptc_love.pdf",height = 5,width = 10,units = "in")
data$ps_gbm <- ps_gbm$ps$es.mean.ATE
#caculate the weight from GBM model
data$weight_gbm = get.weights(ps_gbm, stop.method = "es.mean")

#ps=glm(as.formula(fml),data=data,family=binomial)
#计算倾向评分ps
#data$ps_logit=predict(ps,type="response")
#计算逆概率权重IPTW
#data$weight_logit<-ifelse(data$enoxaparin==1,1/data$ps_logit,1/(1-data$ps_logit))
#使用matching包的方法进行匹配

library(Matching)
if(newmatching==T){
  
  ps_matches_gbm <- Match(Y = data$aki_flag, Tr = data$low_molecular_heparin,
                          X = data$ps_gbm, M = 1, estimand = "ATT", caliper = 0.02,
                          exact = FALSE, replace = FALSE, ties = FALSE )
  
  saveRDS(ps_matches_gbm,"rds/ps_matches_gbm.rds",ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)
}else{
  ps_matches_gbm<-readRDS("rds/ps_matches_gbm.rds", refhook = NULL)
}
#plot(ps_matches_gbm)


data$matchgroup_gbm<-"Unmatched"
data$matchgroup_gbm[ps_matches_gbm$index.control]<-"non_LMH"
data$matchgroup_gbm[ps_matches_gbm$index.treated]<-"LMH"

#get the matched cohort
datamch<-subset(data,matchgroup_gbm!="Unmatched")


#对补液量的较大值进行修正
##在datamch中计算
q90<-quantile(datamch$day1_input,0.90,na.rm = T)
datamch$day1_input[datamch$day1_input>q90]<-q90
q90<-quantile(datamch$day2_input,0.90,na.rm = T)
datamch$day2_input[datamch$day2_input>q90]<-q90
q90<-quantile(datamch$day3_input,0.90,na.rm = T)
datamch$day3_input[datamch$day3_input>q90]<-q90

datamch$day1_input_toweight<-datamch$day1_input/datamch$weight
datamch$day2_input_toweight<-datamch$day2_input/datamch$weight
datamch$day3_input_toweight<-datamch$day3_input/datamch$weight


input_matrixmch<-cbind(datamch$day1_input_toweight,datamch$day2_input_toweight
                       ,datamch$day3_input_toweight)
head(input_matrixmch)
input_meanmch<-apply(input_matrixmch,MARGIN = 1,FUN = mean,na.rm=T)
head(input_meanmch)
datamch$input_mean<-input_meanmch



write.csv(datamch,"data/datamch.csv")

# 1 this is about the inclusion criteria
length(datamch$hadm_id[datamch$matchgroup_gbm=="non_LMH"])
length(datamch$hadm_id[datamch$matchgroup_gbm=="LMH"])

length(datamch$age)#(952:952例，1904)

##################################################################
######################                   #########################
######################    绘制流程图     #########################
######################                   #########################
##################################################################
library(grid)


#上一期我们已经对R语言绘制基本矩形框，文本和线条进行了介绍，本文将在上一期的基础上，通过自编函数快速绘制流程图。

#第一步：自定义绘制基本矩形和文本的函数。利用该函数可以快速的根据字段的长度和宽度绘制相应的矩形框，该矩形框为流程图的基本图元
tableBox <- function(labels, x=0.5, y=0.5) {
  # labels<-c("book_table", "ISBN", "title", "pub")
  # x=0.6
  # y=0.75
  nlabel <- length(labels)#确定标签个数
  
  taplevp <- viewport(x=x, y=y, width=max(stringWidth(labels))+ unit(4, "mm"),
                      
                      height=unit(nlabel, "lines"))
  #根据标签的多少绘制矩形框
  
  pushViewport(taplevp)
  
  grid.roundrect()
  
  if (nlabel > 1) {
    
    for (i in 1:(nlabel-1)) {
      
      fill <- c("white", "grey")[i%%2+1]
      
      grid.clip(y=unit(i, "lines"), just="bottom")
      
      grid.roundrect(gp=gpar(fill=fill))
      
    }
    
  }
  
  grid.clip()#绘制灰色阴影
  
  grid.text(labels, x=unit(2, "mm"), y=unit(nlabel:1-0.5, 'lines'),just='left')#添加文本
  
  popViewport()
  
}

boxGrob <- function(labels, x=0.5, y=0.5) {
  
  grob(labels=labels, x=x, y=y, cl="box")
  
}#存储矩形框的基本信息


drawDetails.box <- function(x, ...) {
  
  tableBox(x$labels, x$x, x$y)
  
}#

# 第三步：以上述两个函数为基础绘制流程图

library(grid)
#计算哪些人被去掉了

antidata<-read.csv("rawdata/antidata3in1.csv")
  lengthofantidata<-length(antidata$age)
numageunder18<-length(antidata$age[as.numeric(antidata$age)<18])
numbleeding<-lengthofantidata-numageunder18-length(data$age)
#确定矩形框的大小和矩形框里面的标签


box1 <- boxGrob(c(paste(length(antidata$age),"patients were screened.")
                  ,paste(length(antidata$age[antidata$database=="mimic3"]),"patients in MIMICIII database")
                  ,paste(length(antidata$age[antidata$database=="mimic4"]),"patients in MIMICIV database")
                  ,paste(length(antidata$age[antidata$database=="eicu"]),"patients in eICU database")),x=0.5,y=0.8)

grid.draw(box1)

box2 <- boxGrob(c("Patients excluded:",paste(numageunder18,"age<18"),paste(numbleeding,"bleeding")), x=0.8, y=0.6)

grid.draw(box2)

box3 <- boxGrob(paste(length(data$age),"patients"), x=0.5, y=0.5)

grid.draw(box3)

box4 <- boxGrob(c("1:1 propensity match"), x=0.5,y=0.4)

grid.draw(box4)

box5 <- boxGrob(c(paste(length(datamch$age[datamch$LMH==0]),"patients"),"in non_LMH group"), x=0.25,y=0.25)

grid.draw(box5)

box6 <- boxGrob(c(paste(length(datamch$age[datamch$LMH==1]),"patients"),"in LMH group"), x=0.75,y=0.25)
#绘制矩形框及标签
grid.draw(box6)


#绘制不同矩形框之间的连接线
grid.lines(c(0.5,0.5),c(0.7,0.53),arrow=arrow(angle = 15,length =unit(0.3,"lines") ))

grid.lines(c(0.65,0.5),c(0.6,0.6),arrow=arrow(angle = 15,length =unit(0.3,"lines") ))

grid.lines(c(0.5,0.5),c(0.47,0.42),arrow=arrow(angle = 15,length =unit(0.3,"lines") ))

grid.lines(c(0.25,0.75),c(0.33,0.33))

grid.lines(c(0.5,0.5),c(0.37,0.33))

grid.lines(c(0.25,0.25),c(0.33,0.3),arrow=arrow(angle = 15,length =unit(0.3,"lines") ))

grid.lines(c(0.75,0.75),c(0.33,0.3),arrow=arrow(angle = 15,length =unit(0.3,"lines") ))
eoffice::topptx(filename = "fig_table/plot.chartflow.pptx",width = 6,height = 4,units = "in")


