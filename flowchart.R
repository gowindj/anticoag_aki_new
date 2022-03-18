
# 1 this is about the inclusion criteria
length(datamch$hadm_id[datamch$matchgroup_gbm=="Control"])
length(datamch$hadm_id[datamch$matchgroup_gbm=="Obesity"])

length(datamch$age)#(952:952例，1904)

##绘制流程图
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

#确定矩形框的大小和矩形框里面的标签
antidata<-read.csv("rawdata/antidata3in1.csv")

box1 <- boxGrob(c(paste(length(antidata$age),"patients were screened.")
                  ,paste(length(antidata$age[antidata$database=="mimic3"]),"patients in MIMICIII database")
                  ,paste(length(antidata$age[antidata$database=="mimic4"]),"patients in MIMICIV database")
                  ,paste(length(antidata$age[antidata$database=="eicu"]),"patients in eICU database")),x=0.5,y=0.8)

grid.draw(box1)

box2 <- boxGrob(c("Patients excluded:",paste(ageunder18,"age<18"),paste(noweight,"without weight"), paste(noheight,"without height")), x=0.8, y=0.6)

grid.draw(box2)

box3 <- boxGrob(paste(length(data$age),"patients"), x=0.5, y=0.5)

grid.draw(box3)

box4 <- boxGrob(c("1:1 propensity match"), x=0.5,y=0.4)

grid.draw(box4)

box5 <- boxGrob(c(paste(length(datamch$age[datamch$obesity==0]),"patients"),"in lean group"), x=0.25,y=0.25)

grid.draw(box5)

box6 <- boxGrob(c(paste(length(datamch$age[datamch$obesity==1]),"patients"),"in obesity group"), x=0.75,y=0.25)
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


