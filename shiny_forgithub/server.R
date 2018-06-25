options(shiny.sanitize.errors = FALSE)
library(reshape2)
library(rCharts)
library(data.table)
library(dplyr)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(ggthemes)
library(ggradar)
library(scales)


shinyServer(function(input,output){
#########################################################################
basic=read.table("basicinfo.txt",header = TRUE,sep="",fileEncoding="UTF-8") ###正确
basic$firstchuo=as.character(basic$firstchuo)

model=read.table("model.txt",header = TRUE,sep="",fileEncoding="UTF-8") ###正确
model$firstchuo=as.character(model$firstchuo)

salary=read.table("salary.txt",header = TRUE,sep="",fileEncoding="UTF-8") ###正确
salary$firstchuo=as.character(salary$firstchuo)

zizhi=read.table("zizhi.txt",header = TRUE,sep="",fileEncoding="UTF-8") ###正确
zizhi$firstchuo=as.character(zizhi$firstchuo)

owing=read.table("owing.txt",header = TRUE,sep="",fileEncoding="UTF-8") ###正确
owing$firstchuo=as.character(owing$firstchuo)

duotou=read.table("duotou.txt",header = TRUE,sep="",fileEncoding="UTF-8") ###正确
duotou$firstchuo=as.character(duotou$firstchuo)

channel=read.table("channel.txt",header = TRUE,sep="",fileEncoding="UTF-8",row.names = NULL) ###正确
channel$first_login_time=as.character(channel$first_login_time)

ceshi1=read.table("ceshi1.txt",header = TRUE,sep="",fileEncoding="UTF-8",row.names = NULL) ###正确
score=read.table("score.txt",header = TRUE,sep="",fileEncoding="UTF-8",row.names = NULL) ###正确
###############渠道评估数据处理
channeleva=read.table("channeleva.txt",header = TRUE,sep="",fileEncoding="UTF-8",row.names = NULL) ###正确
channeleva$firstchuo=as.character(channeleva$firstchuo)
channeleva1=channeleva[channeleva$sourcetype=="app" ,]
#channeleva1=channeleva1[channeleva1$query1m!=-100,]
channeleva2=channeleva[channeleva$sourcetype=="M",]


app=channeleva1 %>% group_by(sourcename) %>% 
summarise(num=n()) %>%
subset(num>20)

m=channeleva2 %>% group_by(sourcename) %>% 
summarise(num=n()) %>%
subset(num>20)

channeleva1=merge(channeleva1,app,"sourcename")
channeleva1$firstchuo=as.character(channeleva1$firstchuo)
channeleva1$num=NULL
####
channeleva1$tcbin=cut(channeleva1$risk_score,breaks=c(0,20,40,60,80,100),include.lowest = TRUE,right = FALSE)
channeleva1$umbin=cut(channeleva1$umeng_score,breaks=c(300,400,500,600,700,850))
channeleva1$jdbin=cut(channeleva1$jdcredit_score,breaks=c(400,550,620,650,700,850))
channeleva1$credit_bin[channeleva1$credit_bin>=10]="十及以上"
channeleva1$cmaxbin=as.character(cut(channeleva1$cmax,breaks=c(0,3000,6000,10000,20000,50000,100000,1000000),include.lowest = TRUE,right = FALSE))

channeleva1$cmaxbin[channeleva1$cmaxbin=="[0,3e+03)"]="[0,0.3w)"
channeleva1$cmaxbin[channeleva1$cmaxbin=="[3e+03,6e+03)"]="[0.3w,0.6w)"
channeleva1$cmaxbin[channeleva1$cmaxbin=="[6e+03,1e+04)"]="[0.6w,1w)"
channeleva1$cmaxbin[channeleva1$cmaxbin=="[1e+04,2e+04)"]="[1w,2w)"
channeleva1$cmaxbin[channeleva1$cmaxbin=="[2e+04,5e+04)"]="[2w,5w)"
channeleva1$cmaxbin[channeleva1$cmaxbin=="[5e+04,1e+05)"]="[5w,5-10w)"
channeleva1$cmaxbin[channeleva1$cmaxbin=="[1e+05,1e+06]"]="大于10w"

channeleva1$omaxbin=as.character(cut(channeleva1$omax,breaks=c(0,5000,10000,20000,30000,50000,100000,10000000),include.lowest = TRUE,right = FALSE))

channeleva1$omaxbin[channeleva1$omaxbin=="[0,5e+03)"]="[0,0.5w)"
channeleva1$omaxbin[channeleva1$omaxbin=="[5e+03,1e+04)"]="[0.5w,1w)"
channeleva1$omaxbin[channeleva1$omaxbin=="[1e+04,2e+04)"]="[1w,2w)"
channeleva1$omaxbin[channeleva1$omaxbin=="[2e+04,3e+04)"]="[2w,3w)"
channeleva1$omaxbin[channeleva1$omaxbin=="[3e+04,5e+04)"]="[3w,5w)"
channeleva1$omaxbin[channeleva1$omaxbin=="[5e+04,1e+05)"]="[5w,5-10w)"
channeleva1$omaxbin[channeleva1$omaxbin=="[1e+05,1e+07]"]="大于10w"

channeleva1$bobin=as.character(cut(channeleva1$boappnum,breaks=c(0,1,2,5,10,20,500),include.lowest = TRUE,right = FALSE))

channeleva1$bobin[channeleva1$bobin=="[0,1)"]="[0,01)"
channeleva1$bobin[channeleva1$bobin=="[1,2)"]="[01,02)"
channeleva1$bobin[channeleva1$bobin=="[2,5)"]="[02,05)"
channeleva1$bobin[channeleva1$bobin=="[5,10)"]="[05,10)"


channeleva1$tdbin1=as.character(cut(channeleva1$td_3m,breaks=c(0,1,2,5,10,20,50,100),include.lowest = TRUE,right = FALSE))

channeleva1$tdbin1[channeleva1$tdbin1=="[0,1)"]="[0,01)"
channeleva1$tdbin1[channeleva1$tdbin1=="[1,2)"]="[01,02)"
channeleva1$tdbin1[channeleva1$tdbin1=="[2,5)"]="[02,05)"
channeleva1$tdbin1[channeleva1$tdbin1=="[5,10)"]="[05,10)"


channeleva1$tdbin2=as.character(cut(channeleva1$final_score,breaks=c(0,20,80,100),include.lowest = TRUE,right = FALSE))
channeleva1$zxbin1=as.character(cut(channeleva1$query1m,breaks=c(0,1,2,5,10,20,50),include.lowest = TRUE,right = FALSE))

channeleva1$zxbin1[channeleva1$zxbin1=="[0,1)"]="[0,01)"
channeleva1$zxbin1[channeleva1$zxbin1=="[1,2)"]="[01,02)"
channeleva1$zxbin1[channeleva1$zxbin1=="[2,5)"]="[02,05)"
channeleva1$zxbin1[channeleva1$zxbin1=="[5,10)"]="[05,10)"

channeleva1$defbin=as.character(cut(channeleva1$message_count_default,breaks=c(0,1,5,10,20,500),include.lowest = TRUE,right = FALSE))

channeleva1$defbin[channeleva1$defbin=="[0,1)"]="[0,01)"
channeleva1$defbin[channeleva1$defbin=="[1,5)"]="[01,05)"
channeleva1$defbin[channeleva1$defbin=="[5,10)"]="[05,10)"

channeleva1$zxbin2=as.character(cut((channeleva1$hoverdue2y+channeleva1$coverdue2y+channeleva1$ooverdue2y),breaks=c(0,1,5,10,20,500),include.lowest = TRUE,right = FALSE))

channeleva1$zxbin2[channeleva1$zxbin2=="[0,1)"]="[0,01)"
channeleva1$zxbin2[channeleva1$zxbin2=="[1,5)"]="[01,05)"
channeleva1$zxbin2[channeleva1$zxbin2=="[5,10)"]="[05,10)"

channeleva2=merge(channeleva2,m,"sourcename")
channeleva2$firstchuo=as.character(channeleva2$firstchuo)
channeleva2$num=NULL
rm(app,m)
##################基本信息

  selectedData1 <- reactive({
  basic[as.Date(basic$firstchuo) >= min(input$dates2) & as.Date(basic$firstchuo) <= max(input$dates2),]
  })

  ##用户资质
  column <- reactive({
    switch(input$mode1,
           "line type" = which(names(selectedData1())=="linetype"),
           "mode type"   = which(names(selectedData1())=="chuomode")
           )
  })


  
####学历分布
##大额渠道VS大额主营VS小额
output$plot10 <- renderChart2({

edu=selectedData1()
#edu=edu %>% group_by(linetype,edu) %>% summarise(num=n()) 
edu=data.frame(table(melt(data.frame(linetype=edu[,column()],edu=edu$edu),id=c("linetype","edu"))))
if(input$mode1!="line type"){
edu$linetype <- factor(edu$linetype,levels=c("gjj","wb","bb","tb","oth"))
}

plot <- hPlot(Freq~linetype, data = edu,group = "edu",type = "column",title="学历分布(大额渠道VS大额主营VS小额)")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
})
##大额
output$plot11 <- renderChart2({

edu=selectedData1()[selectedData1()$linetype=="大额渠道",]
#edu=edu%>% group_by(week,edu) %>% summarise(num=n())

edu=data.frame(table(melt(data.frame(week=edu$week,edu=edu$edu),id=c("week","edu"))))
plot <- hPlot(Freq~week, data = edu,
              group = "edu",
              type = "column",title="大额渠道人群学历分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)
})
##小额
output$plot12 <- renderChart2({

edu=selectedData1()[selectedData1()$linetype=="大额主营",]
#edu=edu%>% group_by(week,edu) %>% summarise(num=n())
edu=data.frame(table(melt(data.frame(week=edu$week,edu=edu$edu),id=c("week","edu")))) 
plot <- hPlot(Freq~week, data = edu,
              group = "edu",
              type = "column",title="大额主营人群学历分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)
})

####年龄分布
##大额渠道VS大额主营VS小额
output$plot1 <- renderChart2({

age=selectedData1()[is.na(selectedData1()$age_bin)==FALSE,]
#age=age %>% group_by(linetype,age_bin) %>% summarise(num=n())
age=data.frame(table(melt(data.frame(linetype=age[,column()],age_bin=age$age_bin),id=c("linetype","age_bin")))) 
if(input$mode1!="line type"){
age$linetype <- factor(age$linetype,levels=c("gjj","wb","bb","tb","oth"))
}

plot <- hPlot(Freq~linetype, data = age,
              group = "age_bin",
              type = "column",title="年龄分布(大额渠道VS大额主营VS小额)")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)
})
##大额
output$plot2 <- renderChart2({

age=selectedData1()[is.na(selectedData1()$age_bin)==FALSE&selectedData1()$linetype=="大额渠道",]
#age=age%>% group_by(week,age_bin) %>% summarise(num=n()) 
age=data.frame(table(melt(data.frame(week=age$week,age_bin=age$age_bin),id=c("week","age_bin")))) 
plot <- hPlot(Freq~week, data = age,
              group = "age_bin",
              type = "column",title="大额渠道人群年龄分布")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)
})
##小额
output$plot3 <- renderChart2({

age=selectedData1()[is.na(selectedData1()$age_bin)==FALSE&selectedData1()$linetype=="大额主营",]
#age=age%>% group_by(week,age_bin) %>% summarise(num=n()) 
age=data.frame(table(melt(data.frame(week=age$week,age_bin=age$age_bin),id=c("week","age_bin")))) 
plot <- hPlot(Freq~week, data = age,
              group = "age_bin",
              type = "column",title="大额主营人群年龄分布")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)
})

####人群类型分布
##大额渠道VS大额主营VS小额
output$plot4 <- renderChart2({

usertype=selectedData1()
#usertype=usertype %>% group_by(linetype,usertype) %>% summarise(num=n())
usertype=data.frame(table(melt(data.frame(linetype=usertype[,column()],usertype=usertype$usertype),id=c("linetype","usertype")))) 
if(input$mode1!="line type"){
usertype$linetype <- factor(usertype$linetype,levels=c("gjj","wb","bb","tb","oth"))
}

plot <- hPlot(Freq~linetype, data = usertype,group = "usertype",type = "column",title="人群类型分布(大额渠道VS大额主营VS小额)")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})
##大额
output$plot5 <- renderChart2({

usertype=selectedData1()[selectedData1()$linetype=="大额渠道",]
#usertype=usertype %>% group_by(week,usertype) %>% summarise(num=n())
usertype=data.frame(table(melt(data.frame(week=usertype$week,usertype=usertype$usertype),id=c("week","usertype")))) 
plot <- hPlot(Freq~week, data = usertype,group = "usertype",type = "column",title="大额渠道人群类型分布")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})
##小额
output$plot6 <- renderChart2({

usertype=selectedData1()[selectedData1()$linetype=="大额主营",]
#usertype=usertype %>% group_by(week,usertype) %>% summarise(num=n()) 
usertype=data.frame(table(melt(data.frame(week=usertype$week,usertype=usertype$usertype),id=c("week","usertype")))) 
plot <- hPlot(Freq~week, data = usertype,group = "usertype",type = "column",title="大额主营人群类型分布")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})

####城市等级分布
##大额渠道VS大额主营VS小额
output$plot7 <- renderChart2({

citylevel=selectedData1()
#citylevel=citylevel %>% group_by(linetype,citylevel_bin) %>% summarise(num=n())
citylevel=data.frame(table(melt(data.frame(linetype=citylevel[,column()],citylevel_bin=citylevel$citylevel_bin),id=c("linetype","citylevel_bin")))) 
if(input$mode1!="line type"){
citylevel$linetype <- factor(citylevel$linetype,levels=c("gjj","wb","bb","tb","oth"))
}

plot <- hPlot(Freq~linetype, data = citylevel,group = "citylevel_bin",type = "column",title="城市等级分布(大额渠道VS大额主营VS小额)")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
})
##大额
output$plot8 <- renderChart2({

citylevel=selectedData1()[selectedData1()$linetype=="大额渠道",]
#citylevel=citylevel %>% group_by(week,citylevel_bin) %>% summarise(num=n())
citylevel=data.frame(table(melt(data.frame(week=citylevel$week,citylevel_bin=citylevel$citylevel_bin),id=c("week","citylevel_bin")))) 
plot <- hPlot(Freq~week, data = citylevel,group = "citylevel_bin",type = "column",title="大额渠道城市等级分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
})
##小额
output$plot9 <- renderChart2({

citylevel=selectedData1()[selectedData1()$linetype=="大额主营",]
citylevel=citylevel %>% group_by(week,citylevel_bin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = citylevel,group = "citylevel_bin",type = "column",title="大额主营城市等级分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
})

##################模型类评分

  selectedData2 <- reactive({
  model[as.Date(model$firstchuo) >= min(input$dates3) & as.Date(model$firstchuo) <= max(input$dates3),]
  })

  column2 <- reactive({
    switch(input$mode2,
           "line type" = which(names(selectedData2())=="linetype"),
           "mode type"   = which(names(selectedData2())=="chuomode")
           )
  })
####模型bin
##大额渠道VS大额主营VS小额
output$plot13 <- renderChart2({
bin=selectedData2()
bin=bin[!is.na(bin$credit_bin),]
bin$credit_bin[bin$credit_bin>=10]="十及以上"
bin$credit_bin=paste("bin",bin$credit_bin,sep="")
#bin=bin %>% group_by(linetype,credit_bin) %>% summarise(num=n()) 
bin=data.frame(table(melt(data.frame(linetype=bin[,column2()],credit_bin=bin$credit_bin),id=c("linetype","credit_bin"))))
if(input$mode2!="line type"){
bin$linetype <- factor(bin$linetype,levels=c("gjj","wb","bb","tb","oth"))
}

plot <- hPlot(Freq~linetype, data = bin,group = "credit_bin",type = "column",title="模型bin分布(大额渠道VS大额主营VS小额)")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 
##大额
output$plot14 <- renderChart2({
bin=selectedData2()[selectedData2()$linetype=="大额渠道",]
bin=bin[!is.na(bin$credit_bin),]
bin$credit_bin[bin$credit_bin>=10]="十及以上"
bin$credit_bin=paste("bin",bin$credit_bin,sep="")
#bin=bin %>% group_by(week,credit_bin) %>% summarise(num=n()) 
bin=data.frame(table(melt(data.frame(week=bin$week,credit_bin=bin$credit_bin),id=c("week","credit_bin")))) 
plot <- hPlot(Freq~week, data = bin,group = "credit_bin",type = "column",title="大额渠道模型bin分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 
##小额
output$plot15 <- renderChart2({
bin=selectedData2()[selectedData2()$linetype=="大额主营",]
bin=bin[!is.na(bin$credit_bin),]
bin$credit_bin[bin$credit_bin>=10]="十及以上"
bin$credit_bin=paste("bin",bin$credit_bin,sep="")
bin=bin %>% group_by(week,credit_bin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = bin,group = "credit_bin",type = "column",title="大额主营模型bin分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

####腾讯分
##大额渠道VS大额主营VS小额
output$plot16 <- renderChart2({
tc=selectedData2()
tc=tc[!is.na(tc$risk_score),]
tc$tcbin=cut(tc$risk_score,breaks=c(0,20,40,60,80,100),include.lowest = TRUE,right = FALSE)
#tc=tc %>% group_by(linetype,tcbin) %>% summarise(num=n()) 
tc=data.frame(table(melt(data.frame(linetype=tc[,column2()],tcbin=tc$tcbin),id=c("linetype","tcbin"))))
if(input$mode2!="line type"){
tc$linetype <- factor(tc$linetype,levels=c("gjj","wb","bb","tb","oth"))
}

plot <- hPlot(Freq~linetype, data = tc,group = "tcbin",type = "column",title="腾讯分分布(大额渠道VS大额主营VS小额)")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 
##大额
output$plot17 <- renderChart2({
tc=selectedData2()[selectedData2()$linetype=="大额渠道",]
tc=tc[!is.na(tc$risk_score),]
tc$tcbin=cut(tc$risk_score,breaks=c(0,20,40,60,80,100),include.lowest = TRUE,right = FALSE)
#tc=tc %>% group_by(week,tcbin) %>% summarise(num=n())
tc=data.frame(table(melt(data.frame(week=tc$week,tcbin=tc$tcbin),id=c("week","tcbin")))) 
plot <- hPlot(Freq~week, data = tc,group = "tcbin",type = "column",title="大额渠道腾讯分分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 
##小额
output$plot18 <- renderChart2({
tc=selectedData2()[selectedData2()$linetype=="大额主营",]
tc=tc[!is.na(tc$risk_score),]
tc$tcbin=cut(tc$risk_score,breaks=c(0,20,40,60,80,100),include.lowest = TRUE,right = FALSE)
tc=tc %>% group_by(week,tcbin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = tc,group = "tcbin",type = "column",title="大额主营腾讯分分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

####友盟分
##大额渠道VS大额主营VS小额
output$plot19 <- renderChart2({
um=selectedData2()
um=um[!is.na(um$umeng_score),]
um$umbin=cut(um$umeng_score,breaks=c(300,400,500,600,700,850))
#um=um %>% group_by(linetype,umbin) %>% summarise(num=n()) 
um=data.frame(table(melt(data.frame(linetype=um[,column2()],umbin=um$umbin),id=c("linetype","umbin"))))
if(input$mode2!="line type"){
um$linetype <- factor(um$linetype,levels=c("gjj","wb","bb","tb","oth"))
}

plot <- hPlot(Freq~linetype, data = um,group = "umbin",type = "column",title="友盟分分布(大额渠道VS大额主营VS小额)")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
}) 
##大额
output$plot20 <- renderChart2({
um=selectedData2()[selectedData2()$linetype=="大额渠道",]
um=um[!is.na(um$umeng_score),]
um$umbin=cut(um$umeng_score,breaks=c(300,400,500,600,700,850))
#um=um %>% group_by(week,umbin) %>% summarise(num=n()) 
um=data.frame(table(melt(data.frame(week=um$week,umbin=um$umbin),id=c("week","umbin")))) 
plot <- hPlot(Freq~week, data = um,group = "umbin",type = "column",title="大额渠道友盟分分布")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
}) 
##小额
output$plot21 <- renderChart2({
um=selectedData2()[selectedData2()$linetype=="大额主营",]
um=um[!is.na(um$umeng_score),]
um$umbin=cut(um$umeng_score,breaks=c(300,400,500,600,700,850))
um=um %>% group_by(week,umbin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = um,group = "umbin",type = "column",title="大额主营友盟分分布")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
}) 

####京东分
##大额渠道VS大额主营VS小额
##大额渠道VS大额主营VS小额
output$plot22 <- renderChart2({
jd=selectedData2()
jd=jd[!is.na(jd$jdcredit_score),]
jd$jdbin=cut(jd$jdcredit_score,breaks=c(400,550,620,650,700,850))
#jd=jd %>% group_by(linetype,jdbin) %>% summarise(num=n()) 

jd=data.frame(table(melt(data.frame(linetype=jd[,column2()],jdbin=jd$jdbin),id=c("linetype","jdbin"))))
if(input$mode2!="line type"){
jd$linetype <- factor(jd$linetype,levels=c("gjj","wb","bb","tb","oth"))
}

plot <- hPlot(Freq~linetype, data = jd,group = "jdbin",type = "column",title="京东分分布(大额渠道VS大额主营VS小额)")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
}) 
##大额
output$plot23 <- renderChart2({
jd=selectedData2()[selectedData2()$linetype=="大额渠道",]
jd=jd[!is.na(jd$jdcredit_score),]
jd$jdbin=cut(jd$jdcredit_score,breaks=c(400,550,620,650,700,850))
#jd=jd %>% group_by(week,jdbin) %>% summarise(num=n())
jd=data.frame(table(melt(data.frame(week=jd$week,jdbin=jd$jdbin),id=c("week","jdbin"))))  
plot <- hPlot(Freq~week, data = jd,group = "jdbin",type = "column",title="大额渠道京东分分布")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
}) 
##小额
output$plot24 <- renderChart2({
jd=selectedData2()[selectedData2()$linetype=="大额主营",]
jd=jd[!is.na(jd$jdcredit_score),]
jd$jdbin=cut(jd$jdcredit_score,breaks=c(400,550,620,650,700,850))
jd=jd %>% group_by(week,jdbin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = jd,group = "jdbin",type = "column",title="大额主营京东分分布")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
}) 
 

##################收入&资产

  selectedData3 <- reactive({
  salary[as.Date(salary$firstchuo) >= min(input$dates4) & as.Date(salary$firstchuo) <= max(input$dates4),]
  })

  column3 <- reactive({
    switch(input$mode3,
           "line type" = which(names(selectedData3())=="linetype"),
           "mode type"   = which(names(selectedData3())=="chuomode")
           )
  })
####税前收入
##大额渠道VS大额主营VS小额
output$plot25 <- renderChart2({
pre=selectedData3()
pre=pre[!is.na(pre$pretax),]
#pre=pre %>% group_by(linetype,pretax) %>% summarise(num=n())
pre=data.frame(table(melt(data.frame(linetype=pre[,column3()],pretax=pre$pretax),id=c("linetype","pretax"))))
if(input$mode3!="line type"){
pre$linetype <- factor(pre$linetype,levels=c("gjj","wb","bb","tb","oth"))
}
   
plot <- hPlot(Freq~linetype, data = pre,group = "pretax",type = "column",title="税前收入分布(大额渠道VS大额主营VS小额)")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
}) 
##大额
output$plot26 <- renderChart2({
pre=selectedData3()[selectedData3()$linetype=="大额渠道",]
pre=pre[!is.na(pre$pretax),]
#pre=pre %>% group_by(week,pretax) %>% summarise(num=n()) 
pre=data.frame(table(melt(data.frame(week=pre$week,pretax=pre$pretax),id=c("week","pretax"))))  
plot <- hPlot(Freq~week, data = pre,group = "pretax",type = "column",title="大额渠道税前收入分布")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
}) 
##小额
output$plot27 <- renderChart2({
pre=selectedData3()[selectedData3()$linetype=="大额主营",]
pre=pre[!is.na(pre$pretax),]
pre=pre %>% group_by(week,pretax) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = pre,group = "pretax",type = "column",title="大额主营税前收入分布")
plot$plotOptions(column = list(stacking = "percent"))
#plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 


##################多头与不良

  selectedData4 <- reactive({
  duotou[as.Date(duotou$firstchuo) >= min(input$dates5) & as.Date(duotou$firstchuo) <= max(input$dates5),]
  })

  column4 <- reactive({
    switch(input$mode4,
           "line type" = which(names(selectedData4())=="linetype"),
           "mode type"   = which(names(selectedData4())=="chuomode")
           )
  })
####借款APP数
##大额渠道VS大额主营VS小额
output$plot28 <- renderChart2({
bo=selectedData4()
bo=bo[!is.na(bo$boappnum),]
bo$bobin=as.character(cut(bo$boappnum,breaks=c(0,1,2,5,10,20,500),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,2)"]="[01,02)"
bo$bobin[bo$bobin=="[2,5)"]="[02,05)"
bo$bobin[bo$bobin=="[5,10)"]="[05,10)"
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
bo=data.frame(table(melt(data.frame(linetype=bo[,column4()],bobin=bo$bobin),id=c("linetype","bobin"))))
if(input$mode4!="line type"){
bo$linetype <- factor(bo$linetype,levels=c("gjj","wb","bb","tb","oth"))
}
plot <- hPlot(Freq~linetype, data = bo,group = "bobin",type = "column",title="借款APP数分布(大额渠道VS大额主营VS小额)")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额渠道
output$plot29 <- renderChart2({
bo=selectedData4()[selectedData4()$linetype=="大额渠道",]
bo=bo[!is.na(bo$boappnum),]
bo$bobin=as.character(cut(bo$boappnum,breaks=c(0,1,2,5,10,20,500),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,2)"]="[01,02)"
bo$bobin[bo$bobin=="[2,5)"]="[02,05)"
bo$bobin[bo$bobin=="[5,10)"]="[05,10)"
#bo=bo %>% group_by(week,bobin) %>% summarise(num=n())
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title="大额渠道借款APP数分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额主营
output$plot30 <- renderChart2({
bo=selectedData4()[selectedData4()$linetype=="大额主营",]
bo=bo[!is.na(bo$boappnum),]
bo$bobin=as.character(cut(bo$boappnum,breaks=c(0,1,2,5,10,20,500),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,2)"]="[01,02)"
bo$bobin[bo$bobin=="[2,5)"]="[02,05)"
bo$bobin[bo$bobin=="[5,10)"]="[05,10)"
bo=bo %>% group_by(week,bobin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = bo,group = "bobin",type = "column",title="大额主营借款APP数分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

####同盾1个月内平台查询次数
##大额渠道VS大额主营VS小额
output$plot31 <- renderChart2({
td=selectedData4()
td=td[!is.na(td$td_1m),]
td$tdbin=as.character(cut(td$td_1m,breaks=c(0,1,2,5,10,20,50,100),include.lowest = TRUE,right = FALSE))
td$tdbin[td$tdbin=="[0,1)"]="[0,01)"
td$tdbin[td$tdbin=="[1,2)"]="[01,02)"
td$tdbin[td$tdbin=="[2,5)"]="[02,05)"
td$tdbin[td$tdbin=="[5,10)"]="[05,10)"
#td=td %>% group_by(linetype,tdbin) %>% summarise(num=n()) 
td=data.frame(table(melt(data.frame(linetype=td[,column4()],tdbin=td$tdbin),id=c("linetype","tdbin"))))
if(input$mode4!="line type"){
td$linetype <- factor(td$linetype,levels=c("gjj","wb","bb","tb","oth"))
}
    
plot <- hPlot(Freq~linetype, data = td,group = "tdbin",type = "column",title="同盾一月内查询数分布(大额渠道VS大额主营VS小额)")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额渠道
output$plot32 <- renderChart2({
td=selectedData4()[selectedData4()$linetype=="大额渠道",]
td=td[!is.na(td$td_1m),]
td$tdbin=as.character(cut(td$td_1m,breaks=c(0,1,2,5,10,20,50,100),include.lowest = TRUE,right = FALSE))
td$tdbin[td$tdbin=="[0,1)"]="[0,01)"
td$tdbin[td$tdbin=="[1,2)"]="[01,02)"
td$tdbin[td$tdbin=="[2,5)"]="[02,05)"
td$tdbin[td$tdbin=="[5,10)"]="[05,10)"
#td=td %>% group_by(week,tdbin) %>% summarise(num=n())
td=data.frame(table(melt(data.frame(week=td$week,tdbin=td$tdbin),id=c("week","tdbin"))))   
plot <- hPlot(Freq~week, data = td,group = "tdbin",type = "column",title="大额渠道同盾一月内查询数分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额主营
output$plot33 <- renderChart2({
td=selectedData4()[selectedData4()$linetype=="大额主营",]
td=td[!is.na(td$td_1m),]
td$tdbin=as.character(cut(td$td_1m,breaks=c(0,1,2,5,10,20,50,100),include.lowest = TRUE,right = FALSE))
td$tdbin[td$tdbin=="[0,1)"]="[0,01)"
td$tdbin[td$tdbin=="[1,2)"]="[01,02)"
td$tdbin[td$tdbin=="[2,5)"]="[02,05)"
td$tdbin[td$tdbin=="[5,10)"]="[05,10)"
td=td %>% group_by(week,tdbin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = td,group = "tdbin",type = "column",title="大额主营同盾一月内查询数分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

####同盾3个月内平台查询次数
##大额渠道VS大额主营VS小额
output$plot34 <- renderChart2({
td=selectedData4()
td=td[!is.na(td$td_3m),]
td$tdbin=as.character(cut(td$td_3m,breaks=c(0,1,2,5,10,20,50,100),include.lowest = TRUE,right = FALSE))
td$tdbin[td$tdbin=="[0,1)"]="[0,01)"
td$tdbin[td$tdbin=="[1,2)"]="[01,02)"
td$tdbin[td$tdbin=="[2,5)"]="[02,05)"
td$tdbin[td$tdbin=="[5,10)"]="[05,10)"
#td=td %>% group_by(linetype,tdbin) %>% summarise(num=n()) 
td=data.frame(table(melt(data.frame(linetype=td[,column4()],tdbin=td$tdbin),id=c("linetype","tdbin")))) 
if(input$mode4!="line type"){
td$linetype <- factor(td$linetype,levels=c("gjj","wb","bb","tb","oth"))
}

   
plot <- hPlot(Freq~linetype, data = td,group = "tdbin",type = "column",title="同盾三月内查询数分布(大额渠道VS大额主营VS小额)")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额渠道
output$plot35 <- renderChart2({
td=selectedData4()[selectedData4()$linetype=="大额渠道",]
td=td[!is.na(td$td_3m),]
td$tdbin=as.character(cut(td$td_3m,breaks=c(0,1,2,5,10,20,50,100),include.lowest = TRUE,right = FALSE))
td$tdbin[td$tdbin=="[0,1)"]="[0,01)"
td$tdbin[td$tdbin=="[1,2)"]="[01,02)"
td$tdbin[td$tdbin=="[2,5)"]="[02,05)"
td$tdbin[td$tdbin=="[5,10)"]="[05,10)"
#td=td %>% group_by(week,tdbin) %>% summarise(num=n())
td=data.frame(table(melt(data.frame(week=td$week,tdbin=td$tdbin),id=c("week","tdbin"))))    
plot <- hPlot(Freq~week, data = td,group = "tdbin",type = "column",title="大额渠道同盾三月内查询数分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额主营
output$plot36 <- renderChart2({
td=selectedData4()[selectedData4()$linetype=="大额主营",]
td=td[!is.na(td$td_3m),]
td$tdbin=as.character(cut(td$td_3m,breaks=c(0,1,2,5,10,20,50,100),include.lowest = TRUE,right = FALSE))
td$tdbin[td$tdbin=="[0,1)"]="[0,01)"
td$tdbin[td$tdbin=="[1,2)"]="[01,02)"
td$tdbin[td$tdbin=="[2,5)"]="[02,05)"
td$tdbin[td$tdbin=="[5,10)"]="[05,10)"
td=td %>% group_by(week,tdbin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = td,group = "tdbin",type = "column",title="大额主营同盾三月内查询数分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

####同盾分
##大额渠道VS大额主营VS小额
output$plot37 <- renderChart2({
td=selectedData4()
td=td[!is.na(td$final_score),]
td$tdbin=as.character(cut(td$final_score,breaks=c(0,20,80,100),include.lowest = TRUE,right = FALSE))
#td=td %>% group_by(linetype,tdbin) %>% summarise(num=n())
td=data.frame(table(melt(data.frame(linetype=td[,column4()],tdbin=td$tdbin),id=c("linetype","tdbin")))) 
if(input$mode4!="line type"){
td$linetype <- factor(td$linetype,levels=c("gjj","wb","bb","tb","oth"))
}
 
plot <- hPlot(Freq~linetype, data = td,group = "tdbin",type = "column",title="同盾分分布(大额渠道VS大额主营VS小额)")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额渠道
output$plot38 <- renderChart2({
td=selectedData4()[selectedData4()$linetype=="大额渠道",]
td=td[!is.na(td$final_score),]
td$tdbin=as.character(cut(td$final_score,breaks=c(0,20,80,100),include.lowest = TRUE,right = FALSE))
#td=td %>% group_by(week,tdbin) %>% summarise(num=n())
td=data.frame(table(melt(data.frame(week=td$week,tdbin=td$tdbin),id=c("week","tdbin"))))    
plot <- hPlot(Freq~week, data = td,group = "tdbin",type = "column",title="大额渠道同盾分分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额主营
output$plot39 <- renderChart2({
td=selectedData4()[selectedData4()$linetype=="大额主营",]
td=td[!is.na(td$final_score),]
td$tdbin=as.character(cut(td$final_score,breaks=c(0,20,80,100),include.lowest = TRUE,right = FALSE))
td=td %>% group_by(week,tdbin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = td,group = "tdbin",type = "column",title="大额主营同盾分分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

####逾期短信数
##大额渠道VS大额主营VS小额
output$plot40 <- renderChart2({
bo=selectedData4()
bo=bo[!is.na(bo$message_count_default)&bo$message_count_default!=-1,]
bo$bobin=as.character(cut(bo$message_count_default,breaks=c(0,1,5,10,20,500),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,5)"]="[01,05)"
bo$bobin[bo$bobin=="[5,10)"]="[05,10)"
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
bo=data.frame(table(melt(data.frame(linetype=bo[,column4()],bobin=bo$bobin),id=c("linetype","bobin"))))
if(input$mode4!="line type"){
bo$linetype <- factor(bo$linetype,levels=c("gjj","wb","bb","tb","oth"))
}

plot <- hPlot(Freq~linetype, data = bo,group = "bobin",type = "column",title="逾期短信数分布(大额渠道VS大额主营VS小额)")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额渠道
output$plot41 <- renderChart2({
bo=selectedData4()[selectedData4()$linetype=="大额渠道",]
bo=bo[!is.na(bo$message_count_default)&bo$message_count_default!=-1,]
bo$bobin=as.character(cut(bo$message_count_default,breaks=c(0,1,5,10,20,500),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,5)"]="[01,05)"
bo$bobin[bo$bobin=="[5,10)"]="[05,10)"
#bo=bo %>% group_by(week,bobin) %>% summarise(num=n()) 
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title="大额渠道逾期短信数分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额主营
output$plot42 <- renderChart2({
bo=selectedData4()[selectedData4()$linetype=="大额主营",]
bo=bo[!is.na(bo$message_count_default)&bo$message_count_default!=-1,]
bo$bobin=as.character(cut(bo$message_count_default,breaks=c(0,1,5,10,20,500),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,5)"]="[01,05)"
bo$bobin[bo$bobin=="[5,10)"]="[05,10)"
bo=bo %>% group_by(week,bobin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = bo,group = "bobin",type = "column",title="大额主营逾期短信数分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##################用户资质

  selectedData5 <- reactive({
  zizhi[as.Date(zizhi$firstchuo) >= min(input$dates6) & as.Date(zizhi$firstchuo) <= max(input$dates6),]
  })
  
  column5 <- reactive({
    switch(input$mode5,
           "line type" = which(names(selectedData5())=="linetype"),
           "mode type"   = which(names(selectedData5())=="chuomode")
           )
  })
####最大信用卡额度
##大额渠道VS大额主营
output$plot43 <- renderChart2({
options(scipen = 200) ##取消科学计数法
bo=selectedData5()
bo=bo[!is.na(bo$cmax)&bo$cmax!=-1,]
bo$cmax=as.numeric(bo$cmax)
bo$bobin=as.character(cut(bo$cmax,breaks=c(0,3000,6000,10000,20000,50000,100000,1000000),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,3e+03)"]="[0,0.3w)"
bo$bobin[bo$bobin=="[3e+03,6e+03)"]="[0.3w,0.6w)"
bo$bobin[bo$bobin=="[6e+03,1e+04)"]="[0.6w,1w)"
bo$bobin[bo$bobin=="[1e+04,2e+04)"]="[1w,2w)"
bo$bobin[bo$bobin=="[2e+04,5e+04)"]="[2w,5w)"
bo$bobin[bo$bobin=="[5e+04,1e+05)"]="[5w,5-10w)"
bo$bobin[bo$bobin=="[1e+05,1e+06]"]="大于10w"
#bo=data.frame(table(melt(data.frame(linetype=bo$linetype,bobin=bo$bobin),id=c("linetype","bobin"))))
bo=data.frame(table(melt(data.frame(linetype=bo[,column5()],bobin=bo$bobin),id=c("linetype","bobin"))))
if(input$mode5!="line type"){
bo$linetype <- factor(bo$linetype,levels=c("gjj","wb","bb","tb","oth"))
}

   
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~linetype, data = bo,group = "bobin",type = "column",title="最高信用卡额度分布(大额渠道VS大额主营)")
plot$plotOptions(column = list(stacking = "percent"))
#plot$yAxis(reversedStacks = FALSE)
return(plot)    
})
 
##大额渠道
output$plot44 <- renderChart2({
options(scipen = 200) ##取消科学计数法
bo=selectedData5()[selectedData5()$linetype=="大额渠道",]
bo=bo[!is.na(bo$cmax)&bo$cmax!=-1,]
bo$cmax=as.numeric(bo$cmax)
bo$bobin=as.character(cut(bo$cmax,breaks=c(0,3000,6000,10000,20000,50000,100000,1000000),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,3e+03)"]="[0,0.3w)"
bo$bobin[bo$bobin=="[3e+03,6e+03)"]="[0.3w,0.6w)"
bo$bobin[bo$bobin=="[6e+03,1e+04)"]="[0.6w,1w)"
bo$bobin[bo$bobin=="[1e+04,2e+04)"]="[1w,2w)"
bo$bobin[bo$bobin=="[2e+04,5e+04)"]="[2w,5w)"
bo$bobin[bo$bobin=="[5e+04,1e+05)"]="[5w,5-10w)"
bo$bobin[bo$bobin=="[1e+05,1e+06]"]="大于10w"
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title="大额渠道最高信用卡额度分布")
plot$plotOptions(column = list(stacking = "percent"))
#plot$yAxis(reversedStacks = FALSE)
return(plot)    
})

##大额主营
output$plot45 <- renderChart2({
options(scipen = 200) ##取消科学计数法
bo=selectedData5()[selectedData5()$linetype=="大额主营",]
bo=bo[!is.na(bo$cmax)&bo$cmax!=-1,]
bo$cmax=as.numeric(bo$cmax)
bo$bobin=as.character(cut(bo$cmax,breaks=c(0,3000,6000,10000,20000,50000,100000,1000000),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,3e+03)"]="[0,0.3w)"
bo$bobin[bo$bobin=="[3e+03,6e+03)"]="[0.3w,0.6w)"
bo$bobin[bo$bobin=="[6e+03,1e+04)"]="[0.6w,1w)"
bo$bobin[bo$bobin=="[1e+04,2e+04)"]="[1w,2w)"
bo$bobin[bo$bobin=="[2e+04,5e+04)"]="[2w,5w)"
bo$bobin[bo$bobin=="[5e+04,1e+05)"]="[5w,5-10w)"
bo$bobin[bo$bobin=="[1e+05,1e+06]"]="大于10w"
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title="大额主营最高信用卡额度分布")
plot$plotOptions(column = list(stacking = "percent"))
#plot$yAxis(reversedStacks = FALSE)
return(plot)    
})

####其他贷款最大额度
##大额渠道VS大额主营
output$plot46 <- renderChart2({
bo=selectedData5()
bo=bo[!is.na(bo$omax)&bo$omax!=-1,]
bo$omax=as.numeric(bo$omax)
bo$bobin=as.character(cut(bo$omax,breaks=c(0,5000,10000,20000,30000,50000,100000,10000000),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,5e+03)"]="[0,0.5w)"
bo$bobin[bo$bobin=="[5e+03,1e+04)"]="[0.5w,1w)"
bo$bobin[bo$bobin=="[1e+04,2e+04)"]="[1w,2w)"
bo$bobin[bo$bobin=="[2e+04,3e+04)"]="[2w,3w)"
bo$bobin[bo$bobin=="[3e+04,5e+04)"]="[3w,5w)"
bo$bobin[bo$bobin=="[5e+04,1e+05)"]="[5w,5-10w)"
bo$bobin[bo$bobin=="[1e+05,1e+07]"]="大于10w"
#bo=data.frame(table(melt(data.frame(linetype=bo$linetype,bobin=bo$bobin),id=c("linetype","bobin"))))

bo=data.frame(table(melt(data.frame(linetype=bo[,column5()],bobin=bo$bobin),id=c("linetype","bobin"))))
if(input$mode5!="line type"){
bo$linetype <- factor(bo$linetype,levels=c("gjj","wb","bb","tb","oth"))
}
   
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~linetype, data = bo,group = "bobin",type = "column",title="其他贷款最大额度分布(大额渠道VS大额主营)")
plot$plotOptions(column = list(stacking = "percent"))
#plot$yAxis(reversedStacks = FALSE)
return(plot)    
})
 
##大额渠道
output$plot47 <- renderChart2({
bo=selectedData5()[selectedData5()$linetype=="大额渠道",]
bo=bo[!is.na(bo$omax)&bo$omax!=-1,]
bo$omax=as.numeric(bo$omax)
bo$bobin=as.character(cut(bo$omax,breaks=c(0,5000,10000,20000,30000,50000,100000,10000000),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,5e+03)"]="[0,0.5w)"
bo$bobin[bo$bobin=="[5e+03,1e+04)"]="[0.5w,1w)"
bo$bobin[bo$bobin=="[1e+04,2e+04)"]="[1w,2w)"
bo$bobin[bo$bobin=="[2e+04,3e+04)"]="[2w,3w)"
bo$bobin[bo$bobin=="[3e+04,5e+04)"]="[3w,5w)"
bo$bobin[bo$bobin=="[5e+04,1e+05)"]="[5w,5-10w)"
bo$bobin[bo$bobin=="[1e+05,1e+07]"]="大于10w"
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title="大额渠道其他贷款最大额度分布")
plot$plotOptions(column = list(stacking = "percent"))
#plot$yAxis(reversedStacks = FALSE)
return(plot)    
})

##大额主营
output$plot48 <- renderChart2({
bo=selectedData5()[selectedData5()$linetype=="大额主营",]
bo=bo[!is.na(bo$omax)&bo$omax!=-1,]
bo$omax=as.numeric(bo$omax)
bo$bobin=as.character(cut(bo$omax,breaks=c(0,5000,10000,20000,30000,50000,100000,10000000),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,5e+03)"]="[0,0.5w)"
bo$bobin[bo$bobin=="[5e+03,1e+04)"]="[0.5w,1w)"
bo$bobin[bo$bobin=="[1e+04,2e+04)"]="[1w,2w)"
bo$bobin[bo$bobin=="[2e+04,3e+04)"]="[2w,3w)"
bo$bobin[bo$bobin=="[3e+04,5e+04)"]="[3w,5w)"
bo$bobin[bo$bobin=="[5e+04,1e+05)"]="[5w,5-10w)"
bo$bobin[bo$bobin=="[1e+05,1e+07]"]="大于10w"
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title="大额主营其他贷款最大额度分布")
plot$plotOptions(column = list(stacking = "percent"))
#plot$yAxis(reversedStacks = FALSE)
return(plot)    
})

####有效信用卡张数
##大额渠道VS大额主营
output$plot49 <- renderChart2({
bo=selectedData5()
bo$vcard=as.numeric(bo$vcard)
bo=bo[!is.na(bo$vcard)&bo$vcard!=-1,]
bo$vcard=as.numeric(bo$vcard)
bo$bobin=as.character(cut(bo$vcard,breaks=c(0,1,3,6,10,20,80),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,3)"]="[01,03)"
bo$bobin[bo$bobin=="[3,6)"]="[03,06)"
bo$bobin[bo$bobin=="[6,10)"]="[06,10)"
#bo=data.frame(table(melt(data.frame(linetype=bo$linetype,bobin=bo$bobin),id=c("linetype","bobin"))))  
bo=data.frame(table(melt(data.frame(linetype=bo[,column5()],bobin=bo$bobin),id=c("linetype","bobin"))))
if(input$mode5!="line type"){
bo$linetype <- factor(bo$linetype,levels=c("gjj","wb","bb","tb","oth"))
}

 
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~linetype, data = bo,group = "bobin",type = "column",title="有效信用卡张数分布(大额渠道VS大额主营)")
plot$plotOptions(column = list(stacking = "percent"))
#plot$yAxis(reversedStacks = FALSE)
return(plot)    
})
 
##大额渠道
output$plot50 <- renderChart2({
bo=selectedData5()[selectedData5()$linetype=="大额渠道",]
bo=bo[!is.na(bo$vcard)&bo$vcard!=-1,]
bo$vcard=as.numeric(bo$vcard)
bo$bobin=as.character(cut(bo$vcard,breaks=c(0,1,3,6,10,20,80),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,3)"]="[01,03)"
bo$bobin[bo$bobin=="[3,6)"]="[03,06)"
bo$bobin[bo$bobin=="[6,10)"]="[06,10)"
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title="大额渠道有效信用卡张数分布")
plot$plotOptions(column = list(stacking = "percent"))
#plot$yAxis(reversedStacks = FALSE)
return(plot)    
})

##大额主营
output$plot51 <- renderChart2({
bo=selectedData5()[selectedData5()$linetype=="大额主营",]
bo=bo[!is.na(bo$vcard)&bo$vcard!=-1,]
bo$vcard=as.numeric(bo$vcard)
bo$bobin=as.character(cut(bo$vcard,breaks=c(0,1,3,6,10,20,80),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,3)"]="[01,03)"
bo$bobin[bo$bobin=="[3,6)"]="[03,06)"
bo$bobin[bo$bobin=="[6,10)"]="[06,10)"
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title="大额主营有效信用卡张数分布")
plot$plotOptions(column = list(stacking = "percent"))
#plot$yAxis(reversedStacks = FALSE)
return(plot)    
})


##################用户负债

  selectedData6 <- reactive({
  owing[as.Date(owing$firstchuo) >= min(input$dates7) & as.Date(owing$firstchuo) <= max(input$dates7),]
  })

  column6 <- reactive({
    switch(input$mode6,
           "line type" = which(names(selectedData6())=="linetype"),
           "mode type"   = which(names(selectedData6())=="chuomode")
           )
  })
  
####其他贷款月还
##大额渠道VS大额主营
output$plot52 <- renderChart2({
bo=selectedData6()
bo=bo[!is.na(bo$rpo)&bo$rpo!=-1,]
bo$rpo=as.numeric(bo$rpo)
bo$bobin=as.character(cut(bo$rpo,breaks=c(0,1000,3000,6000,10000,20000,500000),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1e+03)"]="[0,0.1w)"
bo$bobin[bo$bobin=="[1e+03,3e+03)"]="[0.1w,0.3w)"
bo$bobin[bo$bobin=="[3e+03,6e+03)"]="[0.3w,0.6w)"
bo$bobin[bo$bobin=="[6e+03,1e+04)"]="[0.6,1w)"
bo$bobin[bo$bobin=="[1e+04,2e+04)"]="[1w,2w)"
bo$bobin[bo$bobin=="[2e+04,5e+05]"]="2w以上"
#bo=data.frame(table(melt(data.frame(linetype=bo$linetype,bobin=bo$bobin),id=c("linetype","bobin"))))
bo=data.frame(table(melt(data.frame(linetype=bo[,column6()],bobin=bo$bobin),id=c("linetype","bobin"))))
if(input$mode6!="line type"){
bo$linetype <- factor(bo$linetype,levels=c("gjj","wb","bb","tb","oth"))
}
   
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~linetype, data = bo,group = "bobin",type = "column",title="其他贷款月还分布(大额渠道VS大额主营)")
plot$plotOptions(column = list(stacking = "percent"))
#plot$yAxis(reversedStacks = FALSE)
return(plot)    
})
 
##大额渠道
output$plot53 <- renderChart2({
bo=selectedData6()[selectedData6()$linetype=="大额渠道",]
bo=bo[!is.na(bo$rpo)&bo$rpo!=-1,]
bo$rpo=as.numeric(bo$rpo)
bo$bobin=as.character(cut(bo$rpo,breaks=c(0,1000,3000,6000,10000,20000,500000),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1e+03)"]="[0,0.1w)"
bo$bobin[bo$bobin=="[1e+03,3e+03)"]="[0.1w,0.3w)"
bo$bobin[bo$bobin=="[3e+03,6e+03)"]="[0.3w,0.6w)"
bo$bobin[bo$bobin=="[6e+03,1e+04)"]="[0.6,1w)"
bo$bobin[bo$bobin=="[1e+04,2e+04)"]="[1w,2w)"
bo$bobin[bo$bobin=="[2e+04,5e+05]"]="2w以上"
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title="大额渠道其他贷款月还分布")
plot$plotOptions(column = list(stacking = "percent"))
#plot$yAxis(reversedStacks = FALSE)
return(plot)    
})

##大额主营
output$plot54 <- renderChart2({
bo=selectedData6()[selectedData6()$linetype=="大额主营",]
bo=bo[!is.na(bo$rpo)&bo$rpo!=-1,]
bo$rpo=as.numeric(bo$rpo)
bo$bobin=as.character(cut(bo$rpo,breaks=c(0,1000,3000,6000,10000,20000,500000),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1e+03)"]="[0,0.1w)"
bo$bobin[bo$bobin=="[1e+03,3e+03)"]="[0.1w,0.3w)"
bo$bobin[bo$bobin=="[3e+03,6e+03)"]="[0.3w,0.6w)"
bo$bobin[bo$bobin=="[6e+03,1e+04)"]="[0.6,1w)"
bo$bobin[bo$bobin=="[1e+04,2e+04)"]="[1w,2w)"
bo$bobin[bo$bobin=="[2e+04,5e+05]"]="2w以上"
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title="大额主营其他贷款月还分布")
plot$plotOptions(column = list(stacking = "percent"))
#plot$yAxis(reversedStacks = FALSE)
return(plot)    
})

####信用卡月还
##大额渠道VS大额主营
output$plot55 <- renderChart2({
bo=selectedData6()
bo=bo[!is.na(bo$rpc)&bo$rpc!=-1,]
bo$rpc=as.numeric(bo$rpc)
bo$bobin=as.character(cut(bo$rpc,breaks=c(0,1500,3000,6000,10000,20000,500000),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1.5e+03)"]="[0,0.15w)"
bo$bobin[bo$bobin=="[1.5e+03,3e+03)"]="[0.15w,0.3w)"
bo$bobin[bo$bobin=="[3e+03,6e+03)"]="[0.3w,0.6w)"
bo$bobin[bo$bobin=="[6e+03,1e+04)"]="[0.6,1w)"
bo$bobin[bo$bobin=="[1e+04,2e+04)"]="[1w,2w)"
bo$bobin[bo$bobin=="[2e+04,5e+05]"]="2w以上"
#bo=data.frame(table(melt(data.frame(linetype=bo$linetype,bobin=bo$bobin),id=c("linetype","bobin"))))
bo=data.frame(table(melt(data.frame(linetype=bo[,column6()],bobin=bo$bobin),id=c("linetype","bobin"))))
if(input$mode6!="line type"){
bo$linetype <- factor(bo$linetype,levels=c("gjj","wb","bb","tb","oth"))
}
   
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~linetype, data = bo,group = "bobin",type = "column",title="信用卡月还分布(大额渠道VS大额主营)")
plot$plotOptions(column = list(stacking = "percent"))
#plot$yAxis(reversedStacks = FALSE)
return(plot)    
})
 
##大额渠道
output$plot56 <- renderChart2({
bo=selectedData6()[selectedData6()$linetype=="大额渠道",]
bo=bo[!is.na(bo$rpc)&bo$rpc!=-1,]
bo$rpc=as.numeric(bo$rpc)
bo$bobin=as.character(cut(bo$rpc,breaks=c(0,1500,3000,6000,10000,20000,500000),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1.5e+03)"]="[0,0.15w)"
bo$bobin[bo$bobin=="[1.5e+03,3e+03)"]="[0.15w,0.3w)"
bo$bobin[bo$bobin=="[3e+03,6e+03)"]="[0.3w,0.6w)"
bo$bobin[bo$bobin=="[6e+03,1e+04)"]="[0.6,1w)"
bo$bobin[bo$bobin=="[1e+04,2e+04)"]="[1w,2w)"
bo$bobin[bo$bobin=="[2e+04,5e+05]"]="2w以上"
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title="大额渠道信用卡月还分布")
plot$plotOptions(column = list(stacking = "percent"))
#plot$yAxis(reversedStacks = FALSE)
return(plot)    
})

##大额主营
output$plot57 <- renderChart2({
bo=selectedData6()[selectedData6()$linetype=="大额主营",]
bo=bo[!is.na(bo$rpc)&bo$rpc!=-1,]
bo$rpc=as.numeric(bo$rpc)
bo$bobin=as.character(cut(bo$rpc,breaks=c(0,1500,3000,6000,10000,20000,500000),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1.5e+03)"]="[0,0.15w)"
bo$bobin[bo$bobin=="[1.5e+03,3e+03)"]="[0.15w,0.3w)"
bo$bobin[bo$bobin=="[3e+03,6e+03)"]="[0.3w,0.6w)"
bo$bobin[bo$bobin=="[6e+03,1e+04)"]="[0.6,1w)"
bo$bobin[bo$bobin=="[1e+04,2e+04)"]="[1w,2w)"
bo$bobin[bo$bobin=="[2e+04,5e+05]"]="2w以上"
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title="大额主营信用卡月还分布")
plot$plotOptions(column = list(stacking = "percent"))
#plot$yAxis(reversedStacks = FALSE)
return(plot)    
})

####房贷月还
##大额渠道VS大额主营
output$plot58 <- renderChart2({
bo=selectedData6()
bo=bo[!is.na(bo$rph)&bo$rph!=-1,]
bo$rph=as.numeric(bo$rph)
bo$bobin=as.character(cut(bo$rph,breaks=c(0,1,2000,5000,10000,20000,500000),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="无房贷"
bo$bobin[bo$bobin=="[1,2e+03)"]="(0,0.2w)"
bo$bobin[bo$bobin=="[2e+03,5e+03)"]="(0.2w,0.5w)"
bo$bobin[bo$bobin=="[5e+03,1e+04)"]="[0.5,1w)"
bo$bobin[bo$bobin=="[1e+04,2e+04)"]="[1w,2w)"
bo$bobin[bo$bobin=="[2e+04,5e+05]"]="2w以上"
#bo=data.frame(table(melt(data.frame(linetype=bo$linetype,bobin=bo$bobin),id=c("linetype","bobin"))))
bo=data.frame(table(melt(data.frame(linetype=bo[,column6()],bobin=bo$bobin),id=c("linetype","bobin"))))
if(input$mode6!="line type"){
bo$linetype <- factor(bo$linetype,levels=c("gjj","wb","bb","tb","oth"))
}
   
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~linetype, data = bo,group = "bobin",type = "column",title="房贷月还分布(大额渠道VS大额主营)")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
})
 
##大额渠道
output$plot59 <- renderChart2({
bo=selectedData6()[selectedData6()$linetype=="大额渠道",]
bo=bo[!is.na(bo$rph)&bo$rph!=-1,]
bo$rph=as.numeric(bo$rph)
bo$bobin=as.character(cut(bo$rph,breaks=c(0,1,2000,5000,10000,20000,500000),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="无房贷"
bo$bobin[bo$bobin=="[1,2e+03)"]="(0,0.2w)"
bo$bobin[bo$bobin=="[2e+03,5e+03)"]="(0.2w,0.5w)"
bo$bobin[bo$bobin=="[5e+03,1e+04)"]="[0.5,1w)"
bo$bobin[bo$bobin=="[1e+04,2e+04)"]="[1w,2w)"
bo$bobin[bo$bobin=="[2e+04,5e+05]"]="2w以上"
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title="大额渠道房贷月还分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
})

##大额主营
output$plot60 <- renderChart2({
bo=selectedData6()[selectedData6()$linetype=="大额主营",]
bo=bo[!is.na(bo$rph)&bo$rph!=-1,]
bo$rph=as.numeric(bo$rph)
bo$bobin=as.character(cut(bo$rph,breaks=c(0,1,2000,5000,10000,20000,500000),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="无房贷"
bo$bobin[bo$bobin=="[1,2e+03)"]="(0,0.2w)"
bo$bobin[bo$bobin=="[2e+03,5e+03)"]="(0.2w,0.5w)"
bo$bobin[bo$bobin=="[5e+03,1e+04)"]="[0.5,1w)"
bo$bobin[bo$bobin=="[1e+04,2e+04)"]="[1w,2w)"
bo$bobin[bo$bobin=="[2e+04,5e+05]"]="2w以上"
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
#bo=bo %>% group_by(linetype,bobin) %>% summarise(num=n()) 
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title="大额主营房贷月还分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
})

############################################
######渠道监控
  selectedData7 <- reactive({
  channel[as.Date(channel$first_login_time) >= min(input$dates8) & as.Date(channel$first_login_time) <= max(input$dates8),]
  })

##渠道全流程转化
output$rate1 = renderDataTable({

bo=selectedData7()[selectedData7()$sourcetype=="app",]
app=bo %>% group_by(sourcename) %>% 
summarise(num=n(),chuonum=sum(chuo_status),younum=sum(youe_status),
chuoratio=round(sum(chuo_status)/n(),2),
youratio=round(ifelse(sum(chuo_status)>0,sum(youe_status)/sum(chuo_status),0),2),
fbratio=round(ifelse(sum(youe_status)>0,sum(fb_status)/sum(youe_status),0),2),
zhratio=round(sum(cj_status)/n(),2),
allzhratio=round(sum(allcj_status)/n(),2)) %>%
subset(num>50)
names(app)=c("app渠道名称","登录大额人数","戳额数","有额数","戳额率","有额率","确认率","大额转化率","整体转化率")
#return(app)
datatable(app,caption = 'APP渠道数据')
})
output$rate2 = renderDataTable({

bo=selectedData7()[selectedData7()$sourcetype=="M",]
m=bo %>% group_by(sourcename) %>% 
summarise(num=n(),chuonum=sum(chuo_status),younum=sum(youe_status),
chuoratio=round(sum(chuo_status)/n(),2),
youratio=round(ifelse(sum(chuo_status)>0,sum(youe_status)/sum(chuo_status),0),2),
fbratio=round(ifelse(sum(youe_status)>0,sum(fb_status)/sum(youe_status),0),2),
zhratio=round(sum(cj_status)/n(),2),
allzhratio=round(sum(allcj_status)/n(),2)) %>%
subset(num>50)
names(m)=c("app渠道名称","登录大额人数","戳额数","有额数","戳额率","有额率","确认率","大额转化率","整体转化率")
#return(m)
datatable(m,caption = 'M站渠道数据')

})


####app渠道

  selectedData8 <- reactive({
  channeleva1[as.Date(channeleva1$firstchuo) >= min(input$dates9) & as.Date(channeleva1$firstchuo) <= max(input$dates9),]
  })
##基本信息
  selectedcolumn <- reactive({
    switch(input$basic,
           "edu" = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$edu),
           "usertype"   = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$usertype),
           "citylevel"  = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$citylevel_bin)
           )
  })
  
  selectedorder <- reactive({
    switch(input$basic,
           "edu" = ceshi1$sourcename[order(ceshi1$edu,decreasing=TRUE)],
           "usertype"   = ceshi1$sourcename[order(ceshi1$usertype,decreasing=TRUE)],
           "citylevel"  = ceshi1$sourcename[order(ceshi1$citylevel,decreasing=TRUE)]
           )
  })
  
    
output$plot61 <- renderChart2({
#bo=selectedcolumn()[!is.na(selectedcolumn()$edu),]
bo=data.frame(table(melt(selectedcolumn(),id=c("sourcename","edu"))))
bo$sourcename <- factor(bo$sourcename,levels=selectedorder())
plot <- hPlot(Freq~sourcename, data = bo,group = "edu",type = "column",title=sprintf("大额APP渠道%s分布",input$basic))
plot$plotOptions(column = list(stacking = "percent"))
if(input$basic!="usertype"){
plot$yAxis(reversedStacks = FALSE)}
return(plot)    
})

##模型类评分
  selectedcolumn1 <- reactive({
    switch(input$basic1,
           "bin" = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$credit_bin),
           "tengxun"   = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$tcbin),
           "jd"  = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$jdbin),
           "umeng"  = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$umbin)
           )
  })
  
  selectedorder1 <- reactive({
    switch(input$basic1,
           "bin" = ceshi1$sourcename[order(ceshi1$bin,decreasing=TRUE)],
           "tengxun"   = ceshi1$sourcename[order(ceshi1$tc,decreasing=TRUE)],
           "jd"  = ceshi1$sourcename[order(ceshi1$jd,decreasing=TRUE)],
           "umeng"  = ceshi1$sourcename[order(ceshi1$um,decreasing=TRUE)]
           )
  })
    
output$plot62 <- renderChart2({
#bo=selectedcolumn1()[!is.na(selectedcolumn1()$edu),]
bo=data.frame(table(melt(selectedcolumn1(),id=c("sourcename","edu"))))
bo$sourcename <- factor(bo$sourcename,levels=selectedorder1())
plot <- hPlot(Freq~sourcename, data = bo,group = "edu",type = "column",title=sprintf("大额APP渠道%s分布",input$basic1))
plot$plotOptions(column = list(stacking = "percent"))
if(input$basic1 %in% c("bin","tengxun")){
plot$yAxis(reversedStacks = FALSE) }
return(plot)    
})

##用户资质
  selectedcolumn2 <- reactive({
    switch(input$basic2,
           "max creditcard limit" = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$cmaxbin),
           "max otherloan limit"   = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$omaxbin),
           "salary"  = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$pretax)
           )
  })
  
  selectedorder2 <- reactive({
    switch(input$basic2,
           "max creditcard limit" = ceshi1$sourcename[order(ceshi1$cmax,decreasing=TRUE)],
           "max otherloan limit"   = ceshi1$sourcename[order(ceshi1$omax,decreasing=TRUE)],
           "salary"  = ceshi1$sourcename[order(ceshi1$pre,decreasing=TRUE)],
           )
  })
    
output$plot63 <- renderChart2({
#bo=selectedcolumn2()[!is.na(selectedcolumn2()$edu),]
bo=data.frame(table(melt(selectedcolumn2(),id=c("sourcename","edu"))))
bo$sourcename <- factor(bo$sourcename,levels=selectedorder2())
plot <- hPlot(Freq~sourcename, data = bo,group = "edu",type = "column",title=sprintf("大额APP渠道%s分布",input$basic2))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})

##多头数据
  selectedcolumn3 <- reactive({
    switch(input$basic3,
           "borrow app num" = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$bobin),
           "tongdun_3m"   = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$tdbin1),
           "tongdun_reject"  = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$tdbin2),
           "credit report query_1m"  = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$zxbin1)
           )
  })
  
  selectedorder3 <- reactive({
    switch(input$basic3,
           "borrow app num" = ceshi1$sourcename[order(ceshi1$bo,decreasing=TRUE)],
           "tongdun_3m"   = ceshi1$sourcename[order(ceshi1$td1,decreasing=TRUE)],
           "tongdun_reject"  = ceshi1$sourcename[order(ceshi1$td2,decreasing=TRUE)],
           "credit report query_1m"  = ceshi1$sourcename[order(ceshi1$zx1,decreasing=TRUE)],
           )
  })
    
output$plot64 <- renderChart2({
#bo=selectedcolumn3()[!is.na(selectedcolumn3()$edu),]
bo=data.frame(table(melt(selectedcolumn3(),id=c("sourcename","edu"))))
bo$sourcename <- factor(bo$sourcename,levels=selectedorder3())
plot <- hPlot(Freq~sourcename, data = bo,group = "edu",type = "column",title=sprintf("大额APP渠道%s分布",input$basic3))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
})

##逾期数据
  selectedcolumn4 <- reactive({
    switch(input$basic4,
           "overdue message count" = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$defbin),
           "credit report overdue_2y"   = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$zxbin2)
           )
  })
  
  selectedorder4 <- reactive({
    switch(input$basic4,
           "overdue message count" = ceshi1$sourcename[order(ceshi1$def,decreasing=TRUE)],
           "credit report overdue_2y"   = ceshi1$sourcename[order(ceshi1$zx2,decreasing=TRUE)]
           )
  })
    
output$plot65 <- renderChart2({
#bo=selectedcolumn4()[!is.na(selectedcolumn4()$edu),]
bo=data.frame(table(melt(selectedcolumn4(),id=c("sourcename","edu"))))
bo$sourcename <- factor(bo$sourcename,levels=selectedorder4())
plot <- hPlot(Freq~sourcename, data = bo,group = "edu",type = "column",title=sprintf("大额APP渠道%s分布",input$basic4))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
})
################
##画雷达图
#mm=score[grep("广点通|资产大额秋成科技|资产大额淘钱宝",score$sourcename),]

output$sp1<-renderPlot({ 
      mm1=subset(score,select=sourcename:usertype)
      mm1=mm1[mm1$sourcename %in% input$spider,] %>% mutate_at(vars(edu:usertype),funs(rescale))
      rada=ggradar(mm1,axis.label.size = 5,legend.text.size = 16)
  return(rada)  
})

output$sp2<-renderPlot({ 
      mm2=score[,c(1,5:8)]
      mm2=mm2[mm2$sourcename %in% input$spider,] %>% mutate_at(vars(bin:jd),funs(rescale))
      rada=ggradar(mm2,axis.label.size = 5,legend.text.size = 16)
  return(rada)  
})

output$sp3<-renderPlot({ 
      mm3=score[,c(1,9:11)]
      mm3=mm3[mm3$sourcename %in% input$spider,] %>% mutate_at(vars(max_creditcard:salary),funs(rescale))
      rada=ggradar(mm3,axis.label.size = 5,legend.text.size = 16)
  return(rada)  
})

output$sp4<-renderPlot({ 
      mm4=score[,c(1,12:14)]
      mm4=mm4[mm4$sourcename %in% input$spider,] %>% mutate_at(vars(boapp:zx_query),funs(rescale))
      rada=ggradar(mm4,axis.label.size = 5,legend.text.size = 16)
  return(rada)  
})

output$sp5<-renderPlot({ 
      mm5=score[,c(1,15:16)]
      mm5=mm5[mm5$sourcename %in% input$spider,] %>% mutate_at(vars(ovd_msg:overdue_zx),funs(rescale))
      rada=ggradar(mm5,axis.label.size = 5,legend.text.size = 16)
  return(rada)  
})
######
output$sp6<-renderPlot({ 
      mm6=score %>% group_by(sourcename) %>%
      summarise(basicinfo=edu+city+usertype,
      model=(bin*2+tengxun*1+umeng*1+jd)/5,
      asset=salary+max_creditcard+max_otherloan,
      multiloan=boapp+tongdun+zx_query,
      ovd=ovd_msg+overdue_zx )
      
      
      
      mm6=mm6[mm6$sourcename %in% input$spider,] %>% mutate_at(vars(basicinfo:ovd),funs(rescale))
      rada=ggradar(mm6,axis.label.size = 7,legend.text.size = 16)
  return(rada)  
})

####################################################################################
})