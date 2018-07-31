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

# channel2=read.table("channel_m.txt",header = TRUE,sep="",fileEncoding="UTF-8",row.names = NULL) ###正确
# channel2$first_login_time=as.character(channel2$first_login_time)

channeleva=read.table("channeleva.txt",header = TRUE,sep="",fileEncoding="UTF-8",row.names = NULL)

# dx1=channeleva[channeleva$qudao_type=="APP",] %>% group_by(sourcename) %>% 
# summarise(chuonum=n()) %>% arrange(desc(chuonum))

# dx2=channeleva[channeleva$qudao_type=="M",] %>% group_by(sourcename) %>% 
# summarise(chuonum=n()) %>% arrange(desc(chuonum))

# output$dx <- renderUI({
    # n=ifelse(input$line7=="APP",dx1$sourcename[dx1$chuonum>200],dx2$sourcename[dx2$chuonum>200])
    # checkboxGroupInput("spider", 
                        # h4("渠道选择(戳额数>200的渠道)"), 
                        # choices =n,
                        # selected = n[1:3])

# })

output$dx <- renderUI({
input$line7
})


ceshi=read.table("ceshi.txt",header = TRUE,sep="",fileEncoding="UTF-8",row.names = NULL) ###正确
score=read.table("score.txt",header = TRUE,sep="",fileEncoding="UTF-8",row.names = NULL) ###正确


###############渠道评估数据处理
getNewBin=function(channeleva1){

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

    return(channeleva1)
    }
    
channeleva= getNewBin(channeleva)  


############自定义函数
dataChange=function(data,index1,index2,index3,index4){
    if(index1==1){ ##不需要筛选linetype类型
        data=data[data$biglinetype==index2,]
    }
    if(index1==2){ #需要筛选linetype类型
        if(index2=="APP"){data=data[data$biglinetype==index2&data$linetype==index3[1],]}
        if(index2=="M站"){data=data[data$biglinetype==index2&data$linetype==index3[2],]}
    }
    data=data[is.na(data[[index4]])==FALSE,]
    return(data)
    
    }

cusChange=function(input,data){

    if(input=="mode type"){
        data$linetype <- factor(data$linetype,levels=c("gjj","wb","bb","tb","oth"))
        }else{
            if(input=="channel type"){
                data$linetype <- factor(data$linetype,levels=c("APP信息流","APP贷超","M站信息流","M站贷超","其他app渠道类型","异业合作","大额主营","小额"))
            }
        }
    return(data)
    }

cusChange1=function(input1,input2,data){
    if(input1=="APP"){
        if(input2=="mode type"){data$linetype <- factor(data$linetype,levels=c("gjj","wb","bb","tb","oth"))}
        if(input2=="channel type"){data$linetype <- factor(data$linetype,levels=c("APP信息流","APP贷超","M站信息流","M站贷超","其他app渠道类型","异业合作","大额主营","小额"))}
        if(input2=="line type"){data$linetype <- factor(data$linetype,levels=c("大额渠道","大额主营","小额"))}
    }
        
    if(input1=="M站"){
        if(input2=="mode type"){data$linetype <- factor(data$linetype,levels=c("bb_m","gxb_m","xiaoe_m"))}
        if(input2=="channel type"){data$linetype <- factor(data$linetype,levels=c("APP信息流","APP贷超","M站信息流","M站贷超","其他M站渠道类型","小额_m"))}
        if(input2=="line type"){data$linetype <- factor(data$linetype,levels=c("大额_m","小额_m"))}
    }
    return(data)
    }


ceshiChange=function(data1,data2,input){
    if(input=="APP"){data=data1}
    if(input=="M"){data=data2}
    return(data)

    }
    
#########################

##################基本信息

  selectedData1 <- reactive({
  basic[as.Date(basic$firstchuo) >= min(input$dates2) & as.Date(basic$firstchuo) <= max(input$dates2),]

  })
  
  ##用户资质
  column <- reactive({
    switch(input$mode1,
           "line type" = which(names(selectedData1())=="linetype"),
           "mode type"   = which(names(selectedData1())=="chuomode"),
           "channel type"   = which(names(selectedData1())=="channel_total_category")
           )
  })


  
####学历分布
##大额渠道VS大额主营VS小额
output$plot10 <- renderChart2({

#edu=selectedData1()
edu=dataChange(selectedData1(),1,input$line,c("大额渠道","大额_m"),"edu")
#edu=edu %>% group_by(linetype,edu) %>% summarise(num=n()) 
edu=data.frame(table(melt(data.frame(linetype=edu[,column()],edu=edu$edu),id=c("linetype","edu"))))
edu=cusChange1(input$line,input$mode1,edu)
plot <- hPlot(Freq~linetype, data = edu,group = "edu",type = "column",title="学历分布对比")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
})
##大额
output$plot11 <- renderChart2({

#edu=selectedData1()[selectedData1()$linetype=="大额渠道",]
edu=dataChange(selectedData1(),2,input$line,c("大额渠道","大额_m"),"edu")
n=unique(edu$linetype)

#edu=edu%>% group_by(week,edu) %>% summarise(num=n())

edu=data.frame(table(melt(data.frame(week=edu$week,edu=edu$edu),id=c("week","edu"))))
plot <- hPlot(Freq~week, data = edu,
              group = "edu",
              type = "column",title=sprintf("%s大额--%s人群学历分布",input$line,n))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)
})
##小额
output$plot12 <- renderChart2({

#edu=selectedData1()[selectedData1()$linetype=="大额主营",]
edu=dataChange(selectedData1(),2,input$line,c("大额主营","小额_m"),"edu")
n=unique(edu$linetype)

#edu=edu%>% group_by(week,edu) %>% summarise(num=n())
edu=data.frame(table(melt(data.frame(week=edu$week,edu=edu$edu),id=c("week","edu")))) 
plot <- hPlot(Freq~week, data = edu,
              group = "edu",
              type = "column",title=sprintf("%s大额--%s人群学历分布",input$line,n))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)
})

####年龄分布
##大额渠道VS大额主营VS小额
output$plot1 <- renderChart2({

#age=selectedData1()[is.na(selectedData1()$age_bin)==FALSE,]
age=dataChange(selectedData1(),1,input$line,c("大额渠道","大额_m"),"age_bin")

age=data.frame(table(melt(data.frame(linetype=age[,column()],age_bin=age$age_bin),id=c("linetype","age_bin")))) 

age=cusChange1(input$line,input$mode1,age)
plot <- hPlot(Freq~linetype, data = age,
              group = "age_bin",
              type = "column",title="年龄分布对比")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)
})
##大额
output$plot2 <- renderChart2({

#age=selectedData1()[is.na(selectedData1()$age_bin)==FALSE&selectedData1()$linetype=="大额渠道",]
age=dataChange(selectedData1(),2,input$line,c("大额渠道","大额_m"),"age_bin")
n=unique(age$linetype)

age=data.frame(table(melt(data.frame(week=age$week,age_bin=age$age_bin),id=c("week","age_bin")))) 
plot <- hPlot(Freq~week, data = age,
              group = "age_bin",
              type = "column",title=sprintf("%s大额--%s人群年龄分布",input$line,n))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)
})
##小额
output$plot3 <- renderChart2({

#age=selectedData1()[is.na(selectedData1()$age_bin)==FALSE&selectedData1()$linetype=="大额主营",]
age=dataChange(selectedData1(),2,input$line,c("大额主营","小额_m"),"age_bin")
n=unique(age$linetype)

age=data.frame(table(melt(data.frame(week=age$week,age_bin=age$age_bin),id=c("week","age_bin")))) 
plot <- hPlot(Freq~week, data = age,
              group = "age_bin",
              type = "column",title=sprintf("%s大额--%s人群年龄分布",input$line,n))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)
})

####人群类型分布
##大额渠道VS大额主营VS小额
output$plot4 <- renderChart2({

#usertype=selectedData1()
usertype=dataChange(selectedData1(),1,input$line,c("大额渠道","大额_m"),"usertype")

usertype=data.frame(table(melt(data.frame(linetype=usertype[,column()],usertype=usertype$usertype),id=c("linetype","usertype")))) 
usertype=cusChange1(input$line,input$mode1,usertype)
plot <- hPlot(Freq~linetype, data = usertype,group = "usertype",type = "column",title="人群类型分布对比")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})
##大额
output$plot5 <- renderChart2({

#usertype=selectedData1()[selectedData1()$linetype=="大额渠道",]

usertype=dataChange(selectedData1(),2,input$line,c("大额渠道","大额_m"),"usertype")
n=unique(usertype$linetype)

usertype=data.frame(table(melt(data.frame(week=usertype$week,usertype=usertype$usertype),id=c("week","usertype")))) 
plot <- hPlot(Freq~week, data = usertype,group = "usertype",type = "column",title=sprintf("%s大额--%s人群类型分布",input$line,n))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})
##小额
output$plot6 <- renderChart2({

#usertype=selectedData1()[selectedData1()$linetype=="大额主营",]

usertype=dataChange(selectedData1(),2,input$line,c("大额主营","小额_m"),"usertype")
n=unique(usertype$linetype)

usertype=data.frame(table(melt(data.frame(week=usertype$week,usertype=usertype$usertype),id=c("week","usertype")))) 
plot <- hPlot(Freq~week, data = usertype,group = "usertype",type = "column",title=sprintf("%s大额--%s人群类型分布",input$line,n))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})

####城市等级分布
##大额渠道VS大额主营VS小额
output$plot7 <- renderChart2({

#citylevel=selectedData1()
citylevel=dataChange(selectedData1(),1,input$line,c("大额渠道","大额_m"),"citylevel_bin")

citylevel=data.frame(table(melt(data.frame(linetype=citylevel[,column()],citylevel_bin=citylevel$citylevel_bin),id=c("linetype","citylevel_bin")))) 
citylevel=cusChange1(input$line,input$mode1,citylevel)

plot <- hPlot(Freq~linetype, data = citylevel,group = "citylevel_bin",type = "column",title="城市等级分布对比")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
})
##大额
output$plot8 <- renderChart2({

#citylevel=selectedData1()[selectedData1()$linetype=="大额渠道",]
citylevel=dataChange(selectedData1(),2,input$line,c("大额渠道","大额_m"),"citylevel_bin")
n=unique(citylevel$linetype)

citylevel=data.frame(table(melt(data.frame(week=citylevel$week,citylevel_bin=citylevel$citylevel_bin),id=c("week","citylevel_bin")))) 
plot <- hPlot(Freq~week, data = citylevel,group = "citylevel_bin",type = "column",title=sprintf("%s大额--%s人群城市等级分布",input$line,n))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
})
##小额
output$plot9 <- renderChart2({

#citylevel=selectedData1()[selectedData1()$linetype=="大额主营",]
citylevel=dataChange(selectedData1(),2,input$line,c("大额主营","小额_m"),"citylevel_bin")
n=unique(citylevel$linetype)

citylevel=citylevel %>% group_by(week,citylevel_bin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = citylevel,group = "citylevel_bin",type = "column",title=sprintf("%s大额--%s人群城市等级分布",input$line,n))
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
           "mode type"   = which(names(selectedData2())=="chuomode"),
           "channel type"   = which(names(selectedData2())=="channel_total_category")
           )
  })
####模型bin
##大额渠道VS大额主营VS小额
output$plot13 <- renderChart2({
#bin=selectedData2()
bin=dataChange(selectedData2(),1,input$line1,c("大额渠道","大额_m"),"credit_bin")
bin=bin[!is.na(bin$credit_bin),]
bin$credit_bin[bin$credit_bin>=10]="十及以上"
bin$credit_bin=paste("bin",bin$credit_bin,sep="")
bin=data.frame(table(melt(data.frame(linetype=bin[,column2()],credit_bin=bin$credit_bin),id=c("linetype","credit_bin"))))
bin=cusChange1(input$line1,input$mode2,bin)

plot <- hPlot(Freq~linetype, data = bin,group = "credit_bin",type = "column",title="模型bin分布对比")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 
##大额
output$plot14 <- renderChart2({
#bin=selectedData2()[selectedData2()$linetype=="大额渠道",]
bin=dataChange(selectedData2(),2,input$line1,c("大额渠道","大额_m"),"credit_bin")
n=unique(bin$linetype)

bin=bin[!is.na(bin$credit_bin),]
bin$credit_bin[bin$credit_bin>=10]="十及以上"
bin$credit_bin=paste("bin",bin$credit_bin,sep="")
bin=data.frame(table(melt(data.frame(week=bin$week,credit_bin=bin$credit_bin),id=c("week","credit_bin")))) 
plot <- hPlot(Freq~week, data = bin,group = "credit_bin",type = "column",title=sprintf("%s大额--%s人群模型bin分布",input$line1,n))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 
##小额
output$plot15 <- renderChart2({
#bin=selectedData2()[selectedData2()$linetype=="大额主营",]
bin=dataChange(selectedData2(),2,input$line1,c("大额主营","小额_m"),"credit_bin")
n=unique(bin$linetype)

bin=bin[!is.na(bin$credit_bin),]
bin$credit_bin[bin$credit_bin>=10]="十及以上"
bin$credit_bin=paste("bin",bin$credit_bin,sep="")
bin=bin %>% group_by(week,credit_bin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = bin,group = "credit_bin",type = "column",title=sprintf("%s大额--%s人群模型bin分布",input$line1,n))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 


####repaybin
##大额渠道VS大额主营VS小额
output$plot13_repay <- renderChart2({
rbin=dataChange(selectedData2(),1,input$line1,c("大额渠道","大额_m"),"repaybin")
rbin=rbin[!is.na(rbin$repaybin)&rbin$repaybin!=0,]
rbin=data.frame(table(melt(data.frame(linetype=rbin[,column2()],repaybin=rbin$repaybin),id=c("linetype","repaybin"))))

rbin=cusChange1(input$line1,input$mode2,rbin)

plot <- hPlot(Freq~linetype, data = rbin,group = "repaybin",type = "column",title="repaybin分布对比")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 
##大额
output$plot14_repay <- renderChart2({
rbin=dataChange(selectedData2(),2,input$line1,c("大额渠道","大额_m"),"repaybin")
n=unique(rbin$linetype)

rbin=rbin[!is.na(rbin$repaybin)&rbin$repaybin!=0,]
rbin=data.frame(table(melt(data.frame(week=rbin$week,repaybin=rbin$repaybin),id=c("week","repaybin"))))
plot <- hPlot(Freq~week, data = rbin,group = "repaybin",type = "column",title=sprintf("%s大额--%s人群repaybin分布",input$line1,n))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 
##小额
output$plot15_repay <- renderChart2({
rbin=dataChange(selectedData2(),2,input$line1,c("大额主营","小额_m"),"repaybin")
n=unique(rbin$linetype)

rbin=rbin[!is.na(rbin$repaybin)&rbin$repaybin!=0,]
rbin=data.frame(table(melt(data.frame(week=rbin$week,repaybin=rbin$repaybin),id=c("week","repaybin"))))
plot <- hPlot(Freq~week, data = rbin,group = "repaybin",type = "column",title=sprintf("%s大额--%s人群repaybin分布",input$line1,n))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

####腾讯分
##大额渠道VS大额主营VS小额
output$plot16 <- renderChart2({
tc=dataChange(selectedData2(),1,input$line1,c("大额渠道","大额_m"),"risk_score")
tc=tc[!is.na(tc$risk_score),]
tc$tcbin=cut(tc$risk_score,breaks=c(0,20,40,60,80,100),include.lowest = TRUE,right = FALSE)
tc=data.frame(table(melt(data.frame(linetype=tc[,column2()],tcbin=tc$tcbin),id=c("linetype","tcbin"))))
tc=cusChange1(input$line1,input$mode2,tc)
plot <- hPlot(Freq~linetype, data = tc,group = "tcbin",type = "column",title="腾讯分分布对比")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 
##大额
output$plot17 <- renderChart2({
tc=dataChange(selectedData2(),2,input$line1,c("大额渠道","大额_m"),"risk_score")
n=unique(tc$linetype)

tc=tc[!is.na(tc$risk_score),]
tc$tcbin=cut(tc$risk_score,breaks=c(0,20,40,60,80,100),include.lowest = TRUE,right = FALSE)
tc=data.frame(table(melt(data.frame(week=tc$week,tcbin=tc$tcbin),id=c("week","tcbin")))) 
plot <- hPlot(Freq~week, data = tc,group = "tcbin",type = "column",title=paste(input$line1,"大额--",n,"人群腾讯分分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 
##小额
output$plot18 <- renderChart2({
tc=dataChange(selectedData2(),2,input$line1,c("大额主营","小额_m"),"risk_score")
n=unique(tc$linetype)

tc=tc[!is.na(tc$risk_score),]
tc$tcbin=cut(tc$risk_score,breaks=c(0,20,40,60,80,100),include.lowest = TRUE,right = FALSE)
tc=tc %>% group_by(week,tcbin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = tc,group = "tcbin",type = "column",title=paste(input$line1,"大额--",n,"人群腾讯分分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

####友盟分
##大额渠道VS大额主营VS小额
output$plot19 <- renderChart2({
um=dataChange(selectedData2(),1,input$line1,c("大额渠道","大额_m"),"umeng_score")

um=um[!is.na(um$umeng_score),]
um$umbin=cut(um$umeng_score,breaks=c(300,400,500,600,700,850))
um=data.frame(table(melt(data.frame(linetype=um[,column2()],umbin=um$umbin),id=c("linetype","umbin"))))
um=cusChange1(input$line1,input$mode2,um)

plot <- hPlot(Freq~linetype, data = um,group = "umbin",type = "column",title="友盟分分布对比")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
}) 
##大额
output$plot20 <- renderChart2({
um=dataChange(selectedData2(),2,input$line1,c("大额渠道","大额_m"),"umeng_score")
n=unique(um$linetype)

um=um[!is.na(um$umeng_score),]
um$umbin=cut(um$umeng_score,breaks=c(300,400,500,600,700,850))
um=data.frame(table(melt(data.frame(week=um$week,umbin=um$umbin),id=c("week","umbin")))) 
plot <- hPlot(Freq~week, data = um,group = "umbin",type = "column",title=paste(input$line1,"大额--",n,"人群友盟分分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
}) 
##小额
output$plot21 <- renderChart2({
um=dataChange(selectedData2(),2,input$line1,c("大额主营","小额_m"),"umeng_score")
n=unique(um$linetype)

um=um[!is.na(um$umeng_score),]
um$umbin=cut(um$umeng_score,breaks=c(300,400,500,600,700,850))
um=um %>% group_by(week,umbin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = um,group = "umbin",type = "column",title=paste(input$line1,"大额--",n,"人群友盟分分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
}) 

####京东分
##大额渠道VS大额主营VS小额
output$plot22 <- renderChart2({
jd=dataChange(selectedData2(),1,input$line1,c("大额渠道","大额_m"),"jdcredit_score")
jd=jd[!is.na(jd$jdcredit_score),]
jd$jdbin=cut(jd$jdcredit_score,breaks=c(400,550,620,650,700,850))
jd=data.frame(table(melt(data.frame(linetype=jd[,column2()],jdbin=jd$jdbin),id=c("linetype","jdbin"))))
jd=cusChange1(input$line1,input$mode2,jd)
plot <- hPlot(Freq~linetype, data = jd,group = "jdbin",type = "column",title="京东分分布对比")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
}) 
##大额
output$plot23 <- renderChart2({
jd=dataChange(selectedData2(),2,input$line1,c("大额渠道","大额_m"),"jdcredit_score")
n=unique(jd$linetype)

jd=jd[!is.na(jd$jdcredit_score),]
jd$jdbin=cut(jd$jdcredit_score,breaks=c(400,550,620,650,700,850))
jd=data.frame(table(melt(data.frame(week=jd$week,jdbin=jd$jdbin),id=c("week","jdbin"))))  
plot <- hPlot(Freq~week, data = jd,group = "jdbin",type = "column",title=paste(input$line1,"大额--",n,"人群京东分分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
}) 
##小额
output$plot24 <- renderChart2({
jd=dataChange(selectedData2(),2,input$line1,c("大额主营","小额_m"),"jdcredit_score")
n=unique(jd$linetype)
jd=jd[!is.na(jd$jdcredit_score),]
jd$jdbin=cut(jd$jdcredit_score,breaks=c(400,550,620,650,700,850))
jd=jd %>% group_by(week,jdbin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = jd,group = "jdbin",type = "column",title=paste(input$line1,"大额--",n,"人群京东分分布",sep=""))
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
           "mode type"   = which(names(selectedData3())=="chuomode"),
           "channel type"   = which(names(selectedData3())=="channel_total_category")
           )
  })
####税前收入
##大额渠道VS大额主营VS小额
output$plot25 <- renderChart2({
pre=dataChange(selectedData3(),1,input$line2,c("大额渠道","大额_m"),"pretax")
pre=pre[!is.na(pre$pretax),]
pre=data.frame(table(melt(data.frame(linetype=pre[,column3()],pretax=pre$pretax),id=c("linetype","pretax"))))
pre=cusChange1(input$line2,input$mode3,pre)
plot <- hPlot(Freq~linetype, data = pre,group = "pretax",type = "column",title="税前收入分布")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
}) 
##大额
output$plot26 <- renderChart2({
pre=dataChange(selectedData3(),2,input$line2,c("大额渠道","大额_m"),"pretax")
n=unique(pre$linetype)
pre=pre[!is.na(pre$pretax),]
pre=data.frame(table(melt(data.frame(week=pre$week,pretax=pre$pretax),id=c("week","pretax"))))  
plot <- hPlot(Freq~week, data = pre,group = "pretax",type = "column",title=sprintf("%s大额--%s人群税前收入分布",input$line2,n))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
}) 
##小额
output$plot27 <- renderChart2({
pre=dataChange(selectedData3(),2,input$line2,c("大额主营","小额_m"),"pretax")
n=unique(pre$linetype)

pre=pre[!is.na(pre$pretax),]
pre=pre %>% group_by(week,pretax) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = pre,group = "pretax",type = "column",title=sprintf("%s大额--%s人群税前收入分布",input$line2,n))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
}) 


##################多头与不良

  selectedData4 <- reactive({
  duotou[as.Date(duotou$firstchuo) >= min(input$dates5) & as.Date(duotou$firstchuo) <= max(input$dates5),]
  })

  column4 <- reactive({
    switch(input$mode4,
           "line type" = which(names(selectedData4())=="linetype"),
           "mode type"   = which(names(selectedData4())=="chuomode"),
           "channel type"   = which(names(selectedData4())=="channel_total_category")
           )
  })
####借款APP数
##大额渠道VS大额主营VS小额
output$plot28 <- renderChart2({
bo=dataChange(selectedData4(),1,input$line3,c("大额渠道","大额_m"),"boappnum")
bo=bo[!is.na(bo$boappnum),]
bo$bobin=as.character(cut(bo$boappnum,breaks=c(0,1,2,5,10,20,500),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,2)"]="[01,02)"
bo$bobin[bo$bobin=="[2,5)"]="[02,05)"
bo$bobin[bo$bobin=="[5,10)"]="[05,10)"
bo=data.frame(table(melt(data.frame(linetype=bo[,column4()],bobin=bo$bobin),id=c("linetype","bobin"))))
bo=cusChange1(input$line3,input$mode4,bo)
plot <- hPlot(Freq~linetype, data = bo,group = "bobin",type = "column",title="借款APP数分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额渠道
output$plot29 <- renderChart2({
bo=dataChange(selectedData4(),2,input$line3,c("大额渠道","大额_m"),"boappnum")
n=unique(bo$linetype)

bo=bo[!is.na(bo$boappnum),]
bo$bobin=as.character(cut(bo$boappnum,breaks=c(0,1,2,5,10,20,500),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,2)"]="[01,02)"
bo$bobin[bo$bobin=="[2,5)"]="[02,05)"
bo$bobin[bo$bobin=="[5,10)"]="[05,10)"
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title=paste(input$line3,"大额--",n,"人群借款app数分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额主营
output$plot30 <- renderChart2({
bo=dataChange(selectedData4(),2,input$line3,c("大额主营","小额_m"),"boappnum")
n=unique(bo$linetype)

bo=bo[!is.na(bo$boappnum),]
bo$bobin=as.character(cut(bo$boappnum,breaks=c(0,1,2,5,10,20,500),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,2)"]="[01,02)"
bo$bobin[bo$bobin=="[2,5)"]="[02,05)"
bo$bobin[bo$bobin=="[5,10)"]="[05,10)"
bo=bo %>% group_by(week,bobin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = bo,group = "bobin",type = "column",title=paste(input$line3,"大额--",n,"人群借款app数分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

####同盾1个月内平台查询次数
##大额渠道VS大额主营VS小额
output$plot31 <- renderChart2({
td=dataChange(selectedData4(),1,input$line3,c("大额渠道","大额_m"),"td_1m")
td=td[!is.na(td$td_1m),]
td$tdbin=as.character(cut(td$td_1m,breaks=c(0,1,2,5,10,20,50,100),include.lowest = TRUE,right = FALSE))
td$tdbin[td$tdbin=="[0,1)"]="[0,01)"
td$tdbin[td$tdbin=="[1,2)"]="[01,02)"
td$tdbin[td$tdbin=="[2,5)"]="[02,05)"
td$tdbin[td$tdbin=="[5,10)"]="[05,10)"
td=data.frame(table(melt(data.frame(linetype=td[,column4()],tdbin=td$tdbin),id=c("linetype","tdbin"))))
td=cusChange1(input$line3,input$mode4,td)
    
plot <- hPlot(Freq~linetype, data = td,group = "tdbin",type = "column",title="同盾一月内查询数分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额渠道
output$plot32 <- renderChart2({
td=dataChange(selectedData4(),2,input$line3,c("大额渠道","大额_m"),"td_1m")
n=unique(td$linetype)
td$tdbin=as.character(cut(td$td_1m,breaks=c(0,1,2,5,10,20,50,100),include.lowest = TRUE,right = FALSE))
td$tdbin[td$tdbin=="[0,1)"]="[0,01)"
td$tdbin[td$tdbin=="[1,2)"]="[01,02)"
td$tdbin[td$tdbin=="[2,5)"]="[02,05)"
td$tdbin[td$tdbin=="[5,10)"]="[05,10)"
td=data.frame(table(melt(data.frame(week=td$week,tdbin=td$tdbin),id=c("week","tdbin"))))   
plot <- hPlot(Freq~week, data = td,group = "tdbin",type = "column",title=paste(input$line3,"大额--",n,"人群同盾一月查询数分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额主营
output$plot33 <- renderChart2({
td=dataChange(selectedData4(),2,input$line3,c("大额主营","小额_m"),"td_1m")
n=unique(td$linetype)
td$tdbin=as.character(cut(td$td_1m,breaks=c(0,1,2,5,10,20,50,100),include.lowest = TRUE,right = FALSE))
td$tdbin[td$tdbin=="[0,1)"]="[0,01)"
td$tdbin[td$tdbin=="[1,2)"]="[01,02)"
td$tdbin[td$tdbin=="[2,5)"]="[02,05)"
td$tdbin[td$tdbin=="[5,10)"]="[05,10)"
td=td %>% group_by(week,tdbin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = td,group = "tdbin",type = "column",title=paste(input$line3,"大额--",n,"人群同盾一月查询数分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

####同盾3个月内平台查询次数
##大额渠道VS大额主营VS小额
output$plot34 <- renderChart2({
td=dataChange(selectedData4(),1,input$line3,c("大额渠道","大额_m"),"td_3m")
td=td[!is.na(td$td_3m),]
td$tdbin=as.character(cut(td$td_3m,breaks=c(0,1,2,5,10,20,50,100),include.lowest = TRUE,right = FALSE))
td$tdbin[td$tdbin=="[0,1)"]="[0,01)"
td$tdbin[td$tdbin=="[1,2)"]="[01,02)"
td$tdbin[td$tdbin=="[2,5)"]="[02,05)"
td$tdbin[td$tdbin=="[5,10)"]="[05,10)"
td=data.frame(table(melt(data.frame(linetype=td[,column4()],tdbin=td$tdbin),id=c("linetype","tdbin")))) 
td=cusChange1(input$line3,input$mode4,td)

   
plot <- hPlot(Freq~linetype, data = td,group = "tdbin",type = "column",title="同盾三月内查询数分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额渠道
output$plot35 <- renderChart2({
td=dataChange(selectedData4(),2,input$line3,c("大额渠道","大额_m"),"td_3m")
n=unique(td$linetype)
td=td[!is.na(td$td_3m),]
td$tdbin=as.character(cut(td$td_3m,breaks=c(0,1,2,5,10,20,50,100),include.lowest = TRUE,right = FALSE))
td$tdbin[td$tdbin=="[0,1)"]="[0,01)"
td$tdbin[td$tdbin=="[1,2)"]="[01,02)"
td$tdbin[td$tdbin=="[2,5)"]="[02,05)"
td$tdbin[td$tdbin=="[5,10)"]="[05,10)"
td=data.frame(table(melt(data.frame(week=td$week,tdbin=td$tdbin),id=c("week","tdbin"))))    
plot <- hPlot(Freq~week, data = td,group = "tdbin",type = "column",title=paste(input$line3,"大额--",n,"人群同盾三月查询数分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额主营
output$plot36 <- renderChart2({
td=dataChange(selectedData4(),2,input$line3,c("大额主营","小额_m"),"td_3m")
n=unique(td$linetype)

td=td[!is.na(td$td_3m),]
td$tdbin=as.character(cut(td$td_3m,breaks=c(0,1,2,5,10,20,50,100),include.lowest = TRUE,right = FALSE))
td$tdbin[td$tdbin=="[0,1)"]="[0,01)"
td$tdbin[td$tdbin=="[1,2)"]="[01,02)"
td$tdbin[td$tdbin=="[2,5)"]="[02,05)"
td$tdbin[td$tdbin=="[5,10)"]="[05,10)"
td=td %>% group_by(week,tdbin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = td,group = "tdbin",type = "column",title=paste(input$line3,"大额--",n,"人群同盾三月查询数分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

####同盾分
##大额渠道VS大额主营VS小额
output$plot37 <- renderChart2({
td=dataChange(selectedData4(),1,input$line3,c("大额渠道","大额_m"),"final_score")
td=td[!is.na(td$final_score),]
td$tdbin=as.character(cut(td$final_score,breaks=c(0,20,80,100),include.lowest = TRUE,right = FALSE))
td=data.frame(table(melt(data.frame(linetype=td[,column4()],tdbin=td$tdbin),id=c("linetype","tdbin")))) 
td=cusChange1(input$line3,input$mode4,td)
 
plot <- hPlot(Freq~linetype, data = td,group = "tdbin",type = "column",title="同盾分分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额渠道
output$plot38 <- renderChart2({
td=dataChange(selectedData4(),2,input$line3,c("大额渠道","大额_m"),"final_score")
n=unique(td$linetype)
td=td[!is.na(td$final_score),]
td$tdbin=as.character(cut(td$final_score,breaks=c(0,20,80,100),include.lowest = TRUE,right = FALSE))
td=data.frame(table(melt(data.frame(week=td$week,tdbin=td$tdbin),id=c("week","tdbin"))))    
plot <- hPlot(Freq~week, data = td,group = "tdbin",type = "column",title=paste(input$line3,"大额--",n,"人群同盾分分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额主营
output$plot39 <- renderChart2({
td=dataChange(selectedData4(),2,input$line3,c("大额主营","小额_m"),"final_score")
n=unique(td$linetype)
td=td[!is.na(td$final_score),]
td$tdbin=as.character(cut(td$final_score,breaks=c(0,20,80,100),include.lowest = TRUE,right = FALSE))
td=td %>% group_by(week,tdbin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = td,group = "tdbin",type = "column",title=paste(input$line3,"大额--",n,"人群同盾分分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

####逾期短信数
##大额渠道VS大额主营VS小额
output$plot40 <- renderChart2({
bo=dataChange(selectedData4(),1,input$line3,c("大额渠道","大额_m"),"message_count_default")
bo=bo[!is.na(bo$message_count_default)&bo$message_count_default!=-1,]
bo$bobin=as.character(cut(bo$message_count_default,breaks=c(0,1,5,10,20,500),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,5)"]="[01,05)"
bo$bobin[bo$bobin=="[5,10)"]="[05,10)"
bo=data.frame(table(melt(data.frame(linetype=bo[,column4()],bobin=bo$bobin),id=c("linetype","bobin"))))
bo=cusChange1(input$line3,input$mode4,bo)

plot <- hPlot(Freq~linetype, data = bo,group = "bobin",type = "column",title="逾期短信数分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额渠道
output$plot41 <- renderChart2({
bo=dataChange(selectedData4(),2,input$line3,c("大额渠道","大额_m"),"message_count_default")
n=unique(bo$linetype)
bo=bo[!is.na(bo$message_count_default)&bo$message_count_default!=-1,]
bo$bobin=as.character(cut(bo$message_count_default,breaks=c(0,1,5,10,20,500),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,5)"]="[01,05)"
bo$bobin[bo$bobin=="[5,10)"]="[05,10)"
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title=paste(input$line3,"大额--",n,"人群逾期短信数分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
}) 

##大额主营
output$plot42 <- renderChart2({
bo=dataChange(selectedData4(),2,input$line3,c("大额主营","小额_m"),"message_count_default")
n=unique(bo$linetype)
bo=bo[!is.na(bo$message_count_default)&bo$message_count_default!=-1,]
bo$bobin=as.character(cut(bo$message_count_default,breaks=c(0,1,5,10,20,500),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,5)"]="[01,05)"
bo$bobin[bo$bobin=="[5,10)"]="[05,10)"
bo=bo %>% group_by(week,bobin) %>% summarise(num=n()) 
plot <- hPlot(num~week, data = bo,group = "bobin",type = "column",title=paste(input$line3,"大额--",n,"人群逾期短信数分布",sep=""))
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
           "mode type"   = which(names(selectedData5())=="chuomode"),
           "channel type"   = which(names(selectedData5())=="channel_total_category")
           )
  })
####最大信用卡额度
##大额渠道VS大额主营
output$plot43 <- renderChart2({
options(scipen = 200) ##取消科学计数法
bo=dataChange(selectedData5(),1,input$line4,c("大额渠道","大额_m"),"cmax")
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
bo=data.frame(table(melt(data.frame(linetype=bo[,column5()],bobin=bo$bobin),id=c("linetype","bobin"))))
bo=cusChange1(input$line4,input$mode5,bo)
plot <- hPlot(Freq~linetype, data = bo,group = "bobin",type = "column",title="最高信用卡额度分布")
plot$plotOptions(column = list(stacking = "percent"))
#plot$yAxis(reversedStacks = FALSE)
return(plot)    
})
 
##大额渠道
output$plot44 <- renderChart2({
options(scipen = 200) ##取消科学计数法
bo=dataChange(selectedData5(),2,input$line4,c("大额渠道","大额_m"),"cmax")
n=unique(bo$linetype)
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
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title=paste(input$line4,"大额--",n,"人群最高信用卡额度分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})

##大额主营
output$plot45 <- renderChart2({
options(scipen = 200) ##取消科学计数法
bo=dataChange(selectedData5(),2,input$line4,c("大额主营","小额_m"),"cmax")
n=unique(bo$linetype)
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
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title=paste(input$line4,"大额--",n,"人群最高信用卡额度分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})

####其他贷款最大额度
##大额渠道VS大额主营
output$plot46 <- renderChart2({
bo=dataChange(selectedData5(),1,input$line4,c("大额渠道","大额_m"),"omax")
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
bo=data.frame(table(melt(data.frame(linetype=bo[,column5()],bobin=bo$bobin),id=c("linetype","bobin"))))
bo=cusChange1(input$line4,input$mode5,bo)
plot <- hPlot(Freq~linetype, data = bo,group = "bobin",type = "column",title="其他贷款最大额度分布")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})
 
##大额渠道
output$plot47 <- renderChart2({
bo=dataChange(selectedData5(),2,input$line4,c("大额渠道","大额_m"),"omax")
n=unique(bo$linetype)
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
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title=paste(input$line4,"大额--",n,"人群其他贷款最大额度分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})

##大额主营
output$plot48 <- renderChart2({
bo=dataChange(selectedData5(),2,input$line4,c("大额主营","小额_m"),"omax")
n=unique(bo$linetype)
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
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title=paste(input$line4,"大额--",n,"人群其他贷款最大额度分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})

####有效信用卡张数
##大额渠道VS大额主营
output$plot49 <- renderChart2({
bo=dataChange(selectedData5(),1,input$line4,c("大额渠道","大额_m"),"vcard")
bo$vcard=as.numeric(bo$vcard)
bo=bo[!is.na(bo$vcard)&bo$vcard!=-1,]
bo$vcard=as.numeric(bo$vcard)
bo$bobin=as.character(cut(bo$vcard,breaks=c(0,1,3,6,10,20,80),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,3)"]="[01,03)"
bo$bobin[bo$bobin=="[3,6)"]="[03,06)"
bo$bobin[bo$bobin=="[6,10)"]="[06,10)"
bo=data.frame(table(melt(data.frame(linetype=bo[,column5()],bobin=bo$bobin),id=c("linetype","bobin"))))
bo=cusChange1(input$line4,input$mode5,bo)
 
plot <- hPlot(Freq~linetype, data = bo,group = "bobin",type = "column",title="有效信用卡张数分布")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})
 
##大额渠道
output$plot50 <- renderChart2({
bo=dataChange(selectedData5(),2,input$line4,c("大额渠道","大额_m"),"vcard")
n=unique(bo$linetype)
bo=bo[!is.na(bo$vcard)&bo$vcard!=-1,]
bo$vcard=as.numeric(bo$vcard)
bo$bobin=as.character(cut(bo$vcard,breaks=c(0,1,3,6,10,20,80),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,3)"]="[01,03)"
bo$bobin[bo$bobin=="[3,6)"]="[03,06)"
bo$bobin[bo$bobin=="[6,10)"]="[06,10)"
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title=paste(input$line4,"大额--",n,"人群有效信用卡张数分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})

##大额主营
output$plot51 <- renderChart2({
bo=dataChange(selectedData5(),2,input$line4,c("大额主营","小额_m"),"vcard")
n=unique(bo$linetype)
bo=bo[!is.na(bo$vcard)&bo$vcard!=-1,]
bo$vcard=as.numeric(bo$vcard)
bo$bobin=as.character(cut(bo$vcard,breaks=c(0,1,3,6,10,20,80),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1)"]="[0,01)"
bo$bobin[bo$bobin=="[1,3)"]="[01,03)"
bo$bobin[bo$bobin=="[3,6)"]="[03,06)"
bo$bobin[bo$bobin=="[6,10)"]="[06,10)"
bo=data.frame(table(melt(data.frame(week=bo$week,bobin=bo$bobin),id=c("week","bobin"))))   
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title=paste(input$line4,"大额--",n,"人群有效信用卡张数分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})


##################用户负债

  selectedData6 <- reactive({
  owing[as.Date(owing$firstchuo) >= min(input$dates7) & as.Date(owing$firstchuo) <= max(input$dates7),]
  })

  column6 <- reactive({
    switch(input$mode6,
           "line type" = which(names(selectedData6())=="linetype"),
           "mode type"   = which(names(selectedData6())=="chuomode"),
           "channel type"   = which(names(selectedData6())=="channel_total_category")
           )
  })
  
####其他贷款月还
##大额渠道VS大额主营
output$plot52 <- renderChart2({
bo=dataChange(selectedData6(),1,input$line5,c("大额渠道","大额_m"),"rpo")
bo=bo[!is.na(bo$rpo)&bo$rpo!=-1,]
bo$rpo=as.numeric(bo$rpo)
bo$bobin=as.character(cut(bo$rpo,breaks=c(0,1000,3000,6000,10000,20000,500000),include.lowest = TRUE,right = FALSE))
bo$bobin[bo$bobin=="[0,1e+03)"]="[0,0.1w)"
bo$bobin[bo$bobin=="[1e+03,3e+03)"]="[0.1w,0.3w)"
bo$bobin[bo$bobin=="[3e+03,6e+03)"]="[0.3w,0.6w)"
bo$bobin[bo$bobin=="[6e+03,1e+04)"]="[0.6,1w)"
bo$bobin[bo$bobin=="[1e+04,2e+04)"]="[1w,2w)"
bo$bobin[bo$bobin=="[2e+04,5e+05]"]="2w以上"
bo=data.frame(table(melt(data.frame(linetype=bo[,column6()],bobin=bo$bobin),id=c("linetype","bobin"))))
bo=cusChange1(input$line5,input$mode6,bo)
   
plot <- hPlot(Freq~linetype, data = bo,group = "bobin",type = "column",title="其他贷款月还分布")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})
 
##大额渠道
output$plot53 <- renderChart2({
bo=dataChange(selectedData6(),2,input$line5,c("大额渠道","大额_m"),"rpo")
n=unique(bo$linetype)

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
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title=paste(input$line5,"大额--",n,"人群其他贷款月还分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})

##大额主营
output$plot54 <- renderChart2({
bo=dataChange(selectedData6(),2,input$line5,c("大额主营","小额_m"),"rpo")
n=unique(bo$linetype)


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
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title=paste(input$line5,"大额--",n,"人群其他贷款月还分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})

####信用卡月还
##大额渠道VS大额主营
output$plot55 <- renderChart2({
bo=dataChange(selectedData6(),1,input$line5,c("大额渠道","大额_m"),"rpc")
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
bo=cusChange1(input$line5,input$mode6,bo)
plot <- hPlot(Freq~linetype, data = bo,group = "bobin",type = "column",title="信用卡月还分布")
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})
 
##大额渠道
output$plot56 <- renderChart2({
bo=dataChange(selectedData6(),2,input$line5,c("大额渠道","大额_m"),"rpc")
n=unique(bo$linetype)


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
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title=paste(input$line5,"大额--",n,"人群信用卡月还分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})

##大额主营
output$plot57 <- renderChart2({
bo=dataChange(selectedData6(),2,input$line5,c("大额主营","小额_m"),"rpc")
n=unique(bo$linetype)

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
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title=paste(input$line5,"大额--",n,"人群信用卡月还分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
return(plot)    
})

####房贷月还
##大额渠道VS大额主营
output$plot58 <- renderChart2({
bo=dataChange(selectedData6(),1,input$line5,c("大额渠道","大额_m"),"rph")
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
bo=cusChange1(input$line5,input$mode6,bo)
   
plot <- hPlot(Freq~linetype, data = bo,group = "bobin",type = "column",title="房贷月还分布")
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
})
 
##大额渠道
output$plot59 <- renderChart2({
bo=dataChange(selectedData6(),2,input$line5,c("大额渠道","大额_m"),"rph")
n=unique(bo$linetype)

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
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title=paste(input$line5,"大额--",n,"人群房贷月还分布",sep=""))
plot$plotOptions(column = list(stacking = "percent"))
plot$yAxis(reversedStacks = FALSE)
return(plot)    
})

##大额主营
output$plot60 <- renderChart2({
bo=dataChange(selectedData6(),2,input$line5,c("大额主营","小额_m"),"rph")
n=unique(bo$linetype)

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
plot <- hPlot(Freq~week, data = bo,group = "bobin",type = "column",title=paste(input$line5,"大额--",n,"人群房贷月还分布",sep=""))
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

bo=selectedData7()[selectedData7()$qudao_type=="APP",]
app=bo %>% group_by(sourcename) %>% 
summarise(num=n(),chuonum=sum(chuo_status),younum=sum(youe_status),
chuoratio=round(sum(chuo_status)/n(),2),
youratio=round(ifelse(sum(chuo_status)>0,sum(youe_status)/sum(chuo_status),0),2),
fbratio=round(ifelse(sum(youe_status)>0,sum(fb_status)/sum(youe_status),0),2),
zhratio=round(sum(cj_status)/n(),2),
allzhratio=round(sum(allcj_status)/n(),2)) %>%
subset(num>66)
names(app)=c("app渠道名称","登录大额人数","戳额数","有额数","戳额率","有额率","确认率","大额转化率","整体转化率")
datatable(app,caption = 'APP渠道投放转化数据')
})
output$rate2 = renderDataTable({

bo=selectedData7()[selectedData7()$qudao_type=="M",]
m=bo %>% group_by(sourcename) %>% 
summarise(num=n(),chuonum=sum(chuo_status),younum=sum(youe_status),
chuoratio=round(sum(chuo_status)/n(),2),
youratio=round(ifelse(sum(chuo_status)>0,sum(youe_status)/sum(chuo_status),0),2),
fbratio=round(ifelse(sum(youe_status)>0,sum(fb_status)/sum(youe_status),0),2),
zhratio=round(sum(cj_status)/n(),2),
allzhratio=round(sum(allcj_status)/n(),2)) %>%
subset(num>66)
names(m)=c("app渠道名称","登录大额人数","戳额数","有额数","戳额率","有额率","确认率","大额转化率","整体转化率")
#return(m)
datatable(m,caption = 'M站渠道投放转化数据')

})

output$rate3 = renderDataTable({

bo=selectedData7()[selectedData7()$qudao_type=="APP",]
bo$channel_category[is.na(bo$channel_category)]="其他大额渠道类型"
m=bo %>% group_by(channel_category) %>% 
summarise(num=n(),chuonum=sum(chuo_status),younum=sum(youe_status),
chuoratio=round(sum(chuo_status)/n(),2),
youratio=round(ifelse(sum(chuo_status)>0,sum(youe_status)/sum(chuo_status),0),2),
fbratio=round(ifelse(sum(youe_status)>0,sum(fb_status)/sum(youe_status),0),2),
zhratio=round(sum(cj_status)/n(),2),
allzhratio=round(sum(allcj_status)/n(),2)) 
names(m)=c("渠道类型","登录大额人数","戳额数","有额数","戳额率","有额率","确认率","大额转化率","整体转化率")
#return(m)
datatable(m,caption = '各类型渠道在超级APP上的转化数据')

})

output$rate5 = renderDataTable({

bo=selectedData7()[selectedData7()$qudao_type=="M",]
bo$channel_category[is.na(bo$channel_category)]="其他大额渠道类型"
m=bo %>% group_by(channel_category) %>% 
summarise(num=n(),chuonum=sum(chuo_status),younum=sum(youe_status),
chuoratio=round(sum(chuo_status)/n(),2),
youratio=round(ifelse(sum(chuo_status)>0,sum(youe_status)/sum(chuo_status),0),2),
fbratio=round(ifelse(sum(youe_status)>0,sum(fb_status)/sum(youe_status),0),2),
zhratio=round(sum(cj_status)/n(),2),
allzhratio=round(sum(allcj_status)/n(),2)) 
names(m)=c("渠道类型","登录大额人数","戳额数","有额数","戳额率","有额率","确认率","大额转化率","整体转化率")
#return(m)
datatable(m,caption = '各类型渠道在M站的转化数据')

})





####app渠道

  selectedData8 <- reactive({
  channeleva[as.Date(channeleva$firstchuo) >= min(input$dates9) & as.Date(channeleva$firstchuo) <= max(input$dates9)&channeleva$qudao_type==input$line7,]
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
    ceshi1=ceshi[ceshi$qudao_type==input$line7,]
    switch(input$basic,
           "edu" = ceshi1$sourcename[order(ceshi1$edu,decreasing=TRUE)],
           "usertype"   = ceshi1$sourcename[order(ceshi1$usertype,decreasing=TRUE)],
           "citylevel"  = ceshi1$sourcename[order(ceshi1$citylevel,decreasing=TRUE)]
           )
  })
  
output$rate4 = renderDataTable({
score=score[score$qudao_type==input$line7,]

score=score %>% group_by(sourcename) %>%
      summarise(
      chuonum=sum(num),
      basicinfo=edu+city+usertype,
      model=(bin*2+rbin*2+tengxun*1+umeng*1+jd)/7,
      asset=salary+max_creditcard+max_otherloan,
      multiloan=boapp+tongdun+zx_query,
      ovd=ovd_msg+overdue_zx )

score_mat=data.frame(sourcename=score$sourcename,chuonum=score$chuonum,basicinfo=rank(score$basicinfo),model=rank(score$model),asset=rank(score$asset),multiloan=rank(score$multiloan),ovd=rank(score$ovd))
score_mat$total=round((score_mat$basicinfo+score_mat$model*3+score_mat$asset*2+score_mat$multiloan*2+score_mat$ovd*2))
names(score_mat)=c("渠道名称","戳额人数","基本信息维度得分","模型维度","资产信息维度","多头信息维度","逾期信息维度","综合得分")
#return(m)
datatable(score_mat,caption = '渠道各维度评分展示')

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
           "repaybin" = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$repaybin),
           "tengxun"   = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$tcbin),
           "jd"  = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$jdbin),
           "umeng"  = data.frame(sourcename=selectedData8()$sourcename,edu=selectedData8()$umbin)
           )
  })
  
  selectedorder1 <- reactive({
    ceshi1=ceshi[ceshi$qudao_type==input$line7,]

    switch(input$basic1,
           "bin" = ceshi1$sourcename[order(ceshi1$bin,decreasing=TRUE)],
           "repaybin" = ceshi1$sourcename[order(ceshi1$rbin,decreasing=TRUE)],
           "tengxun"   = ceshi1$sourcename[order(ceshi1$tc,decreasing=TRUE)],
           "jd"  = ceshi1$sourcename[order(ceshi1$jd,decreasing=TRUE)],
           "umeng"  = ceshi1$sourcename[order(ceshi1$um,decreasing=TRUE)]
           )
  })
    
output$plot62 <- renderChart2({
bo=data.frame(table(melt(selectedcolumn1(),id=c("sourcename","edu"))))
bo$sourcename <- factor(bo$sourcename,levels=selectedorder1())
plot <- hPlot(Freq~sourcename, data = bo,group = "edu",type = "column",title=sprintf("大额APP渠道%s分布",input$basic1))
plot$plotOptions(column = list(stacking = "percent"))
if(input$basic1 %in% c("bin","repaybin","tengxun")){
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
    ceshi1=ceshi[ceshi$qudao_type==input$line7,]
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
    ceshi1=ceshi[ceshi$qudao_type==input$line7,]
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
    ceshi1=ceshi[ceshi$qudao_type==input$line7,]
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
      score=score[score$qudao_type==input$line7,]
      mm1=subset(score,select=sourcename:usertype)
      mm1=mm1[mm1$sourcename %in% input$spider,] %>% mutate_at(vars(edu:usertype),funs(rescale))
      rada=ggradar(mm1,axis.label.size = 5,legend.text.size = 16)
  return(rada)  
})

output$sp2<-renderPlot({ 
      score=score[score$qudao_type==input$line7,]
      mm2=score[,c(1,5:9)]
      mm2=mm2[mm2$sourcename %in% input$spider,] %>% mutate_at(vars(bin:jd),funs(rescale))
      rada=ggradar(mm2,axis.label.size = 5,legend.text.size = 16)
  return(rada)  
})

output$sp3<-renderPlot({ 
      score=score[score$qudao_type==input$line7,]
      mm3=score[,c(1,10:12)]
      mm3=mm3[mm3$sourcename %in% input$spider,] %>% mutate_at(vars(max_creditcard:salary),funs(rescale))
      rada=ggradar(mm3,axis.label.size = 5,legend.text.size = 16)
  return(rada)  
})

output$sp4<-renderPlot({
      score=score[score$qudao_type==input$line7,]
      mm4=score[,c(1,13:15)]
      mm4=mm4[mm4$sourcename %in% input$spider,] %>% mutate_at(vars(boapp:zx_query),funs(rescale))
      rada=ggradar(mm4,axis.label.size = 5,legend.text.size = 16)
  return(rada)  
})

output$sp5<-renderPlot({ 
      score=score[score$qudao_type==input$line7,]
      mm5=score[,c(1,16:17)]
      mm5=mm5[mm5$sourcename %in% input$spider,] %>% mutate_at(vars(ovd_msg:overdue_zx),funs(rescale))
      rada=ggradar(mm5,axis.label.size = 5,legend.text.size = 16)
  return(rada)  
})
######
output$sp6<-renderPlot({ 
      score=score[score$qudao_type==input$line7,]
      mm6=score %>% group_by(sourcename) %>%
      summarise(basicinfo=edu+city+usertype,
      model=(bin*2+rbin*2+tengxun*1+umeng*1+jd)/7,
      asset=salary+max_creditcard+max_otherloan,
      multiloan=boapp+tongdun+zx_query,
      ovd=ovd_msg+overdue_zx )
      
      
      
      mm6=mm6[mm6$sourcename %in% input$spider,] %>% mutate_at(vars(basicinfo:ovd),funs(rescale))
      rada=ggradar(mm6,axis.label.size = 7,legend.text.size = 16)
  return(rada)  
})

####################################################################################
})