options(shiny.sanitize.errors = FALSE)
library(shiny)
library(DT)
#library(praise)
library(dplyr)
library(shinydashboard)
library(rCharts)
library(formattable)
library(markdown)
####
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

channeleva=read.table("channeleva.txt",header = TRUE,sep="",fileEncoding="UTF-8",row.names = NULL) ###正确
channeleva$firstchuo=as.character(channeleva$firstchuo)

dx1=channeleva[channeleva$qudao_type=="APP",] %>% group_by(sourcename) %>% 
summarise(chuonum=n())

dx2=channeleva[channeleva$qudao_type=="M",] %>% group_by(sourcename) %>% 
summarise(chuonum=n())



####


dashboardPage(
 dashboardHeader(title="流量监控"),##标题
 dashboardSidebar(
    #selectInput("xcol","X Variable",names(iris)),
    #selectInput("ycol","Y Variable",names(iris),selected="Sepal.Width"),
    #numericInput("clusters","Cluster count",3,min=2,max=9),
    #a(img(src="logo.png",height=60,width=200),
    #href="https://www.hellobi.com/event/137",target="black")
     sidebarMenu(
        menuItem("大额基本数据", tabName = "大额基本数据", icon = icon("dashboard")),

        menuItem("人群画像", tabName = "人群画像", icon = icon("dashboard"),startExpanded = TRUE,
        menuSubItem("基本信息", tabName = "基本信息"),
        menuSubItem("模型类评分", tabName = "模型类评分"),
        menuSubItem("收入", tabName = "收入"),
        menuSubItem("用户资质", tabName = "用户资质"),
        menuSubItem("用户负债", tabName = "用户负债"),        
        menuSubItem("多头", tabName = "多头")
        #menuSubItem("不良", tabName = "不良")
        ),
      
        menuItem("渠道质量监控", tabName = "渠道质量监控", icon = icon("dashboard"),startExpanded = TRUE,
        menuSubItem("渠道全流程转化", tabName = "渠道全流程转化"),
        menuSubItem("APP推广渠道", tabName = "APP推广渠道"),
        menuSubItem("M站推广渠道", tabName = "M站推广渠道")
        )
    )
),
 dashboardBody(
tabItems(
      tabItem("基本信息", 
        fluidRow(
          box(dateRangeInput("dates2", "Select the date range:",
               start = as.character(format(as.Date(min(basic$firstchuo))),"yyyy-mm-dd"),               
               end = as.character(format(as.Date(max(basic$firstchuo))),"yyyy-mm-dd"),
               min = as.character(format(as.Date(min(basic$firstchuo))),"yyyy-mm-dd"),              
               max = as.character(format(as.Date(max(basic$firstchuo))),"yyyy-mm-dd"),
               format = "yyyy-mm-dd"),
              hr(),
              h3("筛选时间,查看不同时间段内的各维度信息。"),
              hr(),
              width = 12
               ),
          box(selectInput('line',h3('筛选业务线'), c("APP","M站")),width = 12),
          box(selectInput('mode1',h3('筛选比较内容'), c("mode type","line type","channel type")),width = 12),
          box(showOutput("plot10","highcharts")),
          box(showOutput("plot1","highcharts")),
          box(showOutput("plot4","highcharts")),
          box(showOutput("plot7","highcharts")),   
          box(showOutput("plot11","highcharts")),
          box(showOutput("plot12","highcharts")),   
          box(showOutput("plot2","highcharts")),
          box(showOutput("plot3","highcharts")),   
          box(showOutput("plot5","highcharts")),   
          box(showOutput("plot6","highcharts")),
          box(showOutput("plot8","highcharts")),   
          box(showOutput("plot9","highcharts"))   
          
        
        )
      
      
      ),
      tabItem("模型类评分",
        fluidRow(
          box(dateRangeInput("dates3", "Select the date range:",
               start = as.character(format(as.Date(min(model$firstchuo))),"yyyy-mm-dd"),           
               end = as.character(format(as.Date(max(model$firstchuo))),"yyyy-mm-dd"),
               min = as.character(format(as.Date(min(model$firstchuo))),"yyyy-mm-dd"),             
               max = as.character(format(as.Date(max(model$firstchuo))),"yyyy-mm-dd"),
               format = "yyyy-mm-dd"),
              hr(),
              h3("筛选时间,查看不同时间段内的各维度信息。"),
              hr(),
              width = 12
               ),
          box(selectInput('line1',h3('筛选业务线'), c("APP","M站")),width = 12),
          box(selectInput('mode2',h3('筛选比较内容'), c("mode type","line type","channel type")),width = 12),
          box(showOutput("plot13","highcharts")),
          box(showOutput("plot13_repay","highcharts")),
          box(showOutput("plot16","highcharts")),
          box(showOutput("plot19","highcharts")),
          box(showOutput("plot22","highcharts")),
          box(showOutput("plot14","highcharts")),
          box(showOutput("plot15","highcharts")),
          box(showOutput("plot14_repay","highcharts")),
          box(showOutput("plot15_repay","highcharts")),   
          box(showOutput("plot17","highcharts")),
          box(showOutput("plot18","highcharts")),  
          box(showOutput("plot20","highcharts")),
          box(showOutput("plot21","highcharts")),   
          box(showOutput("plot23","highcharts")),
          box(showOutput("plot24","highcharts"))  
        )
      ),
      tabItem("收入", 
        fluidRow(
          box(dateRangeInput("dates4", "Select the date range:",
               start = as.character(format(as.Date(min(salary$firstchuo))),"yyyy-mm-dd"),              
               end = as.character(format(as.Date(max(salary$firstchuo))),"yyyy-mm-dd"),
               min = as.character(format(as.Date(min(salary$firstchuo))),"yyyy-mm-dd"),             
               max = as.character(format(as.Date(max(salary$firstchuo))),"yyyy-mm-dd"),
               format = "yyyy-mm-dd"),
              hr(),
              h3("筛选时间,查看不同时间段内的各维度信息。"),
              hr(),
              width = 12
               ),
          box(selectInput('line2',h3('筛选业务线'), c("APP","M站")),width = 12),
          box(selectInput('mode3',h3('筛选比较内容'), c("mode type","line type","channel type")),width = 12),
          box(showOutput("plot25","highcharts")),
          box(showOutput("plot26","highcharts")),
          box(showOutput("plot27","highcharts"))
        )
      
      
      
      ), 
      tabItem("用户资质", 
        fluidRow(
          box(dateRangeInput("dates6", "Select the date range:",
               start = as.character(format(as.Date(min(zizhi$firstchuo))),"yyyy-mm-dd"),              
               end = as.character(format(as.Date(max(zizhi$firstchuo))),"yyyy-mm-dd"),
               min = as.character(format(as.Date(min(zizhi$firstchuo))),"yyyy-mm-dd"),                
               max = as.character(format(as.Date(max(zizhi$firstchuo))),"yyyy-mm-dd"),
               format = "yyyy-mm-dd"),
              hr(),
              h3("筛选时间,查看不同时间段内的各维度信息。"),
              hr(),
              width = 12
               ), 
          box(selectInput('line4',h3('筛选业务线'), c("APP","M站")),width = 12),
          box(selectInput('mode5',h3('筛选比较内容'), c("mode type","line type","channel type")),width = 12),
          box(showOutput("plot43","highcharts")),
          box(showOutput("plot46","highcharts")),
          box(showOutput("plot49","highcharts")),   
          box(showOutput("plot44","highcharts")),
          box(showOutput("plot45","highcharts")),
          box(showOutput("plot47","highcharts")),  
          box(showOutput("plot48","highcharts")),
          box(showOutput("plot50","highcharts")),
          box(showOutput("plot51","highcharts"))  
        )
      ),
      tabItem("用户负债", 
        fluidRow(
          box(dateRangeInput("dates7", "Select the date range:",
               start = as.character(format(as.Date(min(owing$firstchuo))),"yyyy-mm-dd"),              
               end = as.character(format(as.Date(max(owing$firstchuo))),"yyyy-mm-dd"),
               min = as.character(format(as.Date(min(owing$firstchuo))),"yyyy-mm-dd"),                 
               max = as.character(format(as.Date(max(owing$firstchuo))),"yyyy-mm-dd"),
               format = "yyyy-mm-dd"),
              hr(),
              h3("筛选时间,查看不同时间段内的各维度信息。"),
              hr(),
              width = 12
               ),
          box(selectInput('line5',h3('筛选业务线'), c("APP","M站")),width = 12),
          box(selectInput('mode6',h3('筛选比较内容'), c("mode type","line type","channel type")),width = 12),
          box(showOutput("plot52","highcharts")),
          box(showOutput("plot55","highcharts")),
          box(showOutput("plot58","highcharts")),   
          box(showOutput("plot53","highcharts")),
          box(showOutput("plot54","highcharts")),
          box(showOutput("plot56","highcharts")),  
          box(showOutput("plot57","highcharts")),
          box(showOutput("plot59","highcharts")),
          box(showOutput("plot60","highcharts"))  
      
        )
      ),
      tabItem("多头", 
        fluidRow(
          box(dateRangeInput("dates5", "Select the date range:",
               start = as.character(format(as.Date(min(duotou$firstchuo))),"yyyy-mm-dd"),               
               end = as.character(format(as.Date(max(duotou$firstchuo))),"yyyy-mm-dd"),
               min = as.character(format(as.Date(min(duotou$firstchuo))),"yyyy-mm-dd"),             
               max = as.character(format(as.Date(max(duotou$firstchuo))),"yyyy-mm-dd"),
               format = "yyyy-mm-dd"),
              hr(),
              h3("筛选时间,查看不同时间段内的各维度信息。"),
              hr(),
              width = 12               ),
          box(selectInput('line3',h3('筛选业务线'), c("APP","M站")),width = 12),
          box(selectInput('mode4',h3('筛选比较内容'), c("mode type","line type","channel type")),width = 12),
          box(showOutput("plot28","highcharts")),
          box(showOutput("plot31","highcharts")),
          box(showOutput("plot34","highcharts")),   
          box(showOutput("plot37","highcharts")),
          box(showOutput("plot40","highcharts")),
          box(showOutput("plot29","highcharts")),  
          box(showOutput("plot30","highcharts")),
          box(showOutput("plot32","highcharts")),
          box(showOutput("plot33","highcharts")),   
          box(showOutput("plot35","highcharts")),
          box(showOutput("plot36","highcharts")),
          box(showOutput("plot38","highcharts")),          
          box(showOutput("plot39","highcharts")),          
          box(showOutput("plot41","highcharts")),          
          box(showOutput("plot42","highcharts"))         
        
        
        
        )
      
      
      ), 
      tabItem("渠道全流程转化", 
        fluidRow(
          box(
          dateRangeInput("dates8", "Select the date range:",
               start = as.character(format(as.Date(min(channel$first_login_time))),"yyyy-mm-dd"),              
               end = as.character(format(as.Date(max(channel$first_login_time))),"yyyy-mm-dd"),
               min = as.character(format(as.Date(min(channel$first_login_time))),"yyyy-mm-dd"),               
               max = as.character(format(as.Date(max(channel$first_login_time))),"yyyy-mm-dd"),
               format = "yyyy-mm-dd"),width = 12
               ),
          box(selectInput('line6',h3('筛选业务线'), c("APP","M")),width = 12),
          box(dataTableOutput("rate6"),width=12),
          box(dataTableOutput("rate4"),width=12),
          box(dataTableOutput("rate1"),width = 12),
          box(dataTableOutput("rate2"),width = 12),
          box(dataTableOutput("rate3"),width = 12),
          box(dataTableOutput("rate5"),width = 12)
        )
      
      ),
      tabItem("APP推广渠道", 
        fluidRow(
          #box(dataTableOutput("rate1")),
          #box(dataTableOutput("rate2")),
         # box(selectInput('line', '业务线', 
         #       c("app","M"))
         #      ),
          box(dateRangeInput("dates9", "Select the date range:",
               start = as.character(format(as.Date(min(channeleva$firstchuo))),"yyyy-mm-dd"),             
               end = as.character(format(as.Date(max(channeleva$firstchuo))),"yyyy-mm-dd"),
               min = as.character(format(as.Date(min(channeleva$firstchuo))),"yyyy-mm-dd"),               
               max = as.character(format(as.Date(max(channeleva$firstchuo))),"yyyy-mm-dd"),
               format = "yyyy-mm-dd"),
              hr(),
              h3("筛选时间,查看不同时间段内的信息。"),
              width = 12),
          box(selectInput('line7',h3('筛选业务线'), c("APP","M")),width = 12),
          box(
           checkboxGroupInput("spider", 
                              h4("渠道选择(戳额数>200的渠道)"), 
                              choices ="神马SEM大额",
                              selected = "神马SEM大额"
                              ),          
          width = 12          ),
          
          #htmlOutput("dx"),
          box(plotOutput("sp6"),width = 12),
          box(selectInput('basic',h4('基本信息'), c("edu","usertype", "citylevel")),width = 12),
          box(showOutput("plot61","highcharts")), 
          box(plotOutput("sp1")),
##
          box(selectInput('basic1',h3('模型类评分'), c("bin","repaybin","tengxun", "jd","umeng")),width = 12),
          box(showOutput("plot62","highcharts")) , 
          box(plotOutput("sp2")),
##
          box(selectInput('basic2',h3('用户资质'), c("max creditcard limit","max otherloan limit", "salary")),width = 12),
          box(showOutput("plot63","highcharts")) , 
          box(plotOutput("sp3")),
##
          box(selectInput('basic3',h3('多头数据'), c("borrow app num","tongdun_3m", "tongdun_reject","credit report query_1m")),width = 12),
          box(showOutput("plot64","highcharts")) , 
          box(plotOutput("sp4")),
##
          box(selectInput('basic4',h3('逾期数据'), c("overdue message count","credit report overdue_2y")),width = 12),
          box(showOutput("plot65","highcharts")) ,
          box(plotOutput("sp5"))
          #box(dataTableOutput("rate4"),width=12)
          #box(formattableOutput("formattableexample1")),
          #box(formattableOutput("formattableexample2"))
          )
      ),
      tabItem("M站推广渠道"
      #fluidRow(
      #column(12,includeHTML("final.html")))
      
      
      )
  )
 )
)
