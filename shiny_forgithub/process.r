
#################################################################################
options(java.parameters = "-Xmx8048m")
source("D:/source/impala_connect.R")
basic <- dbGetQuery(con, 
"select  * 
from appzc.dx_flowmonitor_basicinfo6 where adddate(inserttime,31)>=current_timestamp()"
)
#basic2 <- dbGetQuery(con, 
#"select  * 
#from appzc.dx_flowmonitor_basicinfo6 where inserttime>='2018-05-25'"
#)

basic$fl=NULL
basic$fl1=NULL
basic$fl2=NULL
basic$repaybin=as.numeric(as.character(basic$repaybin))
basic$repaybin[basic$repaybin==0]=NA


isNa=function(x)
{   rr=integer(0)
    rr=sum(is.na(x))/length(x)
    return(rr)}
    
basic$new1=apply(basic[,which(names(basic)=="jdcredit_score"):which(names(basic)=="coverdue2y")],1,isNa)
basic=basic[-which(basic$new1>=0.75&basic$linetype %in% c("大额主营","大额渠道",'大额_m')),]
basic$new1=NULL
##渠道各种率展示
channel_tot <- dbGetQuery(con, "select  * from appzc.dx_channelmonitor_basicinfo")
channel_app= channel_tot[channel_tot$qudao_type=="APP",]
channel_m= channel_tot[channel_tot$qudao_type=="M",]

#################################渠道大类
channel_app1=channel_app[channel_app$sourcetype=="app",]
channel_m1=channel_app[channel_app$sourcetype=="M",]

channel_app2=channel_m[channel_m$sourcetype=="app",]
channel_m2=channel_m[channel_m$sourcetype=="M",]


channel_sub=read.csv("D:/shinydemo/shiny_forgithub/channel_sub.csv",header = TRUE,stringsAsFactors = FALSE)
ch_app=unique(channel_sub[channel_sub$渠道标签 %in% c("APP贷超","APP信息流") ,])
ch_m=unique(channel_sub[channel_sub$渠道标签 %in% c("M站贷超","M站信息流") ,])
#ch_yi=unique(channel_sub[channel_sub$渠道标签 =="异业合作" ,])

names(ch_app)[2]="channel_category"
names(ch_m)[2]="channel_category"

channel_app1=merge(channel_app1,ch_app,"sourcename",all.x = T)
channel_m1=merge(channel_m1,ch_m,"sourcename",all.x = T)
channel_app=rbind(channel_app1,channel_m1)

channel_app2 = merge(channel_app2,ch_app,"sourcename",all.x = T)
channel_m2 = merge(channel_m2,ch_m,"sourcename",all.x = T)
channel_m = rbind(channel_app2,channel_m2)

channel=rbind(channel_app,channel_m)
##################################


##basicinfo 数据处理
dealBasic=function(data){

#data$fl=NULL
#data$fl1=NULL
#data$fl2=NULL

data$inserttime=NULL
data$firstchuo_m=NULL
data$dweek=NULL
data$dyear=NULL
data$pho_pro=NULL
data$vloan=NULL
data$vvloan=NULL
data$edu_cert=NULL


data$age_bin=cut(data$age,breaks=c(0,18,20,22,30,40,45,55,100)) ##年龄分段
###对城市进行处理
data$citylevel_pho[data$pho_city=="北京"]="m"
data$citylevel_pho[data$citylevel_pho=="m"]=1
data$citylevel_pho[data$citylevel_pho=="a"]=2
data$citylevel_pho[data$citylevel_pho=="b"]=3
data$citylevel_pho[data$citylevel_pho=="c"]=4
data$citylevel_pho[data$citylevel_pho=="d"]=5
data$citylevel_pho[data$citylevel_pho=="e"]=6
data$citylevel_pho=as.numeric(data$citylevel_pho)
data$citylevel_pho[is.na(data$citylevel_pho)]=7
data$citylevel_bin=ifelse(data$citylevel_pho<6,sprintf("%i线",data$citylevel_pho),"6线及以下")

data$pho_city=NULL
data$citylevel_pho=NULL

###对城市进行处理

####准备雷达图数据
#data$edu_score=ifelse(data$edu=="研究生",2,
#    ifelse(data$edu=="本科",1,
#    ifelse(data$edu=="专科",0,-1)))
#data$city_score=ifelse(data$citylevel_pho==1,2,
#    ifelse(data$citylevel_pho==2,1,
#    ifelse(data$citylevel_pho %in% c(3,4),0,-1)))
#data$usertype_score=
#    ifelse(data$usertype=="全新",2,
#    ifelse(data$usertype=="有额未发标",1,
#    ifelse(data$usertype=="已成交" ,0,
#    ifelse(data$usertype %in% c("戳额无额度","发标未成交"),-1,-2))))
#
#data$bin_score=ifelse(data$credit_bin==1,2,
#    ifelse(data$credit_bin %in% c(2,3),1,
#    ifelse(data$credit_bin>3&data$credit_bin<=5,0,
#    ifelse(data$credit_bin>5&data$credit_bin<=8,-1,-2))))
#data$tc_score=ifelse(data$risk_score>0&data$risk_score<=20,2,
#    ifelse(data$risk_score>20&data$risk_score<=40,1,
#    ifelse(data$risk_score>40&data$risk_score<=60,0,
#    ifelse(data$risk_score>60&data$risk_score<=80,-1,-2))))
#data$um_score=ifelse(data$umeng_score>700&data$umeng_score<=850,2,
#    ifelse(data$umeng_score>600&data$umeng_score<=700,1,
#    ifelse(data$umeng_score>500&data$umeng_score<=600,0,
#    ifelse(data$umeng_score>400&data$umeng_score<=500,-1,-2))))
#data$jd_score=ifelse(data$jdcredit_score>700&data$jdcredit_score<=850,2,
#    ifelse(data$jdcredit_score>650&data$jdcredit_score<=700,1,
#    ifelse(data$jdcredit_score>620&data$jdcredit_score<=650,0,
#    ifelse(data$jdcredit_score>550&data$jdcredit_score<=620,-1,-2))))

return(data)
}

basic=dealBasic(basic)


###############渠道数据处理
getMerge=function(channel,basic){

    channeleva=merge(channel[channel$chuo_status==1,],basic,"userid")

    for (i in which(names(channeleva)=="cmax"):which(names(channeleva)=="repaybin")){
        channeleva[,i]=as.numeric(channeleva[,i]) }

    return(channeleva)
    
    }
  

####################
getRank=function(channel,basic,n,ind){
    channeleva=getMerge(channel,basic)
    ceshi1= channeleva[channeleva$sourcetype==ind,] %>% group_by(sourcename) %>%
            summarise(num=n(),
            edu=sum(edu %in% c("1硕士","2本科")&!is.na(edu))/sum(!is.na(edu)),
            usertype=sum(usertype %in% c("4有额未发标","5纯新")&!is.na(usertype))/sum(!is.na(usertype)),
            citylevel=sum(citylevel_bin %in% c("1线","2线")&!is.na(citylevel_bin))/sum(!is.na(citylevel_bin)),
            bin=sum(credit_bin <=2&!is.na(credit_bin))/sum(!is.na(credit_bin)),
            rbin=sum(repaybin <=1&!is.na(repaybin))/sum(!is.na(repaybin)),
            tc=sum(risk_score <=20&!is.na(risk_score))/sum(!is.na(risk_score)),
            jd=sum(jdcredit_score >=700&!is.na(jdcredit_score))/sum(!is.na(jdcredit_score)),
            um=sum(umeng_score >=700&!is.na(umeng_score))/sum(!is.na(umeng_score)),
            cmax=sum(cmax >=50000&!is.na(cmax))/sum(!is.na(cmax)),
            omax=sum(omax >=50000&!is.na(omax))/sum(!is.na(omax)),
            pre=sum(pretax %in% c("05k-08k","08k-15k","15k-30k","30k+"))/n(),
            bo=sum(boappnum<=1&boappnum>=0 &!is.na(boappnum))/sum(!is.na(boappnum)&boappnum>=0),
            td1=sum(td_3m==0 &!is.na(td_3m))/sum(!is.na(td_3m)&td_3m>=0),
            td2=sum(final_score<80 &final_score>=0&!is.na(final_score))/sum(!is.na(final_score)&final_score>=0),
            zx1=sum(query1m<=1 &!is.na(query1m)&query1m>=0&query1m!=-1)/sum(!is.na(query1m)&query1m!=-100&query1m>=0),
            def=sum(message_count_default==0 &!is.na(message_count_default))/sum(!is.na(message_count_default)&message_count_default>=0),
            zx2=sum((hoverdue2y+coverdue2y+ooverdue2y)==0 &ooverdue2y!=-100&ooverdue2y!=-1&!is.na(hoverdue2y)&!is.na(coverdue2y)&!is.na(ooverdue2y))/sum(!is.na(hoverdue2y)&!is.na(coverdue2y)&ooverdue2y!=-1&!is.na(ooverdue2y)&ooverdue2y!=-100)
            ) %>% subset(num>=n)

            ceshi1[ceshi1=="NaN"]=NA
            ceshi1$qudao_type=toupper(ind)
    return(ceshi1)
    }
    

#########################################
score=function(data){

##基本信息打分
    data$edu_score=ifelse(data$edu=="1研究生",5,
        ifelse(data$edu=="2本科",4,
        ifelse(data$edu=="3专科"|is.na(data$edu),3,1)))
    data$city_score=ifelse(data$citylevel_bin=="1线",2,
        ifelse(data$citylevel_bin=="2线",1,
        ifelse(data$citylevel_bin %in% c("3线","4线")|is.na(data$citylevel_bin),0,-1)))
    data$usertype_score=
        ifelse(data$usertype=="5纯新",4,
        ifelse(data$usertype=="4有额未发标",3,
        ifelse(data$usertype=="2已成交"|is.na(data$usertype) ,1,
        ifelse(data$usertype =="2发标未成交",1,0))))

    ##模型类评分
    data$bin_score=ifelse(data$credit_bin==1,5,
        ifelse(data$credit_bin ==2,4,
        ifelse(data$credit_bin ==3,3,
        ifelse((data$credit_bin>3&data$credit_bin<=5)|is.na(data$credit_bin),2,
        ifelse(data$credit_bin>5&data$credit_bin<=8,1,0)))))

    data$rbin_score=
        ifelse(data$repaybin==1,5,
        ifelse(data$repaybin==2,4,
        ifelse(data$repaybin==3|data$repaybin==0|is.na(data$repaybin),3,
        ifelse(data$repaybin ==4,1,0))))
        
    data$tc_score=ifelse(data$risk_score>0&data$risk_score<=20,4,
        ifelse(data$risk_score>20&data$risk_score<=40,3,
        ifelse((data$risk_score>40&data$risk_score<=60)|is.na(data$risk_score),2,
        ifelse(data$risk_score>60&data$risk_score<=80,1,0))))
        
    data$um_score=ifelse(data$umeng_score>800&data$umeng_score<=850,4,
        ifelse(data$umeng_score>700&data$umeng_score<=800,4,
        ifelse((data$umeng_score>600&data$umeng_score<=700)|is.na(data$umeng_score),3,
        ifelse(data$umeng_score>500&data$umeng_score<=600,2,
        ifelse(data$umeng_score>400&data$umeng_score<=500,1,0)))))
        
    data$jd_score=ifelse(data$jdcredit_score>700&data$jdcredit_score<=850,4,
        ifelse(data$jdcredit_score>650&data$jdcredit_score<=700,3,
        ifelse((data$jdcredit_score>620&data$jdcredit_score<=650)|is.na(data$jdcredit_score),2,
        ifelse(data$jdcredit_score>560&data$jdcredit_score<=620,1,0))))
        
    ##用户资质

    data$cmax_score=ifelse(data$cmax>=100000,6,
        ifelse(data$cmax>=50000&data$cmax<100000,5,
        ifelse(data$cmax>=20000&data$cmax<50000 ,4,
        ifelse((data$cmax>=10000&data$cmax<20000)|is.na(data$cmax),2,
        ifelse(data$cmax>=6000&data$cmax<10000,1,0)))))

    data$omax_score=ifelse(data$omax>=100000,5,
        ifelse(data$omax>=50000&data$omax<100000,4,
        ifelse(data$omax>=20000&data$omax<50000 ,3,
        ifelse((data$omax>=10000&data$omax<20000)|is.na(data$omax),1,
        ifelse(data$omax>=6000&data$omax<10000,0,0)))))

    data$pre_score=ifelse(data$pretax=="30k+",6,
        ifelse(data$pretax=="15k-30k",5,
        ifelse(data$pretax=="08k-15k",4,
        ifelse(data$pretax=="05k-08k",3,
        ifelse(data$pretax=="missing",2,
        ifelse(data$pretax=="02k-05k",2,
        ifelse(data$pretax=="0-02k",0,0)))))))
        
    ##多头数据

    data$bo_score=ifelse(data$boappnum==0,4,
        ifelse(data$boappnum>0&data$boappnum<=1,3,
        ifelse(data$boappnum>1&data$boappnum<=5 |is.na(data$boappnum),2,
        ifelse(data$boappnum>5&data$boappnum<=10 ,1,
        ifelse(data$boappnum>10&data$boappnum<=20,0,0)))))

    data$td_score=ifelse(data$td_3m==0,4,
        ifelse(data$td_3m>0&data$td_3m<=1,3,
        ifelse((data$td_3m>1&data$td_3m<=5 )|is.na(data$td_3m),2,
        ifelse(data$td_3m>5&data$td_3m<=10 ,1,
        ifelse(data$td_3m>10&data$td_3m<=20,0,0)))))
        
    data$zx1_score=ifelse(data$query1m==0,4,
        ifelse(data$query1m>0&data$query1m<=1,3,
        ifelse((data$query1m>1&data$query1m<=5 )|is.na(data$query1m),2,
        ifelse(data$query1m>5&data$query1m<=10 ,1,
        ifelse(data$query1m>10&data$query1m<=20,0,0)))))

    ##逾期数据

    data$msg_score=ifelse(data$message_count_default==0,3,
        ifelse(data$message_count_default>0&data$message_count_default<5,2,
        ifelse((data$message_count_default>=5&data$message_count_default<10) |is.na(data$message_count_default),1,
        ifelse(data$message_count_default>=10&data$message_count_default<20,0,-1))))


    data$zx2_score=ifelse((data$hoverdue2y+data$coverdue2y+data$ooverdue2y)==0&data$hoverdue2y>-1&data$coverdue2y>-1&data$ooverdue2y>-1,4,
        ifelse((data$hoverdue2y+data$coverdue2y+data$ooverdue2y)>0&(data$hoverdue2y+data$coverdue2y+data$ooverdue2y)<5&data$hoverdue2y>-1&data$coverdue2y>-1&data$ooverdue2y>-1,2,
        ifelse(((data$hoverdue2y+data$coverdue2y+data$ooverdue2y)>=5&(data$hoverdue2y+data$coverdue2y+data$ooverdue2y)<10&data$hoverdue2y>-1&data$coverdue2y>-1&data$ooverdue2y>-1 )|(data$hoverdue2y==-1|data$coverdue2y==-1|data$ooverdue2y==-1 ),1,
        ifelse((data$hoverdue2y+data$coverdue2y+data$ooverdue2y)>=10&(data$hoverdue2y+data$coverdue2y+data$ooverdue2y)<20&data$hoverdue2y>-1&data$coverdue2y>-1&data$ooverdue2y>-1 ,0,-1))))
        
    data=data[,c(2,which(names(data)=="edu_score"):which(names(data)=="zx2_score"))]   
    
    return(data)
    }
    
#fscore=score(channeleva[channeleva$sourcetype=="app"&channeleva$sourcename %in% ceshi1$sourcename,])
#####准备函数开始
ratio=function(x){
    h=sum(x,na.rm=TRUE)/sum(!is.na(x))
    return(h)
}

######准备函数结束
getScore=function(channel,basic,n,index){
    channeleva=getMerge(channel,basic)

    ceshi1=getRank(channel,basic,n,index)
    
    fscore=score(channeleva[channeleva$sourcetype==index&channeleva$sourcename %in% ceshi1$sourcename,])
    score= fscore %>% group_by(sourcename) %>%
    summarise(
    edu=ratio(edu_score),
    city=ratio(city_score),
    usertype=ratio(usertype_score),
    bin=ratio(bin_score),
    rbin=ratio(rbin_score),
    tengxun=ratio(tc_score),
    umeng=ratio(um_score),
    jd=ratio(jd_score),
    max_creditcard=ratio(cmax_score),
    max_otherloan=ratio(omax_score),
    salary=ratio(pre_score),
    boapp=ratio(bo_score),
    tongdun=ratio(td_score),
    zx_query=ratio(zx1_score),
    ovd_msg=ratio(msg_score),
    overdue_zx=ratio(zx2_score),
    num=n())
    score[score=="NaN"]=0
    score$qudao_type=toupper(index)
    
    return(score)
    }

########执行上面的函数

channeleva1=getMerge(channel_app,basic)
channeleva2=getMerge(channel_m,basic)



ceshi1=getRank(channel_app,basic,66,"app")
ceshi2=getRank(channel_m,basic,66,"M")

channeleva1=channeleva1[channeleva1$sourcename %in% ceshi1$sourcename,]
channeleva2=channeleva2[channeleva2$sourcename %in% ceshi2$sourcename,]


score1=getScore(channel_app,basic,66,"app")
score2=getScore(channel_m,basic,66,"M")

ceshi=rbind(ceshi1,ceshi2)
score=rbind(score1,score2)


#score[score=="NaN"]=0

########################
cleanChanneleva=function(channeleva){
    
    channeleva$userid=NULL
    channeleva$first_chuo_bin=NULL
    channeleva$sourcefeature=NULL
    channeleva$chuo_status=NULL
    channeleva$youe_status=NULL
    channeleva$fb_status=NULL
    channeleva$cj_status=NULL
    channeleva$allcj_status=NULL
    channeleva$inserttime=NULL
    channeleva$first_login_time=NULL
    channeleva$week=NULL
    channeleva$age=NULL
    channeleva$linetype=NULL
    channeleva$age_bin=NULL
    channeleva$citylevel_pho=NULL
    return(channeleva)

}

channeleva1=cleanChanneleva(channeleva1)
channeleva2=cleanChanneleva(channeleva2)
channeleva=rbind(channeleva1,channeleva2)

cleanBasic=function(basic,channel1,channel2) {
    basic$rand=basic$userid%%1000

    basic1=basic[basic$linetype =="大额主营",]
    basic2=basic[basic$linetype =="小额",]
    basic3=basic[basic$linetype=="大额渠道",]
    basic4=basic[basic$linetype =="大额_m",]
    basic5=basic[basic$linetype=="小额_m",]


    set.seed(6666)
    basic1=basic1[basic1$rand %in% sample(0:999,600),]
    basic2=basic2[basic2$rand %in% sample(0:999,150),]
    basic5=basic5[basic5$rand %in% sample(0:999,50),]

    basic1=rbind(basic1,basic2,basic3)
    basic2=rbind(basic4,basic5)

    basic1=merge(basic1,channel1[channel1$chuo_status==1&!is.na(channel1$channel_category),c(which(names(channel1)=="userid"),which(names(channel1)=="channel_category"))],"userid",all.x=T)
    basic1$channel_total_category=ifelse(!is.na(basic1$channel_category),basic1$channel_category,ifelse(basic1$linetype=="大额渠道","其他app渠道类型",basic1$linetype))
    
    basic2=merge(basic2,channel2[channel2$chuo_status==1&!is.na(channel2$channel_category),c(which(names(channel2)=="userid"),which(names(channel2)=="channel_category"))],"userid",all.x=T)
    basic2$channel_total_category=ifelse(!is.na(basic2$channel_category),basic2$channel_category,ifelse(basic2$linetype=="大额_m","其他M站渠道类型",basic2$linetype))
    
    basic=rbind(basic1,basic2)
    
    basic$channel_category=NULL
    basic$userid=NULL
    basic$rand=NULL
    return(basic)

}

basic=cleanBasic(basic,channel_app,channel_m)

basicinfo=basic[,c(2,3,5,6,9,10,11,30,31,32)]
model=basic[,c(1,2,3,6,8,9,10,12,13,14,29,32)]
salary=basic[,c(2,3,6,9,10,15,32)]
duotou=basic[,c(2,3,6,7,8,9,10,16,17,18,32)]
zizhi=basic[,c(2,3,6,9,10,19,20,21,32)]
owing=basic[,c(2,3,6,9,10,22,23,24,32)]


##
write.table(channel,"D:/shinydemo/shiny_forgithub/channel.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改 
#write.table(channel_m,"D:/shinydemo/shiny_forgithub/channel_m.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改 
##
##
write.table(channeleva,"D:/shinydemo/shiny_forgithub/channeleva.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改 
write.table(ceshi,"D:/shinydemo/shiny_forgithub/ceshi.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改 
write.table(score,"D:/shinydemo/shiny_forgithub/score.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改 
##
#write.table(channeleva2,"D:/shinydemo/shiny_forgithub/channeleva2.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改 
##

write.table(basicinfo,"D:/shinydemo/shiny_forgithub/basicinfo.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改 
write.table(model,"D:/shinydemo/shiny_forgithub/model.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改   
write.table(salary,"D:/shinydemo/shiny_forgithub/salary.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改
write.table(duotou,"D:/shinydemo/shiny_forgithub/duotou.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改   
write.table(zizhi,"D:/shinydemo/shiny_forgithub/zizhi.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改   
write.table(owing,"D:/shinydemo/shiny_forgithub/owing.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改   
