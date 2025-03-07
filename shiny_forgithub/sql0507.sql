./*
#############################
##edited by duxin,2018-03-28#
#############################

###sql */
/*大额H5注册用户*/
drop table if exists appzc.dx_dae_user_reg;
create table if not exists appzc.dx_dae_user_reg as
 select
a.userid,a.inserttime,a.f_sourceid as sourceid
from
( select
userid,inserttime,isnull(newsourceid,sourceid) as f_sourceid
from
ods.userregisterchannels
where
isnull(newsourceid,sourceid) in 
(select sourceid from
ods.cooperatesource
where
channelsid in (2911,3080,3081,3082,3083,3084,3085,3086,3087,3088,3089,3090) ) as a ;


/*大额H5首登用户(使用)*/
drop table if exists appzc.dx_dae_user_h5login;
create table if not exists appzc.dx_dae_user_h5login as
 select
a.userid,a.login_time as login_dt,a.sourceid
from
( select
userid,
inserttime as login_time,
sourceid,
row_number () over (partition by userid order by inserttime asc) as r
from
ods.tbloginlogby2018
where
sourceid in 
(select
sourceid
from
ods.cooperatesource
where
channelsid in (2911,3080,3081,3082,3083,3084,3085,3086,3087,3088,3089,3090) ) as a
where
a.r=1 ;

/*大额渠道总用户*/
drop table if exists appzc.dx_dae_user_source;
create table if not exists appzc.dx_dae_user_source as
 select
ttl.userid,reg.inserttime as reg_dt,login.login_dt,
case when reg.userid is not null then 1 else 0 end as reg_user,
case when reg.userid is not null then reg.sourceid else login.sourceid end as f_sourceid
from
( select
userid
from
appzc.dx_dae_user_reg
union
select
userid
from
appzc.dx_dae_user_h5login ) as ttl
left join
appzc.dx_dae_user_reg as reg on ttl.userid=reg.userid
left join
appzc.dx_dae_user_h5login as login on ttl.userid=login.userid ;

/*大额渠道总用户刨去主营渠道尾号用户*/
drop table if exists appzc.dx_dae_user_basic;
create table if not exists appzc.dx_dae_user_basic as
 select
ttl.*,app.inserttime as app_login_dt,
case when app.inserttime<ttl.login_dt and ttl.userid%100<=9  and app.inserttime<'2018-05-04' then 1
     when app.inserttime<ttl.login_dt and ttl.userid%100<=49 and app.inserttime>='2018-05-04' and app.inserttime<'2018-05-09 17:00' then 1
     when app.inserttime<ttl.login_dt and (ttl.userid%100<=79 or ttl.userid%100>=90) and app.inserttime>='2018-05-09 17:00' then 1
     else 0 end as if_main
from
appzc.dx_dae_user_source as ttl
left join
( select
a.userid,a.inserttime,a.sourceid
from
( select
userid,inserttime,sourceid,row_number () over (partition by userid order by inserttime asc) as r
from
ods.tbloginlogby2018
where
inserttime>'2018-04-20'
and
resultcode=0
and
useragent like 'PPD-LoanApp/%'
and
( cast( regexp_extract(useragent,'PPD-LoanApp/([^.]+)',1) as int ) = 6 and cast(regexp_extract(useragent,'6.([^.]+)',1) as int)>=2
or
cast( regexp_extract(useragent,'PPD-LoanApp/([^.]+)',1) as int)>6 ) ) as a 
where
a.r=1 ) as app on ttl.userid=app.userid ;

drop table if exists appzc.dx_dae_user_usertype;
create table if not exists appzc.dx_dae_user_usertype as
select
a.*,
case when a.reg_dt is not null then 'new'
     when a.reg_dt is null and b.creationdate is null then 'undone'
	 when a.reg_dt is null and b.creationdate is not null then 'done'
	 end as usertype
from
appzc.dx_dae_user_basic as a
left join
(select
borrowerid,
creationdate,
row_number () over (partition by borrowerid order by creationdate asc) as r
from
ods.listing
where
statusid in (4,12) ) as b
on
a.userid=b.borrowerid and b.creationdate<a.login_dt and b.r=1 
where a.if_main=0;

drop table if exists appzc.dx_dae_user_det;
create table if not exists appzc.dx_dae_user_det as
 select
a.userid,
cast(a.login_dt as timestamp) as login_dt,
a.f_sourceid,
regsc.sourcefeature,
regsc.sourcename,
a.usertype
from
appzc.dx_dae_user_usertype as a

left join [shuffle]
(
select
user_id,cast(time as timestamp) as imp_dt,row_number () over (partition by user_id order by time asc) as r
from
edw.fact_app_element_imp_daily
where
dt>'2018-04-20' and tgt_event_id='super_bigad_product') as imp on a.userid=imp.user_id and imp.r=1

left join [shuffle]
ods.userregisterchannels as regdt on a.userid=regdt.userid
left join [shuffle]
ods.cooperatesource as regsc on a.f_sourceid=regsc.sourceid
 ;


--###############################人群画像###################
drop table if exists appzc.dx_flowmonitor_basicinfo;
create table appzc.dx_flowmonitor_basicinfo
    as
select
    userid,
    inserttime ,
    credit_bin,
    firstchuo, 
    firstchuo_m,
    week,
    dweek,
    dyear,
    age,
    /*gender, */
    edu_cert,
    edu,
    chuomode,
    message_count_default,
    boappnum,
    linetype

from (
    select 
    mmv.userid,
    mmv.inserttime ,
    mmv.credit_bin,
    substring(mmv.inserttime,1,10) as firstchuo,
    substring(mmv.inserttime,1,7) as firstchuo_m,    
    case when weekofyear(days_sub(mmv.inserttime,6)) = weekofyear(mmv.inserttime) then concat(cast(year(days_sub(mmv.inserttime,6)) as string),'-',cast(weekofyear(days_sub(mmv.inserttime,6)) as string))
    else concat(cast(year(mmv.inserttime) as string),'-',cast(weekofyear(mmv.inserttime) as string)) end as week,
    case when weekofyear(days_sub(mmv.inserttime,6)) = weekofyear(mmv.inserttime) then year(days_sub(mmv.inserttime,6)) else  year(mmv.inserttime) end as dyear,
    case when weekofyear(days_sub(mmv.inserttime,6)) = weekofyear(mmv.inserttime) then weekofyear(days_sub(mmv.inserttime,6)) else  weekofyear(mmv.inserttime) end as dweek,
    mmv.age,
    /*gender, */
    mmv.edu_cert,
    message_count_default,
    boappnum,
    case when first_chuo_mark like '%wb%' then 'wb'
         when first_chuo_mark like '%tb%' then 'tb'
         when first_chuo_mark like '%bb%' then 'bb'
         when first_chuo_mark like '%fd%' then 'gjj'
         else 'oth' end as chuomode,    
    case when mmv.edu_cert='1' then '1研究生' /* 研究生*/
         when mmv.edu_cert='2' then '2本科' /* 本科*/
         when mmv.edu_cert='3' then '3专科' /* 专科*/
         else '4无学历' end as edu, 
    case when b.userid is not null then '大额渠道' else '大额主营' end as linetype,
    ROW_NUMBER()over(partition by mmv.userid order by mmv.inserttime asc) as flag
    from ods.mobilemodelvariable mmv
    
    inner join [shuffle] appzc.dx_datatable_chuo a 
    on mmv.userid=a.userid
    and a.chuo_status=1

    left join [shuffle] appzc.dx_dae_user_basic b 
    on mmv.userid=b.userid 
    and b.if_main=0

    where listingid=-1
    and months=5 
    and realname_renren_match in (13003,13002)
    and gender_renren_match in(301,202)
    )mm
where flag=1 and inserttime >='2018-04-24'

union all 

select
    userid,
    inserttime ,
    credit_bin,
    firstchuo, 
    firstchuo_m,
    week,
    dweek,
    dyear,
    age,
    /*gender, */
    edu_cert,
    edu,
    chuomode,
    message_count_default,
    boappnum,
    '小额' as linetype

from (
    select 
    mmv.userid,
    mmv.inserttime ,
    mmv.credit_bin,
    substring(mmv.inserttime,1,10) as firstchuo, 
    substring(mmv.inserttime,1,7) as firstchuo_m,    
    case when weekofyear(days_sub(mmv.inserttime,6)) = weekofyear(mmv.inserttime) then concat(cast(year(days_sub(mmv.inserttime,6)) as string),'-',cast(weekofyear(days_sub(mmv.inserttime,6)) as string))
    else concat(cast(year(mmv.inserttime) as string),'-',cast(weekofyear(mmv.inserttime) as string)) end as week,
    case when weekofyear(days_sub(mmv.inserttime,6)) = weekofyear(mmv.inserttime) then year(days_sub(mmv.inserttime,6)) else  year(mmv.inserttime) end as dyear,
    case when weekofyear(days_sub(mmv.inserttime,6)) = weekofyear(mmv.inserttime) then weekofyear(days_sub(mmv.inserttime,6)) else  weekofyear(mmv.inserttime) end as dweek,
    mmv.age,
    /*gender, */
    mmv.edu_cert,
    message_count_default,
    boappnum,
    case when first_chuo_mark like '%wb%' then 'wb'
         when first_chuo_mark like '%tb%' then 'tb'
         when first_chuo_mark like '%bb%' then 'bb'
         when first_chuo_mark like '%fd%' then 'gjj'
         else 'oth' end as chuomode,        
    
    case when mmv.edu_cert='1' then '1研究生' /* 研究生*/
         when mmv.edu_cert='2' then '2本科' /* 本科*/
         when mmv.edu_cert='3' then '3专科' /* 专科*/
         else '4无学历' end as edu, 
    ROW_NUMBER()over(partition by mmv.userid order by mmv.inserttime asc) as flag
    
    from ods.mobilemodelvariable mmv

    inner join [shuffle] appzc.dx_datatable_chuo a 
    on mmv.userid=a.userid
    and a.log_status=0
    and chuo_status=0

    where listingid=-1
    and realname_renren_match =11001 
    and gender_renren_match=101 
    and substr(mark,1,2) in ('1','2','3','4','5','2.','3.','4.','5.')

    )mm
where 
    flag=1 
    and  inserttime >='2018-04-24';



/* 基本信息展示*/
drop table if exists appzc.dx_flowmonitor_basicinfo1;
create table appzc.dx_flowmonitor_basicinfo1
as
select distinct *
from (
select 
a.*,
/*cmstr_idnm_pro as id_pro,
cmstr_idnm_city as id_city,
cmstr_pho_pro as pho_pro,*/
cmstr_pho_pro as pho_pro,
cmstr_pho_cit as pho_city,
c.citylevel as citylevel_pho 
/*d.`level` as citylevel_id */

from appzc.dx_flowmonitor_basicinfo a

left join [shuffle] edw.common_user_daily b
on a.userid=b.user_id
and b.dt=strleft(cast(date_add(now(),-1) as string),10)

left join [shuffle] appzc.yqqcsd_citylevel  c 
on b.cmstr_pho_cit=c.city

/*left join [shuffle] appzc.citylevel  d
on b.cmstr_idnm_city=d.city */
) dx2 ;



drop table if exists appzc.dx_flowmonitor_basicinfo2;
create table appzc.dx_flowmonitor_basicinfo2
as 
select a.*, 
case when cj.userid is not null then '2成交'
     when cj.userid is null and fb.userid is not null then '2发标未成交'
     when youe.userid is not null and fb.userid is null then '4有额未发标'
     when chuoe.userid is not null and youe.userid is null then '1戳额无额度'
     when chuoe.userid is null then '5纯新'
     else 'other'  end as usertype
from appzc.dx_flowmonitor_basicinfo1 a 
left join appzc.dx_chuoe chuoe on a.inserttime>chuoe.inserttime and a.userid=chuoe.userid 
left join appzc.dx_youe youe on a.inserttime>youe.inserttime and a.userid=youe.userid 
left join appzc.dx_fb fb on a.inserttime>fb.inserttime and a.userid=fb.userid 
left join appzc.dx_cj cj on a.inserttime>cj.auditingdate and a.userid=cj.userid 
;

 
 
/* 模型类评分*/

drop table if exists appzc.dx_flowmonitor_basicinfo3;
create table appzc.dx_flowmonitor_basicinfo3
as 
select * 
from (
select 
a.*,
cast(b.score as decimal(38,2)) as jdcredit_score,
c.credit_score as umeng_score,
d.risk_score,
row_number()over(partition by a.userid order by b.inserttime desc, c.inserttime desc, d.inserttime desc) fl

from 
appzc.dx_flowmonitor_basicinfo2 a
    left join ods.jdcredit_score b on a.userid=b.userid and a.inserttime>b.inserttime
    left join ods.umeng_newscoreinfos c on a.userid=c.userid and a.inserttime>c.inserttime
    left join ods.social_score_info d on a.userid=d.userid  and a.inserttime>d.inserttime ) dx
where fl=1;
/* 资产*/
--税前收入
drop table if exists appzc.dx_flowmonitor_basicinfo4;
create table appzc.dx_flowmonitor_basicinfo4
as 
select * 
from 
(
select  a.*, 
        case when b.`result` is null or `result` in ('-1','-2','-3') then 'missing'
             when b.`result`='0' then '0-0'
             when b.`result` in ('a','b') then '0-02k'
             when b.`result` in ('c','d','e') then '02k-05k'
             when b.`result` in ('f','g','h') then '05k-08k'
             when b.`result` in ('i','j','k','l','m','n','o') then '08k-15k'
             when b.`result` in ('p','q','r','s','t','u','v','w','x','y') then '15k-30k'
             else '30k+'
             end as pretax ,
        --cast(h.salaryincomeamountaverage6m as decimal(38,2)) as wangyin,
        --cast(w.fundbasenumber as decimal(38,2))  as gjj,
        --case when ys.userid is not null then 1 else 0 end as iscar,
        --case when ly.userid is not null then 1 else 0 end as ishouse,
        row_number() over(partition by a.userid order by b.inserttime desc) fl1
from appzc.dx_flowmonitor_basicinfo3 a 

left join ods.pretax_income_level b 
on a.userid=b.userid and a.inserttime>b.inserttime
/*
left join ods.bankbill_report_debitcard_details h 
on a.userid=h.userid and a.inserttime>h.inserttime

left join (
select
ppduserid as userid,
inserttime,
case when base_number=0 or base_number is null then (monthly_total_income/200)/0.085 else base_number/100 end as fundbasenumber
from ods.fund_userinfo )w
on a.userid=w.userid and a.inserttime>w.inserttime

left join 
(select distinct userid
from (
select userid 
from ods.rhzx_user_loanotherdetails  
where purpose='3' 

union all
 
select userid
from ods.CRD_CD_LN 
where type_dw like '%汽车%') c ) ys
on a.userid=ys.userid 

left join 
(select distinct userid
from (
select userid 
from ods.rhzx_user_loanhouse


union all
 
select userid
from ods.CRD_CD_LN 
where type_dw like '%房%') d ) ly
on a.userid=ly.userid */


) t 
where fl1=1 ;

/* 多头*/
drop table if exists appzc.dx_flowmonitor_basicinfo5;
create table appzc.dx_flowmonitor_basicinfo5
as 
select *
from (

select 
a.*,
b.final_score,
P2P_3m+small_loan_id_3m as td_3m,
P2P_1m+small_loan_id_1m as td_1m,
row_number() over(partition by a.userid order by b.inserttime desc) fl2

from appzc.dx_flowmonitor_basicinfo4 a

left join [shuffle] ods.tongdun_data b 
on a.userid=b.userid 
and a.inserttime>b.inserttime
left join [shuffle] 
(
select 
     userid
    ,inserttime
    
    ,max(case when ruleid='3244586' then nvl(p2p_net_loan_idcard,0) else 0 end ) P2P_3m    
    ,max(case when ruleid='3244584' then nvl(p2p_net_loan_idcard,0) else 0 end ) P2P_1m
        
    ,max(case when ruleid='3244586' then nvl(small_loan_idcard,0) else 0 end ) small_loan_id_3m
    ,max(case when ruleid='3244584' then nvl(small_loan_idcard,0) else 0 end ) small_loan_id_1m
    from edw.tongdun_ruledetail
group by userid,inserttime
) c 
on a.userid=c.userid 
and a.inserttime>c.inserttime ) dx
where fl2=1;

--资质和负债
drop table if exists appzc.dx_flowmonitor_basicinfo6;
create table appzc.dx_flowmonitor_basicinfo6
as 
select 
dx.*, 
cmax,
omax,
vcard,
rph,
rpo,
rpc,
vloan,
vvloan,
query1m,
hoverdue2y,
ooverdue2y,
coverdue2y,
ly.json_repaybin as repaybin



from appzc.dx_flowmonitor_basicinfo5 dx 

left join [shuffle]
(
select *
from 
(
select 
userid,
cmax,
omax,
vcard,
rph,
rpo,
rpc,
vloan,
vvloan,
query1m,
hoverdue2y,
ooverdue2y,
coverdue2y,
ROW_NUMBER()over(partition by userid order by json_inserttime asc) as flag1
from 
(
select *
from (
select 
userid, 
json_inserttime,
json_max_ccard_amount cmax,
json_otherLoanMaxAmount omax,
json_cnt_valid_ccard  vcard,

json_repayamount_loanhouse_month as rph,
cast(json_repayamount_loanother_month as decimal(32,4)) as rpo,
cast(json_repayamount_creditcard_month as decimal(32,4)) as rpc,
json_validotherloannum vloan,
json_unsettledvalidotherloannum vvloan,
'-100' as query1m,
'-100' as hoverdue2y,
'-100' as ooverdue2y,
'-100' as coverdue2y,
ROW_NUMBER()over(partition by userid order by json_inserttime asc) as flag


from  edw.userpataresult
where json_bizid ='13002'
      and dt>='2018-04-24' 
      and json_flow_count='1' ) a  where flag=1
 
union all  

select *
from (
select 
pa.userid,
json_inserttime, 
json_zxFull_maxCCardAmountFull as cmax,
json_otherLoanMaxAmountFull as omax,
json_zxFull_cntValidCCardFull as vcard,
json_houseLoanEveryMonthPaypalAmount rph,
rpo,
rpc,
json_validotherloannumfull vloan,
json_unsettledvalidotherloannum vvloan, 
json_creditReportOneMonthQueryTimes as query1m,
json_loanHouseOverdueMonthsInTwoYears as hoverdue2y,
json_otherLoanOverdueMonthsInTwoYears as ooverdue2y,
json_ccardOverdueMonthsTwoYears as coverdue2y,
ROW_NUMBER()over(partition by pa.userid order by json_inserttime asc,gg.inserttime asc,kk.inserttime asc) as flag


from  edw.userpataresult pa
left join 
(select 
userid ,
inserttime,
sum(cast(regexp_replace(Scheduled_Payment_Amount,',','') as decimal(32,2))) as rpc 
from  ods.CRD_CD_LND 
group by userid ,inserttime) gg
on pa.userid=gg.userid 

left join 
(select 
userid,
inserttime,
sum(case when  Type_Dw not like '%房%' and  state='正常'  and Payment_Rating='按月归还'  and Remain_Payment_Cyc is not null and Remain_Payment_Cyc not in ('--','')  and  Remain_Payment_Cyc<>'0' then round(cast(regexp_replace(Scheduled_Payment_Amount,',','') as decimal(32,2))) end) as rpo
from ods.CRD_CD_LN 
group by userid ,inserttime)kk
on pa.userid=kk.userid 

where 
json_bizid = '13003'
and dt>='2018-04-24' 
and json_flow_count='2' )b where flag=1 ) ly )ys  where flag1=1 )tt

on dx.userid=tt.userid

left join [shuffle]
( 
select userid,json_repaybin,
ROW_NUMBER()over(partition by userid order by json_inserttime asc) as flag

from 
edw.userpataresult l
where 
json_bizid = '13003'
and dt>='2018-04-24' 
and json_flow_count='1'

)ly
on dx.userid=ly.userid  
and ly.flag=1

;






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

channel_app2=merge(channel_app2,ch_app,"sourcename",all.x = T)
channel_m2=merge(channel_m2,ch_m,"sourcename",all.x = T)
channel_m=rbind(channel_app2,channel_m2)

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
channeleva=merge(channel[channel$chuo_status==1,],basic,"userid")

for (i in which(names(channeleva)=="cmax"):which(names(channeleva)=="repaybin")){
channeleva[,i]=as.numeric(channeleva[,i]) }

####################
ceshi1= channeleva[channeleva$sourcetype=="app",] %>% group_by(sourcename) %>%
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
) %>%
subset(num>=66)

ceshi1[ceshi1=="NaN"]=NA
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
    
data=data[,c(2,43:58)]   
return(data)
    }
fscore=score(channeleva[channeleva$sourcetype=="app"&channeleva$sourcename %in% ceshi1$sourcename,])
#####准备函数开始
ratio=function(x){
h=sum(x,na.rm=TRUE)/sum(!is.na(x))
return(h)
}

######准备函数结束
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




########################

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

basic$rand=basic$userid%%1000

basic1=basic[basic$linetype =="大额主营",]
basic2=basic[basic$linetype =="小额",]
basic3=basic[basic$linetype=="大额渠道",]
basic4=basic[basic$linetype =="大额_m",]
basic5=basic[basic$linetype=="小额_m",]


set.seed(6666)
basic1=basic1[basic1$rand %in% sample(0:999,600),]
basic2=basic2[basic2$rand %in% sample(0:999,200),]
basic5=basic5[basic5$rand %in% sample(0:999,100),]

basic=rbind(basic1,basic2,basic3,basic4,basic5)

basic=merge(basic,channel1[channel1$chuo_status==1&!is.na(channel1$channel_category),c(which(names(channel1)=="userid"),which(names(channel1)=="channel_category"))],"userid",all.x=T)
basic$channel_total_category=ifelse(!is.na(basic$channel_category),basic$channel_category,ifelse(basic$linetype=="大额渠道","其他大额渠道类型",basic$linetype))

basic$channel_category=NULL
basic$userid=NULL
basic$rand=NULL


basicinfo=basic[,c(2,3,5,6,9,10,29,30,31)]
model=basic[,c(1,2,3,6,8,9,11,12,13,28,31)]
salary=basic[,c(2,3,6,9,14,31)]
duotou=basic[,c(2,3,6,7,8,9,15,16,17,31)]
zizhi=basic[,c(2,3,6,9,18,19,20,31)]
owing=basic[,c(2,3,6,9,21,22,23,31)]


##
write.table(channel1,"D:/shinydemo/shiny_forgithub/channel.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改 
##
##
write.table(channeleva,"D:/shinydemo/shiny_forgithub/channeleva.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改 
write.table(ceshi1,"D:/shinydemo/shiny_forgithub/ceshi1.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改 
write.table(score,"D:/shinydemo/shiny_forgithub/score.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改 
##

write.table(basicinfo,"D:/shinydemo/shiny_forgithub/basicinfo.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改 
write.table(model,"D:/shinydemo/shiny_forgithub/model.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改   
write.table(salary,"D:/shinydemo/shiny_forgithub/salary.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改
write.table(duotou,"D:/shinydemo/shiny_forgithub/duotou.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改   
write.table(zizhi,"D:/shinydemo/shiny_forgithub/zizhi.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改   
write.table(owing,"D:/shinydemo/shiny_forgithub/owing.txt",quote=FALSE,row.names=FALSE,fileEncoding = "UTF-8") ##地址可更改   




   