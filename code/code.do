****************************************
******计量经济学方法与人口健康研究***********
****************************************

*******************************
******C3 人口健康现状研究*********
*******************************

/*设置工作路径*/
cd ""
/*打开数据*/
use ch3,clear
/*了解数据库*/
describe
/*数据清理*/
keep if age>=60
keep if rsd==2
nmissing id sex age nat rlg rsd edc inc hgt wgt hlt exc hsp hef mnt
drop if inc==.
save ch3_2.dta,replace
/*描述统计*/
use ch3_2.dta, clear
tab1 sex nat rlg edc
/*数据整理*/
gen han=2
replace han=1 if nat==1
label variable han "民族"
label define han 1 "汉族" 2 "少数民族"
label values han han
gen edc1=4
replace edc1=1 if edc==1 | edc==2
replace edc1=2 if edc==3
replace edc1=3 if edc==4
label variable edc1 "受教育程度"
label define edc1 1 "小学以下" 2 "小学" 3 "初中" 4 "高中及以上"
label values edc1 edc1
tab1 han edc1
/*检验分布*/
swilk age inc
tabstat age inc, s(median iqr)
/*描述统计*/
tab1 hlt hsp hef
/*数据整理*/
gen hsp1=hsp
replace hsp1=4 if hsp>4
label variable hsp1 "近12个月住院情况"
label define hsp1 0 "0次" 1 "1次" 2 "2次" 3 "3次" 4 "4次及以上"
label values hsp1 hsp1
tab1 hsp1

***********************************
******C4 人口健康影响因素研究**********
***********************************

/*设置工作路径*/
cd ""
/*打开数据*/
use ch4,clear
/*连续变量单因素回归分析*/
reg bmi i.han, allbase
reg bmi i.rlg, allbase
reg bmi i.rsd, allbase
reg bmi i.edc1,allbase
/*连续变量多因素回归分析*/
reg bmi i.han i.rlg i.rsd i.edc1 i.sex age exc i.hlt, allbase
/*二分类变量单因素回归分析*/
logit obesity i.han, or allbase
logit obesity i.rlg, or allbase
logit obesity i.rsd, or allbase
logit obesity i.edc1, or allbase
/*二分类变量多因素回归分析*/  
logit obesity  i.han i.rlg i.rsd i.edc1 i.sex age exc i.hlt, allbase 

*************************************
******C6 随机实验与人口健康研究**********
*************************************

/*设置工作路径*/
cd ""
/*打开数据*/
use ch6_1,clear
/*检验分布*/
swilk wgt
/*描述统计*/
table exc1, contents (median wgt iqr wgt)
/*中位数检验*/
median wgt, by(exc1)
/*方差齐性检验*/
sdtest wgt, by(exc1)
/*t检验(方差不齐)*/
ttest wgt, by(exc1) une
/*卡方检验*/
tab2 exc1 obesity, row chi
/*打开数据*/
use ch6_2,clear
/*卡方检验*/
tab2 exc2 obesity, row chi

***************************************
******C7 倾向值分析与人口健康研究**********
***************************************

/*设置工作路径*/
cd ""
/*打开数据*/
use ch7,clear
/*基础线性回归*/
reg bmi i.exc1, allbase
/*控制混杂因素线性回归*/
reg bmi i.exc1 age i.sex i.han i.rlg i.rsd i.edc1 i.hlt, allbase
/**一对一匹配**/
/*1.将数据随机排序*/
set seed 8888
gen ranorder=runiform()
sort ranorder
/*2. 进行一对一匹配（有放回且允许并列）*/
psmatch2 exc1 age sex han rlg rsd edc1 hlt, outcome(bmi) n(1) ate ties logit common
/*检验平衡与共同支持假设*/
pstest age sex han rlg rsd edc1 hlt, both graph 
psgraph
/**K近邻匹配**/
psmatch2 exc1 age sex han rlg rsd edc1 hlt, outcome(bmi) n(4) ate ties logit common quietly
/**卡尺匹配**/
sum _pscore
psmatch2 exc1 age sex han rlg rsd edc1 hlt, outcome(bmi) radius cal(0.02) ate ties logit common quietly
/**卡尺内近邻匹配**/
psmatch2 exc1 age sex han rlg rsd edc1 hlt, outcome(bmi) radius cal(0.02) ate ties logit common quietly
/**核匹配**/
psmatch2 exc1 age sex han rlg rsd edc1 hlt, outcome(bmi) kernel ate ties logit common quietly
/**局部线性回归匹配**/
psmatch2 exc1 age sex han rlg rsd edc1 hlt, outcome(bmi) llr ate ties logit common quietly
/**样条匹配**/
psmatch2 exc1 age sex han rlg rsd edc1 hlt, outcome(bmi) spline ate ties logit common quietly
set seed 8888
bootstrap r(att) r(ate) r(atu), reps(500): psmatch2 exc1 age sex han rlg rsd edc1 hlt, outcome(bmi) spline ate ties logit common quietly
/*基础logit回归*/
logit obesity i.exc1, or allbase
/*控制混杂因素logit回归*/
logit obesity i.exc1 age i.sex i.han i.rlg i.rsd i.edc1 i.hlt, allbase
/**逆处理概率加权法**/
tab exc1
gen wt1=0.6806/_pscore
gen wc1=(1-0.6806)/(1-_pscore)
gen pw1=.
replace pw1=wt1 if exc1==1
replace pw1=wc1 if exc1==0
logit obesity i.exc1 age i.sex i.han i.rlg i.rsd i.edc1 i.hlt [pw=pw1], or allbase
/**标准化死亡比加权法**/
gen wt2=1
gen wc2=[_pscore*(1-0.6806)]/[(1-_pscore)*0.6806]
gen pw2=.
replace pw2=wt2 if exc1==1
replace pw2=wc2 if exc1==0
logit obesity i.exc1 age i.sex i.han i.rlg i.rsd i.edc1 i.hlt [pw=pw2], or allbase

***************************************
******C8 工具变量法与人口健康研究**********
***************************************

/*设置工作路径*/
cd ""
/*打开数据*/
use ch8,clear
/*基础回归*/
reg hlt int17, allbase
reg hlt int17 age sex han rlg edc rsd, allbase
/*工具变量法回归*/
ivregress 2sls hlt age sex han rlg edc rsd (int17=int15 med15 inc15)
/*过度识别检验*/
estat overid
/*弱工具检验*/
estat firststage, all forcenonrobust
ssc install ivreg2
ssc install ranktest
/*同时进行过度识别检验和弱工具检验*/
ivreg2 hlt age sex han rlg edc rsd (int17=int15 med15 inc15), orthog (int15 med15 inc15)
/*内生性检验*/
ivreg2 hlt age sex han rlg edc rsd (int17=int15 med15 inc15), endog (int17)
/*豪斯曼检验*/
qui reg hlt int17 age sex han rlg edc rsd 
est store ols
qui ivregress 2sls hlt age sex han rlg edc rsd (int17=int15 med15 inc15)
est store iv
hausman iv ols, constant sigmamore
/*DWH检验*/
qui ivregress 2sls hlt age sex han rlg edc rsd (int17=int15 med15 inc15)
estat endogenous

******************************************
******C9 断点回归设计与人口健康研究************
******************************************

/*设置工作路径*/
cd ""
/*打开数据*/
use ch9,clear
/*精确断点回归*/
scatter retire age
rd hsp age, z0(60) mbw(100) graph
rd hsp age, z0(60) mbw(100) cov(han rlg exc hlt)
rd hsp age, z0(60) bdep oxline graph
rd hsp age, z0(60) mbw(100) x(han rlg exc hlt)
DCdensity age, breakpoint (60) generate (Xj Yj r0 fhat se_fhat) graphname (rdtest.eps)
/*模糊断点回归*/
gen rand=uniform() 
gen randretire=retire
replace randretire=1-retire if rand<0.1
rd hsp randretire age, z0(60) mbw (100)

********************************************
******C10 倍差法估计技术与人口健康研究***********
********************************************

/*设置工作路径*/
cd ""
/*打开数据*/
use ch10,clear
/*diff命令法*/
diff bmi, t(exc) p(t) robust
diff bmi, t(exc) p(t) cov(age hlt rlg) report robust
diff bmi, t(exc) p(t) cov(age hlt rlg) test  robust
/*回归法*/
reg bmi exc##t, robust
reg bmi exc##t age hlt rlg, robust
logit hlt exc##t, or
logit hlt exc##t age bmi rlg,or

**************************************
******C11 面板数据与人口健康研究**********
**************************************

/*设置工作路径*/
cd ""
/*打开数据*/
use ch11-14,clear
/*设定格式*/
xtset id year
/*观察数据结构*/
xtdes
/*数据描述*/
xtsum depression exercise age marriage fruit vegetable  
/*面板数据回归*/
xtreg depression i.exercise i.age i.marriage i.fruit i.vegetable,fe
estimates store FE
xtreg depression i.exercise i.age i.marriage i.fruit i.vegetable,re
estimates store RE
hausman FE RE

******************************************
******C12 结构方程模型与人口健康研究***********
******************************************

/*设置工作路径*/
cd ""
/*打开数据*/
use ch11-14,clear
rename ADL adl 
/*设定格式*/
xtset id year
/*简化的GSEM*/
gsem ///
(exercise fruit vegetable<- LifeStyle) /// measurement piece
(selfhealth depression adl<- Health) /// measurement piece
(Health <- LifeStyle) // structural piece

**************************************
******C13 生存分析与人口健康研究**********
**************************************

/*设置工作路径*/
cd ""
/*打开数据*/
use ch11-14,clear
/*把数据转换为时间跨度数据*/
snapspan id date1 event, gen(date0) replace
order date0, before(date1)
/*设定格式*/
stset date1, id(id) time0(date0) origin(time date0) failure(event==1) 
/*观察数据结构*/
stdes
/*分ADL受限与否的生存时间中位数*/
stci, by(ADL) 
/*log-rank 检验*/
sts test ADL
/*Cox回归*/
stcox i.ADL i.age i.sex i.marriage i.education i.fruit i.vegetable i.selfhealth, efron
estat phtest, detail 
/*绘图*/
sts graph,by(ADL) plot2opts(lp("-")) plot2opts(lp("-")) risktable(, order(1 "——" 2 "------") failevents rowtitle(, justification(left)) title(`"{fontface "宋体":面临风险的人数}"',at(rowtitle))) legend(label(1 `"{fontface "宋体":ADL不受限}"') label(2 `"{fontface "宋体":ADL受限}"') rows(1) ring(1) position(12)) scheme(tufte) title("") xtitle(`"{fontface "宋体":分析时间（月）}"') ytitle(`"{fontface "宋体":生存概率}"') 
graph export "p1.png",as(png) replace width(800) height(600)

****************************************
******C14 贝叶斯估计与人口健康研究**********
****************************************

/*设置工作路径*/
cd ""
/*打开数据*/
use ch11-14,clear
/*设定格式*/
keep if year==2014
/*贝叶斯回归*/
bayesmh depression i.exercise i.age i.marriage i.fruit i.vegetable, likelihood(normal({var})) prior({depression:}, normal(0, {var})) prior({var}, jeffreys) rseed(7)
/*绘图*/
bayesgraph diagnostics {depression:1.exercise}, scheme(tufte)
