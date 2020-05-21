library(jsonlite)
library(dplyr)
#/匯入資料
salary107<-fromJSON("107年各教育程度別初任人員每人每月經常性薪資─按大職類分.json")
salary104<-fromJSON("http://ipgod.nchc.org.tw/dataset/b6f36b72-0c4a-4b60-9254-1904e180ddb1/resource/63ecb4a9-f634-45f4-8b38-684b72cf95ba/download/0df38b73f75962d5468a11942578cce5.json")

#salary104$`大學-薪資`<-as.numeric(gsub("—|…"," ",salary104$`大學-薪資`))
#salary107$`大學-薪資`<-as.numeric(gsub("—|…"," ",salary107$`大學-薪資`))
#salary104$`大學-女/男`<-as.numeric(gsub("—|…"," ",salary104$`大學-女/男`))
#salary107$`大學-女/男`<-as.numeric(gsub("—|…"," ",salary107$`大學-女/男`))
#salary104$`研究所及以上-薪資`<-as.numeric(gsub("—|…"," ",salary104$`研究所及以上-薪資`))
#salary107$`研究所-薪資`<-as.numeric(gsub("—|…"," ",salary107$`研究所-薪資`))

salary104$`大職業別` <- salary107$`大職業別` 
#兩份資料row數是一樣的且大職業別欄位內的資料雖有差異但都不影響真正意思，為了方便後面join就用107年的覆蓋掉104年的。
joinsalary <- inner_join(salary104,salary107,by="大職業別") #將107&104的各職業及教育程度薪資資料join，成立一個新的綜合表格joinsalary
for (i in grep("—|…",joinsalary)){
  joinsalary[grep("—|…", joinsalary[,i]),i] <- ""} #一次整理join完的表格中有-符號的資料，將其取代成空字串。
joinsalary[-2]<-as.data.frame(sapply(joinsalary[-2],as.numeric)) #為了後面計算方便將除了職業別外的薪資欄位全轉成數字。


#Q1
joinsalary$increase_pc <-joinsalary$`大學-薪資.y`/joinsalary$`大學-薪資.x`#新增一個欄位increase_pc，是提高比例(107/104大學薪資)的值
pick107higher<-filter(joinsalary,`大學-薪資.y`>`大學-薪資.x`) #篩選出107比104薪資高的
pick107higher #顯示107比104高的職業 
pick107higher %>% 
  arrange(desc(pick107higher$increase_pc)) %>% #將提高比例由大到小排序 
  head(10) #呈現前10筆

pickmore5pc <- filter(joinsalary,increase_pc>1.05) #薪資提高超過5%的
pickmore5pc #顯示薪資提高超過5%的職業

mainoccupation <- ifelse(grepl("-",pickmore5pc$大職業別), 
                         substr(pickmore5pc$大職業別,1,regexpr("-",pickmore5pc$大職業別)-1),
                         pickmore5pc$大職業別) #判斷大職業別欄位中是否有"-"符號，若有便執行取出前面字串的動作，若否則取出整串
table(mainoccupation) #分析出現次數



#Q2
man104 <-arrange(joinsalary,`大學-女/男.x`) %>% #104男生比女生多
  head(10) %>%  #104年男生比女生多且照差異程度由大到小排序，取前十名(=女/男比率值由小到大排的前十名)
  select(1:14) #篩選104年度的資料欄位(1~14)

man107 <-arrange(joinsalary,`大學-女/男.y`) %>%  #107男生比女生多
  head(10) %>% #107年男生比女生多且照差異程度由大到小排序，取前十名(=女/男比率值由小到大排的前十名)
  select(2,15:27) #篩選107年度的資料欄位(2,15~27)
  
woman104 <-arrange(joinsalary,desc(`大學-女/男.x`))  %>% #104女生比男生多
  head(10) %>% #104年女生比男生多且照差異程度由大到小排序，取前十名(=女/男比率值由大到小排的前十名)
  select(1:14) #篩選104年度的資料欄位(1~14)

woman107 <-arrange(joinsalary,desc(`大學-女/男.y`))%>% #107女生比男生多
  head(10) %>% #107年女生比男生多且照差異程度由大到小排序，取前十名(=女/男比率值由大到小排的前十名)
  select(2,15:27) #篩選107年度的資料欄位(2,15~27)



#Q3 
joinsalary$increase_pc_grad <- joinsalary$`研究所-薪資`/joinsalary$`大學-薪資.y` #在joinsalary表格中新增一個研所/大學薪資增加比率的欄位叫increase_pc_grad。
graduate_diff <- select(joinsalary,2,15:27,29) #以107年度來看，所以取2,15~27,29的欄位
graduate_diff %>%
  arrange(desc(graduate_diff$increase_pc_grad)) %>% #將增加比率由大到小排序
  head(10) #呈現前十筆資料


#Q4
interested_df<-
  data.frame("職業別"=c(joinsalary$大職業別[[78]],joinsalary$大職業別[[127]],joinsalary$大職業別[[120]],joinsalary$大職業別[[113]],joinsalary$大職業別[[50]])
             ,"104大學薪資"=c(joinsalary$`大學-薪資.x`[[78]],joinsalary$`大學-薪資.x`[[127]],joinsalary$`大學-薪資.x`[[120]],joinsalary$`大學-薪資.x`[[113]],joinsalary$`大學-薪資.x`[[50]])
             ,"104研究所薪資"=c(joinsalary$`研究所及以上-薪資`[[78]],joinsalary$`研究所及以上-薪資`[[127]],joinsalary$`研究所及以上-薪資`[[120]],joinsalary$`研究所及以上-薪資`[[113]],joinsalary$`研究所及以上-薪資`[[50]])
           ,"107大學薪資"=c(joinsalary$`大學-薪資.y`[[78]],joinsalary$`大學-薪資.y`[[127]],joinsalary$`大學-薪資.y`[[120]],joinsalary$`大學-薪資.y`[[113]],joinsalary$`大學-薪資.y`[[50]])
           ,"107研究所薪資"=c(joinsalary$`研究所-薪資`[[78]],joinsalary$`研究所-薪資`[[127]],joinsalary$`研究所-薪資`[[120]],joinsalary$`研究所-薪資`[[113]],joinsalary$`研究所-薪資`[[50]]))
interested_df
#建立一個dataframe，內容包含有興趣的五個職業類別、大學畢業薪資及研究所畢業薪資。
interested_df$"104薪資差" <- interested_df$X104研究所薪資-interested_df$X104大學薪資 #新增104年薪資差異的欄位
interested_df$"107薪資差" <- interested_df$X107研究所薪資-interested_df$X107大學薪資 #新增107年薪資差異的欄位
interested_df



