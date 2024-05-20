# pdf report
pdf("/Users/dipika/Desktop/report.pdf")

# get html table from html page as DF
library("rvest")
htmldoc=read_html("https://www.wsj.com/public/resources/documents/info-Degrees_that_Pay_you_Back-sort.html")
htmltables=html_nodes(htmldoc,"table")

# parse html table into DF
df=html_table(htmltables,header=TRUE,fill=TRUE)[[7]] 

colnames(df)

# save DF as csv
write.csv(df,file="/Users/dipika/Desktop/majors.csv")

# cleanup: remove "Percent change from Starting to Mid-Career Salary" column
df$"Percent change from Starting to Mid-Career Salary"=NULL

# rename columns
colnames(df)[c(1,2,3,4,5,6,7)]=c("major","start","media","per10","per25","per75","per90")

# clean up: remove "$" & "," from currency values & convert to numeric
library(dplyr)
dfc=df %>% mutate_at(2:7,function(x) as.numeric(gsub('[\\$,]',"",x))) # df clean

# plot media vs start
library(ggplot2)
ggplot(data=dfc,aes(x=reorder(major,media,FUN=function(x)-x),y=media))+geom_bar(stat="identity",fill="skyblue")+labs(x="Undergraduate Major",y="Mid-Career Median Salary")+ggtitle("Mid-Career Median Salary by Major")+theme(axis.text.x=element_text(angle=45,hjust=1))

# optimal number of clusters
library(factoextra)
k_data=dfc %>% select(c(start,media))
opti_clust=fviz_nbclust(k_data,FUNcluster=kmeans,method='wss')+geom_vline(xintercept=3,linetype=2)+labs(subtitle="Elbow method")
opti_clust

# scale dfc columns 2,3,4,7 to standardize vars for easier data comparison
dfs=dfc %>% select(c(2,3,4,7)) %>% scale()
set.seed(1) 

# clustering
km=kmeans(dfs,centers=3,iter.max=15,nstart=25)
dfl=dfc %>% mutate(clusters=km$cluster) # label dfc cluster

# plot media vs start by cluster
ggplot(dfl,aes(x=start,y=media,color=factor(clusters)))+geom_point(alpha=0.8,size=7)+xlab("Starting Median Salary")+ylab("Mid-Career Median Salary")+ggtitle("K means - Starting vs Mid-Career Salary")+scale_x_continuous(labels=scales::dollar)+scale_y_continuous(labels=scales::dollar)

# cluster plots: low, medium, high payback
library(tidyr)
dfp=dfl %>% select(c(major,per10,per25,media,per75,per90,clusters)) %>% gather(key='percentile',value='salary',-c(major,clusters)) %>% mutate(percentile=factor(percentile,levels=c("per10","per25","media","per75","per90")))
c1=dfp %>% filter(clusters==1) %>% ggplot(aes(x=percentile,y=salary,group=major,color=major))+geom_point()+geom_line()+ggtitle("Cluster 1: Arts")+    theme(axis.text.x=element_text(size=7,angle=25),legend.text=element_text(size=10),legend.key.height=unit(0.25,'cm'))
# c1
c2=dfp %>% filter(clusters==3) %>% ggplot(aes(x=percentile,y=salary,group=major,color=major))+geom_point()+geom_line()+ggtitle("Cluster 2: Business")+theme(axis.text.x=element_text(size=7,angle=25),legend.text=element_text(size=10),legend.key.height=unit(0.25,'cm'))
# c2
c3=dfp %>% filter(clusters==2) %>% ggplot(aes(x=percentile,y=salary,group=major,color=major))+geom_point()+geom_line()+ggtitle("Cluster 3: STEM")+    theme(axis.text.x=element_text(size=7,angle=25),legend.text=element_text(size=10),legend.key.height=unit(0.25,'cm'))
# c3

library(gridExtra)
grid.arrange(c1,c2,c3)

# pdf report
graphics.off()
