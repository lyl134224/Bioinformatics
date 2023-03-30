##bar plot
##2022-09-05
##by yanlin liao
library(ggplot2)



p <- ggplot(data, aes(x, y, fill = group)) +  #根据组别填充颜色
     geom_bar(stat = "identity", 
              position = "dodge",  # "dodge"表示并列，"fill"百分比图，"stack"为堆积图
              alpha = 0.5) +         # 透明度
     geom_text(aes(label = y),     # 柱子上加文本，比如Y轴数字
               vjust = -0.5,       # 垂直向上移动距离
               hjust = 0.5)  +     # 水平向左移动距离
     mytheme + 
     xlab(x) + ylab(y)



############################## 分面画堆积图

library(openxlsx)
library(reshape2)
library(RColorBrewer)
library(ggplot2)


color <- brewer.pal(n = 6, name =  "RdYlBu")[c(1,2,5)]   #调色盘选取颜色

ggplot(data=da2,
       aes(x=variable, y=time, fill=group)) + 
       geom_bar(position="stack",
                stat = "identity",
                width=0.7,
                size=0.25) + 
       facet_grid(. ~ sample)+
  
       scale_fill_manual(values=color) +
       theme(
             axis.title=element_text(size=12,face="plain",color="black"),
             axis.text = element_text(size=10,face="plain",color="black"),
             legend.title=element_text(size=14,face="plain",color="black"),
             axis.text.x = element_text(angle = 45,hjust = 1, vjust = 1),
             legend.background  =element_blank()) + 
              # legend.position = c(0.9,0.73)  
       ylab("time(min)")




