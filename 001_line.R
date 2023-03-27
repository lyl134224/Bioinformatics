##line plot
##2022-09-05 
##by yanlin liao

library(ggplot2)
source("D:/代码库/theme/theme.R")  #先导入theme

p <- ggplot(data, aes(x, y, group = group, color = group)) +  #不同颜色区分组别，只有1组时，group=1。
     geom_line() + 
     mytheme + 
     xlab("") + ylab("")


