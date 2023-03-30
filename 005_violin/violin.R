##violin 
##2022-09-06
##by yanlin liao


library(ggplot2)


p <- ggplot(data, aes(x, y, fill=group)) +  #按组填充颜色 
     geom_violin() + 
     mytheme + 
     xlab(x) + ylab(y)