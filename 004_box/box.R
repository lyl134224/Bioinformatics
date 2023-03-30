##box
##2022-09-06
##by yanlin liao



########################方法1：ggplot

library(ggplot2)
library(ggsignif)


source("D:/代码库/theme/theme.R")


p1 <- ggplot(data, aes(x, y, fill=x, color=x, fill=x)) + # 分组填充颜色, 线条颜色也可以通过分组映射。
      geom_boxplot() + 
      geom_signif(comparisons = list(c("HC", "HP")),   #设置比较组
                  #y_position = c(15.2,15.2),          #线的高度
                  tip_length = 0,                      #显示括号线两端的下降的小竖线，用来指向精确的组别
                  map_signif_level=TRUE,               #布尔值，检验结果P值使用注释或者星号代替
                  annotations = NULL,                  #替换P值注释的字符向量
                  vjust = ,                            #相对于括号线，上下调整文本的距离
                  color = "black") +                   #设置线条颜色
      
      coord_flip() +        # 旋转坐标
      geom_jitter(shape=16, 
                  position = position_jitter(0.2)) +  #增加点的抖动，0.2为x方向的抖动程度
      scale_fill_manual(values = c('#999999', '#E69F00', '#56B4E9')) +  # 设置颜色
      #scale_color_manual(values = c('blue', 'red')) +  # 分组映射线条，设置线条颜色
      mytheme + 
      xlab(x) + 
      ylab(y)



#######################方法2：ggpubr，多组画图

library(ggpubr)

plotlist <- list()
compared <- list(c("low", "high"))

for (i in DEGlist){
  
  df1 <- exp_DEG[, c(i, "Group")]
  p1 <- ggboxplot(df1,
                  x = "Group",
                  y = i,
                  fill = "Group",
                  palette = c("#00AFBB”，“#E7B800"),
                  add = "jitter",
                  size = 0.5,
                  xlab = "USP1 sensitive",
                  ylab = paste0("Gene expression of ( log2(TPM+1) )", i)) +
        stat_compare_means(comparisons = compared,
                           method = "wilcox.test",                        #设置统计方法
                           symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                              symbols = c("***", "**", "*", "ns")))
  p1 <- p1 + 
        theme(axis.title = element_text(size = 17))                 #设置坐标轴标题文本大小
  #ggsave(paste0(i, ".boxplot.pdf"), p1, width = 6.5, height = 6.5)
  plotlist[[i]] <- p1
  
}



ggarrange(plotlist = plotlist, nrow = 2, ncol = 3)     ##设置图片排布几行几列







