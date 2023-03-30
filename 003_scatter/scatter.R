##Scatter 
##2022-09-06
##by yanlin liao


#######################

library(ggplot2)

p <- ggplot(data, aes(x, y, color = groups)) +   #根据组别填充不同颜色
     geom_point(shape=1) +                       #数字反映不同的点形状
     mytheme + 
     xlab(x) + ylab(y)



#######################散点加折线图

p <- ggplot(MC1, aes(x = data, y = num, group = groups)) +     #只有一组时，group=1
     geom_point(shape = 24,                               #设置点形状
                colour = "red",                           #设置点轮廓颜色
                fill= "red",                              #设置填充颜色
                size = 3) +                               #设置点大小
     geom_line(size=0.8,                                  #设置线条粗细 
               colour = "red") +                          #设置线条颜色
     mytheme + 
     xlab("样本接收时间") + ylab("MRD水平") +
     scale_y_log10(limits=c(0.000001, 1.5),               #y轴log10转换，设置范围    
                   expand = c(0,0),                       #设置坐标轴原点位置
                   breaks=c(0.000001, 0.00001,0.0001,0.001,0.01,0.1,1)) +    #设置刻度尺
     ggtitle("MC1")

