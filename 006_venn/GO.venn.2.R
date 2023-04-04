########### data1,data2,data3 是三个dataframe, 首列是ID,第二列是logFC,列名无所谓是什么，data3可以为空。


source("D:/代码库/006_venn/GO.venn.help.R")


GOVenn<-function(data1,data2, data3, title, label, lfc.col, circle.col, plot=T){

  id <- NULL
  if (missing(label)) label<-c('List1','List2','List3')
  if (missing(lfc.col)) lfc.col<-c('orange','gold','cornflowerblue')
  if (missing(circle.col)) circle.col<-c('brown1','chartreuse3','cornflowerblue')
  if (missing(title)) title<-''
  if (missing(data3)==F) {
    three<-T
    overlap<-get_overlap(data1,data2,data3)
    venn_df<-overlap$venn_df
    table<-overlap$table
  }else{
    three<-F
    overlap<-get_overlap2(data1,data2)
    venn_df<-overlap$venn_df
    table<-overlap$table
  }
  
	### calc Venn ###
  if (three){
    center<-data.frame(x=c(0.4311,0.4308,0.6380),y=c(0.6197,0.3801,0.5001),diameter=c(0.4483,0.4483,0.4483))  ### aaa
    outerCircle<-data.frame(x=numeric(),y=numeric(),id=numeric())
	  for (var in 1:3){
		  dat <- circleFun(c(center$x[var],center$y[var]),center$diameter[var],npoints = 100)
		  outerCircle<-rbind(outerCircle,dat)
	  }
	  outerCircle$id<-rep(c(label[1],label[2],label[3]),each=100)
	  outerCircle$id<-factor(outerCircle$id, levels=c(label[1],label[2],label[3]))
  }else{
    center<-data.frame(x=c(0.33,0.6699),y=c(0.5,0.5),diameter=c(0.6180,0.6180))
    outerCircle<-data.frame(x=numeric(),y=numeric(),id=numeric())
    for (var in 1:2){
      dat <- circleFun(c(center$x[var],center$y[var]),center$diameter[var],npoints = 100)
      outerCircle<-rbind(outerCircle,dat)
    }
    outerCircle$id<-rep(c(label[1],label[2]),each=100)
    outerCircle$id<-factor(outerCircle$id, levels=c(label[1],label[2]))
  }

	### calc single pies ### 
  if (three){
    Pie<-data.frame(x=numeric(),y=numeric(),id=numeric())
	  dat <- circleFun(c(center$x[1],max(subset(outerCircle,id==label[1])$y)-0.1),0.15,npoints = 100)
	  Pie<-rbind(Pie,dat)
	  dat <- circleFun(c(center$x[2],min(subset(outerCircle,id==label[2])$y)+0.1),0.15,npoints = 100)
	  Pie<-rbind(Pie,dat)
	  dat <- circleFun(c(max(subset(outerCircle,id==label[3])$x)-0.1,center$y[3]),0.15,npoints = 100)
	  Pie<-rbind(Pie,dat)
	  Pie$id<-rep(1:3,each=100)
	  UP<-Pie[c(1:50,100:150,200:250),]
	  Down<-Pie[c(50:100,150:200,250:300),]
  }else{
    Pie<-data.frame(x=numeric(),y=numeric(),id=numeric())
    dat <- circleFun(c(min(subset(outerCircle,id==label[1])$x)+0.1,center$y[1]),0.2,npoints = 100)
    Pie<-rbind(Pie,dat)
    dat <- circleFun(c(max(subset(outerCircle,id==label[2])$x)-0.1,center$y[2]),0.2,npoints = 100)
    Pie<-rbind(Pie,dat)
    Pie$id<-rep(1:2,each=100)
    UP<-Pie[c(1:50,100:150),]
    Down<-Pie[c(50:100,150:200),]
  }

	### calc single pie text ###
	if (three){
    x<-c();y<-c()
	  for (i in unique(Pie$id)){
		  x<-c(x,rep((min(subset(Pie,id==i)$x)+max(subset(Pie,id==i)$x))/2,2))
		  y<-c(y,(min(subset(Pie,id==i)$y)+max(subset(Pie,id==i)$y))/2+0.02)
		  y<-c(y,(min(subset(Pie,id==i)$y)+max(subset(Pie,id==i)$y))/2-0.02)
	  }
      statu<- rep(c("UP","DOWN"),3)
      pieText<-data.frame(x=x,y=y,label=c(venn_df$UP[1],venn_df$DOWN[1],venn_df$UP[2],venn_df$DOWN[2],venn_df$UP[3],venn_df$DOWN[3]),statu=statu)
  
  }else{
    x<-c();y<-c()
    for (i in unique(Pie$id)){
      x<-c(x,rep((min(subset(Pie,id==i)$x)+max(subset(Pie,id==i)$x))/2,2))
      y<-c(y,(min(subset(Pie,id==i)$y)+max(subset(Pie,id==i)$y))/2+0.03)
      y<-c(y,(min(subset(Pie,id==i)$y)+max(subset(Pie,id==i)$y))/2-0.03)
    }
      statu<- rep(c("UP","DOWN"),2)
    pieText<-data.frame(x=x,y=y,label=c(venn_df$UP[1],venn_df$DOWN[1],venn_df$UP[2],venn_df$DOWN[2]),statu=statu)
  }
  
	### calc overlap pies ### 
  if (three){
    smc<-data.frame(x=c(0.6,0.59,0.31,0.5),y=c(0.66,0.34,0.5,0.5))
    PieOv<-data.frame(x=numeric(),y=numeric())
	  PieOv<-rbind(PieOv,circleFun(c(smc$x[1],smc$y[1]),0.1,npoints = 100))
	  PieOv<-rbind(PieOv,circleFun(c(smc$x[2],smc$y[2]),0.1,npoints = 100))
	  PieOv<-rbind(PieOv,circleFun(c(smc$x[3],smc$y[3]),0.1,npoints = 100))
	  PieOv<-rbind(PieOv,circleFun(c(smc$x[4],smc$y[4]),0.1,npoints = 100))
	  PieOv$id<-rep(1:4,each=100)
	  smc$id<-1:4
	  UPOv<-rbind(smc[1,],PieOv[1:33,],smc[1,],smc[2,],PieOv[100:133,],smc[2,],smc[3,],PieOv[200:233,],smc[3,],smc[4,],PieOv[300:333,],smc[4,])
	  Change<-rbind(smc[1,],PieOv[33:66,],smc[1,],smc[2,],PieOv[133:166,],smc[2,],smc[3,],PieOv[233:266,],smc[3,],smc[4,],PieOv[333:366,],smc[4,])
	  DownOv<-rbind(smc[1,],PieOv[66:100,],smc[1,],smc[2,],PieOv[166:200,],smc[2,],smc[3,],PieOv[266:300,],smc[3,],smc[4,],PieOv[366:400,],smc[4,])
  }else{
    PieOv<-data.frame(x=numeric(),y=numeric(),id=numeric())
    PieOv<-rbind(PieOv,circleFun(c(0.5,0.5),0.1,npoints = 100))
    PieOv$id<-rep(1,100)
    center<-data.frame(x=0.5, y=0.5, id=1)
    UPOv<-rbind(center[1,],PieOv[1:33,])
    Change<-rbind(center[1,],PieOv[33:66,])
    DownOv<-rbind(center[1,],PieOv[66:100,])
  }
  
  ### calc overlap pie text ###
  if (three){
    x<-c();y<-c()
for (i in unique(PieOv$id)){
    x<-c(x,subset(UPOv,id==i)$x[1]+0.02,subset(DownOv,id==i)$x[1]-0.022,subset(Change,id==i)$x[1]+0.01)
    y<-c(y,subset(UPOv,id==i)$y[1]+0.02,subset(DownOv,id==i)$y[1],subset(Change,id==i)$y[1]-0.018)
}
statu<-rep(c("UP","Change","DOWN"),4)
small.pieT<-data.frame(x=x,y=y,label=c(venn_df$UP[5],venn_df$Change[5],venn_df$DOWN[5],venn_df$UP[6],venn_df$Change[6],venn_df$DOWN[6],venn_df$UP[4],venn_df$Change[4],venn_df$DOWN[4],venn_df$UP[7],venn_df$Change[7],venn_df$DOWN[7]),statu=statu)
}else{
    x<-c(subset(UPOv,id==1)$x[1]+0.03,subset(DownOv,id==1)$x[1]-0.05,subset(Change,id==1)$x[1]+0.02)
    y<-c(subset(UPOv,id==1)$y[1]+0.03,subset(DownOv,id==1)$y[1],subset(Change,id==1)$y[1]-0.025)
    statu<-rep(c("UP","Change","DOWN"),1)
    small.pieT<-data.frame(x=x,y=y,label=c(venn_df$UP[3],venn_df$Change[3],venn_df$DOWN[3]),statu=statu)
  }
  
 pieText$statu<-factor(pieText$statu,levels=c("UP","DOWN"))
 small.pieT$statu<-factor( small.pieT$statu,levels=c("UP","DOWN","Change"))

  g<- ggplot()+
    geom_polygon(data=outerCircle, aes(x,y, group=id, fill=id) ,alpha=0.5,color='black')+
	  scale_fill_manual(values=circle.col)+
    guides(fill=guide_legend(title=''))+geom_text(data=pieText, aes(x=x,y=y,label=label,color=statu),size=6)+
    geom_polygon(data=UPOv, aes(x,y,group=id),fill=lfc.col[1],color='grey',alpha=0)+
	  geom_polygon(data=DownOv, aes(x,y,group=id),fill=lfc.col[3],color='grey',alpha=0)+
	  geom_polygon(data=Change, aes(x,y,group=id),fill=lfc.col[2],color='grey',alpha=0)+
	  geom_text(data=small.pieT,aes(x=x,y=y,label=label,color=statu),size=5)+theme_blank+scale_color_manual(values=c("purple","blue","red"))+ labs(title=title)  
    
  
  if (plot) return(g) else return(list(plot=g,table=table))
}


#b <- GOVenn(GL[[3]],GL[[4]],label =c("GM1.MR4D","MR4D.sham"), plot = FALSE)
#data1<-GL[[3]];data2<-GL[[4]]; label<-c("GM1.MR4D","MR4D.sham"), lfc.col, circle.col, plot=T