source("colormap.R")
source("util.R")
source("circle_illusion.R")


N<-100
nvp<-1  # number of view port
type<-3 # type of Fraser-Wilcox illusion
wr<-0.08  # width rate
vr<-0.85 # value rate

title<-paste("type",as.character(type),"_wr",as.character(wr),"_vr",as.character(vr),".png",sep = "")

if(type!=5){
  
  
  
  cmlist=fwmap(type,N,wr,vr)
  cmtitle<-paste("cm_",title,sep = "")
  png(cmtitle)
  plot(cmlist[[1]])
  dev.off() 
  
  col<-get_colormap(cmlist[[1]])
  ph=cmlist[[2]]
  
} else{
  
  co <- colorRampPalette(c(rep("red3", 1), "purple2", rep("purple", 2), rep("magenta2", 1), rep("red2", 2)),interpolate = "spline")
  col <- co(100) 
  ph=pi/2
  
}

circle_illusion(nvp,col,ph,title)