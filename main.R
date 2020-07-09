source("colormap.R")
source("colormap-AK.R")
source("util.R")
source("circle_illusion.R")


N<-100
nvp<-1  # number of view port
type<-2 # type of Fraser-Wilcox illusion
wr<-0.2  # width rate(type4=0.4, 2=0.2)
vr<-0.3 # value rate
sizerate<-1


test<-1

if(type==2 && test==1){mxwr<-0}else{mxwr<-1}

if (test==1){
  for (trn in seq(4, 15, by = 1)){
    for (wr in seq(0, mxwr, by = 0.1)) {
      for (vr in seq(0.1, 0.9, by = 0.1)) {
        
        title<-paste("testtype",as.character(type),"_trn",as.character(trn),"_wr",as.character(wr),"_vr",as.character(vr),".png",sep = "")
        
        cm=fwmap(type,N,wr,vr)
        #cmtitle<-paste("cm_",title,sep = "")
        #png(cmtitle)
        #plot(cmlist[[1]])
        #dev.off() 
        
        col<-get_colormap(cm)
        
        circle_illusion(nvp,col,title,sizerate,trn)
          
      }
    }
  }
} else {
  
  title<-paste("type",as.character(type),"_wr",as.character(wr),"_vr",as.character(vr),".png",sep = "")
  if(type!=5){
    
    
    
    cm=fwmap_AK(type,N,wr,vr)
    cmtitle<-paste("cm_",title,sep = "")
    png(cmtitle)
    plot(cm)
    dev.off() 
    
    col<-get_colormap(cm)
    
  } else{
    
    co <- colorRampPalette(c(rep("red3", 1), "purple2", rep("purple", 2), rep("magenta2", 1), rep("red2", 2)),interpolate = "spline")
    col <- co(100) 
    
  }
  
  circle_illusion(nvp,col,title,sizerate)
  

}