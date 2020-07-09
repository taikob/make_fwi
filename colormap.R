sigmoid <-function(x,shif,lmd,mx){
 
  return(mx*(2/(1+exp(-lmd*(x-shif)))-1)) 
  
}

fwmap <-function(type,N,wr,vr){
  
  y <- c(rep(0,N))
  
  if (type==1){
    
    y[1       : (N/2)] <-as.integer(round((seq(  0,    vr*255,length=N/2))))
    y[(N/2+1) : (N)  ] <-as.integer(round((seq(255,(1-vr)*255,length=N/2))))
    
    return(list(y,pi/2))
    
  }
  
  if (type==21){
    
    sn<-as.integer(N*wr/2)#large rate number
    
    y[1      :sn      ] <-0
    y[(sn+1) :(N/2)   ] <-as.integer(round(vr*255))
    y[(N/2+1):(N/2+sn)] <-255
    y[(N/2+sn+1):(N)  ] <-as.integer(round((1-vr)*255))
    
    return(list(y,pi/2))
    
  }
  
  if (type==22){
    
    ln<-as.integer(N*(1-wr)/2)#large rate number
    
    y[1      :ln      ] <-0
    y[(ln+1) :(N/2)   ] <-as.integer(round(vr*255))
    y[(N/2+1):(N/2+ln)] <-255
    y[(N/2+ln+1):(N)  ] <-as.integer(round((1-vr)*255))
    
    return(list(y,pi/2))
    
  }
  
  if (type==3){
    
    ln<-as.integer(N/4)
    
    y[1       :   ln ] <-0
    y[(  ln+1):(2*ln)] <-as.integer(round(vr*255))
    y[(2*ln+1):(3*ln)] <-255
    y[(3*ln+1):N     ] <-as.integer(round((1-vr)*255))
    
    return(list(y,pi/2))
    
  }
  
  if (type==4){
    
    #wr=0.3
    #vr=0.8
    lmd=wr
    sn<-as.integer(N/4)#small rate number
    
    y[(       1):(sn)    ] <-as.integer(    round(sigmoid((1)       :(sn)    ,1    ,lmd,as.integer(round(   vr *255)))))
    y[(    sn+1):(N/2)   ] <-as.integer(255+round(sigmoid((sn+1)    :(N/2)   ,(N/2),lmd,as.integer(round((1-vr)*255)))))
    y[(N/2   +1):(N/2+sn)] <-as.integer(255-round(sigmoid((N/2+1)   :(N/2+sn),(N/2),lmd,as.integer(round(   vr *255)))))
    y[(N/2+sn+1):(N)     ] <-as.integer(   -round(sigmoid((N/2+sn+1):(N)     ,(N+1),lmd,as.integer(round((1-vr)*255)))))
    
    return(list(y,pi/2))
    
  }
}