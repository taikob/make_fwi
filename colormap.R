sigmoid <-function(x,shif,lmd,mx){
 
  return(mx*(2/(1+exp(-lmd*(x-shif)))-1)) 
  
}

fwmap <-function(type,N,wr,vr){
  
  y <- c(rep(0,N))
  
  if (type==1){
    
    y[1       : (N/2)] <-as.integer(seq(  0,    vr*255,length=N/2))
    y[(N/2+1) : (N)  ] <-as.integer(seq(255,(1-vr)*255,length=N/2))
    
    return(list(y,pi/2))
    
  }
  
  if (type==21){
    
    ln<-as.integer(N*(1-wr)/2)#large rate number
    
    y[1      :ln      ] <-as.integer((1-vr)*255)
    y[(ln+1) :(N/2)   ] <-0
    y[(N/2+1):(N/2+ln)] <-as.integer(vr*255)
    y[(N/2+ln+1):(N)  ] <-255
    
    return(list(y,pi/2))
    
  }
  
  if (type==22){
    
    ln<-as.integer(N*(1-wr)/2)#large rate number
    
    y[1      :ln      ] <-0
    y[(ln+1) :(N/2)   ] <-as.integer(vr*255)
    y[(N/2+1):(N/2+ln)] <-255
    y[(N/2+ln+1):(N)  ] <-as.integer((1-vr)*255)
    
    return(list(y,pi/2))
    
  }
  
  if (type==3){
    
    ln<-as.integer(N/4)
    
    y[1       :   ln ] <-0
    y[(  ln+1):(2*ln)] <-as.integer(vr*255)
    y[(2*ln+1):(3*ln)] <-255
    y[(3*ln+1):N     ] <-as.integer((1-vr)*255)
    
    return(list(y,pi/2))
    
  }
  
  if (type==4){
    
    #wr=0.3
    #vr=0.8
    lmd=wr
    sn<-as.integer(N/4)#small rate number
    
    y[(1)       :(sn)    ] <-as.integer(   -sigmoid((1)       : (sn)   ,sn      ,lmd,as.integer((1-vr)*255)))
    y[(sn    +1):(N/2)   ] <-as.integer(    sigmoid((sn+1)    :(N/2)   ,sn      ,lmd,as.integer(   vr *255)))
    y[(N/2   +1):(N/2+sn)] <-as.integer(255+sigmoid((N/2+1) :(N/2+sn),(N/2+sn),lmd,as.integer((1-vr)*255)))
    y[(N/2+sn+1):(N)     ] <-as.integer(255-sigmoid((N/2+sn):(N)     ,(N/2+sn),lmd,as.integer(   vr *255)))
    
    return(list(y,pi/2))
    
  }
}