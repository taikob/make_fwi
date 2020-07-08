sigmoid <-function(x,shif,lmd,mx){
 
  return(mx*(2/(1+exp(-lmd*(x-shif)))-1)) 
  
}

fwmap <-function(type,N,wr,vr){
  
  y <- c(rep(0,N))
  
  if (type==1){
    
    #wr=0.02
    #vr=0.5
    sn<-as.integer(N*wr/4)#small rate number
    
    y[1         :sn      ] <-255
    y[(sn+1)    :(N/2-sn)] <-as.integer(seq(0,vr*255,length=N/2-2*sn))
    y[(N/2-sn+1):(N/2)   ] <-255
    
    y[(N/2+1)   :(N/2+sn)] <-0
    y[(N/2+sn+1):(N-sn)  ] <-as.integer(seq(255,(1-vr)*255,length=N/2-2*sn))
    y[(N-sn+1)  :(N)     ] <-0
    
    return(list(y,pi/2))
    
  }
  
  if (type==21){
    
    #wr=0.08
    #vr=0.7
    ln<-as.integer(N*(1-wr)/4)#large rate number
    
    y[1         :ln      ] <-as.integer(vr*255)
    y[(ln+1)    :(N/2-ln)] <-0
    y[(N/2-ln+1):(N/2)   ] <-as.integer((1-vr)*255)
    
    y[(N/2+1)   :(N/2+ln)] <-as.integer(vr*255)
    y[(N/2+ln+1):(N-ln)  ] <-255
    y[(N-ln+1)  :(N)     ] <-as.integer((1-vr)*255)
    
    return(list(y,pi/4))
    
  }
  
  if (type==22){
    
    #wr=0.08
    #vr=0.7
    ln<-as.integer(N*(1-wr)/4)#large rate number
    
    y[1         :ln      ] <-0
    y[(ln+1)    :(N/2-ln)] <-as.integer(vr*255)
    y[(N/2-ln+1):(N/2)   ] <-255
    
    y[(N/2+1)   :(N/2+ln)] <-0
    y[(N/2+ln+1):(N-ln)  ] <-as.integer((1-vr)*255)
    y[(N-ln+1)  :(N)     ] <-255
    
    return(list(y,pi/4))
    
  }
  
  if (type==3){
    
    #vr=0.85
    nn<-as.integer(N/4)#large rate number
    
    y[1       :nn    ] <-0
    y[(nn+1)  :(N/2) ] <-as.integer((1-vr)*255)
    y[(N/2+1) :(N-nn)] <-as.integer(vr*255)
    y[(N-nn+1):N     ] <-255
    
    return(list(y,pi/2))
    
  }
  
  if (type==4){
    
    #wr=0.3
    #vr=0.8
    sn<-as.integer(N*wr/4)#small rate number
    ln<-as.integer(N*(1-wr)/4)#large rate number
    lmd=wr
    
    y[1            :(N/2-ln-sn)] <- as.integer(-sigmoid(1:(N/2-ln-sn)          ,(N/2-ln-sn),lmd,as.integer(vr    *255)))
    y[(N/2-ln-sn+1):(N/2)      ] <- as.integer( sigmoid((N/2-ln-sn+1):(N/2)    ,(N/2-ln-sn),lmd,as.integer((1-vr)*255)))
    
    y[(N/2+1)      :(N-ln-sn)  ] <- as.integer(255+sigmoid((N/2+1)      :(N-ln-sn),(N-ln-sn),lmd,as.integer((1-vr)*255)))
    y[(N-ln-sn+1)  :(N)        ] <- as.integer(255-sigmoid((N-ln-sn+1)  :(N)      ,(N-ln-sn),lmd,as.integer((vr*255))))
    
    return(list(y,pi/4))
    
  }
}