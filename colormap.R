sigmoid <-function(x,shif,lmd,mx){
 
  return(mx*(2/(1+exp(-lmd*(x-shif)))-1)) 
  
}

fwmap <-function(type,N,wr,vr){
  
  y <- c(rep(0,N))
  
  if (type==1){
    
    ln<-as.integer(N*wr/2)#large rate number

      
    y[1      :ln      ] <-0
    y[(ln+1) :(N/2)   ] <-as.integer(round(vr*255))
    y[(N/2+1):(N/2+ln)] <-255
    y[(N/2+ln+1):(N)  ] <-as.integer(round((1-vr)*255))
    
  } else if (type==2){
    
    y[1       : (N/2)] <-as.integer(round((seq(  0,    vr*255,length=N/2))))
    y[(N/2+1) : (N)  ] <-as.integer(round((seq(255,(1-vr)*255,length=N/2))))
    
  }
  
  return(y)

}