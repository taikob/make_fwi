get_colormap <-function(vec){
  
  l=length(vec)
  cmap <- c(rep(0,l))
  for (i in 1:l) {
    chr=sprintf("%02X", vec[i])
    cmap[i]<-paste("#",chr,chr,chr,sep = "")
  }
  return(cmap)
  
}