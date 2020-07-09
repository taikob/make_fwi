library(grid)

circle_illusion <-function(nvp,col,title,sizerate,trn){
  
  N <- 100*trn
  id = rep(1:N, 3)
  ao <- embed(seq(0, 2 * pi, length = N + 1), 2)
  
  
  if (nvp!=1){
    
    png(title,width=(160*sizerate),height=(160*sizerate))
    grid.newpage()
    l<- grid.layout(nvp,nvp)
    clp<-TRUE
  
  } else {
    
    png(title,width=(160*sizerate),height=(120*sizerate))
    grid.newpage()
    l<- grid.layout(nvp,nvp,
                    heights=unit(rep(1, nvp),rep("snpc", nvp)), 
                    widths =unit(rep(1, nvp),rep("snpc", nvp)))
    clp<-FALSE
  }
  
  pushViewport(viewport(layout=l))
  
  for (vx in 1:nvp) {
    for (vy in 1:nvp) {
      pushViewport(viewport(layout.pos.col = vx,
                            layout.pos.row = vy,
                            clip=clp))
      for (ri in 1:9) {
        r <- (0.7^(-2:6))[ri]
        if (ri%%2!=0){a<-ao+(pi/(trn))} else{a<-ao}
        x <- r * c(rep(0, N), cos(a[, 1]), cos(a[, 2]))/2 + 0.5
        y <- r * c(rep(0, N), sin(a[, 1]), sin(a[, 2]))/2 + 0.5
        grid.polygon(x, y, id, gp = gpar(col = col, fill = col))
      }    
      popViewport()
    }
  }
  dev.off() 

}