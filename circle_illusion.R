library(grid)

circle_illusion <-function(nvp,col,ph,title){
  
  N <- 1000
  id = rep(1:N, 3)
  ao <- embed(seq(0, 2 * pi, length = N + 1), 2)
  
  
  png(title,width=160,height=120)
  grid.newpage()
  l<- grid.layout(1,1,
                  heights=unit(c(1),c("snpc","snpc")), 
                  widths=unit(c(1),c("snpc","snpc")))
  pushViewport(viewport(layout=l))
  
  for (vx in 1:nvp) {
    for (vy in 1:1) {
      pushViewport(viewport(layout.pos.col = vx,
                            layout.pos.row = vy,
                            width =unit(1,'snpc'),
                            height=unit(1,'snpc')))
      for (ri in 1:9) {
        r <- (0.7^(-2:6))[ri]
        if (ri%%2!=0){a<-ao+ph} else{a<-ao}
        x <- r * c(rep(0, N), cos(a[, 1]), cos(a[, 2]))/2 + 0.5
        y <- r * c(rep(0, N), sin(a[, 1]), sin(a[, 2]))/2 + 0.5
        grid.polygon(x, y, id, gp = gpar(col = col, fill = col))
      }    
      popViewport()
    }
  }
  dev.off() 

}