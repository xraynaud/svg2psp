#' Adds marks to connected segments in a psp
#' 
#' @param psp spatstat psp object
#' @param conn.radius segments having ends separated than less then conn.radius will be assumed to belong to the same set
#' @param conn.angle maximum angle between segments to belong to the same set
#' @export
connectset.psp = function(psp,conn.radius=0,conn.angle=pi/4) {
  addmarks=F
  datapsp = spatstat::selfcut.psp(psp)
  if (!spatstat::is.marked(datapsp)) {
    addmarks=T
    spatstat::marks(datapsp) = sample(1:datapsp$n)
  }
  if (!is.data.frame(spatstat::marks(datapsp))) {
    spatstat::marks(datapsp)=data.frame(marx = spatstat::marks(datapsp), connect= rep(NA,datapsp$n))
  } else {
    if (!"connect" %in% names(spatstat::marks(datapsp))) {
      spatstat::marks(datapsp)$connect =  rep(NA,datapsp$n)
    }
  }
  for (i in 1:datapsp$n) {
    if(is.na(spatstat::marks(datapsp[i])$connect)) {
      datapsp$marks$connect[i] = spatstat::marks(datapsp[i])$marx
    }
    connected = c(
      which(abs(datapsp[i]$ends$x1 -datapsp$ends$x0)<=conn.radius & abs(datapsp[i]$ends$y1 -datapsp$ends$y0)<=conn.radius),
      which(abs(datapsp[i]$ends$x1 -datapsp$ends$x1)<=conn.radius & abs(datapsp[i]$ends$y1 -datapsp$ends$y1)<=conn.radius),
      which(abs(datapsp[i]$ends$x0 -datapsp$ends$x0)<=conn.radius & abs(datapsp[i]$ends$y0 -datapsp$ends$y0)<=conn.radius)
    )
    for (s in connected[connected!=i] ) {
      if (abs(abs(spatstat::angles.psp(datapsp[i])-pi/2) - abs(spatstat::angles.psp(datapsp[s])-pi/2)) <= conn.angle) {
        if (is.na(spatstat::marks(datapsp)$connect[s])) {
          spatstat::marks(datapsp)$connect[s] = spatstat::marks(datapsp)$connect[i]
        } else {
          spatstat::marks(datapsp)$connect[spatstat::marks(datapsp)$connect == spatstat::marks(datapsp)$connect[i]] = spatstat::marks(datapsp)$connect[s]
        }
      }
    }
  }
  if (addmarks) {
    marks = spatstat::marks(datapsp)$connect
    spatstat::unmark(datapsp)
    spatstat::marks(datapsp) = as.numeric(as.factor(marks))
  }
  return(datapsp)
}
