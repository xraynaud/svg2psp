#' Adds marks to connected segments in a psp
#'@param psp spatstat psp object
#'@param conn.radius segments having ends separated than less then conn.radius will be assumed to belong to the same set
#'@param conn.angle maximum angle between segments to belong to the same set
connectset.psp = function(psp,conn.radius=0,conn.angle=pi/4) {
  require(spatstat)
  addmarks=F
  datapsp = selfcut.psp(psp)
  if (!is.marked(datapsp)) {
    addmarks=T
    marks(datapsp) = sample(1:datapsp$n)
  }
  if (!is.data.frame(marks(datapsp))) {
    marks(datapsp)=data.frame(marx = marks(datapsp), connect= rep(NA,datapsp$n))
  } else {
    if (!"connect" %in% names(marks(datapsp))) {
      marks(datapsp)$connect =  rep(NA,datapsp$n)
    }
  }
  for (i in 1:datapsp$n) {
    if(is.na(marks(datapsp[i])$connect)) {
      datapsp$marks$connect[i] = marks(datapsp[i])$marx
    }
    connected = c(
      which(abs(datapsp[i]$ends$x1 -datapsp$ends$x0)<=conn.radius & abs(datapsp[i]$ends$y1 -datapsp$ends$y0)<=conn.radius),
      which(abs(datapsp[i]$ends$x1 -datapsp$ends$x1)<=conn.radius & abs(datapsp[i]$ends$y1 -datapsp$ends$y1)<=conn.radius),
      which(abs(datapsp[i]$ends$x0 -datapsp$ends$x0)<=conn.radius & abs(datapsp[i]$ends$y0 -datapsp$ends$y0)<=conn.radius)
    )
    for (s in connected[connected!=i] ) {
      if (abs(abs(angles.psp(datapsp[i])-pi/2) - abs(angles.psp(datapsp[s])-pi/2)) <= conn.angle) {
        if (is.na(marks(datapsp)$connect[s])) {
          marks(datapsp)$connect[s] = marks(datapsp)$connect[i]
        } else {
          marks(datapsp)$connect[marks(datapsp)$connect == marks(datapsp)$connect[i]] = marks(datapsp)$connect[s]
        }
      }
    }
  }
  if (addmarks) {
    marks = marks(datapsp)$connect
    unmark(datapsp)
    marks(datapsp) = as.numeric(as.factor(marks))
  }
  return(datapsp)
}
