#' 	Cut a psp segments into smaller segments.
#'
#'@param datapsp PSP object to be cut
#'@param maxlength Maximum length of segments in the returned psp object
#'@return Returns a new psp object with segments length of \code{maxlength} a max.
#'#'@export
cut.psp = function(datapsp,maxlength) {

  pts = data.frame(x0 = NA, y0 = NA,x1 = NA,y1 = NA,mark=NA)
  n = 1
  for (i in 1:datapsp$n) {
    seg = datapsp[i]
    while (spatstat::lengths.psp(seg) > maxlength) {
      pts[n,]$mark = spatstat::marks(seg)
      pts[n,]$x0 = seg$ends$x0
      pts[n,]$y0 = seg$ends$y0
      x = (maxlength/spatstat::lengths.psp(seg))*( seg$ends$x1 -  seg$ends$x0) +  seg$ends$x0
      y = (maxlength/spatstat::lengths.psp(seg))*( seg$ends$y1 -  seg$ends$y0) +  seg$ends$y0
      pts[n,]$x1 = x
      pts[n,]$y1 = y
      seg$ends$x0 = x
      seg$ends$y0 = y
      n = n+1
    }
    pts[n,]$mark = spatstat::marks(seg)
    pts[n,]$x0 = seg$ends$x0
    pts[n,]$y0 = seg$ends$y0
    pts[n,]$x1 = seg$ends$x1
    pts[n,]$y1 = seg$ends$y1
    n=n+1
  }

  newpsp = spatstat::as.psp(pts[,1:4],check=F,window=spatstat::as.owin(datapsp))
  if (!is.null(spatstat::marks(datapsp))) {
    spatstat::marks(newpsp) = pts$mark
  }
  return(newpsp)
}
