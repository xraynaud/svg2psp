#' @importFrom spatstat.geom angles.psp is.marked marks `marks<-`
#'   selfcut.psp unmark
NULL

#' Adds marks to connected segments in a psp
#'
#' @param psp spatstat \code{psp} object.
#' @param conn.radius,conn.angle segments will be assigned to belong
#'   to the same set if they have endings less than \code{conn.radius}
#'   units apart and if the form an angle less than \code{conn.angle}.
#' @examples
#' svgfile <- system.file("extdata", "SVG.svg", package = "svg2psp")
#' data <- svg2psp(svgfile,reverse = TRUE, rescale = TRUE)
#' plot(connectedsets.psp(data))
#' @export
connectedsets.psp <- function(psp, conn.radius = 0, conn.angle = 2 * pi) {
  addmarks <- FALSE
  datapsp <- selfcut.psp(psp)
  if (! is.marked(datapsp)) {
    addmarks <- TRUE
    marks(datapsp) <- sample(1:datapsp$n)
  }
  if (!is.data.frame(marks(datapsp))) {
    marks(datapsp) <- data.frame(marx = marks(datapsp),
                                 connect = rep(NA, datapsp$n))
  } else {
    if (!"connect" %in% names(marks(datapsp))) {
      marks(datapsp)$connect <- rep(NA, datapsp$n)
    }
  }
  for (i in 1:datapsp$n) {
    if (is.na(marks(datapsp[i])$connect)) {
      datapsp$marks$connect[i] <- marks(datapsp[i])$marx
    }
    connected <- c(
      which(abs(datapsp[i]$ends$x1 - datapsp$ends$x0) <= conn.radius &
              abs(datapsp[i]$ends$y1 - datapsp$ends$y0) <= conn.radius),
      which(abs(datapsp[i]$ends$x1 - datapsp$ends$x1) <= conn.radius &
              abs(datapsp[i]$ends$y1 - datapsp$ends$y1) <= conn.radius),
      which(abs(datapsp[i]$ends$x0 - datapsp$ends$x0) <= conn.radius &
              abs(datapsp[i]$ends$y0 - datapsp$ends$y0) <= conn.radius)
    )
    for (s in connected[connected != i]) {
      if (abs(abs(angles.psp(datapsp[i]) - pi / 2) -
                abs(angles.psp(datapsp[s]) - pi / 2)) <= conn.angle) {
        if (is.na(marks(datapsp)$connect[s])) {
          marks(datapsp)$connect[s] <-
            marks(datapsp)$connect[i]
        } else {
          mask <-
            marks(datapsp)$connect ==
            marks(datapsp)$connect[i]
          marks(datapsp)$connect[mask] <-
            marks(datapsp)$connect[s]
        }
      }
    }
  }
  if (addmarks) {
    marks <- marks(datapsp)$connect
    unmark(datapsp)
    marks(datapsp) <- as.factor(as.numeric(as.factor(marks)))
  }
  return(datapsp)
}
