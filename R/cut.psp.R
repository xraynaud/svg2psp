#' @importFrom spatstat.geom as.owin as.psp lengths_psp marks
NULL

#' Cut a psp segments into smaller segments.
#'
#' @param x PSP object to be cut
#' @param maxlength Maximum length of segments in the returned psp object
#' @param ... Ignored
#' @return Returns a new psp object with segments length of \code{maxlength} a
#'   max.
#' @examples
#' svgfile = system.file("extdata", "SVG.svg", package = "svg2psp")
#' data = svg2psp(svgfile, reverse = TRUE, rescale = TRUE) # 217 segments
#' cutdata = cut(data,1) # 679 segments
#' @export
cut.psp <- function(x, maxlength, ...) {

  if (!inherits(x, "psp"))
    stop("'x' must be a psp object")
  datapsp <- as.psp(x)

  pts <- data.frame(x0 = NA, y0 = NA, x1 = NA, y1 = NA, mark = NA)
  n <- 1
  for (i in 1:datapsp$n) {
    seg <- datapsp[i]
    while (lengths_psp(seg) > maxlength) {
      pts[n, ]$mark <- marks(seg)
      pts[n, ]$x0 <- seg$ends$x0
      pts[n, ]$y0 <- seg$ends$y0
      x <-
        (maxlength / lengths_psp(seg)) *
        (seg$ends$x1 -  seg$ends$x0) +
        seg$ends$x0
      y <-
        (maxlength / lengths_psp(seg)) *
        (seg$ends$y1 -  seg$ends$y0) +
        seg$ends$y0
      pts[n, ]$x1 <- x
      pts[n, ]$y1 <- y
      seg$ends$x0 <- x
      seg$ends$y0 <- y
      n <- n + 1
    }
    pts[n, ]$mark <- marks(seg)
    pts[n, ]$x0 <- seg$ends$x0
    pts[n, ]$y0 <- seg$ends$y0
    pts[n, ]$x1 <- seg$ends$x1
    pts[n, ]$y1 <- seg$ends$y1
    n <- n + 1
  }

  newpsp <-
    as.psp(pts[, 1:4], check = FALSE,
           window = as.owin(datapsp))
  if (!is.null(marks(datapsp))) {
    marks(newpsp) <- pts$mark
  }
  return(newpsp)
}
