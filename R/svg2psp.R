#' Convert a SVG file to a spatstat psp object.
#'
#' @param file File path of the svg file to convert.
#' @param bezier Parameters for approximating bezier curves (see Details).
#' @param owin Specify a window for the \code{psp}. If \code{NULL}, use details in the SVG file.
#' @param marks Add marks to segments (see details).
#' @param connect Assign segments to sets depending on their distance and orientation.
#' @param upward,rightward directions of the segments (see details).
#' @param rescale rescale \code{psp} to the dimensions given in the svg.
#' @param reverse Define the position of the origin of the \code{psp}.
#' @param ... Arguments passed to \link{cut.psp} or \link{connectedsets.psp}.
#' @details This functions provide a way to import SVG files in R to use with spatstat. Only absolute and relative SVG paths \code{moveto}, \code{lineto}, and \code{curveto} (quadratic and cubic bezier) are implemented at the moment.
#' There seem to be a wide range of interpretations of the W3C SVG specifications, so import is not completely guaranteed. Package tested with SVG produced by \code{autotrace} (\url{http://autotrace.sourceforge.net/}) and \code{Inkscape} (\url{https://inkscape.org/}, Save as Plain SVG).
#'
#' Bezier quadratic and cubic curves are approximated using the De Casteljau algorithm. Quadratic bezier curves are first approximated by a cubic bezier curves. If \code{bezier}=0, bezier curves are converted to bezier polygons (i.e. goes through all control points). If \code{bezier}>0, Bezier curves are approximated by linear segments. The value of the parameter is the number of iterations used in the approximation (see \url{https://en.wikipedia.org/wiki/De_Casteljau's_algorithm} for details).
#'
#' The resulting \code{psp} can have marks attached. If \code{marks=1}, segments of the \code{psp} have a numeric mark depending on the SGV path they belong to. If \code{marks=2}, all segments have a numeric mark depending on the SVG command that created them. If \code{marks=3}, all segments have a unique mark (for debug purposes). Defaults to \code{marks=0} which results in an unmarked \code{psp}. If \code{marks} is a character, then marks will be read from the corresponding field in the svg file.
#'
#' If \code{connect=T}, the resulting \code{psp} is processed through \link{connectedsets.psp}. The resulting \code{psp} will be a marked psp with each mark corresponding to a set.
#'
#' Parameters \code{upward} and \code{rightward} allow to alter the orientation of segments of the \code{psp}. If both are \code{F}, detected segments have the orientation they have in the SVG file. If \code{upward=T}, the direction of of segments is flipped so that all segments will point towards increasing values of the \emph{y} direction. If \code{rightward=T}, the direction of of segments is flipped so that all segments will point towards increasing values of the \emph{x} direction.
#'
#' The \code{reverse} parameter determines the position of the origin of the \code{psp}. \code{reverse = F} (default) considers the origin at the bottom left of the SVG. \code{reverse=T} puts the origin of the image at the top left, as in SVG files.
#'
#' Some SVG file contains size information. \code{rescale} determines whether the resulting \code{psp} dimensions are expressed in pixel units (\code{rescale=F}, \emph{default}) or in the units of the SVG file (\code{rescale=T}, if available).
#' @return A (marked) \code{psp} object
#' @examples
#' #get file path of example data
#' svgfile = system.file("extdata","SVG.svg", package = "svg2psp")
#'
#' # reverse and rescale are set to true to use the "paper" dimension of the SVG.
#' data = svg2psp(svgfile,reverse=TRUE,rescale=TRUE)
#' plot(data)
#' @export
svg2psp = function(file,bezier=5,owin=NULL, marks=NULL, connect = FALSE, upward=FALSE,rightward=FALSE,reverse=TRUE,rescale=TRUE,...) {

  moreargs = list(...)

  svgtranslate = c(0,0)
  svgscale = c(1,1)
  svgunits=NULL
  svgowin=NULL

  datasvg = XML::htmlParse(file)
  #Getting info from svgfile
  svgviewbox = unlist(XML::xpathApply(datasvg, "//svg", XML::xmlGetAttr, "viewbox"))
  svgwidth = XML::xpathApply(datasvg, "//svg", XML::xmlGetAttr, "width")
  svgheight= XML::xpathApply(datasvg, "//svg", XML::xmlGetAttr, "height")
  svgunits = gsub("[0-9 \\.]","",svgwidth)
  svgwidth=as.numeric(gsub("[a-zA-Z]","",svgwidth))
  svgheight=as.numeric(gsub("[a-zA-Z]","",svgheight))

  if (!is.null(svgviewbox)) {
    svgviewbox = as.numeric(unlist(strsplit(svgviewbox," ")))
    svgscale = c(svgwidth/diff(svgviewbox[c(1,3)]),svgheight/diff(svgviewbox[c(2,4)]))
    svgowin=spatstat::owin(svgviewbox[c(1,3)],svgviewbox[c(2,4)])
  }

  if (!is.null(unlist(XML::xpathApply(datasvg, "//g", XML::xmlGetAttr, "transform")))) {
    svgtransform = unlist(strsplit(unlist(XML::xpathApply(datasvg, "//g", XML::xmlGetAttr, "transform"))," "))
    if (grepl("translate",svgtransform)) {
      svgtranslate = svgtransform[grep("translate",svgtransform)]
      svgtranslate = as.numeric(unlist(strsplit(gsub(")","",gsub("translate(","",svgtranslate,fixed=T),fixed=T),",")))
      if (length(svgtranslate)==1) {
        svgtranslate = c(svgtranslate,0)
      }
    }
    if(grepl("scale",svgtransform)) {
      svgscale = ifelse(svgtransform[grep("scale",svgtransform)],svgtransform[grep("scale",svgtransform)],svgscale)
      svgscale = as.numeric(unlist(strsplit(gsub(")","",gsub("scale(","",svgscale,fixed=T),fixed=T),",")))
      if (length(svgscale)==1) {
        svgscale = c(svgscale,svgscale)
      }
    }
  }
#operating on paths
  lpath = list()
  if (is.character(marks)) {
    segment_marks = unlist(XML::xpathApply(datasvg, "//path", XML::xmlGetAttr, marks)) ## get path id for (marks == 1 & svgmarks = T)
  }
  paths = XML::xpathApply(datasvg, "//path", XML::xmlGetAttr, "d")
  paths = gsub("([a-zA-Z])", " \\1 \\2", paths)
#  if (svgmarks & length(segment_marks) != length(paths)) {
#    segment_marks = 1:length(segment_marks)
#    warning("Number of segment id is different then number of paths in SVG file. Falling back to numbered marks")
#  }
  if (marks == 1) {
    segment_marks = 1:length(paths)
  }
  if (is.character(marks)) {
    marks=1
  }
  for (i in 1:length(paths)) {
    pos = c(0,0)
    p =  unlist(strsplit(unlist(strsplit(paths[[i]],"[, ]+")),"\n",fixed=T))
    p = p[1:length(p)]
    closure=F
    datapoints = data.frame(x1=NA,y1=NA,x2=NA,y2=NA,marks=NA)
    cp = 0
    m = 0
    while(length(p)>0) {
     islet = regexpr("[a-zA-Z]",p[1])[1]
      if (islet >0) {
        let = substr(p[1],islet,1)
        if (length(p) > 2) {
          p = p[2:length(p)]
        } else {
          p=c()
        }
      } else {
        if(let=="M") {
          let = "L"
        }
        if(let=="m") {
          let = "l"
        }
      }
      switch(let,
            # Dealing with start positions moveto (m/M)
             "M" = {
               pos = as.numeric(p[1:2])
               if (length(p)> 2) {
                 p = p[3:length(p)]
               } else {
                 continue = F
               }
             },
             "m" = {
               pos = pos + as.numeric(p[1:2])
               if (length(p)> 2) {
                 p = p[3:length(p)]
               } else {
                 continue = F
               }
             },
             # Dealing with lines lineto (l/L)
             "l" = {
               lgt = 2
               cp = cp +1
               m = switch(marks+1,
                          NA,
                      segment_marks[i],
                      m+1,
                      cp
                      )
               oldpos = pos
               pos = pos+as.numeric(p[1:lgt])
               datapoints[cp,] = list(oldpos[1],oldpos[2],pos[1],pos[2],m)
               if (length(p)> lgt) {
                 p = p[(lgt+1):length(p)]
               } else {
                 p=c()
               }
             },
             "L" = {
               lgt = 2
               cp = cp +1
               m = switch(marks+1,
                          NA,
                            segment_marks[i],
                            m+1,
                            cp
               )
               oldpos = pos
               pos = as.numeric(p[1:lgt])
               datapoints[cp,] = list(oldpos[1],oldpos[2],pos[1],pos[2],m)
               if (length(p)> lgt) {
                 p = p[(lgt+1):length(p)]
               } else {
                 p=c()
               }
             },
            # Dealing with vertical lines lineto (v/V)
            "v" = {
              lgt = 1
              cp = cp +1
              m = switch(marks+1,
                         NA,
                         segment_marks[i],
                         m+1,
                         cp
              )
              oldpos = pos
              pos = pos+c(0,as.numeric(p[1:lgt]))
              datapoints[cp,] = list(oldpos[1],oldpos[2],pos[1],pos[2],m)
              if (length(p)> lgt) {
                p = p[(lgt+1):length(p)]
              } else {
                p=c()
              }
            },
            "V" = {
              lgt = 1
              cp = cp +1
              m = switch(marks+1,
                         NA,
                         segment_marks[i],
                         m+1,
                         cp
              )
              oldpos = pos
              pos = c(pos[1],as.numeric(p[1:lgt]))
              datapoints[cp,] = list(oldpos[1],oldpos[2],pos[1],pos[2],m)
              if (length(p)> lgt) {
                p = p[(lgt+1):length(p)]
              } else {
                p=c()
              }
            },
            # Dealing with horizontal lines lineto (h/H)
            "h" = {
              lgt = 1
              cp = cp +1
              m = switch(marks+1,
                         NA,
                         segment_marks[i],
                         m+1,
                         cp
              )
              oldpos = pos
              pos = pos+c(as.numeric(p[1:lgt]),0)
              datapoints[cp,] = list(oldpos[1],oldpos[2],pos[1],pos[2],m)
              if (length(p)> lgt) {
                p = p[(lgt+1):length(p)]
              } else {
                p=c()
              }
            },
            "H" = {
              lgt = 1
              cp = cp +1
              m = switch(marks+1,
                         NA,
                         segment_marks[i],
                         m+1,
                         cp
              )
              oldpos = pos
              pos = c(as.numeric(p[1:lgt]),pos[2])
              datapoints[cp,] = list(oldpos[1],oldpos[2],pos[1],pos[2],m)
              if (length(p)> lgt) {
                p = p[(lgt+1):length(p)]
              } else {
                p=c()
              }
            },
             # Dealing with quadratic bezier curves (Q/q)
             "Q" = {
               lgt = 4
               cp = cp +1
               m = switch(marks+1,
                          NA,
                          segment_marks[i],
                          m+1,
                          cp
               )
               oldpos = pos
               if (bezier==0) {
                 b10 = pos + as.numeric(p[1:2])
                 datapoints[cp,] = list(oldpos[1],oldpos[2],b10[1],b10[2],m)
                 cp = cp+1
                 if (marks == 3) {
                   m = cp
                 }
                 b20 = pos + as.numeric(p[3:4])
                 datapoints[cp,] = list(b10[1],b10[2],b20[1],b20[2],m)
                 cp=cp+1
                 if (marks == 3) {
                   m = cp
                 }
                 oldpos = b20
               } else {
                 if (bezier>1) {
                   breaks = ((1:bezier)/bezier)[-bezier]
                   b00 = oldpos
                   b30 = as.numeric(p[3:4])
                   b10 = b00 + 2/3*(as.numeric(p[1:2])-b00)
                   b20 = b30 + 2/3*(as.numeric(p[1:2])-b30)

                   for (t in breaks) {
                     oldpos = pos
                     b01 = (1-t)*b00 + t*b10
                     b11 = (1-t)*b10 + t*b20
                     b21 = (1-t)*b20 + t*b30

                     b02 = (1-t)*b01 + t*b11
                     b12 = (1-t)*b11 + t*b21
                     pos = (1-t)*b02 + t*b12

                     datapoints[cp,] = list(oldpos[1],oldpos[2],pos[1],pos[2],m)
                     cp = cp+1
                     if (marks == 3) {
                       m = cp
                     }
                   }
                   pos = pos
                   oldpos = pos
                 }
               }
               if (bezier==0 || bezier <= 1) {
                 pos = pos+as.numeric(p[(lgt-1):lgt])
               } else {
                 pos = b30
               }
               datapoints[cp,] = list(oldpos[1],oldpos[2],pos[1],pos[2],m)
               if (length(p)> lgt) {
                 p = p[(lgt+1):length(p)]
               } else {
                 p=c()
               }
             },
             "q" = {
               lgt = 4
               cp = cp +1
               m = switch(marks+1,
                          NA,
                          segment_marks[i],
                          m+1,
                          cp
               )
               oldpos = pos
               if (bezier==0) {
                 b10 = pos + as.numeric(p[1:2])
                 datapoints[cp,] = list(oldpos[1],oldpos[2],b10[1],b10[2],m)
                 cp = cp+1
                 if (marks == 3) {
                   m = cp
                 }
                 b20 = pos + as.numeric(p[3:4])
                 datapoints[cp,] = list(b10[1],b10[2],b20[1],b20[2],m)
                 cp=cp+1
                 if (marks == 3) {
                   m = cp
                 }
                 oldpos = b20
               } else {
                 if (bezier>1) {

                   breaks = ((1:bezier)/bezier)[-bezier]

                   b00 = oldpos
                   b30 = pos + as.numeric(p[3:4])
                   b10 = b00 + 2/3*(as.numeric(p[1:2])-b00)
                   b20 = b30 + 2/3*(as.numeric(p[1:2])-b30)

                   for (t in breaks) {
                     oldpos = pos
                     b01 = (1-t)*b00 + t*b10
                     b11 = (1-t)*b10 + t*b20
                     b21 = (1-t)*b20 + t*b30

                     b02 = (1-t)*b01 + t*b11
                     b12 = (1-t)*b11 + t*b21
                     pos = (1-t)*b02 + t*b12

                     datapoints[cp,] = list(oldpos[1],oldpos[2],pos[1],pos[2],m)
                     cp = cp+1
                     if (marks == 3) {
                       m = cp
                     }
                   }
                   pos = pos
                   oldpos = pos
                 }
               }
               if (bezier==0 || bezier <= 1) {
                 pos = pos+as.numeric(p[(lgt-1):lgt])
               } else {
                 pos = b30
               }
               datapoints[cp,] = list(oldpos[1],oldpos[2],pos[1],pos[2],m)
               if (length(p)> lgt) {
                 p = p[(lgt+1):length(p)]
               } else {
                 p=c()
               }
             },
             #Dealing with cubic bezier curves (c/C)
             "c" = {
               lgt = 6
               cp = cp +1
               m = switch(marks+1,
                          NA,
                          segment_marks[i],
                          m+1,
                          cp
               )
               oldpos = pos
               if (bezier==0) {
                 b10 = pos + as.numeric(p[1:2])
                 datapoints[cp,] = list(oldpos[1],oldpos[2],b10[1],b10[2],m)
                 cp = cp+1
                 if (marks == 3) {
                   m = cp
                 }
                 b20 = pos + as.numeric(p[3:4])
                 datapoints[cp,] = list(b10[1],b10[2],b20[1],b20[2],m)
                 cp=cp+1
                 if (marks == 3) {
                   m = cp
                 }
                 oldpos = b20
               } else {
                 if (bezier>1) {

                   breaks = ((1:bezier)/bezier)[-bezier]

                   b00 = oldpos
                   b10 = pos + as.numeric(p[1:2])
                   b20 = pos + as.numeric(p[3:4])
                   b30 = pos + as.numeric(p[5:6])

                   for (t in breaks) {
                     oldpos = pos
                     b01 = (1-t)*b00 + t*b10
                     b11 = (1-t)*b10 + t*b20
                     b21 = (1-t)*b20 + t*b30

                     b02 = (1-t)*b01 + t*b11
                     b12 = (1-t)*b11 + t*b21
                     pos = (1-t)*b02 + t*b12

                     datapoints[cp,] = list(oldpos[1],oldpos[2],pos[1],pos[2],m)
                     cp = cp+1
                     if (marks == 3) {
                       m = cp
                     }
                   }
                   pos = pos
                   oldpos = pos
                 }
               }
               if (bezier==0 || bezier <= 1) {
                 pos = pos+as.numeric(p[(lgt-1):lgt])
               } else {
                 pos = b30
               }
               datapoints[cp,] = list(oldpos[1],oldpos[2],pos[1],pos[2],m)
               if (length(p)> lgt) {
                 p = p[(lgt+1):length(p)]
               } else {
                 p=c()
               }
             },
             "C" = {
               lgt = 6
               cp = cp +1
               m = switch(marks+1,
                          NA,
                          segment_marks[i],
                          m+1,
                          cp
               )
               oldpos = pos
               if (bezier==0) {
                 pos = as.numeric(p[1:2])
                 datapoints[cp,] = list(oldpos[1],oldpos[2],pos[1],pos[2],m)
                 cp = cp+1
                 if (marks == 3) {
                   m = cp
                 }
                 oldpos = pos
                 pos = as.numeric(p[3:4])
                 datapoints[cp,] = list(oldpos[1],oldpos[2],pos[1],pos[2],m)
                 cp=cp+1
                 if (marks == 3) {
                   m = cp
                 }
                 oldpos = pos
               } else {
                 if (bezier>1) {
                   breaks = ((1:bezier)/bezier)[-bezier]
                   b00 = oldpos
                   b10 = as.numeric(p[1:2])
                   b20 = as.numeric(p[3:4])
                   b30 = as.numeric(p[5:6])

                   for (t in breaks) {
                     oldpos = pos
                     b01 = (1-t)*b00 + t*b10
                     b11 = (1-t)*b10 + t*b20
                     b21 = (1-t)*b20 + t*b30

                     b02 = (1-t)*b01 + t*b11
                     b12 = (1-t)*b11 + t*b21
                     pos = (1-t)*b02 + t*b12

                     datapoints[cp,] = list(oldpos[1],oldpos[2],pos[1],pos[2],m)
                     cp = cp+1
                     if (marks == 3) {
                       m = cp
                     }
                   }
                   oldpos = pos
                 }
               }
               pos = as.numeric(p[(lgt-1):lgt])
               datapoints[cp,] = list(oldpos[1],oldpos[2],pos[1],pos[2],m)
               if (length(p)> lgt) {
                 p = p[(lgt+1):length(p)]
               } else {
                 p=c()
               }
             },
            #Dealing with closure (z/Z)
             "z" = {
               closure=T
             },
             "Z" = {
               closure=T
             },
            stop(paste("Unrecognized character in SVG path:", let))
      )
      if (closure) {
        datapoints[cp+1,] = list(pos[1],pos[2],datapoints[1,1],datapoints[1,2],m)
      }
    }
    datapoints[,1] = svgtranslate[1] + datapoints[,1]
    datapoints[,2] = svgtranslate[2] + datapoints[,2]
    datapoints[,3] = svgtranslate[1] + datapoints[,3]
    datapoints[,4] = svgtranslate[2] + datapoints[,4]

    lpath[[i]] =datapoints
  }
  # all points to produce the psp are now in lpath
  if (is.null(svgowin)) {
    svgowin = spatstat::owin(c(0,svgwidth),c(0,svgheight),unitname=svgunits)
  }
  datapsp = spatstat::as.psp(lpath[[1]],window=svgowin,check=F)
  if (length(lpath)>1) {
    for (i in 2:length(lpath)) {
      datapsp = spatstat::superimpose(datapsp,spatstat::as.psp(lpath[[i]],window=svgowin,check=F))
    }
  }
  # Datapsp contains the data.
  datapsp= datapsp[which(spatstat::lengths.psp(datapsp)>0)]

  # changing orientation of segments of cell borders to point upwards
  if(upward) {
    datapsp=spatstat::superimpose(
      datapsp[datapsp$ends$y1>=datapsp$ends$y0],
      spatstat::as.psp(cbind(x0=datapsp[datapsp$ends$y1<datapsp$ends$y0]$ends$x1,
                             y0=datapsp[datapsp$ends$y1<datapsp$ends$y0]$ends$y1,
                             x1=datapsp[datapsp$ends$y1<datapsp$ends$y0]$ends$x0,
                             y1=datapsp[datapsp$ends$y1<datapsp$ends$y0]$ends$y0),
                             marks =datapsp[datapsp$ends$y1<datapsp$ends$y0]$marks,
                             window=spatstat::as.owin(datapsp)))
  }
  if (rightward) {
    datapsp=spatstat::superimpose(
      datapsp[datapsp$ends$x1>=datapsp$ends$x0],
      spatstat::as.psp(cbind(x0=datapsp[datapsp$ends$x1<datapsp$ends$x0]$ends$x1,
                             y0=datapsp[datapsp$ends$x1<datapsp$ends$x0]$ends$y1,
                             x1=datapsp[datapsp$ends$x1<datapsp$ends$x0]$ends$x0,
                             y1=datapsp[datapsp$ends$x1<datapsp$ends$x0]$ends$y0),
                             marks =datapsp[datapsp$ends$x1<datapsp$ends$x0]$marks,
                             window=spatstat::as.owin(datapsp)))
  }
  if (marks==0) {
    datapsp=spatstat::unmark(datapsp)
  }
# if (marks==3) {
#    spatstat::marks(datapsp) = sample(1:datapsp$n)
#  }
  if (connect) {
    default.args = formals(connectedsets.psp)
    conn.radius = ifelse(is.null(moreargs$conn.radius),default.args$conn.radius,moreargs$conn.radius)
    conn.angle = ifelse(is.null(moreargs$conn.angle),eval(default.args$conn.angle),moreargs$conn.angle)
    datapsp = connectedsets.psp(datapsp,conn.radius=conn.radius,conn.angle=conn.angle)
  }
  if (reverse) {
    datapsp = spatstat::affine(datapsp,diag(c(1,-1)),c(0,diff(spatstat::as.owin(datapsp)$yrange)))
  }
  if (rescale) {
    datapsp = spatstat::affine(datapsp,diag(svgscale))
    datapsp = spatstat::rescale(datapsp,1,unit=svgunits)
  }
  if (!is.null(owin)) {
    datapsp = datapsp[owin]
  }

  if (!is.null(moreargs$maxlength)) {
    datapsp = cut.psp(datapsp,moreargs$maxlength)
  }


  return(datapsp)
}
