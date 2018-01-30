#' Convert a SVG file to a spatstat psp object.
#'
#'@param svgfile File path of the svg file to convert.
#'@param owin Window of the psp.
#'@param clip.owin Should the returned psp clipped to owin.
#'@param rescale Should data recaled to owin size (otherwise expressed in pixels).
#'@param marks Add marks to segments:, F/0: no marks, T/1 all segments have a numeric mark (randomly distributed among segments), 2: all segments have a mark depending on their position in the svg file.
#'@param bezier What to do with Bezier curves. If NULL, bezier curves are converted to bezier polygons (i.e. goes through all control points). If numeric, Bezier curves are converted to linear segments, following the De Casteljau algorithm. \code{bezier}=0 or \code{bezier}=1 draws a straight line from the start of the bezier curve to its end, \code{bezier}=2 draws a straight line from the start to the middle of the curve and then to the end...
#'@param upward should all segments points upward.
#'@param rightward should all segments points to the right of the image.
#'@return A psp object
#'@export
svg2psp = function(svgfile,owin=NULL,bezier=0,marks=F,maxlength=NULL,connect = F, upward=T,rightward=F,conn.radius=NULL,conn.angle=NULL,clip.owin=F,rescale=!is.null(owin)) {

  marks=as.numeric(marks)

  datasvg = XML::htmlParse(svgfile)

  svgtranslate = c(0,0)
  svgscale = c(1,1)

  #Getting info from svgfile

  svgwidth = XML::xpathApply(datasvg, "//svg", XML::xmlGetAttr, "width")
  svgheight= XML::xpathApply(datasvg, "//svg", XML::xmlGetAttr, "height")

  svgunits = gsub("[0-9]","",svgwidth)
  svgwidth=as.numeric(gsub("[a-zA-Z]","",svgwidth))
  svgheight=as.numeric(gsub("[a-zA-Z]","",svgheight))

  if (!is.null(unlist(XML::xpathApply(datasvg, "//g", XML::xmlGetAttr, "transform")))) {
    svgtransform = unlist(strsplit(unlist(XML::xpathApply(datasvg, "//g", XML::xmlGetAttr, "transform"))," "))
    svgtranslate = svgtransform[grep("translate",svgtransform)]
    svgtranslate = as.numeric(unlist(strsplit(gsub(")","",gsub("translate(","",svgtranslate,fixed=T),fixed=T),",")))
    if (length(svgtranslate)==1) {svgtranslate = c(svgtranslate,0)}
    svgscale = svgtransform[grep("scale",svgtransform)]
    svgscale = as.numeric(unlist(strsplit(gsub(")","",gsub("scale(","",svgscale,fixed=T),fixed=T),",")))
    if (length(svgscale)==1) {svgscale = c(svgscale,svgscale)}
  }
  #operating on paths
  lpath = list()
  paths = XML::xpathApply(datasvg, "//path", XML::xmlGetAttr, "d")
  paths = gsub("([a-zA-Z])", " \\1 \\2", paths)

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
      }
      switch(let,
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
             "c" = {
               lgt = 6
               cp = cp +1
               if (marks == 1) {
                 m = m+1
               } else {
                 m = cp
               }
               oldpos = pos
               if (is.null(bezier)) {
                 b10 = pos + as.numeric(p[1:2])
                 datapoints[cp,] = c(oldpos,b10,m)
                 cp = cp+1
                 if (marks == 2) {
                   m = cp
                 }
                 b20 = pos + as.numeric(p[3:4])
                 datapoints[cp,] = c(b10,b20,m)
                 cp=cp+1
                 if (marks == 2) {
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

                     datapoints[cp,] = c(oldpos,pos,m)
                     cp = cp+1
                     if (marks == 2) {
                       m = cp
                     }
                   }
                   pos = pos
                   oldpos = pos
                 }
               }
               if (is.null(bezier) || bezier <= 1) {
                pos = pos+as.numeric(p[(lgt-1):lgt])
               } else {
                 pos = b30
               }
               datapoints[cp,] = c(oldpos,pos,m)
               if (length(p)> lgt) {
                 p = p[(lgt+1):length(p)]
               } else {
                 p=c()
               }
             },
             "C" = {
               lgt = 6
               cp = cp +1
               if (marks == 1) {
                 m = m+1
               } else {
                 m = cp
               }
               oldpos = pos
               if (is.null(bezier)) {
                 pos = as.numeric(p[1:2])
                 datapoints[cp,] = c(oldpos,pos,m)
                 cp = cp+1
                 if (marks == 2) {
                   m = cp
                 }
                 oldpos = pos
                 pos = as.numeric(p[3:4])
                 datapoints[cp,] = c(oldpos,pos,m)
                 cp=cp+1
                 if (marks == 2) {
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
                    pos= (1-t)*b02 + t*b12

                    datapoints[cp,] = c(oldpos,pos,m)
                    cp = cp+1
                    if (marks == 2) {
                      m = cp
                    }
                  }
                  oldpos = pos
                }
              }
              pos = as.numeric(p[(lgt-1):lgt])
              datapoints[cp,] = c(oldpos,pos,m)
              if (length(p)> lgt) {
                p = p[(lgt+1):length(p)]
              } else {
                p=c()
              }
             },
             "l" = {
               lgt = 2
               cp = cp +1
               if (marks == 1) {
                 m = m+1
               } else {
                 m = cp
               }
               oldpos = pos
               pos = pos+as.numeric(p[1:2])
               datapoints[cp,] = c(oldpos,pos,m)
               if (length(p)> lgt) {
                 p = p[(lgt+1):length(p)]
               } else {
                 p=c()
               }
             },
             "L" = {
               lgt = 2
               cp = cp +1
               if (marks == 1) {
                 m = m+1
               } else {
                 m = cp
               }
               oldpos = pos
               pos = as.numeric(p[1:2])
               datapoints[cp,] = c(oldpos,pos,m)
               if (length(p)> lgt) {
                 p = p[(lgt+1):length(p)]
               } else {
                 p=c()
               }
             },
             "z" = {
               closure=T
             },
             "Z" = {
               closure=T
             }
      )
    if (closure) {
      #        datapoints[cp+1,] = c(pos,datapoints[1,1:2])
    }
  }
  datapoints[,1] = svgtranslate[1] + svgscale[1]*datapoints[,1]
  datapoints[,2] = svgtranslate[2] + svgscale[2]*datapoints[,2]
  datapoints[,3] = svgtranslate[1] + svgscale[1]*datapoints[,3]
  datapoints[,4] = svgtranslate[2] + svgscale[2]*datapoints[,4]

  if (marks == 1) {
    datapoints$marks = rep(i,dim(datapoints)[1])
  }
  lpath[[i]] =datapoints

}

datapsp = spatstat::as.psp(lpath[[1]],window=spatstat::owin(c(0,svgwidth),c(0,svgheight)),check=F)
if (length(lpath)>1) {
  for (i in 2:length(lpath)) {
    datapsp = spatstat::superimpose(datapsp,spatstat::as.psp(lpath[[i]],window=spatstat::owin(c(0,svgwidth),c(0,svgheight)),check=F))
  }
}


if (!is.null(maxlength)) {
	datapsp = cut.psp(datapsp,maxlength)
}

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

if (connect) {
  datapsp = connectset.psp(datapsp,conn.radius=conn.radius,conn.angle=conn.angle)
}
if (!marks) {
  datapsp=spatstat::unmark(datapsp)
}
if (marks==1) {
  spatstat::marks(datapsp) = sample(1:datapsp$n)
}
  if (!is.null(owin)) {
    if (rescale) {
      datapsp = spatstat::rescale(datapsp,svgwidth/diff(owin$xrange))
    }
    if (clip.owin) {
      datapsp = datapsp[owin]
    }
  }

  datapsp= datapsp[which(spatstat::lengths.psp(datapsp)>0)]

	return(datapsp)
}
