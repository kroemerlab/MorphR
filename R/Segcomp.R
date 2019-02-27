#' Save mask using rle algorithm
#'
#' 
#' @import EBImage 
#' @import S4Vectors 
#' @param Mask The image mask to be saved
#' @param ExportName The file saving location
#' @return NULL
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @export
#'

SaveSeg = function(Mask, ExportDir,FileName){
  
  if(missing(ExportDir)){ExportDir=getwd()}
  if(missing(FileName)){return('filename shall be provided!')}
  
  Stock = rle(c(Mask))
  writeLines(paste(ncol(Mask), paste(Stock$lengths,collapse='/'),
                   paste(Stock$values,collapse='/'), sep = '\n'),con = file.path(ExportDir,FileName))
  
}

#' Restore compressed mask
#'
#' 
#' @import EBImage 
#' @import S4Vectors 
#' @param fi File path
#' @return A matrix object
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @export
#' 

ReadSeg = function(fi){
  
  Raw = readLines(fi)
  Arg = lapply(Raw[2:3],function(x){
    y=unlist(strsplit(x,'/'))
    if(grepl('FALSE|TRUE',y[[1]])){
      y=as.logical(y)
    };return(as.numeric(y))
    })
  
  Comp = Rle(values = Arg[[2]], lengths = Arg[[1]])
  return(matrix(as.vector(Comp, mode="numeric"), ncol = as.numeric(Raw[1])))
  
}
