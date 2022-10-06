#' Image normalization with automated histogram adjustment
#'
#' Normalize image pixel intensities between 0 and 1
#'
#' @param x An array, or a 2D/3D matrix 
#' @param inputRange The targeted min and max pixel intensities
#' @param autoRange A boolean indicating if inputRange should be calculated automatically
#' @param step A numeric value, indicating probs from quantile option for inputRange automated calculation
#' @param RangeOnly If set to true, only calculated ranges are returned
#' @param separate Shall image frames normalized separately ?
#' 
#' @return An array, a matrix or an image with values ranged from 0 to 1
#'
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @examples
#' m = replicate(100, rnorm(1,mean=0,sd=0.5))
#' m.norm = autoNormalize(m,autoRange=F);m.autoNorm = autoNormalize(m,autoRange=T,step=10**-1)
#' plot(density(m));points(density(m.norm),type='l',col='red');points(density(m.autoNorm),type='l',col='blue')
#'
#' @export
#'

autoNormalize = function(x,inputRange,autoRange=T,RangeOnly=F,separate=T,step=5*10**-3){
  
  if(missing(x)){return('Cannot do it without a proper input!')}
  Di = dim(x);if(length(Di)>3){return('Matrix dimensions cannot be greater than 3')}
  missrg = missing(inputRange)
  #-----------------------------------------------------
  unit.fct=function(x,...){
    if(all(x==0)){
      if(RangeOnly){
        return('Cannot calculate range with constant input')
      }else{
        return(x)
      }
    }else{
      if(missrg & !autoRange){
        inputRange = c(min(x),max(x))
      }else{
        if(autoRange){
          Im.q = quantile(x,probs=seq(0,1,step))
          inputRange = Im.q[c(2,length(Im.q)-1)]
        }
      }
      if(RangeOnly){
        return(inputRange)
      }else{
        y = (x-inputRange[1])/(diff(inputRange))
        y[which(y<0)] = 0;y[which(y>1)] = 1
        return(y)
      }
    }
  }
  #-----------------------------------------------------
  if(!separate|length(Di)<3){
    return(unit.fct(x))
  }else{
    if(separate){
      res=lapply(1:dim(x)[3],function(i){
        return(unit.fct(x[,,i]))
      })
      if(RangeOnly){
        return(res)
      }else{
        return(do.call('abind',list(res,along=3)))
      }
    }
  }
}


  
