dc.polr = function(model, values = NULL, sim.count = 1000, conf.int = 0.95, sigma = NULL, set.seed = NULL, values1 = NULL, values2 = NULL){
  
  if(is.null(values) && (is.null(values1) || is.null(values2))){
    stop("Either values1 and values2 or values has to be specified!")
  }
  if(!is.null(values)){
    l = length(values)
    values1 = values[1 : (l/2)]
    values2 = values[(l/2 + 1) : l]
  }
  print(values1)
  # check for correct imput
  if(length(values1) != length(values2) && length(values2) != length(coefficients(model))){
    stop("values1 or values2 have different length then coef")
  }
  
  # initialize variables
  l = length(values1)
  n = sim.count
  if(is.null(sigma)){
    sigma = vcov(model)
  }
  level.count = length(model$lev)
  kappa.count = level.count - 1
  
  x = list()
  x[[1]] = matrix(values1,nrow=l,ncol=1,byrow=T) 
  x[[2]] = matrix(values2,nrow=l,ncol=1,byrow=T)
  
  draw = matrix(NA,nrow=n,ncol=l+kappa.count,byrow=T)
  beta = matrix(coef(model),nrow=1,ncol=l,byrow=T)
  zeta = matrix(model$zeta,nrow=1,ncol=kappa.count,byrow=T)
  estim = cbind(beta,zeta)
  b = matrix(NA,nrow=n,ncol=l,byrow=T)

  kappa = list()
  for(i in 1:kappa.count){
    kappa[[length(kappa)+1]] = matrix(NA,nrow=n,ncol=1,byrow=TRUE)
  }  
  delta = matrix(NA,nrow=n,ncol=level.count,byrow=TRUE)
  ev = matrix(NA,nrow=n,ncol=2*level.count,byrow=TRUE)
  
  # simulation
  if(!is.null(set.seed)){
    set.seed(set.seed)
  }
  draw[,] = MASS::mvrnorm(n,estim,sigma)
  b[,]<-draw[,1:l]
  for(i in 1:kappa.count){
    kappa[[i]][,] = draw[,l+i]
  }
  
  # calculate the discrete changes
  for (i in 1:n)
  {
    for(j in 1:level.count){
      for(k in 1:2){
        if(j == 1){
          ev[i,j+(k-1)*level.count] = exp(kappa[[j]][i,]-b[i,]%*%x[[k]])/(1+exp(kappa[[j]][i,]-b[i,]%*%x[[k]]))
        }else if(j == level.count){
          ev[i,j+(k-1)*level.count] = 1/(1+exp(kappa[[j-1]][i,]-b[i,]%*%x[[k]]))
        }else{
          ev[i,j+(k-1)*level.count] = exp(kappa[[j]][i,]-b[i,]%*%x[[k]])/(1+exp(kappa[[j]][i,]-b[i,]%*%x[[k]])) -
            exp(kappa[[j-1]][i,]-b[i,]%*%x[[k]])/(1+exp(kappa[[j-1]][i,]-b[i,]%*%x[[k]]))
        }
      }
      delta[i,j] = ev[i,j] - ev[i,j+level.count]
    }
  }
  
  # prepare the results
  upper = conf.int + (1 - conf.int)/2
  lower = (1 - conf.int)/2
  result = matrix(NA,nrow=level.count,ncol=9,byrow=T)
  for(i in 1:level.count){
    result[i,] = c(mean(ev[,i]),quantile(ev[,i],prob=c(lower,upper)),
                   mean(ev[,i+level.count]),quantile(ev[,i+level.count],prob=c(lower,upper)),
                   mean(delta[,i]),quantile(delta[,i],prob=c(lower,upper)))
  }
  colnames(result) = c("Mean1",paste0("1:",100*lower,"%"),paste0("1:",100*upper,"%"),"Mean2",paste0("2:",100*lower,"%"),paste0("2:",100*upper,"%"),"Mean.Diff",paste0("diff:",100*lower,"%"),paste0("diff:",100*upper,"%"))
  rownames(result) = model$lev
  
  return(result)
}