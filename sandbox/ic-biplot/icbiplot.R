##############################################################################################
# 2-Step minimization algorithm to obtain biplot for multivariate interval-censored data;
# Programmer: JPF. Groenen, S. Cecere;
# Port into icensBKL package:  Arnost Komarek
#
# n: number of individuals;
# m: number of variables measured in an interval-censored manner;
# p: chosen dimension of the solution  (default, p=2);
# Output: configurations X_{n\times p}, (individuals) and Y_{m\times p} (objects) ;
# Input data:
# Matrix L_{n \times m}:left-endpoints of the observed intervals;
# Matrix R_{n \times m}:right-endpoints of the observed intervals}; 
# p: number of dimension (default, p=2);
# seed: if required
#
#Output data:
#   1. X matrix stored in the current working directory;
#   2. Y matrix stored in the current working directory;
#   3. Biplot, by default;
##############################################################################################

icbiplot <- function(L, R, p = 2, MaxIter = 10000, tol = 1e-6, plotit = TRUE, seed = NULL, ...)
{
  # number of individuals
  n <- nrow(L)
  
  # number of variables
  m <- ncol(L) 
  L <- as.matrix(L)
  R <- as.matrix(R)
  
  #2-step iterative minimazation procedure
  #iter=0;
  #Initial values:
  if(length(seed) > 0) set.seed(seed)
  X <- matrix(runif(n*p), ncol = p,nrow = n, byrow = TRUE) 
  Y <- matrix(runif(m*p), ncol = p,nrow = m, byrow = TRUE)
  Hhat<- X %*% t(Y)
  
  #approximated event 
  H <- pmax(pmin(R, Hhat), L)

  #calculate singular value decomposition of H (matrix with entries approximated events)
  SVD <- svd(H)
  P <- SVD$u            #left eigenvectors;
  DiagSV <- diag(SVD$d) #singular values;
  Q <- SVD$v            #right eigenvectors
  
  #configuration of the n individuals (rows of X)
  X <- (n/m)^(0.25) * P[  ,1:p] %*% DiagSV[1:p, 1:p]^0.5
  
  #configuration of the m variables (rows of Y)
  Y <- (m/n)^(0.25) * Q[  ,1:p] %*% DiagSV[1:p,1:p]^0.5
  
  #Loss function at iteration -1;
  ss <- sum( (H - X%*%t(Y) )^2 )
  sold <- ss  + 2*tol;
  iter <- 0

  #Iterative minimazation of the loss function;
  while( ( (sold-ss)/ss > tol  && (iter < MaxIter) ) || (iter==0) ){        
    iter <- iter+1
    sold <- ss
    Hhat <- X %*% t(Y)
    H <- pmax(pmin(R,Hhat),L)
    SVD <- svd(H)
    P <- SVD$u
    DiagSV <- diag(SVD$d)
    Q <- SVD$v
    X <- (n/m)^(0.25) * P[,1:p] %*% DiagSV[1:p,1:p]^0.5
    Y <- (m/n)^(0.25)*Q[,1:p] %*% DiagSV[1:p,1:p]^0.5
    ss <- sum( (H - X%*%t(Y))^2 )
  }
             
  #Diagnostic measures
  #DAF
  DAF <- as.matrix(sum((DiagSV[1:p])^2)/sum(H^2))
  
  #Fit per variable;
  FpV <- matrix(0, 1, ncol(H))
  Hhat <- X%*%t(Y)
  for(k in 1:ncol(H)) FpV[,k] <- sum(Hhat[,k]^2)/sum(H[,k]^2)
            
  if(p == 2 && plotit){            #only if p=2 and plot required;
    xmin <- min(min(X[,1]), min(Y[,1]))
    xmax <- max(max(X[,1]), max(Y[,1]))
    ymin <- min(min(X[,2]), min(X[,2]))
    ymax <- max(max(X[,2]), max(Y[,2]))
    
    old.par <- par(c("plt", "pin", "xaxs", "yaxs"))
    on.exit(par(old.par))
    op <- TeachingDemos::squishplot(xlim = c(xmin, xmax), ylim = c(ymin, ymax), 1) 
    plot(c(xmin, xmax), c(ymin, ymax), type = 'n', ylab = '', xlab = '')
    points(X[,1], X[,2], cex = 0.5, col = 'snow4', pch = 4)
    points(Y[,1], Y[,2], cex = 0.8, pch = 19)
  }

  return(list(X = X, Y = Y, H = H, DAF = DAF, FpV = FpV,iter = iter))
}
