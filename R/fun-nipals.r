#' @title Augmented implementation of non-linear iterative PLS
#' 
#' @description This function replicates [ade4::nipals()] with options to retain
#'   elements useful to the [tbl_ord] class and biplot calculations.
#' 

#' @name nipals-ord
#' @include ord-tbl.r
#' @inheritParams ade4::nipals
#' @param ret.cmeans,ret.csd Logical; whether to retain as attributes the column
#'   means and standard deviations used to scale the matrix before running the
#'   nipals algorithm.
NULL

#' @rdname lda-ord
#' @export
nipals_ord <- function(df, nf = 2, rec = FALSE, niter = 100, tol = 1e-9,
                       ret.cmeans = TRUE, ret.csd = TRUE) {
  # df est un data frame contenant eventuellement des valeurs manquantes (NA)
  # nf nombre de facteurs a conserver
  # rec, si rec=T, la reconstitution des donnees sur les nf premiers axes est
  # realisee
  # ****************************************************************************
  # df is a data frame which can contain missing values (NA)
  # nf number of axes to keep
  # rec, if rec=T, data recontsitution is performed with the nf first axes
  # n.max.iter= maximum number of iterations
  
  df <- data.frame(df)
  tol<-1e-9 # tol pour la convergence
  nc <- ncol(df)
  nr <- nrow(df)
  nr.na <- apply(df, 2, function(x) sum(!is.na(x)))
  if (rec)
    x<-list(li=matrix(0,nr,nf),c1=matrix(0,nc,nf),co=matrix(0,nc,nf),
            eig=rep(0,nf),nb=rep(0,nf),rec=matrix(0,nr,nc))
  else
    x<-list(li=matrix(0,nr,nf),c1=matrix(0,nc,nf),co=matrix(0,nc,nf),
            eig=rep(0,nf),nb=rep(0,nf))
  row.names(x$c1)<-names(df)
  row.names(x$co)<-names(df)
  row.names(x$li)<-row.names(df)
  
  #X<-scale(df, center=T, scale=T, na.rm=TRUE)
  cmeans <- colMeans(df, na.rm=TRUE)
  csd <- apply(df, 2, sd, na.rm=TRUE) * sqrt((nr.na - 1) / nr.na)
  X <- sweep(sweep(df, 2, cmeans, "-"), 2, csd, "/")
  x$tab<-X
  for (h in 1:nf) {
    th<-X[,1]
    ph1<-rep(1/sqrt(nc),nc)
    ph2<-rep(1/sqrt(nc),nc)
    diff<-rep(1,nc)
    nb<-0
    while (sum(diff^2, na.rm=TRUE)>tol & nb<=niter) {
      for (i in 1:nc) {
        the<-th[!is.na(X[,i])]
        ph2[i]<-sum(X[,i]*th, na.rm=TRUE)/sum(the*the,na.rm=TRUE)
      }
      ph2<-ph2/sqrt(sum(ph2*ph2,na.rm=TRUE))
      for (i in 1:nr) {
        ph2e<-ph2[!is.na(X[i,])]
        th[i]<-sum(X[i,]*ph2, na.rm=TRUE)/sum(ph2e*ph2e,na.rm=TRUE)
      }
      diff<-ph2-ph1
      ph1<-ph2
      nb<-nb+1
    }
    if(nb>niter) stop(paste("Maximum number of iterations reached for axis", h))
    X<-X-th%*%t(ph1)
    # nombre d'iterations (number of iterations)
    x$nb[h]<-nb
    # coordonnees des lignes (row coordinates)
    x$li[,h]<-th
    # coordonnees des colonnes de variance unit
    # (columns coordinates of unit variance)
    x$c1[,h]<-ph1
    # valeurs propres (pseudo-eigenvalues)
    x$eig[h]<-sum(th*th,na.rm=TRUE)/(nr-1)
    # coord. col. de variance lambda (column coordinates of variance lambda)
    x$co[,h]<-x$c1[,h]*sqrt(x$eig[h])
    
  }
  if (rec) {
    for (h in 1:nf) {
      # tableau reconstitue (reconstitued data)
      x$rec<-x$rec+x$li[,h]%*%t(x$c1[,h])
    }
  }
  if (rec){
    x$rec=as.data.frame(x$rec)
    names(x$rec)<-names (df)
    row.names(x$rec)<-row.names(df)
  }
  
  x$call<-match.call()
  x$nf<-nf
  class(x)<-"nipals"
  if (any(diff(x$eig)>0))
    warning("Eigenvalues are not in decreasing order.",
            " Results of the analysis could be problematics")
  
  if (ret.cmeans) attr(x, "cmeans") <- cmeans
  if (ret.csd) attr(x, "csd") <- csd
  
  return(x)
}

