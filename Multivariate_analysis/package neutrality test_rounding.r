library(MASS);library(KernSmooth);library(corpcor);
#install.packages("corpcor")
#===================  NEUTRALITY TEST (AND ML ESTIMATION OF RHO_ST) ===============
#PROPORTIONALITY BETWEEN TWO MATRICES__________________________________________
#This tests the proportionality between two matrices by a likelihood ration test
#from two sample estimates (S1,S2) of covariance matrices (M1,M2) we test
#Ho: M2 = rho M1   (where rho is a scalar) against the alternative
#H1: M1 and M2 completely unrelated
#this funtion serves as a basis for the exact test between arbitrary k matrices (function k.prop)

#INPUT
#ni:  Vector (length 2) of df associated with each matrices (estimated from manova)
#S:  list of the two matrices estimates (Mean Squares from the manova)
#prints:  whether to print a comment when matrices have to be transformed to positive definite (by function mdf1)
#OUTPUT
#rho: rho estimate for S2=rho S1
#CI:  CI for rho
#LL: 2*log-likelihood ratio of HO versus H1
#DF:  df associated with the LR test
#M1: MLE for S1 (within) under proportionality
#M2: MLE for s2 (between) under proportionality
#pX:  p-val of the test H0 proportional versus H1 unrelated matrices

pairwise.prop<-function(ni,S,prints=F)
{
n1=ni[1];n2=ni[2];S1=S[[1]];S2=S[[2]];
nn=n1+n2;r1=n1/nn;r2=n2/nn;p=dim(S1)[1]
if(dim(S1)[1]!=dim(S2)[1]){print("error matrices of distinct dimensions")}
if(is.positive.definite(S1)==FALSE){S1=round(mdf1(S1,tol=1e-6,tol1=0.001), digits=12);if(prints){print("S1 transformed to positive definite, rounded")}}
if(is.positive.definite(S2)==FALSE){S2=round(mdf1(S2,tol=1e-6,tol1=0.001), digits=12);if(prints){print("S2 transformed to positive definite, rounded")}}
X=n1/n2*S1%*%solve(S2);f=eigen(X)$values;
rho=uniroot(function(r){sum(1/(1+r*f))-p*r2},c(0,1000))$root
M1=r1*S1+r2*S2*(1/rho);M2=rho*M1;
CI1=rho/(1+qnorm(0.025)*sqrt(2/(p*nn)*(1/r1+1/r2)));
CI2=rho/(1-qnorm(0.025)*sqrt(2/(p*nn)*(1/r1+1/r2)));
LL=n1*log(det(M1)/det(S1))+ n2*log(det(M2)/det(S2)) ;  #-2logLik
DF=(p^2+p-2)/2;pX=1-pchisq(LL,df=DF) ;   #pvalue de test chi² -2LL > attendu par diff param
list(rho=rho,CI=c(CI1,CI2),LL=LL,pX=pX,DF=DF,M1=M1,M2=M2);
}


#PROPORTIONALITY BETWEEN K MATRICES___________________________________________________________________
#same as pairwise.prop but for arbitrary number of matrices
#and with a Bartlett correction of the likelihood ratio to account for small df
#The implementation if based on (Eriksen 1987, theorems 3.2 & 6.1)
#the proportionality of all matrices to the M1 matrix is tested:
#Ho: M_i= rho_i*M1 i in [1,k] for the ith matrix
#H1: M_i all mutually unrelated
#NB: when k=2, pairwise.prop is used (quicker) to maximise likelihood
#otherwise (k>2) an algorithm is used to find the MLE

#INPUT
#same as pairwse.prop
#ni:  Vector (length 2) of df associated with each matrices (estimated from manova)
#S:  list of the two matrices estimates (Mean Squares from the manova)
#prints = logical if true, gives information on the matrix transformation used
#and on the time to convergence of the algorithm
#tol = stop criterion for the convergence of the algorithm (Eriksen 1987, theorem 3.2)

#OUTPUT
#So: the MLE for the first matrix (M1) under proportionality
#rho: a vector of the proportionality coefficient for each matrix (by definition rho[1]=1)
#CI: CI for each coefficient of proportionality rho_i (by definition CI[1]=0)
#LL: 2*log-likelihood ratio of HO versus H1
#DF: df associated with the LR test
#pX: p-value of the LR test H0 vs H1
#t1: Bartlett corrected value fo the statistic for the LR test t1=B1*LL (Eriksen 1987)
#pt1: associated Bartlett corrected p-value for the LR test

k.prop<-function(ni,S,prints=F,tol=1E-10)
{
nn=sum(ni);ai=ni/nn;So=S[[1]];p=dim(So)[1];k=length(S);
 for(i in 1:k)
  {if(is.positive.definite(S[[i]])==FALSE){S[[i]]=round(mdf1(S[[i]],tol=1e-6,tol1=0.001), digits=10);if(prints){print(paste("S[[",i,"]] transformed to positive definite and rounded"))}}}

if(k==2)
  {#exact MLE for pairs of matrices
  sol=pairwise.prop(ni,S);Sm=sol$M1;lambda=c(1,sol$rho);
  if(prints){print("exact computation for pairwise matrices")}  
  }
else
  {#algorithm for computing MLE for more than two matrices
  lambda=numeric(k);for (i in 1:k){lambda[i]=tr(S[[i]])/tr(So)};
  stop.cond=1;count=0;#convergence control
    while(stop.cond>tol)
     {lambda1=lambda;count=count+1;
     Sm=ai[[1]]*So;for(i in 2:k){Sm=Sm+ai[i]*S[[i]]/lambda[i]};
     for(i in 2:k){lambda[i]=1/p*tr(solve(Sm)%*%S[[i]])}
     stop.cond=abs(mean((lambda/lambda1)^2)-1)
     }
  if(prints){print(paste(count," iterations to convergence to level ",tol))}  
  }
#confidence intervals for the lambda[i] (4.14 p.119 Fury 1988)
 CI=list();CI[[1]]=c(1,1);
 for(i in 2:k)
 {
 c1=lambda[i]/(1-qnorm(0.025)*sqrt(2/(p*nn)*(1/ai[1]+1/ai[i])));
 c2=lambda[i]/(1+qnorm(0.025)*sqrt(2/(p*nn)*(1/ai[1]+1/ai[i])));
 CI[[i]]=c(c1,c2)
  }
#2 log-likelihood
 LL=ni[1]*log(det(Sm)/det(So));
 for(i in 2:k){LL=LL+ni[i]*log(det(lambda[i]*Sm)/det(S[[i]]))}
 DF=(k-1)*(p^2+p-2)/2;pX=1-pchisq(LL,df=DF) ;   #pvalue for asymptotic test

#Bartlett Correction (Eriksen 1987, p742) caution k = k-1 here
dum1=0;for(i in 1:k){dum1=dum1+logg(ni[i]*p,0)-logh(ni[i],p)};
logB=logh(nn,p)-logg(nn*p,0)+dum1;
B1=exp(logB/((k-1)/2*(p*(p+1)/2-1)));t1=LL/B1
DF=(k-1)*(p^2+p-2)/2;pt1=1-pchisq(t1,df=DF) ;   #pvalue for corrected test

return(list(So=Sm,rho=lambda,CI=CI,LL=LL,pX=pX,DF=DF,pt1=pt1,t1=t1))
}

#=================== EXTRA FUNCTIONS used in the test implementation ===========

#functions for Bartlett correction (p742 of Eriksen 1987)
#NB: we use log-transformations of functions g() and h() of Eriksen
#because R retreives infinity for too large values of the non transformed functions

logg<-function(n,j)
{1/2*log(2*pi)+log(n/2)*(n-j-1)/2-lgamma((n-j)/2)}
logh<-function(n,p)
{sum(logg(n,0:(p-1)))}

#matrix trace
tr<-function(matrice){sum(diag(matrice))};

mdf1<- function (m,tol,tol1)
{
#make.positive.definite function modified to avoid sphericity
#i.e. to get slightly distinct values for the eigenvaues that are zero in the untransformed matrix
    if (!is.matrix(m))
        m <- as.matrix(m)
    d <- dim(m)[1]
    if (dim(m)[2] != d)
        stop("Input matrix is not square!")
    es <- eigen(m)
    esv <- es$values
    if (missing(tol))
       tol <- d * max(abs(esv)) * .Machine$double.eps
    delta <- 2 * tol
    tau <- pmax(0, delta - esv)
    tau <- tau*runif(d,min = 1*(1-tol1),max = 1*(1+tol1))
    dm <- es$vectors %*% diag(tau, d) %*% t(es$vectors)
    return(m + dm)
}
