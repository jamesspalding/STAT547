##Generates two sets of data that are a specified misoriention angle (ANG) apart
##and stores them in arrays A and B. 

##Load packages
library(CircStats)

##Load some needed functions
trace=function(P){sum(diag(P))}

Mis.Ang=function(O,P){
  acos((trace(t(O)%*%P)-1)/2)
}

Q=function(a,b,g){matrix(c(cos(a)*cos(g)-sin(a)*sin(g)*cos(b),-cos(a)*sin(g)-sin(a)*cos(g)*cos(b),sin(a)*sin(b),sin(a)*cos(g)+cos(a)*sin(g)*cos(b),-sin(a)*sin(g)+cos(a)*cos(g)*cos(b),-cos(a)*sin(b),sin(g)*sin(b),cos(g)*sin(b),cos(b)),nrow=3,ncol=3)}

S=function(x,y,z,u,v,w,theta){
c(u*(u*x+v*y+w*z)+(x*(v^2+w^2)-u*(v*y+w*z))*cos(theta)+(v*z-w*y)*sin(theta),v*(u*x+v*y+w*z)+(y*(u^2+w^2)-v*(u*x+w*z))*cos(theta)+(w*x-u*z)*sin(theta),w*(u*x+v*y+w*z)+(z*(u^2+v^2)-w*(u*x+v*y))*cos(theta)+(u*y-v*x)*sin(theta))}

Qinv=function(Q){
  b=acos(Q[3,3])
  if(b==0){
    g=0
    a=acos(Q[1,1])}
  else{
    y=Q[1,3]
    x=Q[2,3]
    g=atan(y/x)
    if (y/x>0) {g=ifelse(x>0,g,g+pi)}
    if (y/x<0) {g=ifelse(x<0,g+pi,g+2*pi)}
    x=Q[3,2]
    y=Q[3,1]
    a=atan(-y/x)
    if (-y/x>0) {a=ifelse(-x>0,a,a+pi)}
    if (-y/x<0) {a=ifelse(-x<0,a+pi,a+2*pi)}}
  return(c(a,b,g))}


##Sets up the 2 population mean matrices. N is the identity matrix and T will be
##rotated about the identity matrix by user-specified misorientation angle, ANG.

ANG=.02 #change as needed

Nor=rnorm(3)
N1=Nor[1]
N2=Nor[2]
N3=Nor[3]
U=N1/sqrt(N1^2+N2^2+N3^2)
V=N2/sqrt(N1^2+N2^2+N3^2)
W=N3/sqrt(N1^2+N2^2+N3^2)

T=matrix(c(S(1,0,0,U,V,W,ANG),S(0,1,0,U,V,W,ANG),S(0,0,1,U,V,W,ANG)),nrow=3)
N=matrix(c(1,0,0,0,1,0,0,0,1),nrow=3)


##Generate two sets of data. Store them in array's called A and B.

##Array A (population mean at identity matrix N as specified by Euler angles
## of a=0, b=0, g=0).
a=0
b=0
g=0
k=5 #change for each case
n=10 #change for each case
thetarot=rvm(n,0,k)
Z=X=Y=vecs=matrix(nrow=n,ncol=3)
for(i in 1:n){
	Nor=rnorm(3)
	N1=Nor[1]
	N2=Nor[2]
	N3=Nor[3]
	U=N1/sqrt(N1^2+N2^2+N3^2)
	V=N2/sqrt(N1^2+N2^2+N3^2)
	W=N3/sqrt(N1^2+N2^2+N3^2)
	vecs[i,]=c(U,V,W)
	X[i,]=Q(a,b,g)%*%S(1,0,0,U,V,W,thetarot[i])
	Y[i,]=Q(a,b,g)%*%S(0,1,0,U,V,W,thetarot[i])
	Z[i,]=Q(a,b,g)%*%S(0,0,1,U,V,W,thetarot[i])
}
A=array(dim=c(3,3,n))
for(i in 1:n){A[,,i]=matrix(c(X[i,],Y[i,],Z[i,]),nrow=3)}

##Array B (population mean at matrix T as specified by Euler angles a,b,g
## extracted from T using the Qinv function)
a=Qinv(T)[1]
b=Qinv(T)[2]
g=Qinv(T)[3]
k=5 #change for each case
n=10 #change for each case
thetarot=rvm(n,0,k)
Z=X=Y=vecs=matrix(nrow=n,ncol=3)
for(i in 1:n){
	Nor=rnorm(3)
	N1=Nor[1]
	N2=Nor[2]
	N3=Nor[3]
	U=N1/sqrt(N1^2+N2^2+N3^2)
	V=N2/sqrt(N1^2+N2^2+N3^2)
	W=N3/sqrt(N1^2+N2^2+N3^2)
	vecs[i,]=c(U,V,W)
	X[i,]=Q(a,b,g)%*%S(1,0,0,U,V,W,thetarot[i])
	Y[i,]=Q(a,b,g)%*%S(0,1,0,U,V,W,thetarot[i])
	Z[i,]=Q(a,b,g)%*%S(0,0,1,U,V,W,thetarot[i])
}
B=array(dim=c(3,3,n))
for(i in 1:n){B[,,i]=matrix(c(X[i,],Y[i,],Z[i,]),nrow=3)}

rm(list=setdiff(ls(), c("A","B"))) #leaves only desired values