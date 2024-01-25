library(CircStats)
library(rgl)

Q=function(a,b,g){matrix(c(cos(a)*cos(g)-sin(a)*sin(g)*cos(b),-cos(a)*sin(g)-sin(a)*cos(g)*cos(b),sin(a)*sin(b),sin(a)*cos(g)+cos(a)*sin(g)*cos(b),-sin(a)*sin(g)+cos(a)*cos(g)*cos(b),-cos(a)*sin(b),sin(g)*sin(b),cos(g)*sin(b),cos(b)),nrow=3,ncol=3)}

S=function(x,y,z,u,v,w,theta){
  c(u*(u*x+v*y+w*z)+(x*(v^2+w^2)-u*(v*y+w*z))*cos(theta)+(v*z-w*y)*sin(theta),v*(u*x+v*y+w*z)+(y*(u^2+w^2)-v*(u*x+w*z))*cos(theta)+(w*x-u*z)*sin(theta),w*(u*x+v*y+w*z)+(z*(u^2+v^2)-w*(u*x+v*y))*cos(theta)+(u*y-v*x)*sin(theta))}

###a,b,g are called Euler angles.  Euler angles are one way to represent a 3x3 orthogonal matrix more compactly by just 3 numbers (you can look at the Wikipedia
###page for Euler angles if you are interested in any details).  When put into the function Q above they will produce a 3x3 matrix that
###provides the center for the orientations.  When a=b=g=0 the function Q(a,b,g) will give you back the identity matrix.
###a can vary from 0 to 2*pi, b can vary from 0 to pi, and g can vary from 0 to 2*pi

###k is a concentration parameter that controls the spread of the data points with higher values of k giving data points that are more
###spread about the center at Q(a,b,g).  You can play around with various k values to see how the spread differs.

a=2
b=3
g=1
k=5
n=100
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
P=array(dim=c(3,3,n))
for(i in 1:n){P[,,i]=matrix(c(X[i,],Y[i,],Z[i,]),nrow=3)}


##The code below plots the n data matrices.
###x,y,z represent the axes that would make the identity matrix
open3d()
text3d(0,0,1.5,text="z")
text3d(0,1.5,0,text="y")
text3d(1.5,0,0,text="x")
lines3d(c(0,1.5),c(0,0),c(0,0))
lines3d(c(0,0),c(0,1.5),c(0,0))
lines3d(c(0,0),c(0,0),c(0,1.5))
spheres3d(0,0,0,1,col=gray(.9))
for(i in 1:n){
  points3d(P[1,1,i],P[2,1,i],P[3,1,i],size=5,col=1)
  points3d(P[1,2,i],P[2,2,i],P[3,2,i],size=5,col=2)
  points3d(P[1,3,i],P[2,3,i],P[3,3,i],size=5,col=3)}
