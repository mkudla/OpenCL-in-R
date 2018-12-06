##############################
## Example 0 - Serializing without OpenCL
##############################

#
# skeleton program - the simplest multicore
#
library(parallel)

k<-rnorm(1000000,mean=0,sd=1)

par2x<-function(x)
  return(2*x)

m<-mclapply(k, par2x, mc.cores=4)

#equivalent to:
m<-2*k

#
# more complex and demonstrates performance gain:
#

lognf<-function(x)
  return(log(x))

m<-mclapply(k, lognf, mc.cores=4)
#equivalent to:
m<-log(k)

system.time(mclapply(k, lognf, mc.cores=1))
system.time(mclapply(k, lognf, mc.cores=4))
hist(k, breaks=60)
hist(as.numeric(m), breaks=60)

##############################
## Example 1 - OpenCL skeleton program 
##############################

# lesson learned: use clFloat to pass arguments to kernel
# otherwise there is a possibility of hard-to-diagnose screw-up

library(OpenCL)
p=oclPlatforms()
d=oclDevices(p[[1]])

code1 = c(" 
          __kernel void calc1( 
          __global float* output, 
          const unsigned int count, 
          __global float* inputx, 
          __global float* inputy,
          const float alpha,
          const float beta
          ) 
          { 
          int i = get_global_id(0); 
          if(i < count){
          output[i] = (inputx[i]+inputy[i]) * alpha + beta;
          } 
          };")

ex.k.1 <- oclSimpleKernel(d[[1]], "calc1", code1, "single")

# embedding kernel in the function
oclf <- function(x, y, alpha = 1.0, beta=0.00, ...) {
  oclRun(ex.k.1, length(x), x, y, alpha, beta, ...)
}
# note the default values for alpha and beta
# now everything is ready to use oclf as a function on x and y (both same size)

#action 1
x<-clFloat(rnorm(1000000, mean=0, sd=1)) # normal distribution
y<-clFloat(rexp(1000000,rate = 0.5)) # exponential distributions
z<-oclf(x,y, alpha=3,beta=0.1) # changing defaults for alpha and beta

hist(as.numeric(x), breaks=60)
hist(as.numeric(y), breaks=60)
hist(z, breaks=60)

##############################
## Example 2 - Mandelbrot OpenCL Code 
##############################

## setup OpenCL 
library(OpenCL) 
p=oclPlatforms() 
d=oclDevices(p[[1]])

## kernel code (calculate mandelbrot) 
code2 = c(" 
          __kernel void calc1( 
          __global float* output, 
          const unsigned int count, 
          __global float* inputx, 
          __global float* inputy) 
          { 
          int i = get_global_id(0); 
          if(i < count){ 
          __private float tempx; 
          __private float tempy; 
          __private float temp; 
          tempx = inputx[i]; 
          tempy = inputy[i]; 
          temp = tempx; 
          for(__private int iter=1; iter<=100; iter++) 
          { 
          temp = tempx; 
          tempx = tempx * tempx - tempy * tempy + inputx[i]; 
          tempy = 2 * temp * tempy + inputy[i]; 
          } 
          output[i] = tempx*tempx+tempy*tempy; 
          } 
          };")


## compile kernel and embeed it into function 
k.calc2 <- oclSimpleKernel(d[[1]], "calc1", code2, "single") 
f <- function(x, y, ...) 
	oclRun(k.calc2, length(x), x, y)

## setup environment
size=4000 
iterations=100 
matxR<-matrix(rep(seq(-2,2,length.out=size),size),size) 
matyR<-t(matxR)
## use function 
image(matrix(f(matxR,matyR),size),zlim=c(-4,4))

############################# 
## Example 2 - Mandelbrot Conventional Code 
##############################
## setup environment 
size=4000 
iterations=100
matxR<-matrix(rep(seq(-2.5,1.5,length.out=size),size),size) 
matyR<-t(matxR)
g <-function(matxR, matyR) 
{ 
  iterx<-matxR 
  itery<-matyR 
  for(i in 1:iterations)  {   
    temp<-iterx   
    iterx<-iterx*iterx-itery*itery+matxR   
    itery<-2*temp*itery+matyR   
  } 
  return(iterx*iterx+itery*itery) 
}
## use function 
image(matrix(g(matxR,matyR),size),zlim=c(-4,4))


# benchmarking OpenCL in the Mandelbrot function
#

#mandelbrot OpenCL - 4000x4000 px, 100 iterations
# time: 0.224
system.time(f(matxR,matyR))
#mandelbrot R - 4000x4000 px, 100 iterations
# time: 39.896
system.time(g(matxR,matyR))
# 178x speed-up



##############################
## Example 3 - image manipulation
##############################

library(OpenCL)
p=oclPlatforms()
d=oclDevices(p[[1]])

code1 = c(" 
          __kernel void calc1( 
          __global float* output, 
          const unsigned int count, 
          __global float* inputx, 
          __global float* inputy,
          const int sizex,
          const int sizey
          ) 
          { 
          int i = get_global_id(0); 
          if(i < count){
          output[i] = (inputx[i]+inputy[i]); //* sizex + sizey;
          } 
          };")

ex.k.1 <- oclSimpleKernel(d[[1]], "calc1", code1, "single")

# embedding kernel in the function
oclf <- function(x, y, sizex = 1, sizey=1, ...) {
  oclRun(ex.k.1, length(x), x, y, sizex, sizey, ...)
}
# note the default values for alpha and beta
# now everything is ready to use oclf as a function on x and y (both same size)

#action 1
x<-clFloat(rnorm(1000000, mean=0, sd=1)) # normal distribution
y<-clFloat(rexp(1000000,rate = 0.5)) # exponential distributions
z<-oclf(x,y, sizex=3,sizey=4) # changing defaults for alpha and beta
