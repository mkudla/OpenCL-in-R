# OpenCl in R by example

Marek Kudla

10/28/15

## INTRODUCTION

For laymen: OpenCL is a High-Throughput Computing language that enables of use of computation accelerators, like GPU or specialized cards to speed-up the computations. R is a statistical programming language that provides many built-in and verified statistical functions and plotting capabilities, yet until recently was single-threaded, so ill-suited to running computer-intensive simulations. With the advent of powerful GPUs and OpenCL it is now possible to bring them from supercomputers or multi-node clusters to single workstations.

Setting the OpenCL in R has many advantages. You can use computing power of OpenCL for number crunching, while still process input and output with high-level R facilities. Typical example would be generating random numbers in R using its distribution functions, processing them with OpenCL, then use R to plot the results. This set-up makes it possible for you to skip solving non-essential problems in C or C++ and focus on just writing the number-crunching code.

## INSTALLATION

I recommend using a 64-bit distribution of R, along with the RStudio, since it is a great IDE and saves time compared to original command line interface. On linux, you need to install OpenCL driver for the GPU you are using. Sometimes it is pretty complex and in the appendix I described more complex scenario. On mac, the system OpenCL libraries are already installed and the only thing left is to install OpenCL package for R by Simon Urbanek.

``` 
>install.packages("OpenCL")
Installing package into ‘/Users/marekkudla/Library/R/3.1/library’ (as ‘lib’ is unspecified)

package ‘OpenCL’ is available as a source package but not as a binary

Warning in install.packages :   package ‘OpenCL’ is not available (for R version 3.1.1) 
```
Ooops, there is no binary form available, but we can compile our own:
```
> install.packages("OpenCL", type="source")
```
then let’s load the library:
```
> library(OpenCL)
```
and check if it works:
```
> p=oclPlatforms()
> p
[[1]] OpenCL platform 'Apple'
> d=oclDevices(p[[1]]) 
> d
[[1]] OpenCL device 'Iris Pro' 
```

Ok, this is cool, because the Iris Pro in this case is 5200 of 4th gen Core processor, a modest GPU that should give about 700 GFLOPS/s. Currently the top of the line gives 8000 GFLOPS/s and processors give barely 100 GFLOPS/s.

## Simple kernel

Rememeber that, even though you are in R environment, you still need to program in OpenCL. You need to be familiar with C, but that is similar enough to R to learn quickly. The language is basically limited C99 with additional built-in math functions. You can use it to process various integers, floating points and even char (bitfield) types. The Simonek's OpenCL implementation has some limitations though: most important is that it is limited to floating point numbers, so strings are out of question. The other one is that while you can use multiple inputs, only one output. Those limitations could be worked around, but would require major effort, so perhaps it is better to move the tasks that would require it to C/C++ host programs. In this way, the R/OpenCL remains a platform for fast prototyping for floating point numbers. Let's create a simple example of calculations in OpenCL on R. And let's be a bit more ambitious than doing 2+2.

## Mandelbrot fractal

```
#############################
## Mandelbrot OpenCL Code 
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
## benchmark function 
system.time(f(matxR,matyR))
```
If you want to compare time, you can run the conventional version of the program.

```
############################# 
## Mandelbrot Conventional Code 
##############################

## setup environment 
size=4000 
iterations=10 
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
## benchmark function 
system.time(g(matxR,matyR))
```


