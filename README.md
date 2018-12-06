# Demonstration of OpenCL in R using Simon Urbanek's OpenCL library

This is a write-up and example code illustrating the use of GPU/CPU acceleration for serious number crunching in R. Nvidia and AMD cards can be 10-20 x faster than CPU in some tasks. However it requires from the user to wite th computational kernel in C-like code.

## Requirements

Simon Urbanek OpenCL R package with its dependencies (that requires OpenCL library to be installed, you need to pick up the correct one from your GPU or CPU).
