# pobj-tsx

This is a project to adopt lock elision to protected objects in Ada language.  
More details are available in *Lock Elision for Protected Objects Using Intel Transactional Synchronization Extensions*, S. Jeong, S. Yang, and B. Burgstaller, Ada-Europe 2017.  

*  Published paper: https://doi.org/10.1007/978-3-319-60588-3_8  
*  Pre-print: http://elc.yonsei.ac.kr/publications/POLockElision_preprint.pdf

## GNARL_PO_Elision.patch

A patch file to enable lock elision for GNU Ada Runtime Library (GNARL).  
GCC 6.3.0 is required to apply the patch.

To apply, put gcc-6.3.0 and the patch file together and type 
<code>patch -p0 --ignore-whitespace < ./GNARL_PO_Elision.patch</code>  
Make sure that GCC directory is named as <code>gcc-6.3.0</code>   

## Benchmarks
Following benchmarks are currently available.
* <code>DiningPhilosophers</code>
* <code>HashTable</code>
* <code>kmeans</code>

## PAPI wrapper
Under <code>lib</code>, PAPI_Binder exists to instrument Intel TSX.  
It can measure total transaction cycles, aborted cycles, commited cycles, and success rate.  
PAPI 5.5.1 is required to use.