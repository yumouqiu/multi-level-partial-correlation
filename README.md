This repository provides the codes for the paper "Inference on Multi-level Brain Connectivities based on fMRI Data" by Yumou Qiu and Xiao-Hua Zhou.

**PC-individual.txt** provides the function *individual(X, ...)* to estimate the locations of nonzero off-diagonal elements in a p by p partial correlation matrix, based on p-dimensional time series data *X* of size n. This function can be used to estimate the nonzero partial correlations among brain regions of an individual using the fMRI data.  

The output *cbind(recovery, Liu)* is two p by p binary (0-1) matrices combining together by columns, where *recovery* is the output of the proposed method with 1 and 0 indicating significant nonzero and zero partial correlations respectively, and *Liu* is the output of the method for time independent data from the paper "Gaussian graphical model estimation with false discovery rate control" by Liu, W., The Annals of Statistics, 41, 2948–2978, 2013.

**PC-population-1Sample.txt** provides the function *population(Z, ...)* to estimate the population level partial correlation matrix based on p-dimensional time series data of size n from m individuals, where the input *Z* is a three-dimensional array of size n by p by m.  

The output *cbind(recovery, Liu, MinPv)* is three p by p binary (0-1) matrices combining together by columns, with 1 and 0 indicating significant nonzero and zero population level partial correlations, respectively. Here, *recovery* is the output of the proposed method, and *Liu* and *MinPv* are the outputs from two existing multiple testing procedures based on subject level estimated partial correlations. 

**PC-population-2Sample.txt** provides the function *population2sample(Z1, Z2, ...)* to test for the differences of population level partial correlation matrices from two groups. The input and output are similar to *population(Z, ...)* for the one-sample case.

The folder **simulation** provides the simualtion codes.
