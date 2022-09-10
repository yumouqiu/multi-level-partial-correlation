This repository provides the codes for the paper "Inference on Multi-level Brain Connectivities based on fMRI Data" by Yumou Qiu and Xiao-Hua Zhou.

**PC-individual.txt** includes the function *individual(X, ...)* to estimate the locations of nonzero off-diagonal elements in a p by p partial correlation matrix, based on p-dimensional time series data *X* of size n. This function can be used to estimate the nonzero partial correlations among brain regions of an individual using the fMRI data.  
The output *cbind(recovery, Liu)* is two p by p binary (0-1) matrices combining together by columns, where *recovery* is the output of the proposed method with 1 and 0 indicating significant nonzero and zero partial correlations respectively, and *Liu* is the output of the method for time independent data from the paper "Gaussian graphical model estimation with false discovery rate control" by Liu, W., The Annals of Statistics, 41, 2948–2978, 2013.

**PC-population-1Sample.txt** includes the function *population(Z, ...)* to estimate 
