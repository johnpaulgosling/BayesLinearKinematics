# BayesLinearKinematics

Adjusting beliefs using Bayes linear (kinematics) rules via an R package. 

This package allows for the representation of second-order beliefs about variables to stored in a `bl` class (which is introduced in this package). The package provides functions to update beliefs using standard Bayes linear rules (as described in Goldstein and Wooff, 2007), to update using kinematics (as described in Goldstein and Shaw, 2004) and to perform operations on beliefs (like subseting and calculating resolutions).

## Installation

You can install the latest version of BayesLinearKinematics from GitHub with:

```R
# install.packages("devtools")
devtools::install_github("johnpaulgosling/BayesLinearKinematics")
```

## Cite us

If you use this package in your work, please cite using the latest citation information:

```R
citation("BayesLinearKinematics")
```

## References

Goldstein, M. & Shaw, S. C. (2004). Bayes linear kinematics and Bayes linear Bayes graphical models. *Biometrika*, **91**, 425-46.

Goldstein, M. & Wooff, D. (2007). *Bayes linear statistics: Theory and methods*. John Wiley & Sons.



