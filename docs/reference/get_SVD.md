# Extract the SVD components from a PCA-like object

Biplots are based on the Singular Value Decomposition, which for a data
matrix is \$\$\mathbf{X} / \sqrt{n} = \mathbf{U} \mathbf{D}
\mathbf{V}^T\$\$ but these are computed and returned in quite different
forms by various PCA-like methods. This function provides a common
interface, returning the components with standard names.

## Usage

``` r
get_SVD(pcobj)
```

## Arguments

- pcobj:

  an object returned by [`prcomp`](https://rdrr.io/r/stats/prcomp.html),
  [`princomp`](https://rdrr.io/r/stats/princomp.html),
  [`PCA`](https://rdrr.io/pkg/FactoMineR/man/PCA.html),
  [`dudi.pca`](https://adeverse.github.io/ade4/reference/dudi.pca.html),
  or [`lda`](https://rdrr.io/pkg/MASS/man/lda.html)

## Value

A list of four elements

- n:

  The sample size on which the analysis was based

- U:

  Left singular vectors, giving observation scores

- D:

  vector of singular values, the diagonal elements of the matrix
  \\\mathbf{D}\\, which are also the square roots of the eigenvalues of
  \\\mathbf{X} \mathbf{X}'\\

- V:

  Right singular vectors, giving variable loadings

## Examples

``` r
data(crime)
crime.pca <- 
  crime |> 
  dplyr::select(where(is.numeric)) |>
  prcomp(scale. = TRUE)

crime.svd <- get_SVD(crime.pca)
names(crime.svd)
#> [1] "n" "U" "D" "V"
crime.svd$D
#> [1] 2.0285363 1.1129788 0.8519487 0.5625229 0.5079119 0.4712106 0.3522159
```
