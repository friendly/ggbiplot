# Wine dataset

Results of a chemical analysis of wines grown in the same region in
Italy, derived from three different cultivars. The analysis determined
the quantities of 13 chemical constituents found in each of the three
types of wines.

The grape varieties (cultivars), 'barolo', 'barbera', and 'grignolino',
are indicated in `wine.class`.

## Usage

``` r
data(wine)
```

## Format

A `wine` data frame consisting of 178 observations (rows) and 13 columns
and vector `wine.class` of factors indicating the cultivars. The
variables are:

- `Alcohol`:

  a numeric vector

- `MalicAcid`:

  Malic acid, a numeric vector

- `Ash`:

  Ash, a numeric vector

- `AlcAsh`:

  Alcalinity of ash, a numeric vector

- `Mg`:

  Magnesium, a numeric vector

- `Phenols`:

  total phenols, a numeric vector

- `Flav`:

  Flavanoids, a numeric vector

- `NonFlavPhenols`:

  Nonflavanoid phenols, a numeric vector

- `Proa`:

  Proanthocyanins, a numeric vector

- `Color`:

  Color intensity, a numeric vector

- `Hue`:

  a numeric vector

- `OD`:

  D280/OD315 of diluted wines, a numeric vector

- `Proline`:

  a numeric vector

## Source

UCI Machine Learning Repository
(<http://archive.ics.uci.edu/ml/datasets/Wine>)

## Examples

``` r
data(wine)
table(wine.class)
#> wine.class
#>     barolo grignolino    barbera 
#>         59         71         48 

wine.pca <- prcomp(wine, scale. = TRUE)
ggscreeplot(wine.pca)                                               

ggbiplot(wine.pca, 
         obs.scale = 1, var.scale = 1, 
         groups = wine.class, ellipse = TRUE, circle = TRUE)
```
