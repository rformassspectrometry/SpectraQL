# Query a Spectra object using MassQL

The `query` function allows to query and subset/filter a `Spectra`
object using a Mass Spec Query Language
[MassQL](https://mwang87.github.io/MassQueryLanguage_Documentation/)
expression.

A MassQL query is expressed in the form
`"QUERY <type of data> WHERE <condition> AND <condition> FILTER <filter> AND <filter>"`,
multiple *conditions* and *filters* can be combined with logical *and*
operations. In the MassQL definition, *conditions* subsets the data to
specific spectra while *filter* restricts the data within a spectrum.
Note that at present MassQL *filters* are not supported. Also note that
MassQL queries are interpreted case insensitive in `SpectraQL`.

See also the package vignette for more details.

## Usage

``` r
# S4 method for class 'Spectra'
query(x, query = character(), ...)
```

## Arguments

- x:

  The `Spectra` object to query.

- query:

  `character(1)` with the MassQL query.

- ...:

  currently ignored.

## Value

Depending on the `<type of data>` part of the MassQL query.

## Type of data

The `"<type of data>"` allows to define which data should be extracted
from the selected spectra. MassQL defines *type of data* being
`"MS1DATA"` or `"MS2DATA"` to retrieve data from MS1 or MS2 scans. By
default peak data will be returned, but in addition, MASSQL defines
additional functions that can be applied to modify the data or select
different data to be returned. In addition *SpectraQL* defines the
special type of data `"*"` which will return the results as a `Spectra`
object. *SpectraQL* supports:

- `"*"`: select all data and return the data subset as a
  [`Spectra::Spectra()`](https://rdrr.io/pkg/Spectra/man/Spectra.html)
  object.

- `"MS1DATA"`: return the
  [`Spectra::peaksData()`](https://rdrr.io/pkg/ProtGenerics/man/protgenerics.html)
  from all selected **MS1** spectra, i.e. a `list` with two column
  matrices with the peaks' m/z and intensity values.

- `"MS2DATA"`: return the
  [`Spectra::peaksData()`](https://rdrr.io/pkg/ProtGenerics/man/protgenerics.html)
  from all selected **MS2** spectra, i.e. a `list` with two column
  matrices with the peaks' m/z and intensity values.

- `"scaninfo(MS1DATA)"`, `"scaninfo(MS2DATA)"`: return the
  [`Spectra::spectraData()`](https://rdrr.io/pkg/ProtGenerics/man/protgenerics.html)
  of all selected spectra.

- `"scansum(MS1DATA)"`, `"scansum(MS2DATA)"`: sum of the peak
  intensities of the selected spectra (TIC, or XIC if combined with
  `"FILTER"`).

## Conditions

Conditions define to which spectra the data set should be subsetted. A
*condition* will subset a `Spectra` object to selected spectra, but will
not (unlike *Filters*, see further below) filter peaks from a spectrum.
Several conditions can be combined with `"and"` (case insensitive). The
syntax for a condition is `"<condition> = <value>"`, e.g.
`"MS2PROD = 144.1"`. Such conditions can be further refined by
additional expressions that allow for example to define acceptable
tolerances for m/z differences. `SpectraQL` supports (case insensitive):

- `"RTMIN"`: minimum retention time (in **seconds**).

- `"RTMAX"`: maximum retention time (in **seconds**).

- `"SCANMIN"`: the minimum scan number (acquisition number).

- `"SCANMAX"`: the maximum scan number (acquisition number).

- `"CHARGE"`: the charge for MS2 spectra.

- `"POLARITY"`: the polarity of the spectra (can be `"positive"`,
  `"negative"`, `"pos"` or `"neg"`, case insensitive).

- `"MS2PROD"` or `"MS2MZ"`: allows to select MS2 spectra that contain a
  peak with particular m/z value(s). See below for examples.

- `"MS2PREC"`: allows to select MS2 spectra with the defined precursor
  m/z value(s). See below for examples.

- `"MS1MZ"`: allows to select MS1 spectra containing peak(s) with the
  defined m/z value(s).

- `"MS2NL"`: allows to look for a neutral loss from precursor in MS2
  spectra.

All conditions involving m/z values allow to specify a mass accuracy
using the optional fields `"TOLERANCEMZ"` and `"TOLERANCEPPM"` that
define the absolute and m/z-relative acceptable difference in m/z
values. One or both fields can be attached to a *condition* such as
`"MS2PREC=100:TOLERANCEMZ=0.1:TOLERANCEPPM=20"` to select for example
all MS2 spectra with a precursor m/z equal to 100 accepting a difference
of 0.1 and 20 ppm. Note that in contrast to MassQL, the default
tolarance and ppm is 0 for all calls.

## Filters

Filters subset the data within spectra, i.e. select which peaks within
spectra should be retrieved. *SpectraQL* supports the following filters:

- `"MS1MZ"`: filters MS1 spectra keeping only peaks with matching m/z
  values (tolerance can be specified with `"TOLERANCEMZ"` and
  `"TOLERANCEPPM"` as for conditions).

- `"MS2MZ"`: filters MS2 spectra keeping only peaks with matching m/z
  values (tolerance can be specified with `"TOLERANCEMZ"` and
  `"TOLERANCEPPM"` as for conditions).

## Author

Andrea Vicini, Johannes Rainer

## Examples

``` r

## Read a data file with MS1 and MS2 spectra
library(MsDataHub)
library(Spectra)
#> Loading required package: S4Vectors
#> Loading required package: stats4
#> Loading required package: BiocGenerics
#> Loading required package: generics
#> 
#> Attaching package: ‘generics’
#> The following objects are masked from ‘package:base’:
#> 
#>     as.difftime, as.factor, as.ordered, intersect, is.element, setdiff,
#>     setequal, union
#> 
#> Attaching package: ‘BiocGenerics’
#> The following objects are masked from ‘package:stats’:
#> 
#>     IQR, mad, sd, var, xtabs
#> The following objects are masked from ‘package:base’:
#> 
#>     Filter, Find, Map, Position, Reduce, anyDuplicated, aperm, append,
#>     as.data.frame, basename, cbind, colnames, dirname, do.call,
#>     duplicated, eval, evalq, get, grep, grepl, is.unsorted, lapply,
#>     mapply, match, mget, order, paste, pmax, pmax.int, pmin, pmin.int,
#>     rank, rbind, rownames, sapply, saveRDS, table, tapply, unique,
#>     unsplit, which.max, which.min
#> 
#> Attaching package: ‘S4Vectors’
#> The following object is masked from ‘package:utils’:
#> 
#>     findMatches
#> The following objects are masked from ‘package:base’:
#> 
#>     I, expand.grid, unname
#> Loading required package: BiocParallel
fls <- MsDataHub::PestMix1_DDA.mzML()
#> see ?MsDataHub and browseVignettes('MsDataHub') for documentation
#> loading from cache
sps_dda <- Spectra(fls)

## Subset to spectra measured between 300 and 400 seconds
query(sps_dda, "QUERY * WHERE RTMIN = 300 AND RTMAX = 400")
#> MSn data (Spectra) with 971 spectra in a MsBackendMzR backend:
#>       msLevel     rtime scanIndex
#>     <integer> <numeric> <integer>
#> 1           1   300.123      2271
#> 2           2   300.395      2272
#> 3           1   300.533      2273
#> 4           1   300.654      2274
#> 5           1   300.775      2275
#> ...       ...       ...       ...
#> 967         1   399.277      3237
#> 968         1   399.406      3238
#> 969         1   399.527      3239
#> 970         2   399.798      3240
#> 971         1   399.936      3241
#>  ... 34 more variables/columns.
#> 
#> file(s):
#> c5b57aca02b_7861
#> Processing:
#>  Filter: select retention time [300..400] on MS level(s)  [Fri Feb  6 08:00:30 2026] 

## To extract peaks data from MS1 or MS2 spectra use "MS1DATA" or "MS2DATA"
## instead of *. Note also that queries are case-insensitive.
pks <- query(sps_dda, "query ms1data where rtmin = 300 and rtmax = 400")
pks
#> List of length 326
head(pks[[1L]])
#>            mz  intensity
#> [1,] 50.01534 0.06152615
#> [2,] 51.01116 0.20895603
#> [3,] 51.94061 0.09404848
#> [4,] 52.01844 2.00179029
#> [5,] 53.00222 0.09347252
#> [6,] 53.01401 0.74939591

## To select (MS2) spectra with a certain precursor m/z the MS2PREC condition
## can be used. Below we extract all spectra with a precursor m/z of 99.9
## accepting also a difference of 10ppm
query(sps_dda, "QUERY * WHERE MS2PREC = 99.967:TOLERANCEPPM=10")
#> MSn data (Spectra) with 1 spectra in a MsBackendMzR backend:
#>     msLevel     rtime scanIndex
#>   <integer> <numeric> <integer>
#> 1         2   884.843      7495
#>  ... 34 more variables/columns.
#> 
#> file(s):
#> c5b57aca02b_7861
#> Processing:
#>  Filter: select spectra with precursor m/z matching 99.967 [Fri Feb  6 08:00:30 2026] 

## It is also possible to specify multiple precursor m/z values:
query(sps_dda, "QUERY * WHERE MS2PREC = (99.967 OR 428.88):TOLERANCEPPM=10")
#> MSn data (Spectra) with 2 spectra in a MsBackendMzR backend:
#>     msLevel     rtime scanIndex
#>   <integer> <numeric> <integer>
#> 1         2   884.843      7495
#> 2         2   887.813      7516
#>  ... 34 more variables/columns.
#> 
#> file(s):
#> c5b57aca02b_7861
#> Processing:
#>  Filter: select spectra with precursor m/z matching 99.967, 428.88 [Fri Feb  6 08:00:30 2026] 

## To select all MS1 spectra that contain a peak with a certain m/z we can
## use the MS1MZ condition. Below we combine this with an absolute tolerance
## using TOLERANCEMZ.
query(sps_dda, "QUERY * WHERE MS1MZ = 100:TOLERANCEMZ=1")
#> MSn data (Spectra) with 4625 spectra in a MsBackendMzR backend:
#>        msLevel     rtime scanIndex
#>      <integer> <numeric> <integer>
#> 1            1     0.231         1
#> 2            1     0.351         2
#> 3            1     0.471         3
#> 4            1     0.591         4
#> 5            1     0.711         5
#> ...        ...       ...       ...
#> 4621         1   899.491      7598
#> 4622         1   899.613      7599
#> 4623         1   899.747      7600
#> 4624         1   899.872      7601
#> 4625         1   899.993      7602
#>  ... 34 more variables/columns.
#> 
#> file(s):
#> c5b57aca02b_7861
#> Processing:
#>  Filter: select MS level(s) 1 [Fri Feb  6 08:00:30 2026] 

## Using MS2DATA in combination with MS1MZ will not return any spectra.
query(sps_dda, "QUERY MS2DATA WHERE MS1MZ = 100:TOLERANCEMZ=1")
#> List of length 0

## In contrast, do select MS2 spectra containing a peak with a certain m/z
## we have to use the condition MS2PROD
query(sps_dda, "QUERY * WHERE MS2PROD = 100:TOLERANCEMZ=1")
#> MSn data (Spectra) with 260 spectra in a MsBackendMzR backend:
#>       msLevel     rtime scanIndex
#>     <integer> <numeric> <integer>
#> 1           2    47.183       370
#> 2           2    79.371       617
#> 3           2    81.661       631
#> 4           2    97.140       744
#> 5           2    97.550       746
#> ...       ...       ...       ...
#> 256         2   884.163      7491
#> 257         2   884.843      7495
#> 258         2   890.443      7535
#> 259         2   893.072      7554
#> 260         2   898.602      7591
#>  ... 34 more variables/columns.
#> 
#> file(s):
#> c5b57aca02b_7861
#> Processing:
#>  Filter: select MS level(s) 2 [Fri Feb  6 08:00:31 2026] 

## MS2MZ can be used as alternative to MS2PROD
query(sps_dda, "QUERY * WHERE MS2MZ = 100:TOLERANCEMZ=1")
#> MSn data (Spectra) with 7602 spectra in a MsBackendMzR backend:
#>        msLevel     rtime scanIndex
#>      <integer> <numeric> <integer>
#> 1            1     0.231         1
#> 2            1     0.351         2
#> 3            1     0.471         3
#> 4            1     0.591         4
#> 5            1     0.711         5
#> ...        ...       ...       ...
#> 7598         1   899.491      7598
#> 7599         1   899.613      7599
#> 7600         1   899.747      7600
#> 7601         1   899.872      7601
#> 7602         1   899.993      7602
#>  ... 34 more variables/columns.
#> 
#> file(s):
#> c5b57aca02b_7861

## Select MS2 spectra containing a peak with neutral loss from
## precursor of 100 allowing a m/z relative ppm tolerance of 5)
res <- query(sps_dda, "QUERY MS2DATA WHERE MS2NL=100:TOLERANCEPPM=5")

## Combine two different conditions: selection of spectra with positive
## polarity and retention time greater than 200
res <- query(sps_dda, "QUERY * WHERE RTMIN = 200 AND POLARITY = Positive")
```
