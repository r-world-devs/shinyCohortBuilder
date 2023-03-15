## Test environments
* local check
  Ubuntu 20.04.5 LTS, 4.2.1 (2022-06-23)
* win-builder
  R version 4.1.3 (2022-03-10)
  R version 4.2.0 (2022-04-22 ucrt)
  R Under development (unstable) (2022-06-05 r82452 ucrt)

## `R CMD check shinyCohortBuilder_0.2.0.tar.gz --as-cran` results

```
* using log directory ‘/home/krystian/Projects/Packages/shinyCohortBuilder.Rcheck’
* using R version 4.2.1 (2022-06-23)
* using platform: x86_64-pc-linux-gnu (64-bit)
...
* checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
Maintainer: ‘Krystian Igras <krystian8207@gmail.com>’

New submission
...
Status: OK
```

Note regarding checking CRAN incoming feasibility.

## `devtools::check()` results

```
0 errors ✓ | 0 warnings ✓ | 0 notes ✓
```

## win-builder result

```
* using log directory 'd:/RCompile/CRANguest/R-oldrelease/shinyCohortBuilder.Rcheck'
* using R version 4.1.3 (2022-03-10)
* using platform: x86_64-w64-mingw32 (64-bit)
* using session charset: ISO8859-1
...
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Krystian Igras <krystian8207@gmail.com>'

Possibly mis-spelled words in DESCRIPTION:
  Analytical (3:46)
  analytical (16:13)
...
Status: 1 NOTE
```

```
* using log directory 'd:/RCompile/CRANguest/R-release/shinyCohortBuilder.Rcheck'
* using R version 4.2.2 (2022-10-31 ucrt)
* using platform: x86_64-w64-mingw32 (64-bit)
...
* checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
Maintainer: ‘Krystian Igras <krystian8207@gmail.com>’
...
Status: OK
```

```
* using log directory 'd:/RCompile/CRANguest/R-devel/shinyCohortBuilder.Rcheck'
* using R Under development (unstable) (2023-03-12 r83975 ucrt)
* using platform: x86_64-w64-mingw32 (64-bit)
...
* checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
Maintainer: ‘Krystian Igras <krystian8207@gmail.com>’
...
Status: OK
```
Possibly mis-spelled words in DESCRIPTION NOTE mentions usage of word 'analytical' - two word was intentionally used.
