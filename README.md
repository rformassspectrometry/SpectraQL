# `MassQL` for `Spectra`

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check-bioc](https://github.com/RforMassSpectrometry/SpectraQL/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/RforMassSpectrometry/SpectraQL/actions?query=workflow%3AR-CMD-check-bioc)
[![codecov](https://codecov.io/github/rformassspectrometry/SpectraQL/graph/badge.svg?token=PSUEEOF0GH)](https://codecov.io/github/rformassspectrometry/SpectraQL)
[![:name status badge](https://rformassspectrometry.r-universe.dev/badges/:name)](https://rformassspectrometry.r-universe.dev/)
[![license](https://img.shields.io/badge/license-Artistic--2.0-brightgreen.svg)](https://opensource.org/licenses/Artistic-2.0)

The Mass Spec Query Language
([MassQL](https://mwang87.github.io/MassQueryLanguage_Documentation/)) is a
domain specific language meant to be a succinct way to express a query in a mass
spectrometry (MS) centric fashion. It is inspired by SQL, but it attempts to
bake in assumptions of MS to make querying much more natural for MS users.

The *SpectraQL* package provides support for the MassQL language in R,
for MS data represented by `Spectra` objects defined in Bioconductor's
[Spectra](https://bioconductor.org/packages/Spectra) package.

For an introduction and use cases see the [package
vignette](https://rformassspectrometry.github.io/SpectraQL/articles/SpectraQL.html)
or the [package
documentation](https://rformassspectrometry.github.io/SpectraQL/reference/index.html).

# Contributions

Contributions are highly welcome and should follow the [contribution
guidelines](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html#contributions).
Also, please check the coding style guidelines in the [RforMassSpectrometry
vignette](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html).


# License

The *MsIO* code is provided under a permissive [Artistic 2.0
license](https://opensource.org/licenses/Artistic-2.0). The
documentation, including the manual pages and the vignettes, are
distributed under a [CC BY-SA
license](https://creativecommons.org/licenses/by-sa/4.0/).
