<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Key functions}
%\VignetteEncoding{UTF-8}
-->

Some key functions in taxize, what they do, and their data sources.
======

Function name | What it does | Source |
----------- | ----------- | ----------- |
children | Get direct children | COL, NCBI, ITIS |
classification | Upstream classification | COL, NCBI, ITIS, Tropicos, EOL, GBIF, NBN |
comm2sci | Get scientific from common names | EOL, NCBI, ITIS, Tropicos |
downstream | Downstream taxa to specified rank | COL, ITIS |
get_ids | Get taxonomic identifiers | COL, NCBI, ITIS, Tropicos, EOL, GBIF, NBN |
resolve | Resolve names using any resolver | GNR, TNRS, iPlant |
sci2comm | Get common from scientific names | EOL, NCBI, ITIS |
synonyms | Retrieve synonyms given input names/identifiers | COL, NCBI, ITIS, Tropicos |
upstream | Retrieve upstream taxa given names/identifiers | COL, ITIS |

### Acronyms

* COL: Catalogue of Life
* NCBI: National Center for Biotechnology Information
* ITIS: Integrated Taxonomic Information Service
* EOL: Encylopedia of Life
* GBIF: Global Biodiversity Information Facility
* NBN: National Biodiversity Network (UK)
* iPlant: iPlant Name Resolution Service
* GNR: Global Names Resolver
* TNRS: Taxonomic Name Resolution Service
