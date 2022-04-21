I am taking over as maintainer from Scott Chamberlain.
Scott told me that there was an error on a CRAN check due to a down internet service.
The error no longer appears at https://cran.r-project.org/web/checks/check_results_taxize.html so I do not know what the problem was exactly.
However, I have checked all of the test code to make sure that tests requiring internet services will not be run on CRAN, so the problem is probably solved.

Update after first submission:

I have checked for invalid URLs using `urlchecker::url_check` and fixed all except for:

```
x Error: inst/CITATION:8:22 403: Forbidden
   url            = "https://f1000research.com/articles/2-191/v2",
                     ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x Error: inst/CITATION:13:37 403: Forbidden
        "F1000Research, 2:191. URL: https://f1000research.com/articles/2-191/v2")
                                    ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
```

I am not sure what the problem is here.
The link works fine in a browser and does not redirect to another URL.


## Test environments and check results

### Local computer: Pop!_OS 20.04 LTS, R version 4.0.3

0 errors | 0 warnings | 0 notes

### win-builder (devel)

0 errors | 0 warnings | 1 notes

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Zachary Foster <zacharyfoster1989@gmail.com>'

New maintainer:
  Zachary Foster <zacharyfoster1989@gmail.com>
Old maintainer(s):
  Scott Chamberlain <myrmecocystus@gmail.com>

Found the following (possibly) invalid URLs:
  URL: https://codecov.io/gh/ropensci/taxize (moved to https://app.codecov.io/gh/ropensci/taxize)
    From: README.md
    Status: 200
    Message: OK
  URL: https://f1000research.com/articles/2-191/v2
    From: inst/CITATION
    Status: 403
    Message: Forbidden
  ...
  ...
  ...
```

I am taking over as maintainer from Scott Chamberlain.

I got many notes about unconfirmed URLs like:

```
  URL: https://github.com/JulietteLgls
    From: README.md
    Status: 429
    Message: Too Many Requests
```

probably because there are so many to check.

### Rhub 

0 errors | 0 warnings | 1 notes

Same as win-builder notes 


## Reverse dependencies

* I have run R CMD check on the 25 downstream dependencies; there were no errors related to taxize.

