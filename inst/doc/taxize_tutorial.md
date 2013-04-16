# Install the development branch of taxize on github

```r
# install.packages('devtools') library(devtools) install_github('taxize_',
# 'ropensci', 'local_sql')
library(taxize)
```

```
## Loading required package: RSQLite
```

```
## Loading required package: DBI
```

```r
library(plyr)
```


# Download the sqlite database from dropbox
https://www.dropbox.com/s/gz1vvsu2d0qps19/itis2_sqlite.zip

# Set path to database
Just set the path to your sqlite database once, then choose `locally = TRUE` in your ITIS function calls to use the local database. This creates an object in your options named "conn", which the ITIS functions call when `locally = TRUE`.

```r
taxize:::taxize_options(localpath = "~/github/ropensci/sql/itis2.sqlite")  # change to your path
```


# Some examples
## Search by scientific name

### Single search 
#### Using the web API 

```r
searchbyscientificname("Tardigrada")
```

```
##           combinedname    tsn
## 1   Rotaria tardigrada  58274
## 2 Notommata tardigrada  58898
## 3  Pilargis tardigrada  65562
## 4           Tardigrada 155166
## 5     Heterotardigrada 155167
## 6     Arthrotardigrada 155168
## 7       Mesotardigrada 155358
## 8         Eutardigrada 155362
## 9  Scytodes tardigrada 866744
```


### Using local search

```r
searchbyscientificname("Tardigrada", locally = TRUE)
```

```
##           combinedname    tsn
## 1   Rotaria tardigrada  58274
## 2 Notommata tardigrada  58898
## 3  Pilargis tardigrada  65562
## 4           Tardigrada 155166
## 5     Heterotardigrada 155167
## 6     Arthrotardigrada 155168
## 7       Mesotardigrada 155358
## 8         Eutardigrada 155362
## 9  Scytodes tardigrada 866744
```


### As you can see, there is some performance improvement with even single queries

```r
system.time(searchbyscientificname("Tardigrada"))
```

```
##    user  system elapsed 
##   0.013   0.001   0.867
```

```r
system.time(searchbyscientificname("Tardigrada", locally = TRUE))
```

```
##    user  system elapsed 
##   0.530   0.085   0.618
```


### Search for multiple names

#### Using the web API, you have to submit one at a time using e.g., an lapply fxn

```r
spnames <- c("oryza sativa", "Chironomus riparius", "Helianthus annuus", "Quercus lobata", 
    "Poa annua", "Lampetra tridentata", "Mordacia lapicida", "Bathyraja abyssicola", 
    "Arhynchobatis asperrimus", "Alytes obstetricans")
head(ldply(spnames, searchbyscientificname))
```

```
##                     combinedname    tsn
## 1                   Oryza sativa  41976
## 2        Oryza sativa var. fatua 566528
## 3    Oryza sativa ssp. rufipogon 797955
## 4     Oryza sativa var. elongata 801263
## 5 Oryza sativa var. grandiglumis 801264
## 6    Oryza sativa var. latifolia 801265
```


#### Using local search, you can submit many names in one vector

```r
head(searchbyscientificname(srchkey = spnames, locally = TRUE))
```

```
##                     combinedname    tsn
## 1            Alytes obstetricans 662327
## 2       Arhynchobatis asperrimus 564294
## 3           Bathyraja abyssicola 564114
## 4            Chironomus riparius 129313
## 5              Helianthus annuus  36616
## 6 Helianthus annuus ssp. jaegeri 525928
```


### Searches with many queries is where we see the major time savings 

```r
system.time(ldply(spnames, searchbyscientificname))  # web API
```

```
##    user  system elapsed 
##   0.110   0.007   9.539
```

```r
system.time(searchbyscientificname(srchkey = spnames, locally = TRUE))  # local sql search
```

```
##    user  system elapsed 
##   2.790   0.100   2.907
```



## Get synonyms

```r
# many TSNs with the web API
ldply(c(202385, 36616, 19370, 41107), getsynonymnamesfromtsn)
```

```
## http://www.itis.gov/ITISWebService/services/ITISService/getSynonymNamesFromTSN?tsn=202385
```

```
## http://www.itis.gov/ITISWebService/services/ITISService/getSynonymNamesFromTSN?tsn=36616
```

```
## http://www.itis.gov/ITISWebService/services/ITISService/getSynonymNamesFromTSN?tsn=19370
```

```
## http://www.itis.gov/ITISWebService/services/ITISService/getSynonymNamesFromTSN?tsn=41107
```

```
##                                   name    tsn
## 1                 Ursus arctos nelsoni 202385
## 2                    Helianthus aridus  36616
## 3              Helianthus lenticularis 514571
## 4       Helianthus annuus ssp. jaegeri 514587
## 5  Helianthus annuus ssp. lenticularis 525928
## 6       Helianthus annuus ssp. texanus 525929
## 7  Helianthus annuus var. lenticularis 525930
## 8   Helianthus annuus var. macrocarpus 536095
## 9       Helianthus annuus var. texanus 536096
## 10       Quercus lobata var. argillara  19370
## 11       Quercus lobata var. insperata 195111
## 12       Quercus lobata var. turbinata 195112
## 13         Quercus lobata var. walteri 195113
## 14                     Quercus hindsii 195114
## 15             Poa annua var. aquatica  41107
## 16              Poa annua var. reptans 538978
## 17                         Aira pumila 538979
## 18                    Catabrosa pumila 785854
## 19                      Ochlopoa annua 787993
## 20                      Poa aestivalis 791574
## 21                          Poa algida 793946
## 22                Poa annua var. annua 793954
## 23            Poa annua var. eriolepis 802116
## 24         Poa annua var. rigidiuscula 802117
## 25                Poa annua f. reptans 802119
```

```r

# Local search with sql
taxize:::taxize_options(localpath = "~/github/ropensci/sql/itis2.sqlite")  # set to use your path
getsynonymnamesfromtsn(tsn = 526852, locally = TRUE)  # a single TSN
```

```
##                              name    tsn
## 1      Acer negundo ssp. interius  28752
## 2                   Acer interius 508253
## 3                Negundo interius 517491
## 4 Negundo aceroides ssp. interius 526197
```

```r
getsynonymnamesfromtsn(tsn = c(202385, 36616, 19370, 41107), locally = TRUE)  # many TSNs
```

```
##                                   name    tsn
## 1        Quercus lobata var. argillara 195111
## 2        Quercus lobata var. insperata 195112
## 3        Quercus lobata var. turbinata 195113
## 4          Quercus lobata var. walteri 195114
## 5                      Quercus hindsii 195115
## 6                    Helianthus aridus 514571
## 7              Helianthus lenticularis 514587
## 8       Helianthus annuus ssp. jaegeri 525928
## 9  Helianthus annuus ssp. lenticularis 525929
## 10      Helianthus annuus ssp. texanus 525930
## 11 Helianthus annuus var. lenticularis 536095
## 12  Helianthus annuus var. macrocarpus 536096
## 13      Helianthus annuus var. texanus 536097
## 14             Poa annua var. aquatica 538978
## 15              Poa annua var. reptans 538979
## 16                         Aira pumila 785854
## 17                    Catabrosa pumila 787993
## 18                      Ochlopoa annua 791574
## 19                      Poa aestivalis 793946
## 20                          Poa algida 793954
## 21                Poa annua var. annua 802116
## 22            Poa annua var. eriolepis 802117
## 23         Poa annua var. rigidiuscula 802119
## 24                Poa annua f. reptans 803667
## 25                Ursus arctos nelsoni 202384
```

```r

# any you can return the index on the data.frame, that is, the TSN you
# searched with for easy parsing afterwards
getsynonymnamesfromtsn(tsn = c(202385, 36616, 19370, 41107), locally = TRUE, 
    returnindex = TRUE)
```

```
##    querystring    tsn                        combinedName
## 1        19370 195111       Quercus lobata var. argillara
## 2        19370 195112       Quercus lobata var. insperata
## 3        19370 195113       Quercus lobata var. turbinata
## 4        19370 195114         Quercus lobata var. walteri
## 5        19370 195115                     Quercus hindsii
## 6        36616 514571                   Helianthus aridus
## 7        36616 514587             Helianthus lenticularis
## 8        36616 525928      Helianthus annuus ssp. jaegeri
## 9        36616 525929 Helianthus annuus ssp. lenticularis
## 10       36616 525930      Helianthus annuus ssp. texanus
## 11       36616 536095 Helianthus annuus var. lenticularis
## 12       36616 536096  Helianthus annuus var. macrocarpus
## 13       36616 536097      Helianthus annuus var. texanus
## 14       41107 538978             Poa annua var. aquatica
## 15       41107 538979              Poa annua var. reptans
## 16       41107 785854                         Aira pumila
## 17       41107 787993                    Catabrosa pumila
## 18       41107 791574                      Ochlopoa annua
## 19       41107 793946                      Poa aestivalis
## 20       41107 793954                          Poa algida
## 21       41107 802116                Poa annua var. annua
## 22       41107 802117            Poa annua var. eriolepis
## 23       41107 802119         Poa annua var. rigidiuscula
## 24       41107 803667                Poa annua f. reptans
## 25      202385 202384                Ursus arctos nelsoni
##                           author
## 1                          Jeps.
## 2                  (Jeps.) Jeps.
## 3                          Jeps.
## 4                          Jeps.
## 5                         Benth.
## 6                          Rydb.
## 7              Douglas ex Lindl.
## 8                (Heiser) Heiser
## 9  (Douglas ex Lindl.) Cockerell
## 10                        Heiser
## 11  (Douglas ex Lindl.) Steyerm.
## 12               (DC.) Cockerell
## 13             (Heiser) Shinners
## 14                         Asch.
## 15                      Hausskn.
## 16                         Pursh
## 17       (Pursh) Roem. & Schult.
## 18                (L.) H. Scholz
## 19                      J. Presl
## 20                         Trin.
## 21                            L.
## 22                      E. Desv.
## 23                    L.H. Dewey
## 24          (Hausskn.) T. Koyama
## 25                 Merriam, 1914
```



# Here's what the one of the functions looks like with the sql syntax

```r
gettsnbyvernacularlanguage <- function(language = NA, locally = FALSE) {
    if (locally) {
        sqlconn <- getOption("conn")
        query_TSNS_BY_LANGUAGE <- paste("select", paste("CASE", paste(sapply(language, 
            function(x) paste("WHEN language LIKE ", paste("'", x, "'", sep = ""), 
                " THEN ", paste0("'", x, "'"), sep = ""), USE.NAMES = FALSE), 
            collapse = " "), "END AS querystring,"), "tsn, vernacular_name from vernaculars where ", 
            paste0(" language like ", sapply(language, function(x) paste("'", 
                x, "'", sep = ""), USE.NAMES = FALSE), collapse = " OR "), " order by tsn, vernacular_name;")
        temp <- dbGetQuery(conn = sqlconn, query_TSNS_BY_LANGUAGE)
        return(data.frame(language = temp$querystring, comname = temp$vernacular_name, 
            tsn = temp$tsn))
    } else {
        url = "http://www.itis.gov/ITISWebService/services/ITISService/getTsnByVernacularLanguage"
        args <- list()
        if (!is.na(language)) 
            args$language <- language
        tt <- getForm(url, .params = args, ..., curl = curl)
        out <- xmlParse(tt)
        namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
        nodes <- getNodeSet(out, "//ax21:commonName", namespaces = namespaces)
        comname <- sapply(nodes, xmlValue)
        nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
        tsn <- sapply(nodes, xmlValue)
        data.frame(comname = comname, tsn = tsn)
    }
}
```


#### Note:
Not all function that use ITIS data have a local sql option yet, but many do.  Send any bug reports [here](https://github.com/ropensci/taxize_/issues)
