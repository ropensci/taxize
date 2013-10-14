# Reviewer response notes

## Summary response at top

We thank the reviewers for their comments. A detailed listing of reviewer comments and our discussion about them can be found here: https://github.com/ropensci/taxize_/issues/178. The following is a summary of the changes made in response to reviewer's comments:

* We have improved language where pointed out by reviews: removed sentences, changed awkward language, and corrected spelling and grammar mistakes. 
* In response to comments by two of the three reviewer's we have added a third appendix that goes over using API keys and how to install the development version of the software.
* There were a number of suggestions about changing the software itself (improving naming of functions and outputs). We agree with these suggestions, and although the changes to the software have not been made yet, we plan on making the changes in the next version of the software. 

## Response to Will

We appreciate Dr. Pearse's comments on our manuscript. We agree that species taxonomy does not equate to phylogenetic history, so we added the following sentence to the first paragraph: "Although taxonomic classifications are human constructs created to understand the real phylogeny of life \cite{benton2000}, they are nonetheless essential to organize the vast diversity of organisms." We fixed the citation in Table 1, and reordered the functions in the table to be alphebetical. Thanks very much for pointing out that Phylomatic now accepts mammals in addition to Angiosperm plants - we adjusted the language accordingly. 

We removed the description of Taxonstand and the Plantlist.org (and associated references) that this reviewer referred to as it wasn't necessary and improves reading. 
This reviewer asked for better explanation of using API keys and the .Rprofile file. In response, we have added a new appendix (Appendix C) that explains using API keys and installing the development version of taxize. 

## Response to Ethan

We appreciate Dr. White's comments on our manuscript. 

We removed the two sentences "Science workflows can now easily incorporate text, code, and images in a single executable document. Reproducible documents should become mainstream in biology to avoid mistakes, and make collaboration easier."

In response to this reviewer's comment about clarification on APIs and authentication, and Dr. Pearse's comments on the same issue, we have added a new appendix (Appendix C) that explains using API keys and installing the development version of taxize. 

This reviewer commented that the section on Aggregating data to a specific taxonomic rank referred to an example, but none appeared to be present. The example is now in the paper.

## Response to Gavin

We appreciate Dr. Simpson's very thorough comments on our manuscript! The following are responses to Dr. Simpson's comments:

- ...there is some inconsistency in the naming conventions used. For example there is the 'tpl_search()' function to search The Plant List, but the equivalent function to search uBio is 'ubio_namebank()'. Whilst this may reflect specific aspects of terminology in use at the respective data stores, it does not help the user gain familiarity with the package by having them remember inconsistent function names.

We agree that we should definitely improve naming conventions for functions. However, we think it's better to change the function names as needed in an upcoming version of the software after we have had time work on the problem. 

- Consider adding in more conventional indications of R outputs, or physically separate input from output by breaking up the chunks of code to have whitespace between the grey-background chunks.

We have used comments (pound signs) for the reults of function calls within each code block to indicate output as separate from code input. This way users can copy/paste code directly into R to try it out.

- in one location I noticed something amiss with the layout; in the first code block at the top of page 5, the printed output looks wrong here. I would expect the attributes to print on their own line and the data in the attribute to also be on its own separate line.

This was a problem with the typsetting, and we have fixed it.

- the inconsistency in the naming of the output object columns. For example, in the two code chunks shown in column 1 of page 4, the first block has an object printed with column names 'matched_name' and 'data_source_title', whilst camelCase is used in the outputs shown in the second block.

We agree that we should definitely improve naming conventions for object columns. However, we think it's better to change the column names as needed in an upcoming version of the software after we have had time work on the problem. 

- I was a little confused about the example in the section Resolve Taxonomic Names on page 4. Should the taxon name be "Helianthus annuus" or "Helianthus annus"? In the 'mynames' definition you include 'Helianthus annuus' in the character vector but the output shown suggests that the submitted name was 'Helianthus annus' (1 "u") in rows with rownames 9 and 10 in the output shown.

Fixed.

- Abstract: replace "easy" with "simple" in "...fashion that's easy...", and move the details about availability and the URI to the end of the sentence.

Fixed. 

- Page 2, Column 1, Paragraph 2: You have "In addition, there is no one authoritative taxonomic names source...", which is a little clumsy to read. How about "In addition, there is no one authoritative source of taxonomic names..."?

Changed.

- Pg 2, C1, P2-3: The abbreviated data sources are presented first (in paragraph 2) and subsequently defined (in para 3). Restructure this so that the abbreviated forms are explained upon first usage.

Changed.

- Pg 2, C2, P2: Most R packages are "in development" so I would drop the qualifier and reword the opening sentence of the paragraph.

Changed.

- Pg 2, C2, P6: Change "and more can easily be added" to "and more can be easily added" seems to flow better?

Changed.

- Pg 5, paragraph above Figure 1: You refer to converting the object to an ape phylo object and then repeat essentially the same information in the next sentence. Remove the repetition.

Removed.

- Pg 6, C1: The header may be better as "Which taxa are children of the taxon of interest".

Changed.

- Pg 6: In the section "IUCN status", the term "we" is used to refer to both the authors and the user. This is confusing. Reserve "we" for reference to the authors and use something else ("a user" perhaps) for the other instances. Check this throughout the entire manuscript.

Fixed.

- Pg 6, C2: in the paragraph immediately below the 'grep()' for "RAG1", two consecutive sentences begin with "However".

Changed.

- Pg 7: The first sentence of "Aggregating data...." reads "In biology, one can asks questions...". It should be "one asks" or "one can ask"

Changed.

- Pg 7, Conclusions: The first sentence reads "information is increasingly sought out by biologists". I would drop "out" as "sought" is sufficient on its own.

Changed.

- Appendices: Should the two figures in the Appendices have a different reference to differentiate them from Figure 1 in the main body of the paper? As it stands, the paper has two Figure 1s, one on page 5 and a second on page 12 in the Appendix.

Fixed.

- On Appendix Figure 2: The individual points are a little large. Consider reducing the plotting character size. I appreciate the effect you were going for with the transparency indicating density of observation through overplotting, but the effect is weakened by the size of the individual points.

Although we agree that the styling of the figure could be improved, we are going to leave it as is because it's not important to understanding the material.

- Should the phylogenetic trees have some scale to them? I presume the height of the stems is an indication of phylogenetic distance but the figure is hard to calibrate without an associated scale. A quick look at Paradis (2012) Analysis of Phylogenetics and Evolution with R would suggest however that a scale is not consistently applied to these trees. I am happy to be guided by the authors as they will be more familiar with the conventions than I.

A scale could be used for sure. However, our focus is on showing readers that they can get data with which to make phylogeny, not on how to properly create and display a phylogeny. Thus, we are leaving the phylogeny as is without a scale.