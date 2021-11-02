ids <- c(
  '01', # domain
  '05','10','20','25', # kingdom
  '30','40','45', # phylum/division
  '50','60','70','80','81','82', # class
  '83','84','85','86','87', # cohort
  '90','100','110','120','125', # order
  '130','140','150', # family
  '155','160','170', # tribe
  '180','190','200','210','215','217', # genus
  '220','225','230', # species/etc.
  '240','250','255','260','265', # variety/sub/form/etc.
  '280', # genetic variants
  '300' # unspecified
)
ranks <- c(
  'domain', # domain
  'superkingdom','kingdom','subkingdom','infrakingdom,superphylum', # kingdom
  'phylum,division','subphylum,subdivision','infradivision', # phylum/division
  'superclass','class','subclass','infraclass','subterclass','parvclass', # class
  'megacohort','supercohort','cohort','subcohort','infracohort', # cohort
  'superorder','order','suborder','infraorder','parvorder', # order
  'superfamily','family','subfamily', # family
  'supertribe','tribe','subtribe', # tribe
  'genus','subgenus','section','subsection','species group,series','species subgroup', # genus
  'species','infraspecies','subspecies,forma specialis', # species/etc.
  'variety,varietas','subvariety,race','stirp','form,forma,morph','subform', # variety/sub/form/etc.
  'biotype,isolate,pathogroup,serogroup,serotype,strain,aberration', # genetic variants
  'unspecified,no rank,unranked,clade' # unspecified
)
rank_ref <- data.frame(
  rankid = ids,
  ranks = ranks
)
# NOTE: "version = 2" is so that we don't have to require R > 3.5
save(rank_ref, file = "data/rank_ref.RData", version = 2)
# NOTES: 
# - genetic variants: placed slightly higher above 'unspecified' just to denote
#   they're not no rank, but they're not really a taxonomic rank either


#### rank_ref_zoo
ids_zoo <- c(
  '01', # domain
  '05','10','20','25', # kingdom
  '30','40','45', # phylum/division
  '47', '50','60','70','80','81','82', # class
  '83','84','85','86','87', # cohort
  '90','100','110','120','125', # order
  '126','127', # section/subsection
  '130','135','140','150', # family
  '155','160','170', # tribe
  '180','190','215','217', # genus
  '220','225','230', # species/etc.
  '240','250','255','260','265', # variety/sub/form/etc.
  '280', # genetic variants
  '300' # unspecified
)
ranks_zoo <- c(
  'domain', # domain
  'superkingdom','kingdom','subkingdom','infrakingdom,superphylum', # kingdom
  'phylum,division','subphylum,subdivision','infraphylum,infradivision', # phylum/division
  'gigaclass', 'superclass','class','subclass','infraclass','subterclass','parvclass', # class
  'megacohort','supercohort','cohort','subcohort','infracohort', # cohort
  'superorder','order','suborder','infraorder','parvorder', # order
  'section','subsection', # section/subsection
  'superfamily','epifamily','family','subfamily', # family
  'supertribe','tribe','subtribe', # tribe
  'genus','subgenus','species group,series','species subgroup', # genus
  'species','infraspecies','subspecies,forma specialis', # species/etc.
  'variety,varietas','subvariety,race','stirp','form,forma,morph','subform', # variety/sub/form/etc.
  'biotype,isolate,pathogroup,serogroup,serotype,strain,aberration', # genetic variants
  'unspecified,no rank,unranked,clade' # unspecified
)
rank_ref_zoo <- tibble::tibble(
  rankid = ids_zoo,
  ranks = ranks_zoo
)
# NOTE: "version = 2" is so that we don't have to require R > 3.5
usethis::use_data(rank_ref_zoo, overwrite = TRUE)
# NOTES: 
# - genetic variants: placed slightly higher above 'unspecified' just to denote
#   they're not no rank, but they're not really a taxonomic rank either
