# clade <- rank_ref[NROW(rank_ref), ]
# clade$ranks <- paste0(clade$ranks, ",clade")
# rank_ref <- rbind(rank_ref[-NROW(rank_ref), ], clade)
# rank_ref

# cohorts <- c("megacohort", "supercohort", "cohort", "subcohort", "infracohort")
# cohort_ids <- c()

# -----------------
ids <- c('01','05','10','20','25','30','40','45','50','60','70','80','83','84',
    '85','86','87','90','100','110','120','125','130','140','150','160','170',
    '180','190','200','210','215','217','220','225','230','240','250','255',
    '260','265','270','300')
ranks <- c('domain','superkingdom','kingdom','subkingdom','infrakingdom,superphylum',
    'phylum,division','subphylum,subdivision','infradivision','superclass','class',
    'subclass','infraclass','megacohort','supercohort','cohort','subcohort','infracohort',
    'superorder','order','suborder','infraorder','parvorder','superfamily','family',
    'subfamily','tribe','subtribe','genus','subgenus','section','subsection',
    'species group','species subgroup','species','infraspecies','subspecies',
    'variety,varietas','subvariety,race','stirp','form,forma,morph','aberration',
    'subform','unspecified,no rank,unranked,clade')
rank_ref <- data.frame(
  rankid = ids,
  ranks = ranks,
  stringsAsFactors = FALSE
)
save(rank_ref, file = "data/rank_ref.RData")
