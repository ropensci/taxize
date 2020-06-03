library(taxize)
orders <- apgOrders()
orders <- tibble::as_tibble(orders)
orders
apg_orders <- dplyr::arrange(orders, order) %>% 
  dplyr::mutate(accepted_name = ifelse(is.na(synonym), order, synonym))

families <- apgFamilies()
families$family <- gsub("\"", "", families$family)
families <- tibble::as_tibble(families)
families
apg_families <- dplyr::arrange(families, family) %>% 
  dplyr::mutate(accepted_name = ifelse(is.na(synonym), family, synonym))

save(apg_orders, file = "data/apg_orders.RData", version = 2)
save(apg_families, file = "data/apg_families.RData", version = 2)
