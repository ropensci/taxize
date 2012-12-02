# tests for iplant_tnrsmatch fxn in taxize
context("iplant_tnrsmatch")

mynames <- c("shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus", "rubus ulmifolius", "asclepias curassavica", "pistacia lentiscus")

test_that("iplant_tnrsmatch returns the correct value", {
	expect_that(iplant_tnrsmatch(retrieve = 'all', taxnames = c('helianthus annuus', 'acacia', 'gossypium'), output = 'names')[1,2], 
							matches("Asteraceae"))
})

test_that("iplant_tnrsmatch returns the correct class", {
	expect_that(iplant_tnrsmatch(retrieve = 'all', taxnames = c('helianthus annuus', 'acacia', 'gossypium'), output = 'names'),
							is_a("data.frame"))
})
