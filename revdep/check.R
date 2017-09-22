library("devtools")

res <- revdep_check(threads = 4, type = "mac.binary", skip = c("taxlist", "aptg"))
revdep_check_save_summary()
revdep_check_print_problems()
#revdep_email(date = "Jan 18", only_problems = FALSE, draft = TRUE)
