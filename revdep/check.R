library("devtools")

res <- revdep_check(threads = 4, type = "mac.binary", skip = c("taxlist", "aptg"))
revdep_check_save_summary()
revdep_check_print_problems()
revdep_email(date = "Sep 25", version = "v0.9.0", only_problems = FALSE, draft = TRUE)
