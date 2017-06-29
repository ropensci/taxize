library("devtools")

res <- revdep_check(threads = 2, type = "mac.binary")
revdep_check_save_summary()
revdep_check_print_problems()
#revdep_email(date = "Jan 18", only_problems = FALSE, draft = TRUE)
