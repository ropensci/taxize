library("devtools")

res <- revdep_check(threads = 4, type = "mac.binary")
revdep_check_save_summary()
revdep_check_print_problems()
#revdep_email(date = "June 9", only_problems = TRUE, draft = FALSE)
