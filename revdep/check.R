library("devtools")

res <- revdep_check(threads = 4, type = "mac.binary")
revdep_check_save_summary()
revdep_check_print_problems()
revdep_email(date = "Dec 15", version = "0.8.0", only_problems = FALSE, draft = TRUE)
