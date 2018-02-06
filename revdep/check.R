library("devtools")

res <- revdep_check_resume(threads = 4, type = "mac.binary", skip = c("RNeXML"))
# res <- revdep_check(threads = 4, type = "mac.binary")
revdep_check_save_summary()
revdep_check_print_problems()
revdep_email(date = "Feb 7", version = "v0.9.2", only_problems = FALSE, draft = TRUE)
