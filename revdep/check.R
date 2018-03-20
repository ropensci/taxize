library("revdepcheck")

res <- revdep_check(num_workers = 4)
revdep_email(date = "Feb 7", version = "v0.9.2", only_problems = FALSE, draft = TRUE)
