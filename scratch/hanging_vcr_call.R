library(vcr)
library(taxize)

# Works as expected
children(161994, "itis")

# Hangs indefinitely
use_cassette("deleteme", {
  children(161994, "itis")
})

# Clean up
file.remove("deleteme.yml")
