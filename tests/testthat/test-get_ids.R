# tests for get_ids fxn in taxize
context("get_ids")


test_that("get_ids returns the correct values and classses", {
  skip_on_cran()

  tt <- get_ids("Chironomus riparius", db = "ncbi",
    messages = FALSE, suppress = TRUE)

  expect_equal(tt[[1]][[1]], "315576")
  expect_is(tt, "ids")
  expect_is(tt[[1]], "uid")
  expect_is(tt[[1]][[1]], "character")
})

test_that("get_ids accepts ask and verbose arguments", {
  skip_on_cran()

  expect_message(get_ids("Pinus contorta", db = "ncbi",
    suppress = TRUE))
  expect_message(get_ids("Pinus contorta", db = "ncbi",
    messages = FALSE, suppress = TRUE), NA)
})

nn <- c('Imperata brasiliensis','Hylebates cordatus','Apocopis intermedius',
  'Paspalum subciliatum','Bromus nottowayanus','Chimonobambusa marmorea',
  'Panicum adenophorum','Otatea glauca','Himalayacalamus falconeri',
  'Briza lamarckiana','Trisetum turcicum','Brachiaria subulifolia',
  'Boissiera squarrosa','Arthrostylidium pubescens','Neyraudia reynaudiana'
  ,'Bromus gunckelii','Poa sudicola','Pentameris thuarii',
  'Calamagrostis inexpansa','Willkommia texana','Helictotrichon cantabricum',
  'Muhlenbergia tenuifolia','Sporobolus ioclados','Bambusa cerosissima',
  'Axonopus flabelliformis','Glyceria lithuanica','Pentaschistis malouinensis',
  'Perrierbambus madagascariensis','Hierochloe alpina','Hemarthria compressa',
  'Zizania latifolia','Festuca altaica','Gigantochloa wrayi','Festuca alpina',
  'Aegilops caudata','Elymus cognatus','Agrostis gracililaxa',
  'Gymnopogon foliosus')

test_that("works on a variety of names", {
  skip_on_cran()
  skip_on_ci()

  expect_is(sw(get_ids(nn[13], db = c("ncbi", "itis", "tropicos"),
    suppress = TRUE, ask = FALSE, messages = FALSE)), "ids")
  expect_is(sw(get_ids(nn[14], db = c("ncbi", "tropicos"),
    suppress = TRUE, ask = FALSE, messages = FALSE)), "ids")
  expect_is(sw(get_ids(nn[15], db = c("ncbi", "itis", "tropicos"),
    suppress = TRUE, ask = FALSE, messages = FALSE)), "ids")
})
