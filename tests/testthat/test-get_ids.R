# tests for get_ids fxn in taxize
context("get_ids")


test_that("get_ids returns the correct values and classses", {
  skip_on_cran()

  tt <- get_ids(names="Chironomus riparius", db = 'ncbi', messages=FALSE)

  expect_equal(tt[[1]][[1]], "315576")

  expect_that(tt, is_a("ids"))
  expect_that(tt[[1]], is_a("uid"))
  expect_that(tt[[1]][[1]], is_a("character"))
})

test_that("get_ids accepts ask and verbose arguments", {
  skip_on_cran()

  expect_message(get_ids(names="Pinus contorta", db = 'ncbi'))
  expect_message(get_ids(names="Pinus contorta", db = 'ncbi', messages=FALSE), NA)
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
        'Aegilops caudata','Elymus cognatus','Agrostis gracililaxa','Gymnopogon foliosus')

test_that("works on a variety of names", {
  skip_on_cran()

  expect_is(sw(get_ids(nn[13], db = c('ncbi','itis','col','tropicos'), 
    ask=FALSE, verbose=FALSE)), "ids")
  expect_is(sw(get_ids(nn[14], db = c('ncbi','itis','col','tropicos'), 
    ask=FALSE, verbose=FALSE)), "ids")
  expect_is(sw(get_ids(nn[15], db = c('ncbi','itis','col','tropicos'), 
    ask=FALSE, verbose=FALSE)), "ids")
})
