test_that("Domino jackets", {
	skip_if_not(has_runtime_dependencies())
	skip_if_not(cache_has("Rozrywki_Naukowe_Fig._049.jpg"))
	skip_if_not(cache_has("optical_illusion_dominoes.jpg"))
	skip_if_not(cache_has("kittens_playing_dominoes.jpg"))
	f <- sbgj_dominoes_all(instructions = TRUE)
	expect_true(file.exists(f))
	unlink(f)
})
