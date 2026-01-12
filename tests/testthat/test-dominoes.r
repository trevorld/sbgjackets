test_that("Domino jackets", {
	skip_if_not(has_runtime_dependencies())
	skip_if_not(cache_has("Rozrywki_Naukowe_Fig._049.jpg"))
	f <- sbgj_dominoes_double6()
	expect_true(file.exists(f))
	unlink(f)

	skip_if_not(cache_has("optical_illusion_dominoes.jpg"))
	f <- sbgj_dominoes_double9()
	expect_true(file.exists(f))
	unlink(f)

	skip_if_not(cache_has("kittens_playing_dominoes.jpg"))
	f <- sbgj_dominoes_double12()
	expect_true(file.exists(f))
	unlink(f)
})
