test_that("Playing card jackets", {
	skip_if_not(has_runtime_dependencies())
	skip_if_not(cache_has("bavarian_pattern.jpg"))
	f <- pcbj_bavarian_pattern()
	expect_true(file.exists(f))
	unlink(f)

	skip_if_not(cache_has("fournier_1907.jpg"))
	f <- pcbj_castilian_pattern()
	expect_true(file.exists(f))
	unlink(f)

	skip_if_not(cache_has("worshipful_1897.jpg"))
	f <- pcbj_english_pattern()
	expect_true(file.exists(f))
	unlink(f)
})
