test_that("`pcbj_dotaro_deck()`", {
	skip_if_not(has_runtime_dependencies())
	skip_if_not_installed("dotaro.deck")
	f <- pcbj_dotaro_deck()
	expect_true(file.exists(f))
	unlink(f)
})
