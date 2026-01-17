test_that("`pcbj_decktet()`", {
	skip_if_not(has_runtime_dependencies())
	skip_if_not(cache_has("decktet_box_cover.jpg"))
	skip_if_not(cache_has("decktet-rules.pdf"))
	f <- pcbj_decktet(double = TRUE)
	expect_true(file.exists(f))
	unlink(f)
})
