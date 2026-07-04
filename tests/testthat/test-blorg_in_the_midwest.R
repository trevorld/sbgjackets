test_that("`pcw_blorg_in_the_midwest()`", {
	skip_if_not(has_runtime_dependencies())
	skip_if_not(cache_has("Blorg_in_the_Midwest_rules.pdf"))
	f <- pcw_blorg_in_the_midwest()
	expect_true(file.exists(f))
	unlink(f)
})
