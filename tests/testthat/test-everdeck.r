test_that("`pcbj_everdeck()`", {
	skip_if_not(has_runtime_dependencies())
	skip_if_not(cache_has("Everdeck_Packvelopes.zip"))
	f <- pcbj_everdeck()
	expect_true(file.exists(f))
	unlink(f)
})
