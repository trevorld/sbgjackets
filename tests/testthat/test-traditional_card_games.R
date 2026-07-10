test_that("`pcbj_bridge()`", {
	skip_if_not(has_runtime_dependencies())
	skip_if_not(cache_has("bridge_postcards.jpg"))
	f <- pcbj_bridge(double = TRUE)
	expect_true(file.exists(f))
	unlink(f)
})

test_that("`pcbj_canasta()`", {
	skip_if_not(has_runtime_dependencies())
	skip_if_not(cache_has("little_red_riding_hood_1.jpg"))
	skip_if_not(cache_has("little_red_riding_hood_2.png"))
	skip_if_not(cache_has("little_red_riding_hood_3.jpg"))
	skip_if_not(cache_has("little_red_riding_hood_4.jpg"))
	f <- pcbj_canasta()
	expect_true(file.exists(f))
	unlink(f)
})

test_that("`pcbj_pinochle()`", {
	skip_if_not(has_runtime_dependencies())
	skip_if_not_installed("gtable")
	skip_if_not(cache_has("pinochle.jpg"))
	f <- pcbj_pinochle(double = TRUE)
	expect_true(file.exists(f))
	unlink(f)
})

test_that("`pcbj_poker()`", {
	skip_if_not(has_runtime_dependencies())
	skip_if_not(cache_has("dogs_playing_poker.png"))
	f <- pcbj_poker(double = TRUE)
	expect_true(file.exists(f))
	unlink(f)
})
