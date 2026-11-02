open_two_devices <- function() {
	grDevices::pdf(NULL)
	d2 <- grDevices::dev.cur()
	grDevices::pdf(NULL)
	d3 <- grDevices::dev.cur()
	grDevices::dev.set(d2)
	unname(c(d2, d3))
}

close_devices <- function(devs) {
	for (d in intersect(devs, grDevices::dev.list())) {
		grDevices::dev.off(d)
	}
}

test_that("restore_dev()", {
	devs <- open_two_devices()
	on.exit(close_devices(devs), add = TRUE)
	grDevices::dev.set(devs[2L])
	restore_dev(devs[1L])
	expect_equal(grDevices::dev.cur()[[1L]], devs[1L])
	restore_dev(devs[1L])
	expect_equal(grDevices::dev.cur()[[1L]], devs[1L])
	n <- length(grDevices::dev.list())
	restore_dev(1L)
	restore_dev(max(devs) + 10L)
	expect_length(grDevices::dev.list(), n)
	expect_equal(grDevices::dev.cur()[[1L]], devs[1L])
})

test_that("close_temp_dev()", {
	devs <- open_two_devices()
	on.exit(close_devices(devs), add = TRUE)
	old <- grDevices::dev.list()
	grDevices::pdf(NULL)
	temp <- grDevices::dev.cur()
	close_temp_dev(temp, devs[1L])
	expect_equal(grDevices::dev.cur()[[1L]], devs[1L])
	expect_equal(grDevices::dev.list(), old)
	close_temp_dev(temp, devs[1L])
	expect_equal(grDevices::dev.list(), old)
})

test_that("local_agg_capture()", {
	devs <- open_two_devices()
	on.exit(close_devices(devs), add = TRUE)
	old <- grDevices::dev.list()
	f <- function(fail = FALSE) {
		temp <- local_agg_capture()
		grDevices::dev.set(devs[2L])
		if (fail) {
			stop("boom")
		}
		temp
	}
	temp <- f()
	expect_true(temp > max(devs))
	expect_equal(grDevices::dev.cur()[[1L]], devs[1L])
	expect_equal(grDevices::dev.list(), old)

	expect_error(f(fail = TRUE), "boom")
	expect_equal(grDevices::dev.cur()[[1L]], devs[1L])
	expect_equal(grDevices::dev.list(), old)
})

test_that("restore_devices()", {
	devs <- open_two_devices()
	on.exit(close_devices(devs), add = TRUE)
	grDevices::dev.set(devs[2L])
	restore_devices(devs[1L])
	expect_equal(grDevices::dev.cur()[[1L]], devs[1L])
	grDevices::dev.off(devs[2L])
	old <- grDevices::dev.list()
	restore_devices(devs[2L])
	expect_equal(grDevices::dev.cur()[[1L]], devs[1L])
	expect_equal(grDevices::dev.list(), old)
})

test_that("restore_devices() closes devices if none were open", {
	skip_if(!is.null(grDevices::dev.list()), "graphics devices already open")
	devs <- open_two_devices()
	on.exit(close_devices(devs), add = TRUE)
	restore_devices(1L)
	expect_null(grDevices::dev.list())
})
