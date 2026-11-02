# Make `dev` the active device again, unless it is the null device (1),
# no longer open, or already active.
# NB. `dev.set()` calls the device's (de)activate callbacks even if `dev` is
# already current, and `dev.set(1L)` would open a new device.
restore_dev <- function(dev) {
	if (dev > 1L && dev %in% grDevices::dev.list() && grDevices::dev.cur() != dev) {
		grDevices::dev.set(dev)
	}
	invisible(NULL)
}

# Close the temporary device `temp` and return to `old` (the result of
# `dev.cur()` before `temp` was opened).  `old` is selected *first* so that
# closing `temp` doesn't make some other device current in between.
# Safe to call more than once.
close_temp_dev <- function(temp, old) {
	force(temp) # e.g. `close_temp_dev(dev.cur(), old)` must not see `old`
	restore_dev(old)
	if (temp > 1L && temp != old && temp %in% grDevices::dev.list()) {
		grDevices::dev.off(temp)
	}
	invisible(NULL)
}

# Open a temporary `ragg::agg_capture()` device that is closed (returning to
# the previously active device) when the calling function exits.
# Used so that e.g. `marquee::marquee_grob()` and `{pnpmisc}` don't open (and
# leave open) a new graphics device when none is open.
local_agg_capture <- function(envir = parent.frame()) {
	old <- grDevices::dev.cur()
	ragg::agg_capture(
		width = pnpmisc:::JACKET_4x6_WIDTH,
		height = pnpmisc:::JACKET_4x6_HEIGHT,
		units = "in"
	)
	temp <- grDevices::dev.cur()
	handler <- bquote(close_temp_dev(.(temp), .(old)))
	do.call(base::on.exit, list(handler, add = TRUE), envir = envir)
	invisible(temp)
}

# Used via `on.exit()` by the top-level jacket functions, whose callees
# (e.g. `{grImport2}`, `{marquee}`, `{pnpmisc}`) may open or switch devices.
# If no device was open beforehand, close any devices they left open;
# otherwise return to the previously active device.
restore_devices <- function(current_dev) {
	if (current_dev > 1) {
		restore_dev(current_dev)
	} else {
		grDevices::graphics.off()
	}
	invisible(NULL)
}
