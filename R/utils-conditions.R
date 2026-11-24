# Classed conditions (SPEC 10; Jorge, 2026-11-24). The warnings and messages
# of MFO(), read_cart(), MFO_zones(), MFO_kinetics(), MFO_protocol() and
# MFO_reliability() carry the classes mfo_warning_<id> and mfo_warning (or
# mfo_message_<id> and mfo_message) and their numbers as fields, so another
# program, such as the app, can word them in its own language. In R the
# message is the English text, as before. Example:
#   w <- tryCatch(MFO(...), mfo_warning_negative_fat = function(w) w)
#   w$n   # number of steps with negative fat oxidation

# call. = TRUE keeps the call of the function that warns, as warning() does
.mfo_warning <- function(id, message, ..., call. = FALSE) {
  warning(warningCondition(message, ...,
                           class = c(paste0("mfo_warning_", id), "mfo_warning"),
                           call = if (call.) sys.call(-1)))
}

.mfo_message <- function(id, message, ...) {
  message(structure(class = c(paste0("mfo_message_", id), "mfo_message", "message",
                              "condition"),
                    list(message = paste0(message, "\n"), call = NULL, ...)))
}
