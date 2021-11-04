

# test <- split(dt, by = "id")
#
# tt <- lapply(test, function(x) {
#   bdots:::dgaussPars(x, "looks", "times", TRUE)
# })
#
# rr <- vector("list", length = length(test))
# for (i in seq_along(test)) {
#   rr[[i]] <-  bdots:::dgaussPars(test[[i]], "looks", "times", TRUE)
#   if (length(rr[[i]]) != 6) print(i)
# }
#
# debugonce(bdots:::dgaussPars)
# bdots:::dgaussPars(dt[id == 139, ], "looks", "times", TRUE)
