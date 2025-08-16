library(future)

message("*** Utility functions ...")

message("- is_na() ...")
stopifnot(is_na(NA), !is_na(TRUE), !is_na(FALSE), !is_na(1),
          !is_na(NULL), !is_na(1:2), !is_na(rep(NA, times = 3)),
          !is_na(rep(TRUE, 3)), !is_na(letters))

message("- is_false() ...")
stopifnot(is_false(FALSE), !is_false(TRUE), !is_false(NA), !is_false(1),
          !is_false(NULL), !is_false(1:2), !is_false(rep(FALSE, times = 3)),
          !is_false(rep(TRUE, times = 3)), !is_false(letters))

message("- attached_packages() ...")
print(attached_packages())


message("- hpaste() & printf() ...")
# Some vectors
x <- 1:6
y <- 10:1
z <- LETTERS[x]

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Abbreviation of output vector
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
printf("x = %s.\n", hpaste(x))
## x = 1, 2, 3, ..., 6.

printf("x = %s.\n", hpaste(x, max_head = 2))
## x = 1, 2, ..., 6.

printf("x = %s.\n", hpaste(x, max_head = 3)) # Default
## x = 1, 2, 3, ..., 6.

# It will never output 1, 2, 3, 4, ..., 6
printf("x = %s.\n", hpaste(x, max_head = 4))
## x = 1, 2, 3, 4, 5 and 6.

# Showing the tail
printf("x = %s.\n", hpaste(x, max_head = 1, max_tail = 2))
## x = 1, ..., 5, 6.

# Turning off abbreviation
printf("y = %s.\n", hpaste(y, max_head = Inf))
## y = 10, 9, 8, 7, 6, 5, 4, 3, 2, 1

## ...or simply
printf("y = %s.\n", paste(y, collapse = ", "))
## y = 10, 9, 8, 7, 6, 5, 4, 3, 2, 1

# Adding a special separator before the last element
# Change last separator
printf("x = %s.\n", hpaste(x, last_collapse = " and "))
## x = 1, 2, 3, 4, 5 and 6.

message("- mcat(), mprintf(), mprint() and mstr() ...")
mcat("Hello world!\n")
mprintf("Hello %s!\n", "world")
mprint("Hello world!")
mstr("Hello world!")

message("- trim() ...")
mprint(trim(" hello "))
stopifnot(trim(" hello ") == "hello")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# is_os()
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("- is_os() ...")
for (os in c("darwin", "freebsd", "irix", "linux", "openbsd",
             "solaris", "windows")) {
  mprintf("is_os('%s') = %s", os, is_os(os))
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# import_future()
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("*** import_future() ...")

future <- import_future("future")
stopifnot(identical(future, future::future))

future <- import_future("<unknown function>", default = future::future)
stopifnot(identical(future, future::future))

res <- try(import_future("<unknown function>"), silent = TRUE)
stopifnot(inherits(res, "try-error"))

message("*** import_future() ... DONE")

message("*** Utility functions ... DONE")

