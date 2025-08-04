#input where comment is a number of symbols instead of one
multiple_comment_char.read.table <- function(file, comment.char = "//", ...) {
clean.lines <- sub(paste0(comment.char, ".*"), "", readLines(file))
read.table(..., text = paste(clean.lines, collapse = "\n"),comment.char="")
}
