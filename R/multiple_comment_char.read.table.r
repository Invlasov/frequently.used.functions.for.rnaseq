#' This function acts same as read table, but is able to handle comments with more than 1 char, like ## for parsing vcfs.
#' I didn't write it, I stole it from somwhere on stack overflow.
#' @param file input path for file to read
#' @param comment.char new comment char to process.
#' @param ... Various parameters you would pass to a read.table.
#' @export
#' @return Table from  without multi-comment lines.

multiple_comment_char.read.table <- function(file, comment.char = "##", ...) {
clean.lines <- sub(paste0(comment.char, ".*"), "", readLines(file))
input<-read.table(file,..., text = paste(clean.lines, collapse = "\n"),comment.char="")
return(input)
}
