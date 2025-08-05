#This function is for working with vcfs with multiple alternative alleles. It collapses DNA string from alt allele field of variant annotation file to get merged string with comma delimited alt alleles.
#' This function is for working with vcfs with multiple alternative alleles. It collapses DNA string from alt allele field of variant annotation file to get merged string with comma delimited alt alleles.
#' @param dnastring A vector of alternative alleles.
#' @param collapse A delimiter to put stuff in.
#' @return A string of with collapse delimited alt alleles.
#' @export

dna_string_merger<-function(dnastring,collapse=", ") {
		for_return<-character(length=length(dnastring))
		for (i in 1:length(dnastring)) {
		what_is_that<-as.character(dnastring[[i]])
			if (length(what_is_that)>1) {
			for_return[i]<-paste0(what_is_that,collapse=collapse)
			} else {
			for_return[i]<-what_is_that
			}
		}
		return(for_return)
	}
