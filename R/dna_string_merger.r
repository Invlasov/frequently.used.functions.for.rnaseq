#This function is for working with vcfs with multiple alternative alleles. It collapses DNA string from alt allele field of variant annotation file to get merged string with comma delimited alt alleles.
dna_string_merger<-function(dnastring,sep=", ",collapse=", ") {
		for_return<-character(length=length(dnastring))
		for (i in 1:length(dnastring)) {
		what_is_that<-as.character(dnastring[[i]])
			if (length(what_is_that)>1) {
			for_return[i]<-paste(what_is_that,sep=sep,collapse=collapse)
			} else {
			for_return[i]<-what_is_that
			}
		}
		return(for_return)
	}
