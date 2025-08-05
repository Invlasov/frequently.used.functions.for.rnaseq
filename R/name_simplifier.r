#' These scripts simplify names by removing some symbols. Mostly for system path creation and naming headers. _lower and upper also put them in a lower or upper case.
#' @param x Vector of characters to replace stuff in.
#' @param replacement_in Characters to replace.
#' @param replacement_out Characcters to put in instead.
#' @param deletion Characters to delete
#' @export
#' @return Vector of characters with replacements.

name_simplifier<-function(x,replacement_in=" ",replacement_out="_",deletion=c("\\)","\\(","%")) {
	if (!any((is.na(replacement_in))) & !any(is.na(replacement_out))) {
		if (length(replacement_out)!=length(replacement_in)) {
			cycles<-ceiling(length(replacement_in)/length(replacement_out))
			toblerone<-rep("",length=(cycles*length(replacement_out)))
			# print(length(toblerone))
			for (i in 1:cycles) {
				target<-(1+length(replacement_out)*(i-1)):(length(replacement_out)*i)
				# print(target)
				toblerone[target]<-replacement_out
			
			}
			replacement_vec<-toblerone[1:(length(replacement_in))]
			# print(replacement_vec)
		} else {
			replacement_vec<-replacement_out
		}
			#print(paste0(replacement_in))
			#print(replacement_vec)
			
		for (i in 1:length(replacement_in)) {
		x<-gsub(replacement_in[i],replacement_vec[i],x)
		}
		for (i in 1:length(deletion)) {
		# print(paste0("test_",deletion[i]))
		x<-gsub(deletion[i],"",x)
		}
		return(x)

	}
}

name_simplifier_lower<-function(x,...) {
	out_x<-name_simplifier(x,...)
	out_x<-tolower(out_x)
	return(out_x)
}

name_simplifier_upper<-function(x,...) {
	out_x<-name_simplifier(x,...)
	out_x<-toupper(out_x)
	return(out_x)
}
