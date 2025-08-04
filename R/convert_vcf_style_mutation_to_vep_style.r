#method to convert_vcf_style_mutation_to_vep_style
convert_vcf_style_mutation_to_vep_style<-function(ref_allele,alt_allele) {
	 if (nchar(ref_allele)>nchar(alt_allele)) {
		#print(substr(ref_allele,1,nchar(alt_allele)))
		if (substr(ref_allele,1,nchar(alt_allele))==alt_allele) {
			output<-c(substr(ref_allele,2,nchar(ref_allele)),"-")
		} else {
			output<-c(ref_allele,alt_allele)
		}
	} else if (nchar(ref_allele)<nchar(alt_allele)) {
		if (substr(alt_allele,1,nchar(ref_allele))==ref_allele) {
			output<-c("-",substr(alt_allele,2,nchar(alt_allele)))
		} else {output<-c(ref_allele,alt_allele)}
	} else {output<-c(ref_allele,alt_allele)}
	
	return(output)
}
