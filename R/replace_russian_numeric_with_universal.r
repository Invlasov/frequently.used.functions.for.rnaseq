#replacecs russian numeric decimal , with universal . and saves output as numeric
replace_russian_numeric_with_universal<-function(x) {
	x<-as.character(x)
	x<-gsub(",",".",x)
	return(as.numeric(x))

}
