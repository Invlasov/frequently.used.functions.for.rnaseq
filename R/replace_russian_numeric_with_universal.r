#' This function replacecs russian numeric decimal "," with universal "." and saves output as numeric.
#' @param x Vector to replace decimals in. Most likely it will be character vector.
#' @return Numeric vector with replaced decimals.
#' @export

replace_russian_numeric_with_universal<-function(x) {
	x<-as.character(x)
	x<-gsub(",",".",x)
	return(as.numeric(x))

}
