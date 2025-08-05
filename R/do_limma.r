# wrapper to run limma.Should always be used instead of running limma manually for consistency-s sake.
#' This will be a test run for adding roxygen comment to files
#' This is wrapper to run limma. Warning - it requires specifically formatted local ensembl annotation to run correctly
#' @param count_matrix Numeric matrix. A matrix with genes in rows and samples in columns
#' @param design_matrix Numeric matrix. A design matrix, with smaples in row matching columns of count and and variables in columns. Should be generated through model.matrix from stats
#' @param output_file_result A path to a file. FC and p.val for chosen contrast for each expressed gene are going to be written in the file with that path
#' @param output_file_degs A path to a file. FC and p.val for only degs (if any) for each expressed gene are going to be written in the file with that path
#' @param local_ensembl_annotation A path to a file. File with specially prepared local annotations TO ADD IN THE FUTURE - SCRIPT FOR CREATING THOSE
#' @param contrast_num Number. A number of contrast to take (by default - 2). Make sure to never write 1 here - it is the intercept
#' @param threshold_fc Number. A threshold to filter by FC. Doesn't do anything if do_fc_filtration=false
#' @param threshold_pval Number. A threshold to filter adjusted by p.val. Doesn't do anything if do_fc_filtration=false
#' @param method_string Character string. One of possible methods for adjustment for stats::p.adjust. Hommel by default
#' @param output_plot_path A path to a file. If not NA - a plot of mena-variance trend will be written there.
#' @param pre_filtration Boolean. Do we do FC filtration before p. adjustment? True by default.
#' @param do_fc_filtration Boolean. Do we do FC filtration at all? True by default.
#' @importFrom grDevices png dev.off 
#' @importFrom stats p.adjust
#' @importFrom utils capture.output read.table write.table
#' @export
#' return Writes outputs to the designated paths
do_limma<-function(count_matrix,design_matrix,output_file_result,output_file_degs,local_ensembl_annotation,contrast_num=2,threshold_fc=1.5,threshold_pval=0.05,method_string="hommel",output_plot_path=NA,pre_filtration=TRUE,do_fc_filtration=TRUE) {

	if (!is.na(output_plot_path)) {
		png(output_plot_path, width=1600,height=900) 
	}
	dge <- edgeR::DGEList(counts=count_matrix)
	dge <- edgeR::calcNormFactors(dge, method="TMM")
	voomed <- limma::voom(dge, design_matrix, plot=TRUE)
	fit <- limma::lmFit(voomed, design_matrix)
	fit <- limma::eBayes(fit)
	fit_coefficients<-fit$coefficients
	fit_p_val<-fit$p.value
	if (ncol(local_ensembl_annotation)>3) {local_ensembl_annotation<-local_ensembl_annotation[,c(1,3,4)]}
		output<-data.frame(local_ensembl_annotation,fit_coefficients[,contrast_num],fit_p_val[,contrast_num])
		# print(ncol(output))
		new_colnames<-c("Ensembl_gene_id","Gene_symbol","Gene_name","log_fc_limma","p.val")
		colnames(output)<-new_colnames
		# print(length(new_colnames))
		output$p.val.adjusted<-p.adjust(output$p.val,method=method_string)

	write.table(output,file=output_file_result,row.names=FALSE,quote=FALSE,sep="\t",col.names=TRUE)
	if (do_fc_filtration) {
		if (pre_filtration) {
			degs_filter_fc<-c(abs(output[,4])>=log(threshold_fc,base=2))
			output<-output[degs_filter_fc,]
			degs_filter_fc<-rep(TRUE,nrow(output))
		} else {
			degs_filter_fc<-c(abs(output[,4])>=log(threshold_fc,base=2))
		}
			
	} else {
		degs_filter_fc<-rep(TRUE,nrow(output))
	}
	output$p.val.adjusted<-p.adjust(output$p.val,method=method_string)
	output_degs<-output[(output$p.val.adjusted<=threshold_pval) & degs_filter_fc,]
	if (!is.na(output_plot_path)) {
		dev.off()
	}
	if (nrow(output_degs)>0) {
		write.table(output_degs,file=output_file_degs,row.names=FALSE,quote=FALSE,sep="\t",col.names=TRUE)
	}
}
