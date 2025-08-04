#wrapper to run limma.Should always be used instead of running limma manually for consistency-s sake.
do_limma<-function(count_matrix,design_matrix,output_file_result,output_file_degs,local_ensembl_annotation,contrast_num=2,threshold_fc=1.5,threshold_pval=0.05,method_string="hommel",output_plot_path,pre_filtration=TRUE,do_fc_filtration=TRUE) {
     require(limma)
	 require(edgeR)

	 png(output_plot_path, width=1600,height=900) 
	 dge <- DGEList(counts=count_matrix)
     dge <- calcNormFactors(dge, method="TMM")
     voomed <- voom(dge, design_matrix, plot=TRUE)
     fit <- lmFit(voomed, design_matrix)
     fit <- eBayes(fit)
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
	dev.off()
     if (nrow(output_degs)>0) {
         write.table(output_degs,file=output_file_degs,row.names=FALSE,quote=FALSE,sep="\t",col.names=TRUE)
     }
 }
