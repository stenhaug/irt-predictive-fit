anova_pvalue <- function(model2, model1){
	if(class(model1) != class(model2)){
		return(NA_real_)
	}
	quiet(anova(model2, model1)$p[2])
}
