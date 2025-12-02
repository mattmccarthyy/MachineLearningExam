require(glmnet)

regularized.model <- function(x, response, alpha=1, ...){
# wrapping glmnet into a more user-friendly function
	# prepare the data
	if(!(response %in% names(x))){
		stop("The response name provided is not included in the dataset")
	}
	fml = as.formula(paste(response, "~."))
	xm = model.matrix(fml, data=x)[,-1]
	y = x[[response]]

	if(class(y)=="factor"){
		if(length(levels(y))==2){
			fam = "binomial"
		} else {
			fam = "multinomial"
		}
	} else {
		fam = "gaussian"
	}
	
	lam = cv.glmnet(xm, y, alpha=alpha, 
		family=fam, ...) 
	mod = glmnet(xm, y, alpha=alpha, lambda=lam$lambda.min, 
		family=fam, ...)
	mod.fit = predict(mod, newx=xm)
	return(list(model=mod, fitted.values=mod.fit, 
			y=response, family=fam))
}

predict.regularized.model <- function(fit, test.data, ...){
	fml = as.formula(paste(fit$y, "~."))
	newx = model.matrix(fml, data=test.data)[,-1]
	return(predict(fit$model, newx, ...))
}

