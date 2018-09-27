# Load required libraries
local({
  if(! library(lodown,logical.return=T)) {
    install.packages('devtools');
    require(devtools);
    install_github('ajdamico/lodown',dependencies=T);
  }
  libs <- c('survey','mitools','lodown');
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});


## HELPFUL: https://www.andrewheiss.com/blog/2018/03/08/amelia-broom-huxtable/

scf_MIcombine2 <-
	function (results, variances, call = sys.call(), df.complete = Inf,
            force.names=F, ...) {
		m <- length(results)
		oldcall <- attr(results, "call")

		if (missing(variances)) {
			variances <- suppressWarnings(lapply(results, vcov))
			results <- lapply(results, coef)
		}

    # Force *results* and *variances* to have the same structure across implicates
    if (force.names==T) {
      results.names <- unique(Reduce(c,lapply(results,names)));
      results <- lapply(results,
        function(x) {
          x <- setNames(x[results.names],results.names);
          return(x); });
      variances <- lapply(variances,
        function(x) {
          # Add missing columns or rows
          orig.cols <- colnames(x);
          orig.rows <- rownames(x);
          miss.cols <- results.names[! results.names %in% orig.cols];
          miss.rows <- results.names[! results.names %in% orig.rows];
          x <- cbind(x,matrix(rep(NA,length(miss.cols)*nrow(x)),
                              ncol=length(miss.cols)));
          x <- rbind(x,matrix(rep(NA,length(miss.rows)*ncol(x)),
                              nrow=length(miss.rows)));
          colnames(x) <- c(orig.cols,miss.cols);
          rownames(x) <- c(orig.rows,miss.rows);
          # Order
          x <- x[results.names,];
          x <- x[,results.names];
          return(x); });
    }

		vbar <- variances[[1]]
		cbar <- results[[1]]
		for (i in 2:m) {
			cbar <- cbar + results[[i]]
			# MODIFICATION:
			# vbar <- vbar + variances[[i]]
		}
		cbar <- cbar/m
		# MODIFICATION:
		# vbar <- vbar/m
		evar <- var(do.call("rbind", results))
		r <- (1 + 1/m) * evar/vbar
		df <- (m - 1) * (1 + 1/r)^2
		if (is.matrix(df)) df <- diag(df)
		if (is.finite(df.complete)) {
			dfobs <- ((df.complete + 1)/(df.complete + 3)) * df.complete *
			vbar/(vbar + evar)
			if (is.matrix(dfobs)) dfobs <- diag(dfobs)
			df <- 1/(1/dfobs + 1/df)
		}
		if (is.matrix(r)) r <- diag(r)
		rval <- list(coefficients = cbar, variance = vbar + evar *
		(m + 1)/m, call = c(oldcall, call), nimp = m, df = df,
		missinfo = (r + 2/(df + 3))/(r + 1))
		class(rval) <- "MIresult"
		rval
  }

glance.MIresult <- function(x) {
  output <- data.frame(m=x$nimp,df.residual=mean(x$df,na.rm=T));
  return(output);
}

tidy.MIresult <- function(x, conf.int = FALSE, conf.level = 0.95) {
  output <- data.frame(term=names(coef(x)),estimate=coef(x),
                       std.error=sqrt(diag(vcov(x))),df=x$df,stringsAsFactors=F);
  #model.df <- glance(x)$df
  output[['statistic']] <- output[['estimate']]/output[['std.error']];
  #output[['p.value']] <- 2*pt(abs(output$statistic),model.df,lower.tail=F);
  output[['p.value']] <- 2*pt(abs(output$statistic),output$df,lower.tail=F);

  if (conf.int & conf.level) {
    # Convert conf.level to tail values (0.025 when it's 0.95)
    a <- (1 - conf.level) / 2

    output[['conf.low']] <- output$estimate + output$std.error * qt(a, output$df);
    output[['conf.high']] <- output$estimate + output$std.error * qt((1-a), output$df);
  }

  return(output);
}
