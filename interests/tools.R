# Load required libraries
local({
  libs <- c('data.table','ggplot2','interp','rgl','lubridate');
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});

# Load treasury zero coupon interest rates
if (!all(sapply(c('treasury.zero','zero.yield','zero.yield.m'),exists))) {
  source('zero.R',chdir=T);
}

# Load auxiliary tools
if (!all(sapply(c('mseq','intercalate','replace.na','group.agg'),exists))) {
  source('../aux.R',chdir=T);
}

# Convert interst rates compounded at one interval to another
# From and to are the compounding frequency(per year for annual rates).
# Inf means continously compounded.
int.conv <- function(int,from,to) {
  # Check that all arguments have the same length as interest or length==1
  stopifnot(Reduce(`&`,lapply(list(from,to),function(x) length(x) %in% c(1, length(int)))));

  # Check that all from and to frequencies are strictly positive
  stopifnot(Reduce(`&`,lapply(list(from,to),function(x) x[!is.na(x)]>0)));

  # Create data.table and keep track of order
  work <- data.table(int=int,from=from,to=to)[,id:=.I];

  # If from and to are equal, skip everything
  work[from==to, out:=int]

  # Convert from continous compounding to non-contionous
  work[is.na(out) & from==Inf & to!=Inf,out:=(exp(int/100)^(1/to)-1)*to*100]
  # Convert from non-continous compounding to contionous
  work[is.na(out) & from!=Inf & to==Inf,out:=log((1+int/100/from)^from)*100]
  # Convert from and to different non continous compounding
  work[is.na(out) & from!=Inf & to!=Inf,out:=((1+int/100/from)^(from/to)-1)*to*100]

  # Return results, respect original order
  return(work[order(id),out]);
}

## 3D interpolation
# Maybe raster 2D domain and use libraries that work with rasters. xyValues() replaced with extract()
# https://stat.ethz.ch/pipermail/r-sig-geo/2010-June/008663.html
# 1) akima (3D spline) akima::interp(). It has bad license,
#   - Linear but not spline akima can be replaced by GPL package interp::interp()
# 2) double spline(spline independently for each dimension)
# 3) deldir (with triangles, linear)
# 4) Bilinear interpolation with fields::interp.surface().
#   - Can't extrapolate and requires uniform spacing between intervals.
# 5) Linearize inputs and apply splinefun or other interpolation on 1/2D 
#   - Linearize with: Hilbert curve, Moore curve, Sierpinski curve

interp.int <- function(dates, maturities, data.int=treasury.zero, na.maturity=NA) {
  # NAs should be replace by only one value
  stopifnot(length(na.maturity)==1);

  # Don't modify original data.int, copy by reference
  data.int <- copy(data.int);

  # Transform dates to numbers. Recast dates in case they are characters.
  if (!is.numeric(dates)) dates <- as.numeric(as.Date(dates));
  data.int[,date:=as.numeric(as.Date(date))];

  # Don't accept negative maturities
  stopifnot(all(is.na(maturities) | (maturities>=0)));

  # If interpolation is just for one date, expand 
  if(length(dates)==1) dates <- rep(dates,length(maturities));

  # Same length
  stopifnot(length(dates)==length(maturities));

  # Safely deal with points outside of range.
  # Dates are well(regularly) distributed. Just move outside values to border
  date.limit <- data.int[,.(min=min(date),max=max(date))];
  dates <- ifelse(dates>date.limit$max, date.limit$max, dates);
  dates <- ifelse(dates<date.limit$min, date.limit$min, dates);

  # For maturities, lower bound exists and is equal across dates. Lower values -> Error.
  # Upper bound is heterogeneous accross dates. Move upper values -> upper bound
  ## Use linear interpolation for upper boundary
  data.mat.max <- data.int[,.(x=as.numeric(date),y=maturity)][,.(y=max(y)),by=x];
  maturities.max <- approx(x=data.mat.max$x,y=data.mat.max$y,
                           xout=as.numeric(dates),method='linear')$y;
  maturities <- ifelse(maturities>maturities.max, maturities.max, maturities);
  ## Use alphahull::ahull() to compute boundary of interpolation ranger
  ## Just rectangular
  #maturity.limit <- data.int[,.(min=0,max=max(maturity))];
  #maturities <- ifelse(maturities>maturity.limit$max, maturity.limit$max, maturities);
  #maturities <- ifelse(maturities<maturity.limit$min, maturity.limit$min, maturities);

  # Create (date,x,y) points from (date,maturity,int) points.
  # Add points (date,0,0) for all dates.
  data.train <- rbindlist(list(
    data.table(x=unique(data.int[!is.na(date),date],by='date'),y=0,z=0),
    data.int[,.(x=date,y=maturity,z=int)]
    ), use.names=T);

  # 2D Picewise linear interpolation
  # Extrapolation impossible for picewise linear outside of convex hull
  return(interp::interp(x=data.train$x,y=data.train$y,z=data.train$z,
                        xo=dates,yo=maturities,method='linear',
                        output='points',extrap=F)$z);

  # Akima interpolation. Bad license
  # Linear interpolation Ok, spline interpolation very slow
  #akima::interp(x=data.train$x,y=data.train$y,z=data.train$z,
  #              xo=dates,yo=maturities,extrap=T,linear=T)

  ## The following methods require regular grids, convert data to wide format
  #data.train.w <- merge(data.train,expand.grid(x=unique(data.train[,x],by='x'),
  #                             y=unique(data.train[,y],by='y')),all=T);
  #setkey(data.train.w,x,y);
  #data.train.w[,z:=mean(z),by=.(x,y)]
  #data.train.w <- list(
  #  x=unique(data.train.w[,x],by='x'),
  #  y=unique(data.train.w[,y],by='y'),
  #  z=matrix(data=as.vector(as.matrix(data.train.w[,z])),byrow=T,
  #                  nrow=length(unique(data.train.w[,x],by='x'))));
  ## Check data.train.w
  #stopifnot(dim(data.train.w$z)==c(length(data.train.w$x),length(data.train.w$y)))

  ## Bilinear interpolation
  ## Quick, but fails lossless test
  #return(fields::interp.surface(obj=data.train.w,
  #                              loc=cbind(x=dates,y=maturities)));

  ## Akima bilinear interpolation. Bad license
  ## Also, can't deal with NAs on grid
  #return(akima::bilinear(x=data.train.w$x,y=data.train.w$y,z=data.train.w$z,
  #       x0=dates,y0=maturities)$z);

  ## Double spline interpolation
  ## For each data.int maturity serie, interpolate int with respect to dates
  #data.train.sub2 <- data.table(x=numeric(),y=numeric(),z=numeric());
  #for (yy in unique(data.train[,y],by='y')) {
  #  data.train.sub <- data.train[y==yy,];
  #  data.train.sub2 <- rbindlist(list(data.train.sub2,
  #    data.table(x=dates,
  #               y=yy,
  #               z=spline(x=data.train.sub$x,y=data.train.sub$z,xout=dates)$y
  #               )), use.names=T);
  #}
  ## For date, interpolate interest with interpolated date pairs
  #return(data.table(date=dates,maturity=maturities)[,z:=spline(
  #             x=data.train.sub2[x==date,y],
  #             y=data.train.sub2[x==date,z],xout=maturity)$y,by=date]$z);
}

interp.int2 <- function(data.int=treasury.zero, by, na.maturity=NA) {
  # NAs should be replace by only one value
  stopifnot(length(na.maturity)==1);

  # Don't modify original data.int, copy by reference
  data.int <- copy(data.int);

  by <- as.data.table(by);

  stopifnot('int' %in% colnames(data.int));

  datacols <- copy(colnames(data.int));
  groupcols <- copy(colnames(by));
  stopifnot(all(groupcols %in% datacols) & all(setdiff(datacols,'int') %in% groupcols));

  # Transform dates to numbers. Recast dates in case they are characters.
  by[,date:=as.numeric(as.Date(date))];
  data.int[,date:=as.numeric(as.Date(date))];

  # Safely deal with points outside of range.
  # Dates are well(regularly) distributed. Just move outside values to border
  date.limit <- data.int[,.(min=min(date),max=max(date))];
  by[,date:=pmax(pmin(date,date.limit$max),date.limit$min)];

  if ('log_maturity' %in% groupcols) {
    # For maturities, lower bound exists and is equal across dates. Lower values -> Error.
    # Upper bound is heterogeneous accross dates. Move upper values -> upper bound
    ## Use linear interpolation for upper boundary
    data.log_mat.max <- data.int[,.(x=as.numeric(date),y=log_maturity)][,.(y=max(y)),by=x];
    by[,log_maturity.max:=approx(x=data.log_mat.max$x,y=data.log_mat.max$y,
                                 xout=as.numeric(date),method='linear')$y];
    data.log_mat.min <- data.int[,.(x=as.numeric(date),y=log_maturity)][,.(y=min(y)),by=x];
    by[,log_maturity.min:=approx(x=data.log_mat.min$x,y=data.log_mat.min$y,
                                 xout=as.numeric(date),method='linear')$y];
    by[,log_maturity:=pmax(pmin(log_maturity,log_maturity.max),log_maturity.min)];
    by[,c('log_maturity.max','log_maturity.min'):=.(NULL,NULL)];
    ### Or Just rectangular
    #log_maturity.limit <- data.int[,.(min=min(log_maturity),max=max(log_maturity))];
    #by[,maturity:=pmax(pmin(log_maturity,log_maturity.limit$max),log_maturity.limit$min)];

    # Create (date,x,y) points from (date,maturity,int) points.
    # Add points (date,0,0) for all dates.
    data.train <- data.int[,.(x=date,y=log_maturity,z=int)];

    # Rename variables
    by[,c('x','y','date','log_maturity'):=.(date,log_maturity,NULL,NULL)];
  } else {
    # Don't accept negative maturities
    stopifnot(by[,all(is.na(maturity) | (maturity>=0))]);

    # For maturities, lower bound exists and is equal across dates. Lower values -> Error.
    # Upper bound is heterogeneous accross dates. Move upper values -> upper bound
    ## Use linear interpolation for upper boundary
    #data.mat.max <- data.int[,.(x=as.numeric(date),y=maturity)][,.(y=max(y)),by=x];
    #by[,maturity.max:=approx(x=data.mat.max$x,y=data.mat.max$y,
    #                         xout=as.numeric(date),method='linear')$y];
    #by[,date:=pmax(pmin(maturity,maturity.max),0)];
    #by[,maturity.max:=NULL];
    data.mat.max <- data.int[,.(x=as.numeric(date),y=maturity)][,.(y=max(y)),by=x];
    by[,maturity.max:=approx(x=data.mat.max$x,y=data.mat.max$y,
                                 xout=as.numeric(date),method='linear')$y];
    data.mat.min <- data.int[,.(x=as.numeric(date),y=maturity)][,.(y=min(y)),by=x];
    by[,maturity.min:=approx(x=data.mat.min$x,y=data.mat.min$y,
                                 xout=as.numeric(date),method='linear')$y];
    by[,maturity:=pmax(pmin(maturity,maturity.max),maturity.min)];
    by[,c('maturity.max','maturity.min'):=.(NULL,NULL)];

    ## Use alphahull::ahull() to compute boundary of interpolation ranger
    ## Or Just rectangular
    #maturity.limit <- data.int[,.(min=0,max=max(maturity))];
    #by[,maturity:=pmax(pmin(maturity,maturity.limit$max),maturity.limit$min)];

    # Create (date,x,y) points from (date,maturity,int) points.
    # Add points (date,0,0) for all dates.
    data.train <- rbindlist(list(
      data.table(x=unique(data.int[!is.na(date),date],by='date'),y=0,z=0),
      data.int[,.(x=date,y=maturity,z=int)]
      ), use.names=T);

    # Rename variables
    by[,c('x','y','date','maturity'):=.(date,maturity,NULL,NULL)];
  }

  # 2D Picewise linear interpolation
  # Extrapolation impossible for picewise linear outside of convex hull
  return(by[,interp::interp(x=data.train$x,y=data.train$y,z=data.train$z,
                            xo=x,yo=y,method='linear',
                            output='points',extrap=F)$z]);

  # Akima interpolation. Bad license
  # Linear interpolation Ok, spline interpolation very slow
  #return(by[akima::interp(x=data.train$x,y=data.train$y,z=data.train$z,
  #                        xo=x,yo=y,extrap=T,linear=T)]);

  ## The following methods require regular grids, convert data to wide format
  #data.train.w <- merge(data.train,expand.grid(x=unique(data.train[,x],by='x'),
  #                             y=unique(data.train[,y],by='y')),all=T);
  #setkey(data.train.w,x,y);
  #data.train.w[,z:=mean(z),by=.(x,y)]
  #data.train.w <- list(
  #  x=unique(data.train.w[,x],by='x'),
  #  y=unique(data.train.w[,y],by='y'),
  #  z=matrix(data=as.vector(as.matrix(data.train.w[,z])),byrow=T,
  #                  nrow=length(unique(data.train.w[,x],by='x'))));
  ## Check data.train.w
  #stopifnot(dim(data.train.w$z)==c(length(data.train.w$x),length(data.train.w$y)))

  ## Bilinear interpolation
  ## Quick, but fails lossless test
  #return(by[fields::interp.surface(obj=data.train.w,
  #                                 loc=cbind(x=x,y=y))]);

  ## Akima bilinear interpolation. Bad license
  ## Also, can't deal with NAs on grid
  #return(by[akima::bilinear(x=data.train.w$x,y=data.train.w$y,z=data.train.w$z,
  #          x0=x,y0=y)$z]);

  ## Double spline interpolation
  ## For each data.int maturity serie, interpolate int with respect to dates
  #data.train.sub2 <- data.table(x=numeric(),y=numeric(),z=numeric());
  #for (yy in unique(data.train[,y],by='y')) {
  #  data.train.sub <- data.train[y==yy,];
  #  data.train.sub2 <- rbindlist(list(data.train.sub2,
  #    by[,.(x=x, y=yy,
  #          z=spline(x=data.train.sub$x,y=data.train.sub$z,xout=x)$y
  #               )], use.names=T);
  #}
  ## For date, interpolate interest with interpolated date pairs
  #return(by[,z:=spline(x=data.train.sub2[x==date,y],
  #                     y=data.train.sub2[x==date,z],xout=y)$y,by=x]$z);
}

## Test that interpolation is lossless
#treasury.zero[!is.na(int),.(int,int2=interp.int(dates=date,maturities=maturity,data.int=.SD))][,
#                all.equal(int,int2,check.attributes=F,tolerance=0.001)];

## Graphic inspection tests
#plot3d(treasury.zero$date,treasury.zero$maturity,treasury.zero$int)
#test <- as.data.table(expand.grid(x=seq(from=as.numeric(treasury.zero[,min(date)]),
#                          to=as.numeric(treasury.zero[,max(date)]),length.out=200),
#                    y=seq(from=treasury.zero[,min(maturity)],
#                          to=treasury.zero[,max(maturity)],length.out=200)));
#setkey(test[,z:=interp.int(dates=x,maturities=y,data.int=treasury.zero)],x,y);
#persp3d(x=unique(test[,x],by='x'),y=unique(test[,y],by='y'),
#        z=matrix(test[,z],nrow=length(unique(test[,x],by='x')),byrow=T),
#        col='blue');

# Given a year and maturity, interpolates interest for each month and
# aggregates at year level using weights. Returns average of interest rate.
interp.int.y <- function(data.int=treasury.zero, weights, by, na.maturity=NA,
                         geomean=F) {

  # Cast input as data.table
  weights <- copy(as.data.table(weights));
  by <- as.data.table(by);

  # Check that *date* column exists on *data* and *weights*
  stopifnot('date' %in% colnames(weights));

  # Required data columns to group by
  rgroupcols <- c('year', 'maturity');
  rdatacols <- c('date', 'maturity');

  # Check that required data columns to group by are present in *by*
  stopifnot(all(rgroupcols %in% colnames(by)));
  # Check that required data columns to group by are present in *data.int*
  stopifnot(all(rdatacols %in% colnames(data.int)));
  # Check that only one other variable is set in *data.int*
  stopifnot(sum(!colnames(data.int) %in% rdatacols)==1);

  # For each unique combination, the interest rate will be obtained
  # First, expand year to months
  data <- unique(x=by[,mget(rgroupcols)],by=rgroupcols);
  data <- data[,cbind(.SD[rep(1:.N,each=12),],
                       month=rep(1:12,times=.N))];

  # Create date variable for *data*
  data[,date:=as.Date(ISOdate(year,month,1))];
  data[,month:=NULL];

  # Get interest rate to *data*
  #data[,int:=interp.int(dates=date, maturities=maturity, data.int=data.int,
  #                      na.maturity=na.maturity)];
  data[,int:=interp.int2(by=list(date=date, maturity=maturity), data.int=data.int,
                         na.maturity=na.maturity)];

  if (geomean==T) {
    data[,int:=(1+int/100)]
    output <- group.agg(data=data,
                        weights=weights[,year:=(lubridate::year(date))],
                        by=by,geomean=T);
    output <- (output-1)*100;
  } else {
    output <- group.agg(data=data,
                        weights=weights[,year:=(lubridate::year(date))],
                        by=by);
  }

  return(output);
}

# NPV for continuous interst rates
# Relative dates to present, in years and numeric
# interest rate in number, not percentage
NPV.c <-function(cfs, reldates, int){
   return(sum(cfs*exp(-int * reldates)));
}

# IRR for continuous interst rates
# Relative dates to present, in years and numeric
IRR.c <- function(cfs, reldates, tol=NULL, upper=Inf, lower=0, max.iters=1000,npvreltol=NULL) {
  # Not tested for past dates in cash flows
  stopifnot(all(reldates>=0));

  # Cash flows and dates must have the same length
  stopifnot(length(unique(lapply(mget(c('cfs','reldates')),length)))==1);

  # If cashflows sum to zero, interest rate is zero. Algos produce NaN
  if (sum(cfs)==0) return(0);

  # Set default tolerance for algorithms
  if(is.null(tol) | length(tol)==0) tol <- 1.5e-8;

  # Set default tolerance for acepting NPV=0.
  # Relative to Cash flow 0, so abs(npv)<npvreltol*abs(cf0)
  if(is.null(npvreltol) | length(npvreltol)==0) npvreltol <- 1e-4;

  ## Optim function
  #return(optim(function(x){NPV.c(cfs=cfs, reldates=reldates, x)^2},
  #             par=0.1,method='L-BFGS-B',lower=0,control=list(maxit=max.iters))$par);

  ## nlminb function
  #return(nlminb(objective=function(x){NPV.c(cfs=cfs, reldates=reldates, x)^2},
  #              start=0.1,lower=lower,upper=upper,control=list(eval.max=max.iters))$par);

  # Dates can't be repeated for cf at t=0, all cashflows must be grouped
  # Also, at least a time 0 figure must be given (NPV)
  stopifnot(sum(reldates==0)==1);

  ## Secant method
  ## https://en.wikipedia.org/wiki/Internal_rate_of_return#Calculation
  ## https://en.wikipedia.org/wiki/Secant_method
  next_r <- function(r_n1, r_n0, npv_n1, npv_n0, c_0, n) {
    return(r_n1 - npv_n1/(npv_n1-npv_n0)*(r_n1-r_n0));
  }

  # Use improved secant method if 1st cashflow negative and future
  # cashflows semi-positive. I didn't have success
  # Improvements on Secant Method for Estimating IRR, Mooten and Thron
  #next_r <- function(r_n1, r_n0, npv_n1, npv_n0, c_0, n) {
  #  return(r_n1 - (r_n1-r_n0)*npv_n1/(npv_n1-npv_n0)*
  #            (1-npv_n0/(npv_n0-3*npv_n1+2*c_0)*
  #             ifelse(npv_n1>npv_n0,
  #                    1.4 + ifelse(npv_n1>0,n*r_n1,0)*ifelse(npv_n0>0,0.5,0.3),
  #                    1)));
  #}

  cf0 <- sum(cfs[reldates==0]);
  r_n0 <- log(sum(cfs[reldates!=0])/abs(cf0))*2/(length(cfs))
  r_n1 <- r_n0 * log(sum(cfs[reldates!=0])/abs(cf0))/
                 log(sum(cfs[reldates!=0])/NPV.c(cfs=cfs[reldates!=0],
                                                 reldates=reldates[reldates!=0],
                                                 int=r_n0));
  r_n1 <- ifelse(!is.finite(r_n1),r_n0,r_n1);

  npv_n0 <- NPV.c(cfs=cfs, reldates=reldates, int=r_n0);
  npv_n1 <- NPV.c(cfs=cfs, reldates=reldates, int=r_n1);

  err <- Inf;
  iters <- 0;
  while(iters < max.iters) {
    r_n2 <- next_r(r_n1, r_n0, npv_n1, npv_n0, cf0, length(cfs)-1);

    # If next value can't be computed, return this one
    # Very likely due to NPV already zero and producing division-by-zero
    if (!is.finite(r_n2)) {
      r_n2 <- r_n1;
      break;
    }

    r_n0 <- r_n1;
    r_n1 <- r_n2;
    npv_n0 <- npv_n1;
    npv_n1 <- NPV.c(cfs=cfs, reldates=reldates, int=r_n1);

    iters <- iters + 1;
  }

  # If solution is acceptable, return it
  if ((!is.na(r_n1)) & (abs(npv_n1)<npvreltol*abs(cf0))) return(r_n1);

  # Otherwise return NA
  return(NA_real_);
}

# Calculate present value of an annuity.
# Interest in continously compounded percentage
annuity.pv.c <- function(int,ypayfreq,npay,pay=0,balloon=0) {
  # All inputs must have the same length
  stopifnot(all(unlist(lapply(mget(c('pay','ypayfreq','npay','balloon')),length)) %in%
                  c(1,length(int))));

  return(ifelse(int==0,
                pay*npay+balloon,
                pay/(exp(int/100/ypayfreq)-1)*(1-exp(-int/100*npay/ypayfreq)) +
                  balloon/exp(int/100*npay/ypayfreq)));
}

# Solves interest rate for annuity. Returns continously compounded interest rate.
annuity.IRR.c <- function(amount,ypayfreq,npay,pay,
                          balloon=rep(0,length(amount)),extrapol=T,tol=NULL) {

  # All argumenst must have the same length
  stopifnot(length(unique(lapply(mget(c('amount','ypayfreq','npay','pay','balloon')),length)))==1);

  # Don't accept negative ypayfreq
  stopifnot(all(is.na(ypayfreq) | (ypayfreq>=0)));

  # Create data.table from input. Keep track of order.
  data.input <- data.table(amount=amount,ypayfreq=ypayfreq,
                           npay=npay,pay=pay,balloon=balloon)[,id:=.I];

  # Only work for cases that are unique, group id
  data.input[,gid:=.GRP,by=.(amount,ypayfreq,npay,pay,balloon)];

  # Only do work for rows with enough data for periodic payments
  # For each of them, select first memeber of group id
  data.work <- data.input[!(is.na(amount) | is.na(ypayfreq) | is.na(npay) | is.na(pay)),
                          .SD[1,], by=gid];

  # One of c(maturity,ypayfreq,npay) is redundant. Maturity is the one with less
  # accuracy, discard it. Overestimating the number of payments is very detrimental
  # to interest rate calculation. Small inacuracies in the time of payments are not
  # so severe. Use npay and ypayfreq.

  # Expand payments for each loan
  data.work <- data.work[,
          # Repeat each row of original data.table by number of payments
    cbind(.SD[rep(1:.N,times=npay),],
          # Assign an id to each payment
          payid=mseq(from=1,to=npay,by=1,strict.to=T),
          # Calculate the cashflow of that payment, take balloon payment into account
          cf=rep(pay,npay) +
               rep(intercalate(rep(0,.N), replace.na(balloon)),
                   intercalate(npay-1, 1)),
          # Relative time of payment. Use scale param so sequence is consistent with payid
          cftime=mseq(from=1/ypayfreq,to=npay/ypayfreq,length.out=npay))];

  # Add loans that only have maturity and balloon payment
  # Select only the first member per group id
  data.work <- rbindlist(list(
      data.work,
      data.input[!(is.na(amount) | is.na(ypayfreq) | is.na(npay) | is.na(balloon)) &
                   is.na(pay),
                 cbind(.SD[1,], payid=1, cf=balloon[1], cftime=npay[1]/ypayfreq[1]),by=gid]),
    use.names=T);

  # Estimate continuous rate (IRR.c)
  data.out <- data.work[,.(intc=IRR.c(cfs=c(-amount[1],cf),
                                      reldates=c(0,cftime), tol=tol/100,
                                      upper=Inf,
                                      lower=0
                                      )*100),by=gid];

  data.out <- merge(data.input[,.(id,gid)],data.out[,.(gid,intc)],by='gid',all.x=T);

  # Return continous interest rate
  return(as.numeric(data.out[order(id),intc]));
}

# Solves interest rate for annuity. Returns annually compounded interest rate.
annuity.IRR <- function(amount,ypayfreq,npay,pay,
                          balloon=rep(0,length(amount)),extrapol=T,tol=NULL) {

  intc <- annuity.IRR.c(amount,ypayfreq,npay,pay,balloon,extrapol,tol);

  # Transform continuous rate to annual compounding rate
  return((exp(intc/100)-1)*100);
}

# IRR for annuities. Continuous interest percentage.
# Relative dates to present, in years and numeric
annuity.int.c.internal <- function(amount, ypayfreq, npay, pay=0, balloon=0,
                                   tol=NULL, upper=Inf, lower=0, max.iters=1000,
                                   npvreltol=NULL) {
  ## Parameters must be positive
  stopifnot(Reduce(`&`,lapply(list(amount,ypayfreq,npay,pay,balloon),
                                   function(x) all(is.na(x) | (x>=0)))));

  # All inputs must have the same length
  stopifnot(all(unlist(lapply(mget(c('pay','ypayfreq','npay','balloon')),length)) %in%
                  c(1,length(amount))));

  # If balloon is NA, replace it with 0. It is not essential.
  balloon <- ifelse(is.na(balloon),0,balloon);

  # If cashflows sum to zero, interest rate is zero. Algos produce NaN
  if (all((npay*pay+balloon-amount)==0)) return(rep(0,length(amount)));

  # Set default tolerance for algorithms
  if(is.null(tol) | length(tol)==0) tol <- 1.5e-8;

  # Set default tolerance for acepting NPV=0.
  # Relative to Cash flow 0, so abs(npv)<npvreltol*abs(cf0)
  if(is.null(npvreltol) | length(npvreltol)==0) npvreltol <- 1e-4;

  ## Optim function
  #return(optim(function(x){
  #                (-amount + annuity.pv.c(int=x, ypayfreq=ypayfreq,npay=npay,
  #                                        pay=pay, balloon=balloon))^2},
  #             par=0.1,method='L-BFGS-B',lower=0,control=list(maxit=max.iters))$par);

  ## nlminb function
  #return(nlminb(objective=function(x){
  #                (-amount + annuity.pv.c(int=x, ypayfreq=ypayfreq, npay=npay,
  #                                        pay=pay, balloon=balloon))^2},
  #              start=0.1,lower=lower,upper=upper,
  #              control=list(eval.max=max.iters))$par);

  ## Solve it like a polinomial
  ## (balloon+pay)-balloon*x-(amount+pay)*x^npay+amount*x^(npay+1)=0;
  ## x=exp(intc/ypayfreq)
  #p <- c(balloon+pay,balloon,rep(0,npay-2),-(amount+pay),amount)

  ## Secant method
  ## https://en.wikipedia.org/wiki/Internal_rate_of_return#Calculation
  ## https://en.wikipedia.org/wiki/Secant_method
  next_r <- function(r_n1, r_n0, npv_n1, npv_n0, c_0, n) {
    return(r_n1 - (r_n1-r_n0)*npv_n1/(npv_n1-npv_n0));
  }

  # I didn't have success with improved secant method
  # Improvements on Secant Method for Estimating IRR, Mooten and Thron

  r_n0 <- log((pay*npay+balloon)/amount)*2/(npay+1)
  r_n1 <- r_n0 * log((pay*npay+balloon)/amount)/
                 log((pay*npay+balloon)/annuity.pv.c(int=r_n0*100,
                                                     ypayfreq=ypayfreq,
                                                     npay=npay, pay=pay,
                                                     balloon=balloon));
  r_n1 <- ifelse(!is.finite(r_n1),r_n0,r_n1);

  npv_n0 <- -amount + annuity.pv.c(int=r_n0*100,ypayfreq=ypayfreq,
                                  npay=npay,pay=pay,balloon=balloon);
  npv_n1 <- -amount + annuity.pv.c(int=r_n1*100,ypayfreq=ypayfreq,
                                  npay=npay,pay=pay,balloon=balloon);
  err <- Inf;
  iters <- 0;
  while(iters < max.iters) {
    r_n2 <- next_r(r_n1, r_n0, npv_n1, npv_n0, c_0=-amount, n=npay);

    # If next value can't be computed, return this one
    # Very likely due to NPV already zero and producing division-by-zero
    #if (all((!is.finite(r_n2)) | (abs(npv_n1[is.finite(npv_n1)])<tol))) {
    if (all(!is.finite(r_n2))) {
      r_n2 <- r_n1;
      break;
    }

    r_n0 <- ifelse(!is.finite(r_n2),r_n0,r_n1);
    r_n1 <- ifelse(!is.finite(r_n2),r_n1,r_n2);

    npv_n0 <- ifelse(!is.finite(r_n2),npv_n0,npv_n1);
    npv_n1 <- ifelse(!is.finite(r_n2),npv_n1,
                     -amount + annuity.pv.c(int=r_n1*100, ypayfreq=ypayfreq, npay=npay,
                                           pay=pay, balloon=balloon));

    iters <- iters + 1;
  }

  # If solution looks acceptable, use it
  intc <- ifelse(is.finite(r_n1) & ((1+npv_n1/amount)>0) & (abs(log(1+npv_n1/amount))<npvreltol),
                 r_n1,NA_real_);

  ## If cashflows sum to zero, interest rate is zero. Algos produce NaN
  #intc <- ifelse((npay*pay+balloon-amount)==0,0,intc);

  # Return solution
  return(intc*100);
}

annuity.int.c <- function(amount, ypayfreq, npay, pay=0, balloon=0, tol=NULL,
                          upper=Inf, lower=0, max.iters=1000, npvreltol=NULL) {

  input <- data.table(amount=amount, ypayfreq=ypayfreq, npay=npay,
                      pay=pay, balloon=balloon)[,id:=.I];

  input[,gid:=.GRP,by=.(amount,ypayfreq,npay,pay,balloon)];
  work <- input[(!(is.na(amount) | (amount==0) | is.na(ypayfreq) | is.na(npay) |
                   ((is.na(pay) | (pay==0)) & (is.na(balloon) | (balloon==0))))),
                .SD[1,], by=gid];

  ## Parameters must be positive
  #stopifnot(work[,Reduce(`&`,lapply(list(amount,ypayfreq,npay,pay,balloon),
  #                                  function(x) all(is.na(x) | (x>=0))))]);

  ## If balloon is NA, replace it with 0. It is not essential.
  #work[,balloon:=ifelse(is.na(balloon),0,balloon)];

  ## Set default tolerance for algorithms
  #if(is.null(tol) | length(tol)==0) tol <- 1.5e-8;

  ## Set default tolerance for acepting NPV=0.
  ## Relative to Cash flow 0, so abs(npv)<npvreltol*abs(cf0)
  #if(is.null(npvreltol) | length(npvreltol)==0) npvreltol <- 1e-4;

  ### Secant method
  ### https://en.wikipedia.org/wiki/Internal_rate_of_return#Calculation
  ### https://en.wikipedia.org/wiki/Secant_method
  #next_r <- function(r_n1, r_n0, npv_n1, npv_n0, c_0, n) {
  #  return(r_n1 - (r_n1-r_n0)*npv_n1/(npv_n1-npv_n0));
  #}
  ## The "improved secant mentod" is useless, doesn't work
  ## Improvements on Secant Method for Estimating IRR, Mooten and Thron

  #work[,r_n0:=log((pay*npay+balloon)/amount)*2/(npay+1)];
  #work[,r_n1:=r_n0 * log((pay*npay+balloon)/amount)/
  #                   log((pay*npay+balloon)/annuity.pv.c(int=r_n0*100,
  #                                                       ypayfreq=ypayfreq,
  #                                                       npay=npay, pay=pay,
  #                                                       balloon=balloon))];
  #work[((!is.finite(r_n1)) & is.finite(r_n0)), r_n1:=r_n0];

  #work[,npv_n0:= -amount + annuity.pv.c(int=r_n0*100,ypayfreq=ypayfreq,
  #                                      npay=npay,pay=pay,balloon=balloon)];
  #work[,npv_n1:= -amount + annuity.pv.c(int=r_n1*100,ypayfreq=ypayfreq,
  #                                      npay=npay,pay=pay,balloon=balloon)];

  #work[,unsol:=(is.finite(r_n1) & is.finite(npv_n1) & (npv_n1 != 0))];

  #err <- Inf;
  #iters <- 0;
  #while((iters < max.iters) & (work[,sum(unsol,na.rm=T)]>0)) {
  #  #work[unsol==T,r_n2:=next_r(r_n1, r_n0, npv_n1, npv_n0, c_0=-amount, n=npay)];
  #  work[unsol==T,r_n2:=next_r(r_n1, r_n0, npv_n1, npv_n0)];

  #  # If next value can't be computed, return this one
  #  # Very likely due to NPV already zero and producing division-by-zero
  #  #work[unsol & ((!is.finite(r_n2)) | (abs(npv_n1[is.finite(npv_n1)])<tol)),unsol:=F];
  #  work[(unsol==T) & (! is.finite(r_n2)),unsol:=F];

  #  work[unsol==T,
  #       c('r_n0','r_n1','npv_n0','npv_n1'):=.(r_n1,r_n2,npv_n1,
  #           -amount + annuity.pv.c(int=r_n2*100, ypayfreq=ypayfreq,
  #                                  npay=npay, pay=pay, balloon=balloon))];

  #  iters <- iters + 1;
  #}

  ## If solution looks acceptable, use it
  ## Return interest percentage
  #work[,intc:=ifelse(is.finite(r_n1) & (abs(npv_n1)<npvreltol*abs(amount)),
  #                   r_n1, NA_real_)*100];

  work[,intc:=annuity.int.c.internal(amount=amount, pay=pay, ypayfreq=ypayfreq,
                                     npay=npay, balloon=balloon, tol=tol,
                                     upper=upper, lower=lower, max.iters=max.iters,
                                     npvreltol=npvreltol)];

  # Merge work done for all inputs
  out <- merge(input[,.(id,gid)],work[,.(gid,intc)],by='gid',all.x=T);

  # Return continous interest rate
  return(as.numeric(out[order(id),intc]));
}
#system.time(test <- loans[,annuity.int.c(amount=amount, ypayfreq=ypayfreq, npay=npay,
#                                         pay=pay, balloon=balloon_amount)])

spot2annuity.int.c <- function(spot.intc, by, interp=T, ...) {
  # Create data.table from input. Keep track of order.
  input <- as.data.table(by)[,id:=.I];

  # Check that these columns exist
  stopifnot(c('date','ypayfreq','npay','pay','balloon') %in% colnames(input));

  # Don't accept negative ypayfreq, npay, pay or balloon
  stopifnot(input[!Reduce(`&`,lapply(mget(c('ypayfreq','npay','pay','balloon')),
                          function(x) (is.na(x) | (x>=0)))), .N]==0);

  if (any(class(spot.intc) %in% 'function')) {
    getint <- spot.intc;
  } else {
    if ('log_maturity' %in% colnames(spot.intc)) {
      # Check that at least *date*, *maturity* and *int* are defined for spot.intc 
      stopifnot(c('date','log_maturity','int') %in% colnames(spot.intc));

      if (interp==T) {
        getint <- function(date, maturity) {
          interp.int2(by=list(date=date,log_maturity=log(maturity)), data.int=spot.intc);
        }
      } else {
        stop('Interpolation must be used when using log_maturity')
      }
    } else {
      # Check that at least *date*, *maturity* and *int* are defined for spot.intc 
      stopifnot(c('date','maturity','int') %in% colnames(spot.intc));

      if (interp==T) {
        getint <- function(date, maturity) {
          #interp.int(dates=date, maturities=maturity, data.int=spot.intc);
          interp.int2(by=list(date=date,maturity=maturity), data.int=spot.intc);
        }
      } else {
        getint <- function(date, maturity) {
          spot.intc[.(date=date,maturity=maturity),];
        }
      }
    }
  }

  # Only work for cases that are unique, group id
  input[,gid:=.GRP,by=.(date,ypayfreq,npay,pay,balloon)];

  ## Only do work for rows with enough data for periodic payments
  ## For each of them, select first memeber of group id
  #work <- input[!(is.na(date) | is.na(ypayfreq) | is.na(npay) | is.na(pay)),
  #              .SD[1,], by=gid];

  ## One of c(maturity,ypayfreq,npay) is redundant. Maturity is the one with less
  ## accuracy, discard it. Overestimating the number of payments is very detrimental
  ## to interest rate calculation. Small inacuracies in the time of payments are not
  ## so severe. Use npay and ypayfreq.

  ## Expand payments for each loan
  #work <- work[,
  #        # Repeat each row of original data.table by number of payments
  #  cbind(.SD[rep(1:.N,npay),],
  #        # Assign an id to each payment
  #        payid=mseq(from=1,to=npay,by=1,strict.to=T),
  #        # Calculate the cashflow of that payment, inclue balloon payment
  #        cf=rep(pay,npay) +
  #             rep(intercalate(rep(0,.N), replace.na(balloon)),
  #                 intercalate(npay-1, 1)),
  #        # Relative time of payment.
  #        cftime=mseq(from=1/ypayfreq,to=npay/ypayfreq,length.out=npay))];

  ## Add loans that only have maturity and balloon payment
  ## Select only the first member per group id
  #work <- rbindlist(list(
  #    work,
  #    input[(!(is.na(date) | is.na(ypayfreq) | is.na(npay) | is.na(balloon))) &
  #                 is.na(pay),
  #          cbind(.SD[1,], payid=1, cf=balloon[1], cftime=npay[1]/ypayfreq[1]),
  #          by=gid]),
  #  use.names=T);

  ## For each cash flow, this is the continuous risk free interest rate
  #work[,cfintc:=getint(date=date, maturity=cftime)];

  ## Sum risk free discounted cash flows to get risk free present value
  #work <- work[,cbind(.SD[1,],pv=NPV.c(cfs=cf,reldates=cftime, int=cfintc/100)),by=gid];

  work <- input[(!(is.na(date) | is.na(ypayfreq) | is.na(npay))) &
                (!(is.na(pay) | is.na(balloon))),
                .SD[1,], by=gid];

  #work[,pv:=NPV.c(cfs=rep(pay,npay)+c(rep(0,npay-1),balloon),
  #                reldates=seq(1/ypayfreq,npay/ypayfreq,length.out=npay),
  #                int=getint(date,seq(1/ypayfreq,npay/ypayfreq,length.out=npay))/100),by=gid];

  work[,pv:=0];
  for (pp in 1:work[,max(npay,na.rm=T)]) {
    work[pp <= npay,
         pv:=pv+replace.na((replace.na(pay)+ifelse(npay==pp,replace.na(balloon),0))*
                 exp(-getint(date,pp/ypayfreq)/100*pp/ypayfreq))];
  }

  # Estimate continuous risk free rate for dates in which PV can be calculated.
  work[,intc:=annuity.int.c(amount=pv, ypayfreq=ypayfreq, npay=npay,
                            pay=pay,balloon=balloon)];

  out <- merge(input[,.(id,gid)],work[,.(gid,intc)], by='gid',all.x=T);

  # Return continous interest rate
  return(as.numeric(out[order(id),intc]));
}

spot2annuity.int.c.y <- function(spot.intc, weights, by, ...) {
  # Cast input as data.table
  weights <- copy(as.data.table(weights));
  by <- as.data.table(by);

  # Check that *date* column exists on *data* and *weights*
  stopifnot('date' %in% colnames(weights));

  # Columns for calculating annuity
  annuitycols <- c('year', 'ypayfreq', 'npay', 'pay', 'balloon');

  # Check that columns to group by are present in *by*
  stopifnot(annuitycols %in% colnames(by));

  # For each unique combination, the interest rate will be obtained
  # First, expand year to months
  data <- unique(x=by[,mget(annuitycols)],by=annuitycols);
  data <- data[,cbind(.SD[rep(1:.N,each=12),],
                       month=rep(1:12,times=.N))];

  # Create date variable for *data*
  data[,date:=as.Date(ISOdate(year,month,1))];
  data[,month:=NULL];

  # Get interest rate to *data*
  data[,intc:=spot2annuity.int.c(
                spot.intc,
                by=mget(c('date','ypayfreq', 'npay', 'pay', 'balloon')), ...)];

  return(group.agg(data=data,
                   weights=weights[,year:=(lubridate::year(date))],
                   by=by));
}

spot2annuity.int.y <- function(...) {
  # Solve for countinously compounded risk free rate
  intc <- spot2annuity.int.c.y(...);

  # Transform continuous risk free rate to annually compounded rate
  #return((exp(intc/100)-1)*100);
  return(int.conv(intc,from=Inf,to=1));
}

# Create interest rate for annuity from zero coupon. Returns continously compounded interest rate.
# Dates, ypayfreqs(payment frequencies in payments/year)
annuity.rfint.c <- function(dates,ypayfreq,npay,pay,
                            balloon=rep(0,length(dates)),extrapol=T,tol=NULL) {

  # All argumenst must have the same length
  stopifnot(length(unique(lapply(mget(c('dates','ypayfreq','npay','pay','balloon')),length)))==1);

  # Don't accept negative ypayfreq
  stopifnot(all(is.na(ypayfreq) | (ypayfreq>=0)));

  # Create data.table from input. Keep track of order.
  input <- data.table(date=dates,ypayfreq=ypayfreq,
                      npay=npay,pay=pay,
                      balloon=replace.na(balloon))[,id:=.I];

  # Only work for cases that are unique, group id
  input[,gid:=.GRP,by=.(date,ypayfreq,npay,pay,balloon)];

  # Only do work for rows with enough data for periodic payments
  # For each of them, select first memeber of group id
  work <- input[!(is.na(date) | is.na(ypayfreq) |
                  is.na(npay) | is.na(pay)),
                .SD[1,], by=gid];

  ## One of c(maturity,ypayfreq,npay) is redundant. Maturity is the one with less
  ## accuracy, discard it. Overestimating the number of payments is very detrimental
  ## to interest rate calculation. Small inacuracies in the time of payments are not
  ## so severe. Use npay and ypayfreq.

  ## Expand payments for each loan
  #work <- work[,
  #        # Repeat each row of original data.table by number of payments
  #  cbind(.SD[rep(1:.N,npay),],
  #        # Assign an id to each payment
  #        payid=mseq(from=1,to=npay,by=1,strict.to=T),
  #        # Calculate the cashflow of that payment, take balloon payment into account
  #        cf=rep(pay,npay) +
  #             rep(intercalate(rep(0,.N), replace.na(balloon)),
  #                 intercalate(npay-1, 1)),
  #        # Relative time of payment. Use scale param so sequence is consistent with payid
  #        cftime=mseq(from=1/ypayfreq,to=npay/ypayfreq,length.out=npay))];

  ## Add loans that only have maturity and balloon payment
  ## Select only the first member per group id
  #work <- rbindlist(list(
  #    work,
  #    input[(!(is.na(date) | is.na(ypayfreq) | is.na(npay) | is.na(balloon))) &
  #            is.na(pay),
  #          cbind(.SD[1,], payid=1, cf=balloon[1],
  #                cftime=npay[1]/ypayfreq[1]),by=gid]),
  #  use.names=T);

  ## For each cash flow, this is the continuous risk free interest rate
  #work[,cfintrfc:=zero.yield.m(date,cftime,extrapol=extrapol)];

  ## Sum risk free discounted cash flows to get risk free present value
  ##work[,pvrf:=NPV.c(cfs=cf,reldates=cftime,
  ##                       int=cfintrfc/100),by=gid];
  #work <- work[,cbind(.SD[1,],pvrf=NPV.c(cfs=cf,reldates=cftime,
  #                                                 int=cfintrfc/100)),by=gid];

  work <- input[(!(is.na(date) | is.na(ypayfreq) | is.na(npay))) &
                (!(is.na(pay) | is.na(balloon))),
                .SD[1,], by=gid];
  #work[,pvrf:=NPV.c(cfs=rep(pay,npay)+c(rep(0,npay-1),balloon),
  #                  reldates=seq(1/ypayfreq,npay/ypayfreq,length.out=npay),
  #                  int=zero.yield.m(date,seq(1/ypayfreq,npay/ypayfreq,length.out=npay),
  #                                   extrapol=extrapol)/100),by=gid];

  work[,pvrf:=0];
  for (pp in 1:work[,max(npay,na.rm=T)]) {
    work[pp <= npay,
         pvrf:=pvrf+replace.na((replace.na(pay)+ifelse(npay==pp,replace.na(balloon),0))*
                      exp(-zero.yield.m(date,pp/ypayfreq,extrapol=extrapol)/100*pp/ypayfreq))];
  }

  # Estimate continuous risk free rate for dates in which risk free PV can be calculated.
  # Calculate risk free NPV and add point to payments c(cf=-NPV,time=0), then IRR
  #work <- work[!is.na(pvrf),
  #            .(intrfc=IRR.c(c(-NPV.c(cfs=cf,reldates=cftime,
  #                                    int=cfintrfc/100),cf),
  #                           c(0,cftime), tol=tol/100,
  #                           upper=max(cfintrfc/100),
  #                           lower=min(cfintrfc/100)
  #                           )*100),by=gid];
  #work <- work[!is.na(pvrf),.SD[1,],by=gid];
  work[,intrfc:=annuity.int.c(amount=pvrf, ypayfreq=ypayfreq, npay=npay,
                              pay=pay,balloon=balloon)];

  out <- merge(input[,.(id,gid)],work[,.(gid,intrfc)],
               by='gid',all.x=T);

  # Return continous interest rate
  return(as.numeric(out[order(id),intrfc]));
}

# Create interest rate for annuity from zero coupon. Returns continously compounded interest rate.
# Dates, maturities in days, payfreqs(payment frequencies in days)
annuity.rfint <- function(dates,ypayfreq,npay,pay,
                          balloon=rep(0,length(dates)),extrapol=T,tol=NULL) {
  # Solve for countinously compounded risk free rate
  intrfc <- annuity.rfint.c(dates,ypayfreq,npay,pay,balloon,extrapol,tol);

  # Transform continuous risk free rate to annually compounded rate
  return(int.conv(intrfc,from=Inf,to=1));
}

# Plot graph of annuity interst rate approximated for: 2yr, payments each 3 months
#local({
#  test <- data.table(date=unique(floor_date(treasury.zero$date,'month')),ypayfreq=2,npay=8,pay=200,balloon=10)
#  test[,int:=annuity.rfint(dates=date,ypayfreq=ypayfreq,npay,pay,balloon)];
#  ggplot(test,aes(x=date,y=int)) + geom_line();
#});

annuity.rfint.c.y <- function(weights, by, ...) {
  # Cast input as data.table
  weights <- copy(as.data.table(weights));
  by <- as.data.table(by);

  # Check that *date* column exists on *data* and *weights*
  stopifnot('date' %in% colnames(weights));

  # Columns for calculating annuity
  annuitycols <- c('year', 'ypayfreq', 'npay', 'pay', 'balloon');

  # Check that columns to group by are present in *by*
  stopifnot(annuitycols %in% colnames(by));

  # For each unique combination, the interest rate will be obtained
  # First, expand year to months
  data <- unique(x=by[,mget(annuitycols)],by=annuitycols);
  data <- data[,cbind(.SD[rep(1:.N,each=12),],
                       month=rep(1:12,times=.N))];

  # Create date variable for *data*
  data[,date:=as.Date(ISOdate(year,month,1))];
  data[,month:=NULL];

  # Get interest rate to *data*
  data[,rfintc:=annuity.rfint.c(dates=date, ypayfreq=ypayfreq, npay=npay,
                                pay=pay, balloon=balloon, ...)];

  return(group.agg(data=data,
                   weights=weights[,year:=(lubridate::year(date))],
                   by=by));
}

annuity.rfint.y <- function(weights, by, extrapol=T, tol=NULL) {
  # Solve for countinously compounded risk free rate
  intrfc <- annuity.rfint.c.y(weights, by, extrapol, tol);

  # Transform continuous risk free rate to annually compounded rate
  return(int.conv(intrfc,from=Inf,to=1));
}
