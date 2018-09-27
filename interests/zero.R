# Load required libraries
local({
  libs <- c('data.table','lubridate');
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});

## Zero coupon yields
# Annual interest continuously compounded
# https://www.federalreserve.gov/pubs/feds/2006/200628/index.html
# https://www.quandl.com/data/FED/SVENY-US-Treasury-Zero-Coupon-Yield-Curve
# https://www.federalreserve.gov/pubs/feds/2005/200533/200533abs.html
# https://www.frbsf.org/economic-research/indicators-data/treasury-yield-premiums/

treasury.zero <- melt(fread('../data/FED-SVENY.csv',stringsAsFactors=T),
                      id.vars='Date')[!is.na(value),];
setnames(treasury.zero,c('date','maturity','int'));
# Convert maturity strings to numbers, maturity in years
levels(treasury.zero$maturity) <- gsub('SVENY','',levels(treasury.zero$maturity));
treasury.zero[,maturity:=as.numeric(levels(maturity))[maturity]];
# Conver dates from strings to dates
treasury.zero[,date:=as.Date(date)];
# Make sure interest is a number
treasury.zero[,int:=as.numeric(int)];

## Plot treasury.zero
#local({
#  require('rgl')
#  treasury.zero.w <- dcast(treasury.zero,date~maturity,value.var='int')
#  treasury.zero.w <- list(x=treasury.zero.w$date,
#                          y=as.numeric(colnames(treasury.zero.w)[-1]),
#                          z=as.matrix(treasury.zero.w[,-1]))
#  persp3d(x=treasury.zero.w$x,y=treasury.zero.w$y,
#          z=treasury.zero.w$z,col='blue');
#});

## Parameters for zero coupon yields
# Wide format
treasury.zero.params <- fread('../data/FED-PARAMS.csv');
setnames(treasury.zero.params,c('date','b0','b1','b2','b3','t1','t2'));
# Conver dates from strings to dates
treasury.zero.params[,date:=as.Date(date)];


# Raw zero.yield function
# UNSAFE. Do not call directly, use zero.yield wrapper
zero.yield.r <- function(n,b0,b1,b2,t1,b3=NA,t2=NA) {
  # Transform n to matrix, just in case that multiple columns have to be used
  # And nrow is always used for n (instead of switching to length when n is vector)
  n <- as.matrix(n);
  # Check that main parameters have the same length
  stopifnot(length(unique(sapply(c('b0','b1','b2','t1'),function(x) length(get(x)))))==1);

  # Check that main parameters have the same length as *n* or have length 1
  stopifnot(length(b0) %in% c(1,nrow(n)));

  # If extra parmeters b3 and t2 are used, they must be the same length as main params
  if (!all(is.na(b3) | is.na(t2))) {
    stopifnot(length(unique(sapply(c('b0','b3','t2'),function(x) length(get(x)))))==1);
  }

  # Use Nelson-Siegel or Svensson (1994) depending on available parameters
  return(b0+b1*(1-exp(-n/t1))/(n/t1)+b2*((1-exp(-n/t1))/(n/t1)-exp(-n/t1))+
           # Add Svensson term if b3 or t2 are used. ifelse output is shaped by test.
           ifelse(rep(T,length(n)) & (is.na(b3) | is.na(t2)),0,
                  b3*((1-exp(-n/t2))/(n/t2)-exp(-n/t2))));
}

# Test if treasury.zero can be created from treasury.zero.params
#local({
#  # Expand each date(with params) for each maturity 
#  maturities <- unique(treasury.zero$maturity);
#
#  # Create data.table with parameters
#  treasury.zero.est <- copy(treasury.zero.params)[
#    rep(1:.N,each=length(maturities)),
#    cbind(.SD,maturity=rep(maturities,nrow(.SD)/length(maturities)))];
#
#  # Estimate zero coupon interest rate
#  treasury.zero.est[,int:=zero.yield.r(n=maturity,b0,b1,b2,t1,b3,t2)];
#  
#  # Merge and compare given interest rates and estimated ones
#  treasury.zero.both <- merge(treasury.zero,
#                              treasury.zero.est[,.(date,maturity,int2=int)],
#                              by=c('date','maturity'))
#  # Test that both interest rates are similar
#  treasury.zero.both[,all.equal(int,int2,tolerance=0.01)];
#})

## Visualize unsafe(*zero.yield.r*) extrapolation shape
#local({
#  maturities <- c(0.25,0.5,0.75,1:50)
#
#  # Create data.table with parameters
#  treasury.zero.est <- copy(treasury.zero.params)[
#    rep(1:.N,each=length(maturities)),
#    cbind(.SD,maturity=rep(maturities,nrow(.SD)/length(maturities)))];
#
#  # Estimate zero coupon interest rate with *zero.yield.r*
#  treasury.zero.est[,int:=zero.yield.r(n=maturity,b0,b1,b2,t1,b3,t2)];
#
#  treasury.zero.w <- dcast(treasury.zero.est,date~maturity,value.var='int')
#  treasury.zero.w <- list(x=treasury.zero.w$date,
#                          y=as.numeric(colnames(treasury.zero.w)[-1]),
#                          z=as.matrix(treasury.zero.w[,-1]))
#  persp3d(x=treasury.zero.w$x,y=treasury.zero.w$y,
#          z=treasury.zero.w$z,col='blue');
#})

zero.yield <- function(dates, maturities, data.param=treasury.zero.params,
                       data.range=treasury.zero) {
  # Dont modify original data.param
  data.param <- copy(data.param)

  # Make sure maturities are numeric
  maturities <- as.numeric(maturities);

  # Transform dates to numbers. Recast dates in case they are characters.
  if (!is.numeric(dates)) dates <- as.numeric(as.Date(dates));

  # Make sure dates are within range
  date.limit <- as.list(data.param[,.(min=min(date),max=max(date))]);
  dates <- ifelse(dates>date.limit$max, date.limit$max, dates);
  dates <- ifelse(dates<date.limit$min, date.limit$min, dates);

  # Make sure maturities are within range
  # For maturities, lower bound is granted.
  # Upper bound determined by FED guys. Move upper values -> upper bound
  ## Use linear interpolation for upper boundary
  maturity.max <- data.range[,.(x=as.numeric(date),y=maturity)][,.(y=max(y)),by=x];
  maturities.max <- approx(x=maturity.max$x,y=maturity.max$y,
                           xout=as.numeric(dates),method='linear')$y;
  maturities <- ifelse(maturities>maturities.max, maturities.max, maturities);

  data.basic <- merge(data.table(date=dates,maturity=maturities),
                      data.param, by='date',all.x=T);

  return(data.basic[,.(zero.yield.r(maturity,b0,b1,b2,t1,b3,t2))]);
}

# Visualize safe(*zero.yield*) extrapolation shape
#local({
#  maturities <- c(0.25,0.5,0.75,1:50)
#
#  # Create data.table with parameters
#  treasury.zero.est <- data.table(date=unique(treasury.zero.params[,date],by='date'));
#  treasury.zero.est <- treasury.zero.est[
#    rep(1:.N,each=length(maturities)),
#    cbind(.SD,maturity=rep(maturities,nrow(.SD)/length(maturities)))];
#
#  # Estimate zero coupon interest rate with *zero.yield*
#  treasury.zero.est[,int:=zero.yield(dates=date,maturities=maturity)];
#
#  treasury.zero.w <- dcast(treasury.zero.est,date~maturity,value.var='int')
#  treasury.zero.w <- list(x=treasury.zero.w$date,
#                          y=as.numeric(colnames(treasury.zero.w)[-1]),
#                          z=as.matrix(treasury.zero.w[,-1]))
#  persp3d(x=treasury.zero.w$x,y=treasury.zero.w$y,
#          z=treasury.zero.w$z,col='blue');
#});

zero.yield.m <- function(dates, maturities, data.param=treasury.zero.params,
                         data.range=treasury.zero, extrapol=T) {
  # Do the average using countinuously compounded interest yields
  # Never average parameters, the properties of these parameters haven't been studied
  # Only average continuosly compounded interest rates
  # Make sure maturities are numeric
  maturities <- as.numeric(maturities);

  # Make sure dates are within range
  date.limit <- as.list(data.param[,.(min=min(date),max=max(date))]);
  dates <- ifelse(dates>date.limit$max, ifelse(extrapol,date.limit$max,NA), dates);
  dates <- ifelse(dates<date.limit$min, ifelse(extrapol,date.limit$min,NA), dates);
  # Restore class of dates. ifelse doesnt behave.
  class(dates) <- "Date";

  # Make sure maturities are within range
  # For maturities, lower bound is granted.
  # Upper bound determined by FED guys. Move upper values -> upper bound
  ## Use linear interpolation for upper boundary
  maturity.max <- data.range[,.(x=as.numeric(date),y=maturity)][,.(y=max(y)),by=x];
  maturities.max <- approx(x=maturity.max$x,y=maturity.max$y,
                           xout=as.numeric(dates),method='linear')$y;
  maturities <- ifelse(maturities>maturities.max, maturities.max, maturities);

  # Monthly dates as first day of that month
  dates <- floor_date(dates, "month");

  # Create data.table with input data
  data.input <- data.table(date=dates,maturity=maturities);
  # Keep track of original order
  data.input[,id:=.I];

  # Create data.table for estimations
  data.est <- unique(data.input,by=c('date','maturity'));

  # Decompose dates of month-dates to estimate
  data.est[,c('year','month'):=.(year(date),month(date))];
  # Include all possible days of these months
  data.est <- data.est[rep(1:.N,each=31),];
  data.est <- data.est[,day:=rep(1:31,nrow(data.est)/31)];
  # Remove date columns so it doesn't conflict with date of parameters
  data.est[,date:=NULL];

  # Dont modify original data.param
  data.param <- copy(data.param);

  # Decompose dates of parameters data.table
  data.param[,c('year','month','day'):=.(year(date),month(date),day(date))];

  data.est <- merge(data.est, data.param, by=c('year','month','day'), all=F);
  data.est[,int:=zero.yield.r(maturity,b0,b1,b2,t1,b3,t2)];
  data.est <- data.est[,.(maturity=maturity,int=mean(int,na.rm=T)),by=.(year,month,maturity)];
  # Reconstruct date
  data.est[,date:=as.Date(ISOdate(year,month,1))];
  

  data.out <- merge(data.input, data.est[,.(date,maturity,int)],
                    all.x=T, by=c('date','maturity'));
  return(data.out[order(id),int]);
}

# Visualize safe monthly zero coupon curve
#local({
#  maturities <- c(0.25,0.5,0.75,1:50)
#
#  # Create data.table with parameters
#  treasury.zero.est <- data.table(date=seq.Date(treasury.zero.params[,min(date)],
#                                                treasury.zero.params[,max(date)],by='month'));
#  treasury.zero.est <- treasury.zero.est[
#    rep(1:.N,each=length(maturities)),
#    cbind(.SD,maturity=rep(maturities,nrow(.SD)/length(maturities)))];
#
#  # Estimate zero coupon interest rate with *zero.yield*
#  treasury.zero.est[,int:=zero.yield(dates=date,maturities=maturity)];
#
#  treasury.zero.w <- dcast(treasury.zero.est,date~maturity,value.var='int')
#  treasury.zero.w <- list(x=treasury.zero.w$date,
#                          y=as.numeric(colnames(treasury.zero.w)[-1]),
#                          z=as.matrix(treasury.zero.w[,-1]))
#  persp3d(x=treasury.zero.w$x,y=treasury.zero.w$y,
#          z=treasury.zero.w$z,col='blue');
#});
