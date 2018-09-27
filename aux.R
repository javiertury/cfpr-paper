# Load required libraries
local({
  libs <- c('data.table','lubridate','stringi');
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});

## Multi-seq
## Create multiple sequencies and concadenates it. *from* and *to* accept multiple inputs.
## When multiple inputs are given, each pair is taken to form a sequence and these are concatenated
## according to pair order.
mseq <- function(from=1, to=1, by=NULL, length.out=NULL, strict.to=F, scale=1) {
  # Cannot have both by and length.out at the same time
  stopifnot(is.null(by) | is.null(length.out));

  # If both by and length.out are undefined, set by
  if(is.null(by) & is.null(length.out)) by <- 1;

  # If length.out is defined, set by
  # Careful, never divide by 0
  if(!is.null(length.out)) by <- ((to - from)/ifelse(length.out>=2,(length.out - 1),1));

  return(as.numeric(unlist(apply(cbind(from=from, to=to, by=by, scale=scale),1,
                      function(x) {
                        res <- seq(from=x['from'],to=x['to'],by=x['by']);
                        # If to is not a multiple of by, add it to the end
                        # Chek multiple. Multiple if remainder is 0.
                        # Tolerance?, better deal with it elsewhere
                        if(strict.to & (res[length(res)] != x['to'])) {
                          res <- c(res,x['to']);
                        }
                        return(res*x['scale']);
                      }))));
}

# Intercalate 2 or more vectors
intercalate <- function(...) {
  return(c(rbind(...)));
}

# Replaces NAs in a vector with 0 or other values
replace.na <- function(x,replacement=0) {
  return(ifelse(is.na(x),replacement,x));
}

# Use spline to interpolate a serie, respecting 'sum' or 'mean' restrictions.
# To this end, convert the flow *sum* or *mean* into period leves. Interpolate
# those levels and convert levels back to flow *sum* or *mean*.
# The vector (x.orig,x) defines period breakpoints
spline.flow <- function(x,x.orig,y,xout,xout.orig,
                        restriction='sum',strictly.positive=T,extrapol=F) {
  # x(period ending) and y must have the same length
  stopifnot(length(unique(lapply(list(x,y),length)))==1);

  # x.orig and xout.orig must be of length 1
  stopifnot(all(lapply(list(x.orig,xout.orig),length)==1));

  # Starting time of a period must be earlier than ending period
  stopifnot(x.orig < min(x,na.rm=T));
  stopifnot(xout.orig < min(xout,na.rm=T));

  # Check that restriction argument is valid
  stopifnot(restriction %in% c('sum','mean'))

  # If flows are not strictly positive, use default spline.
  method <- 'fmm';
  # If flows must be strictly positive, use a spline method that
  # guarantees montonicity.
  #if (strictly.positive==T) {method <- 'monoH.FC'};
  if (strictly.positive==T) {method <- 'hyman'};

  # Order input data by x. Order x the last one, otherwise inconsistent.
  z <- y[order(x)];
  x <- x[order(x)];

  # If *mean*, transform mean into sum restriction
  if (restriction=='mean') {
    z <- c(diff(c(x.orig, x))*z);
  }

  # Convert flow sum series to cumulative sum(level)
  #if (restriction %in% c('sum','mean')) {
  z <- cumsum(c(0,z));
  #}

  # Interpolate cumulative sum(level)
  out <- spline(x=c(x.orig,x),y=z,xout=c(xout.orig,xout),method=method);

  # Order output data by x. Order x the last one, otherwise inconsistent.
  out[['y']] <- out[['y']][order(out[['x']])];
  out[['x']] <- out[['x']][order(out[['x']])];

  # Convert interpolated cumulative sum(level) to sum flow series
  #if (restriction %in% c('sum','mean')) {
  out[['y']] <- as.numeric(diff(out[['y']]));
  out[['x']] <- out[['x']][-1];
  #}

  # If *mean*, transform mean into sum restriction
  if (restriction=='mean') {
    out[['y']] <- c(out[['y']]/as.numeric(diff(c(xout.orig, xout))));
  }

  # Order output data by xout.
  out[['y']] <- out[['y']][order(order(xout))];
  out[['x']] <- out[['x']][order(order(xout))];

  # Return values
  return(out)
}

# Convertion: the dates of periodic/flow data are set at the first day of the flow
# For montly flows, the day is set to 1; for quarterly flows, the date is set
# to the first month in the quarter and day 1. For years, month and day are 1.

# Interpolate monthly flow (with restriction) to daily flow
spline.m2d <- function(x,y, ...) {
  ## Check that all dates are unique
  #stopifnot(length(unique(year(x)*12+month(x)))==length(x));
  # Check that all months are consecutive, ordered and unique
  stopifnot(all(diff(year(x)*12+month(x))==1));
  # Check that for each month, the day is set to 1 (convention)
  stopifnot(all(day(x)==1));

  # Set interval breakpoints for input dates by month
  #x.orig <- ((min(x,na.rm=T) %m-% months(1)) + days_in_month(min(x,na.rm=T) %m-% months(1))-1);
  x.orig <- (min(x,na.rm=T)-1);
  x <- (x + months(1)-1);

  # Set interval breakpoints for output dates by day
  xout.orig <- x.orig;
  xout <- seq.Date(x.orig+1,max(x,na.rm=T),by='day');

  out <- spline.flow(x=x, x.orig=x.orig, y=y, xout=xout, xout.orig=xout.orig, ...);

  # Convert *x* to date
  class(out[['x']]) <- 'Date';

  return(out);
}

# Interpolate quarterly flow (with restriction) to daily flow
spline.q2d <- function(x,y, ...) {
  # Check that quarter dates are first month of quarter with day 1(Convention)
  stopifnot(all((month(x)%%3)==1));
  stopifnot(all(day(x)==1));
  ## Check that all dates are unique
  #stopifnot(length(unique(year(x)*4+quarter(x)))==length(x));
  # Check that all quarters are consecutive, ordered and unique
  stopifnot(all(diff(year(x)*12+month(x))==3));

  # Set interval breakpoints for input dates by quarter
  x.orig <- (min(x,na.rm=T)-1);
  x <- ((x %m+% months(3))-1);

  # Set interval breakpoints for output dates by day
  xout.orig <- x.orig;
  xout <- seq.Date(x.orig+1,max(x,na.rm=T),by='day');

  out <- spline.flow(x=x, x.orig=x.orig, y=y, xout=xout, xout.orig=xout.orig, ...);

  # Convert *x* to date
  class(out[['x']]) <- 'Date';

  return(out);
}

# Interpolate quarterly flow (with restriction) to monthly flow
spline.q2m <- function(x,y, ...) {
  # Check that quarter dates are first month of quarter with day 1(Convention)
  stopifnot(all((month(x)%%3)==1));
  stopifnot(all(day(x)==1));
  ## Check that all dates are unique
  #stopifnot(length(unique(year(x)*4+quarter(x)))==length(x));
  # Check that all quarters are consecutive, ordered and unique
  stopifnot(all(diff(year(x)*12+month(x))==3));

  # Set interval breakpoints for input dates by quarter
  x.orig <- min(x,na.rm=T) %m-% months(1);
  x <- (x %m+% months(2));

  # Set interval breakpoints for output dates by month
  xout.orig <- x.orig;
  xout <- seq.Date(x.orig+months(1),max(x,na.rm=T),by='month');

  out <- spline.flow(x=x, x.orig=x.orig, y=y, xout=xout, xout.orig=xout.orig, ...);

  # Convert *x* to date
  class(out[['x']]) <- 'Date';

  return(out);
}

# Interpolate yearly flow (with restriction) to daily flow
spline.y2d <- function(x,y, ...) {
  # Check that quarter dates are first month of quarter with day 1(Convention)
  stopifnot(all(month(x)==1));
  stopifnot(all(day(x)==1));
  ## Check that all dates are unique
  #stopifnot(length(unique(year(x)*4+quarter(x)))==length(x));
  ## Make sure that x and y have the same length
  stopifnot(length(x)==length(y));
  #neworder <- order(x);
  #y <- y[order(x)]
  #x <- x[order(x)]
  # Check that all years are consecutive, ordered and unique
  stopifnot(all(diff(year(x))==1));

  # Set interval breakpoints for input dates by year
  x.orig <- (min(x,na.rm=T)-1);
  x <- ((x %m+% years(1))-1);

  # Set interval breakpoints for output dates by day
  xout.orig <- x.orig;
  xout <- seq.Date(x.orig+1,max(x,na.rm=T),by='day');

  out <- spline.flow(x=x, x.orig=x.orig, y=y, xout=xout, xout.orig=xout.orig, ...);

  # Convert *x* to date
  class(out[['x']]) <- 'Date';

  return(out);
}

group.agg <- function(data, weights, by, geomean=F) {
  # Cast input as data.table
  data <- as.data.table(data);
  by <- as.data.table(by);
  if(!is.null(weights)) weights <- as.data.table(weights);

  # Columns to group by. Use copy, otherwise it is assigned by reference.
  groupcols <- copy(colnames(by));

  cols.data <- copy(colnames(data));
  cols.weights <- copy(colnames(weights));

  # Check that all group columns are used
  stopifnot(all(groupcols %in% c(cols.data,cols.weights)));

  # The weight column is exclusive to weights data.table
  stopifnot((('weight' %in% cols.weights) | (is.null(weights))) &
            (!'weight' %in% cols.data) &
            (!'weight' %in% groupcols));

  # These are the columns to weight by
  #weightcols <- intersect(cols.weights,cols.data);

  # These are the columns to output
  outcols <- copy(colnames(data)[(!colnames(data) %in% groupcols) &
                                 (!colnames(data) %in% cols.weights)]);

  # There must be at least one output variable
  stopifnot(length(outcols) >= 1)

  # Negative weights are not allowed.
  stopifnot(weights[!is.na(weight), all(weight>=0)]);

  # Give each input an id, to keep track of original order
  input <- copy(by)[,id:=.I];

  # Reduce *data* and *weights* to usable subset
  groupcols.data <- intersect(groupcols,cols.data);
  groupcols.weights <- intersect(groupcols,cols.weights);

  groupcols.use <- union(groupcols.weights,groupcols.data);

  #data <- merge(unique(input[,mget(groupcols.data)],by=groupcols.data),
  #              data,by=groupcols.data);
  #weights <- merge(unique(input[,mget(groupcols.weights)],by=groupcols.weights),
  #                 weights,by=groupcols.weights);

  # If weights are null, give each observation the same weight
  # otherwise use weights
  if (! is.null(weights)) {
    # Merge weights and data
    work <- merge(data, weights,
                  by=intersect(cols.weights,cols.data), allow.cartesian=T);
  } else {
    work <- copy(data)[,weight:=1];
  }

  # Aggregate data per group according to weights
  if (geomean==T) {
    work <- work[!Reduce(`|`,lapply(mget(c('weight',outcols,groupcols.use)),is.na)),
                 lapply(mget(outcols),function(x) prod(x^(weight/sum(weight)))),
                 by=groupcols.use];
  } else {
    work <- work[!Reduce(`|`,lapply(mget(c('weight',outcols,groupcols.use)),is.na)),
                 lapply(mget(outcols),function(x) sum(x*(weight/sum(weight)))),
                 by=groupcols.use];
  }

  # Merge results with for each input
  work <- merge(input, work[,mget(c(groupcols.use,outcols))], by=groupcols.use, all.x=T);

  # Return result in original order
  return(work[order(id),mget(outcols)]);
}

## Test that spline.m2d, spline.q2d and spline.q2m work correctly.
#ggplot(cfpb.vol[credit_type=='all_nrev' & season_adj==F,
#         spline.m2d(x=date,y=vol,restriction='sum')][,
#           .(x=as.Date(ISOdate(year(x)[1],month(x)[1],1)),y=sum(y)),by=.(year(x),month(x))],
#       aes(x=x,y=y)) +
#  geom_line() + geom_point(data=cfpb.vol[credit_type=='all_nrev' & season_adj==F,],
#                           aes(x=date,y=vol));
#ggplot(nyfed.orig.q[cs_all==T & loan_type=='auto',
#       spline.q2d(x=date,y=vol,restriction='sum')][,
#         .(x=as.Date(ISOdate(year(x)[1],quarter(x)[1]*3-2,1)),y=sum(y)),by=.(year(x),quarter(x))],
#       aes(x=x,y=y)) +
#  geom_line() + geom_point(data=nyfed.orig.q[cs_all==T & loan_type=='auto',], aes(x=date,y=vol));
#ggplot(nyfed.orig.q[cs_all==T & loan_type=='auto',
#       spline.q2m(x=date,y=vol,restriction='sum')][,
#         .(x=as.Date(ISOdate(year(x)[1],quarter(x)[1]*3-2,1)),y=sum(y)),by=.(year(x),quarter(x))],
#       aes(x=x,y=y)) +
#  geom_line() + geom_point(data=nyfed.orig.q[cs_all==T & loan_type=='auto',], aes(x=date,y=vol));

# Calculate probability of input(years) happening after(or before) the event of interest
pr.event.m <- function(event, date, before=F) {
  # Only one event at a time
  stopifnot(length(event)==1);

  # Check event is of class Date, otherwise coherce 
  if (class(event)!='Date') event <- as.Date(event);
  # Check that date is of class Date, otherwise coherce 
  if (class(date)!='Date') date <- as.Date(date);

  out <- ifelse((lubridate::year(event)==lubridate::year(date)) &
                (lubridate::month(event)==lubridate::month(date)),
              1-(day(event)-1)/days_in_month(event),
              date>=event);

  # If *before*, calculate probability of input happening *before*
  # the event of interest
  if (before) out <- (1 - out);

  return(out);
}

# Calculate probability of input(years) happening after(or before) the event of interest
pr.event.y <- function(event, date, before=F) {
  # Only one event at a time
  stopifnot(length(event)==1);

  # Check event is of class Date, otherwise coherce 
  if (class(event)!='Date') event <- as.Date(event);
  # Check that date is of class Date, otherwise coherce 
  if (class(date)!='Date') date <- as.Date(date);

  out <- ifelse(lubridate::year(event)==lubridate::year(date),
              1-(yday(event)-1)/(yday(ceiling_date(event,'year')-1)),
              date>=event);

  # If *before*, calculate probability of input happening *before*
  # the event of interest
  if (before) out <- (1 - out);

  return(out);
}

# Calculate probability of input(years) happening after(or before) the event of interest
# Aggregated yearly
pr.event.m2y <- function(event, weights, by, before=F) {
  # Only one event at a time
  stopifnot(length(event)==1);

  # Check event is of class Date, otherwise coherce 
  if (class(event)!='Date') event <- as.Date(event);

  # Cast input as data.table
  weights <- copy(as.data.table(weights));
  by <- as.data.table(by);

  # Check that *date* column exists on *data* and *weights*
  stopifnot('date' %in% colnames(weights));

  # Check that columns to group by are present in *by*
  stopifnot('year' %in% colnames(by));

  # Columns to group by. Use copy, otherwise it is assigned by reference.
  groupcols <- copy(colnames(by));

  # Keep track of original order
  by[,id:=.I];

  # For years different to event, calculate probability of input happening
  # after the event of interest. These dates, don't need weights, and missing weights don't
  # cause missing observations.
  by[year!=lubridate::year(event), pr:=as.numeric(year >= lubridate::year(event))];

  if (lubridate::year(event) %in% by[,year]) {
    # For the year of the event, expand year to months
    data <- unique(by[year==lubridate::year(event),mget(groupcols)],by=groupcols);
    data <- data[,cbind(.SD[rep(1:.N,each=12),mget(groupcols)], month=rep(1:12,times=.N))];

    # Create date variable for *data*
    data[,date:=as.Date(ISOdate(year,month,1))];

    # Calculate probability of input happening after the event
    data[,pr:=pr.event.m(event=event,date=date,before=F)];
    data[,month:=NULL];

    by[year==lubridate::year(event),
       pr:=group.agg(data=data,
                     weights=weights[,year:=lubridate::year(date)],
                     by=mget(groupcols))];
  }

  # If *before*, calculate probability of input happening *before* the event
  if (before) by[,pr:=(1-pr)];

  # Return ordered result
  return(as.numeric(by[order(id),pr]));
}

pr.event.m2d2y <- function(event, weights, by, before=F) {
  # Only one event at a time
  stopifnot(length(event)==1);

  # Check event is of class Date, otherwise coherce 
  if (class(event)!='Date') event <- as.Date(event);

  # Cast input as data.table
  weights <- copy(as.data.table(weights));
  by <- as.data.table(by);

  # Check that *date* column exists on *data* and *weights*
  stopifnot('date' %in% colnames(weights));

  # Check that required columns to group by are present in *by* and *weight*
  stopifnot('year' %in% colnames(by));
  stopifnot(c('date','weight') %in% colnames(weights));

  # Required columns to group and weight by. Use copy, otherwise it is assigned by reference.
  groupcols <- copy(colnames(by));
  weightcols <- copy(colnames(weights)[!(colnames(weights) %in% c('date','weight'))]);

  # Keep track of original order
  by[,id:=.I];

  # For years different to event, calculate probability of input happening
  # after the event of interest
  by[year!=lubridate::year(event), pr:=as.numeric(year >= lubridate::year(event))];

  if (lubridate::year(event) %in% by[,year]) {
    # For the year of the event, expand year to days
    data <- unique(by[year==lubridate::year(event),mget(groupcols)],by=groupcols);
    data <- data[,cbind(.SD[rep(1:.N,each=sum(days_in_month(ISOdate(year[1],1:12,1)))),mget(groupcols)],
                        month=rep(rep(1:12,days_in_month(ISOdate(year[1],1:12,1))),times=.N),
                        day=rep(mseq(rep(1,12),days_in_month(ISOdate(year[1],1:12,1))),times=.N))];

    # Create date variable for *data*
    data[,date:=as.Date(ISOdate(year,month,day))];

    # Calculate probability of input happening after the event
    data[,pr:=as.numeric(date >= event)];
    data[,c('month','day'):=NULL];

    # For each monthly weight around year of event, interpolate it to daily weight.
    weights <- weights[abs(year(date)-year(event))<=1,
                       setnames(as.data.table(spline.m2d(x=date,y=weight)),c('date','weight')),
                       by=weightcols];

    # Aggregate daily pr(event) by weight
    by[year==lubridate::year(event),
       pr:=group.agg(data=data,
                     weights=weights[,year:=lubridate::year(date)],
                     by=mget(groupcols))];
  }

  # If *before*, calculate probability of input happening *before* the event
  if (before) by[,pr:=(1-pr)];

  # Return ordered result
  return(as.numeric(by[order(id),pr]));
}

pr.event.d2y <- function(event, weights, by, before=F) {
  # Only one event at a time
  stopifnot(length(event)==1);

  # Check event is of class Date, otherwise coherce 
  if (class(event)!='Date') event <- as.Date(event);

  # Cast input as data.table
  weights <- copy(as.data.table(weights));
  by <- as.data.table(by);

  # Check that *date* column exists on *data* and *weights*
  stopifnot('date' %in% colnames(weights));

  # Check that required columns to group by are present in *by* and *weight*
  stopifnot('year' %in% colnames(by));
  stopifnot(c('date','weight') %in% colnames(weights));

  # Required columns to group and weight by. Use copy, otherwise it is assigned by reference.
  groupcols <- copy(colnames(by));
  weightcols <- copy(colnames(weights)[!(colnames(weights) %in% c('date','weight'))]);

  # Keep track of original order
  by[,id:=.I];

  # For years different to event, calculate probability of input happening
  # after the event of interest
  by[year!=lubridate::year(event), pr:=as.numeric(year >= lubridate::year(event))];

  if (lubridate::year(event) %in% by[,year]) {
    # For the year of the event, expand year to days
    data <- unique(by[year==lubridate::year(event),mget(groupcols)],by=groupcols);
    data <- data[,cbind(.SD[rep(1:.N,each=sum(days_in_month(ISOdate(year[1],1:12,1)))),mget(groupcols)],
                        month=rep(rep(1:12,days_in_month(ISOdate(year[1],1:12,1))),times=.N),
                        day=rep(mseq(rep(1,12),days_in_month(ISOdate(year[1],1:12,1))),times=.N))];

    # Create date variable for *data*
    data[,date:=as.Date(ISOdate(year,month,day))];

    # Calculate probability of input happening after the event
    data[,pr:=as.numeric(date >= event)];
    data[,c('month','day'):=NULL];

    # Aggregate daily pr(event) by weight
    by[year==lubridate::year(event),
       pr:=group.agg(data=data,
                     weights=weights[,year:=lubridate::year(date)],
                     by=mget(groupcols))];
  }

  # If *before*, calculate probability of input happening *before* the event
  if (before) by[,pr:=(1-pr)];

  # Return ordered result
  return(as.numeric(by[order(id),pr]));
}

# Aggregate monthly data to yearly data by using weights and matching on extra arguments(...)
year.agg <- function(data, weights, by, geomean=F) {
  # Cast input as data.table
  data <- copy(as.data.table(data));
  weights <- copy(as.data.table(weights));
  by <- as.data.table(by);

  # Check that *date* column exists on *data* and *weghts*
  stopifnot(('date' %in% colnames(data)) & ('date' %in% colnames(weights)));

  # Check that *year* column exists *by*
  stopifnot('year' %in% colnames(by));

  return(group.agg(data=data[,year:=lubridate::year(date)],
                   weights=weights[,year:=lubridate::year(date)],
                   by=by,geomean=geomean));
}

custom.convert <- function(x,type=NULL) {
  # Validate types
  stopifnot(type %in% c('numeric','integer','factor','character','logical',
                        'complex','raw','POSIXct'));

  # Guess automatically if type not supplied
  if(is.null(type)) {
    converter <- type.convert;
  } else {
    converter <- get(paste0('as.',type));
  }

  return(converter(x));
}

load_flat <- function(file,vars,skip=0,nrows=Inf,format='fwf',compression='none') {
  # What should be opened
  toopen <- file;
  if (compression=='zip') toopen <- paste0('zcat "',file,'"');

  # Read data
  if (format=='fwf') {
    # Use fread
    dat <- fread(toopen,header=F,sep='\n',
                  skip=skip,nrows=nrows)[,
              setNames(lapply(seq_len(nrow(vars)),
                              function(x) 
                                custom.convert(gsub(paste0(paste0('^\\s*',c('NA','na','Na'),'\\s*$'),
                                                           collapse='|'),'',
                                                    stri_sub(V1, vars$b_s[x], vars$b_e[x]))
                                               ,vars$var_type[x])),
                       vars$var_name)];
  } else if (format=='csv') {
    dat <- fread(toopen,header=F,sep=',',
                  skip=skip,nrows=nrows,na.strings=c('NA','na','Na'))
    dat <- dat[,setNames(lapply(seq_len(nrow(vars)),
                                function(x) custom.convert(.SD[[x]],vars$var_type[x])),
                         vars$var_name)];
  } else {
    stop(paste('load_flat: unknown format ',format));
  }
  return(dat);
}
