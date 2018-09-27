# Load required libraries
local({
  libs <- c('data.table');
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});

## All urban consumers, monthly and non seasonal. From U.S. Bureau of Labor Statistics.
local({
  cpi.files <- list(
    all                     ='CPIAUCSL.csv',
    medical                 ='CPIMEDNS.csv',
    recreation              ='CPIRECNS.csv',
    durables                ='CUUR0000SAD.csv',
    education               ='CUUR0000SAE1.csv',
    tuition                 ='CUUR0000SEEB.csv',
    vehicles                ='CUUR0000SETA.csv',
    'new car'               ='CUUR0000SETA01.csv',
    'used car'              ='CUUR0000SETA02.csv',
    'private transportation'='CUUR0000SAT1.csv',
    housing                 ='CPIHOSNS.csv',
    'household furnishing'  ='CUUR0000SAH3.csv'
    );

  cpi.m <- NULL
  for (bb in names(cpi.files)) {
    # Read file with cpi data
    tmp <- fread(paste0('data/CPI_csv_2/',cpi.files[[bb]]),
                 stringsAsFactors=T,na.strings=c('.'),
                 header=T)
    # Set name of variables and tag which basket of group this file is 
    setnames(tmp,c('date','cpi'));
    tmp[,basket:=as.factor(bb)];

    # Format dates
    tmp[,date:=as.Date(date)];

    cpi.m <- rbindlist(list(cpi.m,tmp),
                       use.names=T);
  }

  ## Reindex CPI so it's 100 on 2010-01-01 for all baskets
  cpi.m <- cpi.m[,cpi:=cpi/cpi[date==as.Date(ISOdate(2010,1,1))]*100,by=basket]

  assign('cpi.m',cpi.m,envir=.GlobalEnv);
});

cpi.y <- cpi.m[,.(cpi=mean(cpi,na.rm=T)),by=.(basket,year=year(date))];

cpi.y.f <- function(year,basket='all') {
  if(length(year)==0) return(numeric(0));

  # Check that length of arguments is equal or one of them is length 1
  stopifnot((length(year)==length(basket)) | (1 %in% c(length(year),length(basket))))

  input <- data.table(year,basket)[,id:=.I];
  work <- merge(input,cpi.y,all.x=T,by=c('year','basket'))
  return(work[order(id),cpi]);
}
