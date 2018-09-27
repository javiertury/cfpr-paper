# Load required libraries
libs <- c('data.table','openxlsx','tempdisagg');
for (i in libs) {
    if( !is.element(i, .packages(all.available=T))){
        capture.output(install.packages(i));
    }
    suppressMessages(require(i,character.only=T));
}
rm(i);

# Load auxiliary tools
if (!all(sapply(c('mseq','spline.q2m'),exists))) {
  source('../aux.R',chdir=T);
}

## https://www.newyorkfed.org/data-and-statistics

## Origination volume from NY Federal Reserve panel
## Volume in billions of $
local({
  # Mortgage origination volume by credit score, in billions of $
  mort <- as.data.table(read.xlsx('../data/frbny_orig/HHD_C_Report_2018Q1.xlsx',
                                  sheet='Page 6 Data', startRow=4,cols=1:9,
                                  rowNames=F,colNames=T));
  # Auto loan origination volume by credit score, in billions of $
  auto <- as.data.table(read.xlsx('../data/frbny_orig/HHD_C_Report_2018Q1.xlsx',
                                  sheet='Page 8 Data', startRow=4,cols=1:9,
                                  rowNames=F,colNames=T));

  # Check before removing useless Zero column
  stopifnot(mort[,all(Zero=='0')]);
  stopifnot(auto[,all(Zero=='0')]);
  mort[,Zero:=NULL];
  auto[,Zero:=NULL];

  # Define credit score range(300-850) and standardize variables
  setnames(mort,c('X1','<620','760+','TOTAL'),c('date','300-619','760-850','300-850'));
  setnames(auto,c('X1','<620','760+','TOTAL'),c('date','300-619','760-850','300-850'));

  ## Origination volume, pre 2003 data
  # Auto and Mortgage orignation volume, in billions of $
  old <- as.data.table(read.xlsx('../data/frbny_orig/pre2003_data.xlsx',
                                 sheet='Origination Data',rows=4:19,
                                 cols=1:3,rowNames=F,colNames=T));
  # Mortgage origination volume by credit score, in billions of $
  old.mort <- as.data.table(read.xlsx('../data/frbny_orig/pre2003_data.xlsx',
                                      sheet='Origination Data',rows=4:19,
                                      cols=4:11,rowNames=F,colNames=T));
  ## Auto loan data, with data form 2003 to 2004 that no other data set has
  old.auto <- as.data.table(read.xlsx('../data/frbny_orig/LSE_2016q3_new-autos-scally_data.xlsx',
                                      sheet='fig1',rows=5:72,
                                      cols=1:6,rowNames=F,colNames=T));

  # Define credit score range(300-850) and standardize variables
  setnames(old,c('X1','Auto.loan','Mortgage'),c('date','automobile','mortgage'));
  setnames(old.mort,c('X1','<620','780+'),c('date','300-619','780-850'));
  setnames(old.auto,c('X1','<620','760+'),c('date','300-619','760-850'));

  # Melt data tables and obtaine two variables: loan_type and cs_range
  mort <- melt(mort,id.vars='date',value.name='vol',variable.name='cs_range');
  mort[,loan_type:=as.factor('mortgage')];
  auto <- melt(auto,id.vars='date',value.name='vol',variable.name='cs_range');
  auto[,loan_type:=as.factor('automobile')];
  old <- melt(old,id.vars='date',value.name='vol',variable.name='loan_type');
  old[,cs_range:=as.factor('300-850')];
  old.mort <- melt(old.mort,id.vars='date',value.name='vol',variable.name='cs_range');
  old.mort[,loan_type:=as.factor('mortgage')];
  old.auto <- melt(old.auto,id.vars='date',value.name='vol',variable.name='cs_range');
  old.auto[,loan_type:=as.factor('automobile')];

  # Construct total credit score category for old.auto.vol.o
  old.auto <- rbindlist(list(old.auto[,.(loan_type=loan_type[1],
                                         cs_range=as.factor('300-850'),
                                         vol=sum(vol)),by=date],
                             old.auto),use.names=T);

  # Check that overlap of *old.auto* matches *auto* and *old*
  stopifnot(merge(auto[loan_type=='automobile',], setnames(copy(old.auto),'vol','vol2'),
                  by=c('date','loan_type','cs_range'))[,isTRUE(all.equal(vol,vol2,tolerance=0.01))]);
  # *old.auto* does not match *old*
  #stopifnot(merge(old[loan_type=='automobile',],setnames(copy(old.auto),'vol','vol2'),
  #                by=c('date','loan_type','cs_range'))[,isTRUE(all.equal(vol,vol2,tolerance=0.01))]);

  # Merge data tables and reorder. Take care of *old.auto* which overlaps with *auto* and *old*
  vol.q <- Reduce(function(...) merge(..., all=T),
                  list(mort, auto, old.mort, old.auto[grepl('^0[0-3]',date),],
                       old[(!(loan_type=='automobile' & grepl('^0[0-3]',date))),]));
  setcolorder(vol.q,c('date','loan_type','cs_range','vol'));

  # Fix dates
  vol.q[,c('year','quarter'):=lapply(tstrsplit(date,split=':Q'),as.numeric)];
  vol.q[,year:=ifelse(year>90,1900,2000)+year];
  vol.q[,date:=as.Date(ISOdate(year,quarter*3-2,1))];

  # Check that date/loan_type/cs_range are unique
  stopifnot(vol.q[,.N]==nrow(unique(vol.q,by=c('date','loan_type','cs_range'))));

  # Split credit score range
  vol.q[,c('cs_range_l','cs_range_h'):=lapply(tstrsplit(cs_range,split='-'),as.numeric)];
  vol.q[,cs_range:=NULL];
  # Check that ranges make sense
  stopifnot(vol.q[,all(cs_range_l<cs_range_h)]);

  # Tag all credit score ranges that include all
  vol.q[,cs_all:=ifelse(cs_range_l==min(cs_range_l) & cs_range_h==max(cs_range_h),T,F),
        by=.(loan_type,date)];

  # Interpolate monthly data from quarterly data
  vol.m <- vol.q[order(date),
                 .(date=as.Date(ISOdate(rep(year,each=3),
                                  mseq(3*quarter-2,to=3*quarter), 1)),
                   vol=td(vol ~ 1,conversion='sum',to=3,method='denton-cholette')$values),
        by=.(loan_type,cs_range_l,cs_range_h,cs_all)];
  #vol.m <- vol.q[order(date),
  #               .(date=as.Date(ISOdate(rep(year,each=3),
  #                                mseq(3*quarter-2,to=3*quarter), 1)),
  #                 vol=spline.q2m(x=date,y=vol,restriction='sum')$y),
  #      by=.(loan_type,cs_range_l,cs_range_h,cs_all)];

  # Check that interpolation respects original data
  stopifnot(merge(copy(vol.m)[,.(vol2=sum(vol),date=min(date,na.rm=T)),
                              by=.(loan_type,cs_range_l,cs_range_h,
                                   cs_all,year(date),quarter(date))],
                  vol.q,by=c('date','loan_type','cs_range_l','cs_range_h','cs_all'))[,
            isTRUE(all.equal(vol,vol2,tolerance=0.01))]);

  # Tag all credit score ranges that include all
  vol.m[,cs_all:=ifelse(cs_range_l==min(cs_range_l) & cs_range_h==max(cs_range_h),T,F),
        by=.(loan_type,date)];

  #ggplot(vol.m[loan_type=='mortgage' & cs_all==T,],aes(x=date,y=vol)) + geom_line() +
  #  geom_line(data=vol.q[loan_type=='mortgage' & cs_all==T,.(date,vol=vol/3)],col='green');

  setkey(vol.q,date,loan_type,cs_all,cs_range_l);
  setkey(vol.m,date,loan_type,cs_all,cs_range_l);

  # Export to global environment
  nyfed.orig.q <<- vol.q;
  nyfed.orig <<- vol.m;
});
