# Load required libraries
libs <- c('data.table','openxlsx');
for (i in libs) {
    if( !is.element(i, .packages(all.available=T))){
        capture.output(install.packages(i));
    }
    suppressMessages(require(i,character.only=T));
}
rm(i);

## Yearly data for government backed loans. Federal Home Loans(FHL)
## https://www.fhfa.gov/DataTools/Downloads/Pages/Public-Use-Databases.aspx

## HMDA data about mortgage originations, better than FHL.
## Monthly frequency for selected statistics.
## Number of loans(num) in thousands, Volume(vol) in millions of dollars.
local({
  hmda.num <- as.data.table(read.xlsx('../data/bcfp_hmda_2017-mortgage-market-activity-trends_tables.xlsx',
                  sheet='Table S1A',rows=3:29,cols=NULL,rowNames=F,colNames=T),stringsAsFactors=T);
  hmda.vol <- as.data.table(read.xlsx('../data/bcfp_hmda_2017-mortgage-market-activity-trends_tables.xlsx',
                  sheet='Table S1D',rows=3:29,cols=NULL,rowNames=F,colNames=T));

  hmda.num <- setnames(hmda.num,1,'chars');
  hmda.num[,chars:=as.factor(chars)];
  hmda.vol <- setnames(hmda.vol,1,'chars');
  hmda.vol[,chars:=as.factor(chars)];

  hmda.num <- melt(hmda.num,id.vars='chars',variable.name='date',value.name='num');
  hmda.vol <- melt(hmda.vol,id.vars='chars',variable.name='date',value.name='vol');

  hmda.num[,date:=as.Date(paste0('1.',sub('^([^.]{1,3})[^.]*','\\1',date)),format='%d.%b.%Y')];
  hmda.vol[,date:=as.Date(paste0('1.',sub('^([^.]{1,3})[^.]*','\\1',date)),format='%d.%b.%Y')];

  hmda.num[,c('category','subcategory'):=tstrsplit(chars, split=': ')];
  hmda.num[,category:=tolower(category)];
  hmda.num[,subcategory:=gsub('(\\([0-9]+\\))*$','',tolower(subcategory))];
  hmda.num[,chars:=NULL];
  hmda.vol[,c('category','subcategory'):=tstrsplit(chars, split=': ')];
  hmda.vol[,category:=tolower(category)];
  hmda.vol[,subcategory:=gsub('(\\([0-9]+\\))*$','',tolower(subcategory))];
  hmda.vol[,chars:=NULL];

  hmda.num[,c('orig','app'):=.(grepl('(O|o)riginations?',subcategory),
                                grepl('(A|a)pplications?',subcategory))];
  # Check that originations and applications don't overlap and that all rows are classified
  stopifnot(hmda.num[,all(xor(orig,app))])
  hmda.vol[,c('orig','app'):=.(grepl('(O|o)riginations?',subcategory),
                                grepl('(A|a)pplications?',subcategory))];
  # Check that originations and applications don't overlap and that all rows are classified
  stopifnot(hmda.vol[,all(xor(orig,app))])

  # Export hmda.num
  hmda.orig <<- merge(hmda.num,hmda.vol,all=T,by=c('date','category','subcategory','orig','app'));
});

#ggplot(melt(merge(hmda.orig[category=='total' & orig==T,.(date,hmdavol=vol/1000)],
#            nyfed.orig[loan_type=='mortgage' & cs_all==T,.(date,vol)],by='date'),id.vars='date'),
#       aes(x=date,y=value,col=variable)) + geom_line();
