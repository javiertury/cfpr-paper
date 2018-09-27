# Load required libraries
local({
  if(! library(lodown,logical.return=T)) {
    install.packages('devtools');
    require(devtools);
    install_github('ajdamico/lodown',dependencies=T);
  }
  libs <- c('data.table','haven','plm','stargazer','ggplot2','psych',
            'lubridate','multiwayvcov','survey','mitools','lodown');
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});

# Load data
if (!all(sapply(c('dat','imp1','imp2','imp3','imp4','imp5','rw'),exists))) {
  source('scf2013/load.R',chdir=T);
}

# Create loans data.table
if(!exists('loans')) source('scf2013/loans.R',chdir=T);

# Load treasury zero coupon interest rates
if (!all(sapply(c('treasury.zero'),exists))) {
  source('interests/zero.R',chdir=T);
}

# Load interest rate tools
if (!all(sapply(c('annuity.rfint','annuity.rfint.y','annuity.rfint.c.y','interp.int.y'),exists))) {
  source('interests/tools.R',chdir=T);
}

# Load mooodys interest rate(for risk premium)
if (!all(sapply(c('moodys.m'),exists))) source('interests/moodys.R',chdir=T);

# Loand HQM Corporate bonds spot rate
if (!all(sapply(c('hqm.m'),exists))) source('interests/hqm.R',chdir=T);

# Load regular treasury interest rates
if (!all(sapply(c('treasury.m'),exists))) {
  source('interests/treasury.R',chdir=T);
}

## Load APOR rates
#if (!all(sapply(c('apor.f','apor.a'),exists))) {
#  source('interests/apor.R',chdir=T);
#}

# Load auxiliary tools
if (!all(sapply(c('pr.event.m2y','pr.event.m2d2y'),exists))) source('aux.R',chdir=T);

# Load information about aggregate loan volumes
#if (!all(sapply(c('ccreditvol.hld'),exists))) source('loan_aggregate/ccredit.R',chdir=T);
#if (!all(sapply(c('hmda.orig'),exists))) source('loan_aggregate/hmda.R',chdir=T);
#if (!all(sapply(c('nyfed.orig'),exists))) source('loan_aggregate/nyfed.orig.R',chdir=T);
if (!all(sapply(c('cfpb.vol.age'),exists))) source('loan_aggregate/cfpb.orig.R',chdir=T);

# Load CPI indexes
if (!all(sapply(c('cpi.m'),exists))) source('data_cpi.R',chdir=T);

# Load dates of interest
if (!all(sapply(c('reg.dates'),exists))) source('study_dates.R',chdir=T);


# Risk premium from moodys
# risk premium = 20+ moodys BBA - 20yr treasury
# 30yr treasury series was discontinued at several points in time, not reliable
mktrp.m <- merge(moodys.m[grade=='baa',.(date,mktint=int)],
                 treasury.m[maturity==20,.(date,mktrfint=int)],
                 by='date')[,mktrp:=mktint-mktrfint][,.(date,mktrp)]

mktrpc.m <- merge(moodys.m[grade=='baa',.(date,mktintc=int.conv(int,2,Inf))],
                 treasury.m[maturity==20,.(date,mktrfintc=int.conv(int,2,Inf))],
                 by='date')[,mktrpc:=mktintc-mktrfintc][,.(date,mktrpc)]

# Variable to control for term structure of interest rates
termstruct.m <- treasury.m[,.(termstruct=cor(int,maturity,use='pairwise.complete.obs')),by=date];
termstructc.m <- treasury.m[,.(termstruct=cor(int.conv(int,2,Inf),
                                             maturity,use='pairwise.complete.obs')),by=date];

## Geometric mean of interest rates by month for APOR
## By maturity
#apor.f.m <- apor.f[,.(int=(prod((1+int/100)^(1/length(int)))-1)*100),
#                   by=.(date=floor_date(date,'month'),maturity)];
## By year of first possible adjustment
#apor.a.m <- apor.a[,.(int=(prod((1+int/100)^(1/length(int)))-1)*100),
#                   by=.(date=floor_date(date,'month'),first_adjust)];
## Aggregate by date
##apor.a.m.uncond <- apor.a.m[,.(int=(prod((1+int/100)^(1/first_adjust/sum(1/first_adjust)))-1)*100),
##                            by=date];
#
### For adjustable loans with missing *adjust_firstyear*
## Compute probability of each *adjust_firstyear* from loans dataset using maturity
## Then weight apor by the probability of *adjust_firstyear* given each maturity
#local({
#  # Simpler model model and with better distributed residuals
#  reg_1year2mat <- lm(I(adjust_firstyear/maturity) ~ log(maturity), data=loans[maturity>0,]);
#
#  # Get SD of residuals
#  sd_1year2mat <- sd(residuals(reg_1year2mat));
#  # Get maximum and minimum dominium for inference, transform from log to linear
#  lim_1year2mat <- exp(c(min=min(reg_1year2mat$model['log(maturity)']),
#                         max=max(reg_1year2mat$model['log(maturity)'])));
#  lim_1year <- c(min=1,max=apor.a.m[,max(first_adjust)])
#  max_mat <- loans[,max(maturity,na.rm=T)];
#
#  # Get regular apor.a.m dataset and create one row for each possible maturity
#  apor.a.mat.m <- apor.a.m[,cbind(.SD[rep(1:.N,max_mat)],maturity=rep(1:max_mat,.N))];
#  apor.a.mat.m[,adjust_firstyear_p:=predict(reg_1year2mat,
#                   newdata=data.table(maturity=pmax(pmin(maturity,lim_1year2mat['max']),
#                                                    lim_1year2mat['min'])))*maturity];
#  # Make sure that *adjust_firstyear_p* is within allowable range
#  apor.a.mat.m[,adjust_firstyear_p:=pmin(pmax(adjust_firstyear_p,lim_1year['min']),lim_1year['max'])];
#
#  # Weight each first_adjust by the probability conditional on maturity
#  # Probability of an adjust_firstyear in [first_adjust,first_adjust+1)
#  #apor.a.mat.m[,weight:=dnorm((first_adjust-adjust_firstyear_p)/(maturity*sd_1year2mat))*10^6];
#  apor.a.mat.m[,weight:=(pnorm((first_adjust+1-adjust_firstyear_p)/(maturity*sd_1year2mat))-
#                         pnorm((first_adjust  -adjust_firstyear_p)/(maturity*sd_1year2mat)))];
#  apor.a.mat.m[first_adjust==lim_1year['min'],
#               weight:=pnorm((first_adjust-adjust_firstyear_p)/(maturity*sd_1year2mat))];
#  apor.a.mat.m[first_adjust==lim_1year['max'],
#               weight:=(1-pnorm((first_adjust-adjust_firstyear_p)/(maturity*sd_1year2mat)))];
#
#  # Check that all weights/probabilities are ok
#  stopifnot(apor.a.mat.m[,all(weight>=0 & weight<=1)]);
#
#  # Calculate weighted average
#  #apor.a.mat.m <<- apor.a.mat.m[,.(int=(prod((1+int/100)^(weight/sum(weight)))-1)*100),
#  #                              by=.(date,maturity)];
#  # For calculating APOR tables they use simple arithmetic averages. Use the same.
#  apor.a.mat.m <<- apor.a.mat.m[,.(int=sum(int*weight)), by=.(date,maturity)];
#});

# Merge dat and loans datasets into regdat
dat.attributes <- c('wgt','weight','sex','age','age_partner','income','income_total',
                    'networth', 'income_regular');
regdat <- merge(loans, dat[,mget(c('id','repid',dat.attributes))],
                by=c('id','repid'), all.x=T);

# Map credit volume sereis to loan types
loantype.vol.map <- list(
     # serie       # purpose
     'mortgage'  =c('mortgage', 'proploan', 'mproploan', 'remodloan'),
     'automobile'=c('carloan', 'vecloan'),
     'student'   =c('eduloan'));

# Check that all purposes mapped exist
stopifnot(all(unlist(loantype.vol.map) %in% levels(regdat$loan_type)))

# Use the sum of all series volume for the rest of purposes
loantype.vol.map <- c(loantype.vol.map,
  'all_nrev'=list(c(levels(regdat$loan_type)[! levels(regdat$loan_type) %in% unlist(loantype.vol.map)],
                    NA)));

## Map credit volume sereis to loan purposes
#pur.vol.map <- list(
#     # serie       # purpose
#     'mortgage'  =c('home', 'home reform'),
#     'automobile'=c('vehicles', 'other vehicles'),
#     'student'   =c('important expenses'));
#
## Check that all purposes mapped exist
#stopifnot(all(unlist(pur.vol.map) %in% levels(regdat$pur_c)))
#
## Use the sum of all series volume for the rest of purposes
#pur.vol.map <- c(pur.vol.map,
#  'all_nrev'=list(c(levels(regdat$pur_c)[! levels(regdat$pur_c) %in%
#                                           unlist(pur.vol.map)],NA)));

# Map credit volume sereis to loan purposes
pur2.vol.map <- list(
     # serie       # purpose
     'mortgage'  =c('home', 'home reform'),
     'automobile'=c('vehicles', 'other vehicles'),
     'student'   =c('education'));

# Check that all purposes mapped exist
stopifnot(all(unlist(pur2.vol.map) %in% levels(regdat$pur2_c)))

pur2.vol.map <- c(pur2.vol.map,
  'all_nrev'=list(c(levels(regdat$pur2_c)[! levels(regdat$pur2_c) %in%
                                            unlist(pur2.vol.map)],NA)));

pur2.basket.map <- list(
     # serie       # purpose
     'all'                    =c('unknown','other expenses','important expenses',
                                 'family expenses','investment'),
     'medical'                =c('medical'),
     'recreation'             =c('entertainment'),
     'education'              =c('education'),
     # 'tuition'                =c('education'),
     'vehicles'               =c('vehicles','other vehicles'),
     # 'private transportation' =c('vehicles','other vehicles'),
     # 'vehicles'               =c('other vehicles'),
     # 'new car'                =c('new vehicles'),
     # 'used car'               =c('used vehicles'),
     # 'durables'               =c('education'),
     'household furnishing'   =c('home appliances'),
     'housing'                =c('home','home reform'));

stopifnot(all(unlist(pur2.basket.map) %in% regdat[,levels(pur2_c)]))

# Age at origination. Has to be done after merging dat(age) with loans(year)
regdat[,age_orig:=age-(2013-year)];
regdat[,age_partner_orig:=age_partner-(2013-year)];

local({
  # Assign age groups
  for (aa in age_group.map[,'age_group']) {
    # Get min and max age
    age_lim <- age_group.map[age_group.map[,'age_group']==aa,2:3];

    # Assign age groups
    regdat[(round(age_orig) >= age_lim[1]) & (round(age_orig) <= age_lim[2]),
           age_group:=as.factor(aa)];
  }

  # Assign volume series group
  for (ss in names(pur2.vol.map)) {
    # Get valid purposes for that serie
    pp <- pur2.vol.map[[ss]];

    # Assign volume series group
    regdat[pur2_c %in% pp, series_vol:=as.factor(ss)];
  }

  # Assign cpi series group
  for (ss in names(pur2.basket.map)) {
    # Get valid purposes for that serie
    pp <- pur2.basket.map[[ss]];

    # Assign volume series group
    regdat[pur2_c %in% pp, series_cpi:=as.factor(ss)];
  }

  # Weights and subsets by group
  groups <- list(
       list(groupcols=c('series_vol','age_group'),
            weights=cfpb.vol.age[season_adj==F,
                          .(date,series_vol=credit_type,year=year(date),
                            age_group,weight=vol)],
            subrows=regdat[year >= 2005,which=T])
       );

  for (gg in groups) {
    groupcols <- gg[['groupcols']];
    weights <- gg[['weights']];
    subrows <- gg[['subrows']];

    print('creating events')
    # For each event
    for (ee in names(reg.dates)) {
      # Create event date
      event.d <- reg.dates[[ee]]
      # Assign probability of event happening for each loan in that age cohort
      regdat[subrows,c(ee):=pr.event.m2d2y(event=event.d, weights=weights,
                                           by=c(list(year=year),mget(groupcols)))];
    }

    print('calculating cpi amount')
    regdat[subrows,ppp:=cpi.m[basket=='all' & date==as.Date('2010-01-01'),cpi]/
                        year.agg(data=cpi.m[basket=='all',.(date,cpi)],
                                 weights=weights, by=c(list(year=year), mget(groupcols)))];
    regdat[subrows,ppp2:=cpi.m[date==as.Date('2010-01-01'),setNames(cpi,basket)][series_cpi]/
                         year.agg(data=cpi.m[,.(date,series_cpi=basket,cpi)],
                                  weights=weights, by=c(list(year=year,series_cpi=series_cpi),
                                                        mget(groupcols)))];

    print('calculating risk free interest rate')
    # Calculate risk free interest rate for that age cohort
    # For maturity, *npay/ypayfreq* has more resolution than *maturity*
    regdat[intersect(subrows,regdat[(loan_type != 'line') & (!flex %in% c('total','high')),which=T]),
           rfintc:= spot2annuity.int.c.y(spot.intc=function(date, maturity) {
                                           return(zero.yield.m(dates=date,
                                                               maturities=maturity,extrapol=T))
                                         },
                                         weights=weights,
                                         by=c(list(year=year, ypayfreq=ypayfreq, npay=npay,
                                                   pay=pay,balloon=replace.na(balloon_amount)),
                                              mget(groupcols)))];
    # Can't do it for lines of credit(LOC), they don't have *year*
    # LOCs get interest for most recent date
    regdat[intersect(subrows,regdat[(loan_type == 'line') | (flex %in% c('total','high')),which=T]),
           rfintc:=interp.int.y(data.int=copy(treasury.m)[,int:=int.conv(int,2,Inf)],
                                weights=weights,
                                by=c(list(year=year, maturity=20),
                                     mget(groupcols)))];

    print('calculating market risk premium')
    # Market risk premium
    regdat[subrows,mktrp:=year.agg(data=mktrp.m, weights=weights,
                                   by=c(list(year=year), mget(groupcols)))]

    print('calculating comparable risky interest rate')
    ## Interest rate with risk for comparable maturities
    regdat[intersect(subrows,regdat[(loan_type != 'line') & (!flex %in% c('total','high')),which=T]),
           riskintc:=spot2annuity.int.c.y(
      spot.intc=copy(hqm.m[year(date) <= regdat[subrows,max(year,na.rm=T)] &
                           year(date) >= regdat[subrows,min(year,na.rm=T)],])[,
                           c('int','log_maturity'):=.(int.conv(int,2,Inf),log(maturity))][,
                           .(int=mean(int,na.rm=T),log_maturity=mean(log_maturity,na.rm=T)),
                           by=.(date,round=round(log_maturity*2))][,
                           c('round','maturity','log_maturity'):=.(NULL,exp(log_maturity),NULL)],
      weights=weights, by=c(list(year=year, ypayfreq=ypayfreq, npay=npay,
                                 pay=pay, balloon=replace.na(balloon_amount)),
                            mget(groupcols)))];
    regdat[intersect(subrows,regdat[(loan_type == 'line') | (flex %in% c('total','high')),which=T]),
           riskintc:=interp.int.y(data.int=copy(hqm.p.m)[,int:=int.conv(int,2,Inf)],
                                weights=weights,
                                by=c(list(year=year, maturity=20),
                                     mget(groupcols)))];

    print('calculating termstructure')
    # Term structure
    regdat[subrows,termstruct:=year.agg(data=termstruct.m, weights=weights,
                                        by=c(list(year=year), mget(groupcols)))];

    #print('calculating comparable apor rate')
    ## APOR for loans with comparable characteristics
    ## Non adjustable rate loans
    #regdat[intersect(subrows,regdat[adjust %in% c(F,NA),which=T]),
    #       comp_apor:=year.agg(data=apor.f.m, weights=weights,
    #                           by=c(list(year=year,
    #                                     maturity=pmin(pmax(floor(maturity),1),
    #                                                   apor.f.m[,max(maturity)])),
    #                                mget(groupcols)))];

    ## Adjustable rate loans
    ## For loans that have *adjust_firstyear*
    #regdat[intersect(subrows,regdat[adjust==T & (!is.na(adjust_firstyear)),which=T]),
    #       comp_apor:=year.agg(data=apor.a.m,
    #                           weights=weights,
    #                           by=c(list(first_adjust=pmin(pmax(adjust_firstyear,1),
    #                                       apor.a.m[,max(first_adjust)]),
    #                                     year=year), mget(groupcols)))];
    ## For loans that don't have *adjust_firstyear*, but have maturity
    #regdat[intersect(subrows,regdat[adjust==T & is.na(adjust_firstyear),which=T]),
    #       comp_apor:=year.agg(data=apor.a.mat.m, weights=weights,
    #                           by=c(list(year=year, maturity=maturity),
    #                                mget(groupcols)))];

    print('calculating comparable treasury rate')
    # Calculate average APR for treasury securities with comparable characteristics
    regdat[subrows,comp_treasury:=interp.int.y(data.int=treasury.m,weights=weights,
                                               by=c(list(year=year, maturity=maturity),
                                                    mget(groupcols)))];
  }
});

# Calculate continously compounded interest rate from loan characteristics
# For maturity, *npay/ypayfreq* has more resolution than *maturity*
regdat[,newintc:=annuity.int.c(amount=amount,ypayfreq=ypayfreq,npay=npay,
                               pay=replace.na(pay),
                               balloon=replace.na(balloon_amount))];

regdat[,newintc_reasonable:=F];
regdat[newintc < 50 & newintc >= 0, newintc_reasonable:=T];

# Transform newintc from continous compounding to APR
regdat[,newapr:=int.conv(newintc,from=Inf,to=ypayfreq)];

## Transform riskintc from continous compounding to APR
regdat[loan_type!='line',riskapr:=int.conv(riskintc,from=Inf,to=ypayfreq)];
regdat[(loan_type == 'line') | (flex %in% c('total','high')),
       riskapr:=int.conv(riskintc,from=Inf,
                       to=ifelse(!is.na(ypayfreq),ypayfreq,
                                 ifelse(!is.na(typicalypayfreq),
                                        typicalypayfreq,12)))];

# Transform rfintc from continous compounding to APR
regdat[loan_type!='line',rfapr:=int.conv(rfintc,from=Inf,to=ypayfreq)];
regdat[(loan_type == 'line') | (flex %in% c('total','high')),
       rfapr:=int.conv(rfintc,from=Inf,
                       to=ifelse(!is.na(ypayfreq),ypayfreq,
                                 ifelse(!is.na(typicalypayfreq),
                                        typicalypayfreq,12)))];

# Transform *int* from APR to continous compounding
regdat[,intc:=int.conv(int,from=ypayfreq,to=Inf)];

# Create premium of loan
regdat[,premium:=int-rfapr];
regdat[,premiumc:=intc-rfintc];
regdat[,newpremium:=newapr-rfapr];
regdat[,newpremiumc:=newintc-rfintc];

# Winsorize dependent variables
regdat[,int:=winsor(as.numeric(int),trim=0.01,na.rm=T)];
regdat[newintc_reasonable==T,newapr:=winsor(as.numeric(newapr),trim=0.01,na.rm=T)];
regdat[,premium:=winsor(as.numeric(premium),trim=0.01,na.rm=T)];
regdat[newintc_reasonable==T,newpremium:=winsor(as.numeric(newpremium),trim=0.01,na.rm=T)];
#regdat[,riskapr:=winsor(as.numeric(riskapr),trim=0.01,na.rm=T)];

# Caculated amount adjusted by cpi
regdat[,amount_cpi:=ppp*amount];
regdat[,amount_cpi2:=ppp*amount];

regdat[,main_mortgage:=F];
regdat[loan_type=='mortgage', main_mortgage:=T];

regdat[,other_mortgage:=F];
regdat[loan_type=='proploan', other_mortgage:=T];

regdat[,any_mortgage:=F];
regdat[loan_type %in% c('mortgage','proploan'), any_mortgage:=T];

regdat[,adjust_mortgage:=F];
regdat[adjust_c == 'adjustable', adjust_mortgage:=T];

regdat[,eduloan:=F];
regdat[loan_type =='eduloan', eduloan:=T];

regdat[,education:=F];
regdat[pur2_c =='education', education:=T];

regdat[,eduinst:=F];
regdat[grepl('^School',insttype_c), eduinst:=T];

regdat[,eduafter2008:=F];
regdat[(year >= 2008) & (loan_type=='eduloan'), eduafter2008:=T];

regdat[,fedguarant_d:=F];
regdat[fedguarant_c=='yes',fedguarant_d:=T];

regdat[,pmi_d:=F];
regdat[pmi_c=='yes',pmi_d:=T];

regdat[,borrowmore_d:=F];
regdat[borrowmore_c=='yes',borrowmore_d:=T];

regdat[,refinance_d:=F];
regdat[refinance_c=='yes',refinance_d:=T];

regdat[,payincludes_d:=F];
regdat[! payincludes_c %in% c('unknown','nothing'),payincludes_d:=T];

regdat[,ftc:=F];
regdat[enforce %in% c('unknown','ftc'),ftc:=T];

regdat[,newhome:=F];
regdat[pur_c=='home' & refinance_d==F, newhome:=T];
regdat[,nonewhome:=(!newhome)];

regdat[,newcover_mdia:=F];
regdat[other_mortgage==T | (main_mortgage==T & (pur_c!='home' | refinance_d==T)),
       newcover_mdia:=T];

regdat[,ftc_mortgage:=F];
regdat[ftc==T & any_mortgage==T, ftc_mortgage:=T];

regdat[,pel:=F];
#regdat[eduloan==T & flex=='none' & any_mortgage==F, pel:=T];
regdat[education==T & any_mortgage==F, pel:=T];

regdat[,any_flex:=F];
regdat[flex!='none', any_flex:=T];

regdat[,no_flex:=(!any_flex)];

# Year fixed effects by gorups of 2 or 3 years
regdat[,year2:=year - (year %% 2)];
regdat[,year3:=(year-1) - ((year-1) %% 3)];
regdat[,year2_c:=as.factor(year2)];
regdat[is.na(year2),year2_c:=as.factor('unknown')];
regdat[,year3_c:=as.factor(year3)];
regdat[is.na(year3),year3_c:=as.factor('unknown')];

# Check that all implicates have the same number of rows and columns
#stopifnot(regdat[-exrows,length(unique(c(table(repid))))==1]);
stopifnot(regdat[,length(unique(c(table(repid))))==1]);

## Exclude observations. No, better use built-in subset.
exrows <- NULL
exrows2 <- NULL

## Exclude government loans and programs, not part of free market
## Sometimes they are used to implement regulations to affect the market
exrows <- union(regdat[id %in% regdat[insttype_cr == 'government',id],which=T],
                exrows);
exrows2 <- union(regdat[insttype_cr == 'government',which=T],
                exrows2);

## Exclude loans at interes rate 0
## These loans usually don't follow markets and non-profits are usually exempted from most regulations.
exrows <- union(regdat[id %in% regdat[int == 0,id],which=T],
                exrows);
exrows2 <- union(regdat[(!is.finite(int)) | (int == 0),which=T],
                exrows2);

# Construct imputed replicate-weighted survey design
scf_design <- svrepdesign(
  weights = ~wgt,
  repweights = merge(regdat[order(id,loan_type,loan_type_order,
                                  repid),][repid==1,.(id)],
                     rw, by.x='id',by.y='yy1',all.x=T)[order(id),-1],
  data = imputationList(split(regdat[order(id,loan_type,loan_type_order,repid),],
                              by='repid',
                              sorted=T)),
  scale = 1,
  rscales = rep(1/998,999),
  # use the mean of the replicate statistics as the center
  # when calculating the variance, as opposed ot the main weight's statistic
  mse = T,
  type = 'other',
  combined.weights = T);

# Exclude government and zero interest rates(non-profit, non market motives, hidden motives...)
scf_design <- subset(scf_design, (is.finite(int) & (int!=0)) & (insttype_cr != 'government'));
