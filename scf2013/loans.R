# Load required libraries
local({
  libs <- c('data.table','fitdistrplus','fBasics','ghyp');
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});

# Load auxiliary tools
if (!all(sapply(c('replace.na'),exists))) {
  source('../aux.R',chdir=T);
}

# Load interest rate tools
if (!all(sapply(c('replace.na','annuity.pv.c'),exists))) {
  source('../interests/tools.R',chdir=T);
}

# Load description names of variables
source('./names.R',chdir=T);

dat[,'carloan1_carnew':=car1_new]
dat[,'carloan2_carnew':=car2_new]
dat[,'carloan3_carnew':=car3_new]

# Collapse loan variables in a data table named 'loans'
loans.cols.cat <- c('mortgage','mproploan','line','remodloan','proploan',
                    'carloan','vecloan','eduloan','consloan');
loans.cols.terms <- c('int','amount','maturity','year','npay','pur','reg','pay',
                      'payfreq','typicalpay','typicalpayfreq','insttype',# 'inst',
                      'left','repay','schedule','balloon','balloon_amount',
                      'payincludes', 'fedguarant','govprogram','pmi','finpur','sechome','limit',
                      'accint','paying','deferred','payyear',
                      'adjust','adjust_firstyear','adjust_origint',
                      'adjust_eff','adjust_freq','carnew');
loans.cols <- grep(paste0('^(?:',paste(loans.cols.cat,collapse='|'),
                     ')[0-9]*_(?:',paste(loans.cols.terms,collapse='|'),')$'),
                   colnames(dat),
                   value=T);

#loans.cols;
loans <- dat[,mget(c('id','repid',loans.cols))];

## Recast loans data table
# Replace 0s with NA, at melting many of these NAs can be removed
# Don't remove them at this point. They will be removed later, to maintain the same
# number of observations per implicate
loans[is.na(loans) | loans==0,] <- NA_integer_;
loans <- melt(loans,id.vars=c('id','repid'), na.rm=F);

loans[,loan_type:=variable];
# Create loan types from var. names efficiently (use factors)
levels(loans$loan_type) <- sapply(strsplit(levels(loans$loan_type),'_'),head,1)

# Store order loan within loan type
loans[,loan_type_order:=as.integer(gsub('[a-zA-Z]+([0-9]+)','\\1',
                                        levels(loans$loan_type),))[loan_type]];

# Clear loan type from reference to loan order
levels(loans$loan_type) <- gsub('([a-zA-Z]+)[0-9]+','\\1',levels(loans$loan_type),);

# Clear var. names
levels(loans$variable) <- sub('^[^_]+_','',levels(loans$variable));

# Recast data.table
loans <- dcast(loans, id + repid + loan_type + loan_type_order ~ variable, value.var=c('value'));

# Code for annual interest rate *int*
# No interest(zero)=-1
loans[int==-1,int:=0];
# Some values are -2, but this is not coded
loans[int==-2,int:=NA];
# Interest is reported as interest rate*10,000. Divide by 100 to get interest percentage.
# Example: interest rate=0.0124, interest percentage 1.24%, coding in scf=124.
loans[,int:=int/100];
stopifnot(loans[int<0,.N]==0);

# Remove rows for which all the ids are NA.
# Do it this way so implicates have all the same length.
loans <- loans[,if(any(!is.na(int))) .SD, by=.(id,loan_type,loan_type_order)];

# Make sure that property loans already described elsewhere are not duped
stopifnot(loans[id %in% dat[proploan1==4,id] & loan_type=='proploan' & loan_type_order==1,.N]==0);
stopifnot(loans[id %in% dat[proploan2==4,id] & loan_type=='proploan' & loan_type_order==2,.N]==0);

## Deal with variables coding
# *year*, *amount*, *left* variables already ok
loans[,year_c:=as.factor(year)];
loans[is.na(year),year_c:='unknown'];
loans[,year_c:=factor(year_c,levels=rev(levels(year_c)))];

# Code for line of credit limit
# Draw period over(line expired)=-2
loans[loan_type=='line' & (limit == -2), limit:= NA_integer_];
# Set credit limit for other loans as 0
loans[loan_type!='line', limit:=0];

# Create variable *size*, means size of loan.
# It's amount for close-end loans and limit for open end loans
loans[loan_type!='line',size:=amount];
loans[loan_type=='line',size:=limit];

## Recover missing purposes(public code)
# Cars, purpose 3 (public code)
loans[loan_type=='carloan' & is.na(pur),  pur:=3];
# Other vehicles, purpose 6 (public code)
loans[loan_type=='vecloan' & is.na(pur),  pur:=6];
# Purchase first and second properties(includes home), purpose 1 (public code)
loans[loan_type=='proploan' & is.na(pur), pur:=1];
# Other loans used to purchase principal residence, purpose 1 (public code)
loans[loan_type=='mproploan' & is.na(pur),pur:=1];
# Home improvements, purpose 2 (public code)
loans[loan_type=='remodloan' & is.na(pur),pur:=2];
## Education loan, purpose 9 (public code) important expenses
## Or create custom purpose code 83 for education
loans[loan_type=='eduloan' & is.na(pur),  pur:=9];

loans[,pur2:=pur]
loans[loan_type=='eduloan', pur2:=83];
## Split education loans from group 9(important expenses)
loans[(loan_type %in% c('consloan','mortgage','proploan')) & (pur2==9) &
      (insttype %in% c(32,45,80)), pur2:=83];
## Split medical loans from group 9(important expenses)
loans[(loan_type %in% c('consloan','mortgage','proploan')) & (pur2==9) &
      (insttype %in% c(17,22)), pur2:=82];

# Convert purpose codes to factors
loans[,pur_c:=as.factor(pur)];
loans[,pur2_c:=as.factor(pur2)];

# Make sure that all purpose codes have a name
stopifnot(all(levels(loans$pur_c) %in% pur.names.public[,1]));
stopifnot(all(levels(loans$pur2_c) %in% pur.names.custom[,1]));

# Map purpose codes to description
# Mapping method: map=setNames(values,names); map[names] -> values
levels(loans$pur_c) <- setNames(pur.names.public[,2],pur.names.public[,1])[levels(loans$pur_c)];
levels(loans$pur2_c) <- setNames(pur.names.custom[,2],pur.names.custom[,1])[levels(loans$pur2_c)];
loans[is.na(pur_c),pur_c:=as.factor('unknown')];
loans[is.na(pur2_c),pur2_c:=as.factor('unknown')];
loans[,pur_c:=factor(pur_c,levels=rev(levels(pur_c)))];
loans[,pur2_c:=factor(pur2_c,levels=rev(levels(pur2_c)))];

# Code if line of credit is secured by home *sechome*
# Yes=1, No=5
loans[,sechome:= (sechome==1)];
loans[sechome==T,     sechome_c:='yes'];
loans[sechome==F,     sechome_c:='no'];
loans[is.na(sechome), sechome_c:='unknown'];
loans[,sechome_c:=factor(sechome_c,levels=rev(levels(sechome_c)))];

# Code number of payments *npay*.
# No set number of payments=-1, Unable to calculate from maturity=-7
# Flexible loans, with no set maturity, like lines of credit
loans[,flex:=as.factor('none')];
loans[(loan_type != 'line') & npay==-1,
      c('flex','npay','maturity'):=.(as.factor('total'),0,0)];
loans[npay %in% c(-1,-7), npay:=NA_integer_];

# Code maturity *maturity*.
# No set number of years=-1, Unable to calculate from npay=-7
loans[maturity %in% c(-1,-7), maturity:=NA_integer_];
# For lines of credit, set maturity to 0
loans[loan_type=='line',c('npay','maturity'):=.(0,0)];

# Code payment amount *pay*.
# None(zero)=-1, No regular payment=-2
loans[pay == -1, pay:=0];
loans[pay == -2, pay:=NA_real_];

# Code payment amount *typicalpay*.
# None(zero)=-1, No regular payment=-2
loans[typicalpay == -1, typicalpay:=0];
loans[typicalpay == -2, typicalpay:=NA_real_];

# Convert frequency codes to factors.
loans[,payfreq:=as.factor(payfreq)];
loans[,ypayfreq:=payfreq];
# ypayfreq only for valid payment frequencies.
loans[,ypayfreq:=as.factor(sub('(^-.*$|^0$|^22$)',NA,levels(ypayfreq)))[ypayfreq]];

# Make sure that all frequency codes have a name
stopifnot(all(levels(loans$payfreq) %in% payfreq.names[,1]));
stopifnot(all(levels(loans$ypayfreq) %in% as.character(payfreq.ypayfreq[,1])))

# Map frequency codes to description
# Mapping method: map=setNames(values,names); map[names] -> values
levels(loans$payfreq) <- setNames(payfreq.names[,2],payfreq.names[,1])[levels(loans$payfreq)];
levels(loans$ypayfreq) <- setNames(payfreq.ypayfreq[,2],
                                  payfreq.ypayfreq[,1])[levels(loans$ypayfreq)];
loans[,ypayfreq:=as.numeric(levels(ypayfreq))[ypayfreq]];
# For lump sump payments, use this
loans[ypayfreq==-1,
      ypayfreq:=ifelse((!is.na(maturity)) & maturity>0,
                       1/maturity,NA_real_)];

# Convert *typicalpayfreq* codes to factors
loans[,typicalpayfreq:=as.factor(typicalpayfreq)];
loans[,typicalypayfreq:=typicalpayfreq];
# typicalypayfreq only for valid payment frequencies.
loans[,typicalypayfreq:=as.factor(sub('(^-.*$|^0$|^22$)',NA,
                                      levels(typicalypayfreq)))[typicalypayfreq]];

# Make sure that all *typicalpayfreq* codes have a name
stopifnot(all(levels(loans$typicalpayfreq) %in% payfreq.names[,1]));
stopifnot(all(levels(loans$typicalypayfreq) %in% as.character(payfreq.ypayfreq[,1])))

# Map *typicalpayfreq* codes to description
# Mapping method: map=setNames(values,names); map[names] -> values
levels(loans$typicalpayfreq) <- setNames(payfreq.names[,2],payfreq.names[,1])[levels(loans$typicalpayfreq)];
levels(loans$typicalypayfreq) <- setNames(payfreq.ypayfreq[,2],
                                  payfreq.ypayfreq[,1])[levels(loans$typicalypayfreq)];
loans[,typicalypayfreq:=as.numeric(levels(typicalypayfreq))[typicalypayfreq]];
# For lump sump payments, use this
loans[typicalypayfreq==-1,
      typicalypayfreq:=ifelse((!is.na(maturity)) & maturity>0,
                       1/maturity,NA_real_)];

# Convert payment on schedule codes to factors
loans[,schedule:=as.factor(schedule)];

# Make sure that all schedule codes have a name
stopifnot(all(levels(loans$schedule) %in% schedule.names[,1]));

# Map frequency codes to description
# Mapping method: map=setNames(values,names); map[names] -> values
levels(loans$schedule) <- setNames(schedule.names[,2],schedule.names[,1])[levels(loans$schedule)];

# Code for expected repay year *pay*. Also create *reverse_anuity*.
# Reverse anuity loan=-1, not expecting to repay=-2
loans[!is.na(repay),reverse_anuity:=F];
loans[repay == -1,reverse_anuity:=T];
loans[repay %in% c(-1,-2),repay:=NA_integer_];

## Convert institution codes to factors
#loans[,inst_c:=as.factor(inst)];
#
## Make sure that all institution codes have a name
#stopifnot(all(levels(loans$inst_c) %in% inst.names[,1]));
#
## Map institution codes to description
## Mapping method: map=setNames(values,names); map[names] -> values
#levels(loans$inst_c) <- setNames(inst.names[,2],inst.names[,1])[levels(loans$inst_c)];
#loans[is.na(inst),inst_c:=as.factor('unknown')];
#loans[,inst_c:=factor(inst_c,levels=rev(levels(inst_c)))];

# Convert institution type codes to factors
loans[,insttype_c:=as.factor(insttype)];
loans[,insttype_cr:=as.factor(insttype)];

# Make sure that all institution codes have a name
stopifnot(all(levels(loans$insttype_c) %in% inst.names[,1]));
stopifnot(all(levels(loans$insttype_cr) %in% as.character(inst.names.reduced[[1]])));

# Map institution codes to description
# Mapping method: map=setNames(values,names); map[names] -> values
levels(loans$insttype_c) <- setNames(inst.names[,2],inst.names[,1])[levels(loans$insttype_c)];
loans[is.na(insttype_c),insttype_c:=as.factor('unknown')];
loans[,insttype_c:=factor(insttype_c,levels=rev(levels(insttype_c)))];
levels(loans$insttype_cr) <- setNames(inst.names.reduced[[2]],inst.names.reduced[[1]])[levels(loans$insttype_cr)];
loans[is.na(insttype_cr),insttype_cr:=as.factor('unknown')];
loans[,insttype_cr:=factor(insttype_cr,levels=rev(levels(insttype_cr)))];

# Institution rule enforcer *enforce*
for (aa in names(enforce.insttype)) {
  loans[insttype %in% enforce.insttype[[aa]], enforce:=as.factor(aa)];
}
loans[is.na(enforce), enforce:=as.factor('unknown')];

# Code for regular installment dummy *reg*
# Regular installment=1, Other kind=2
loans[,reg:=(reg==1)];

# Code for adjustable rate dummy *adjust*
# Yes=1, No=5
loans[,adjust:=adjust==1];

# Create categorical variable for adjust
loans[adjust==T    , adjust_c:=as.factor('adjustable')];
loans[adjust==F    , adjust_c:=as.factor('fixed')     ];
loans[is.na(adjust), adjust_c:=as.factor('unknown')   ];
loans[,adjust_c:=factor(adjust_c,levels=rev(levels(adjust_c)))];

# First year that interest rate could have changed
# Don't know, but could have changed already=-2, other values == year
loans[adjust_firstyear==-2,adjust_firstyear:=NA]
# Calculate the first year that interest rate could have changed relative to start year
loans[adjust_firstyear>0 & year>0,adjust_firstyear:=adjust_firstyear-year]
# Changes in interest before the loan was taken don't make any sense.
#loans[,adjust_firstyear:=pmax(adjust_firstyear,0)];
loans[adjust_firstyear<0,adjust_firstyear:=NA];
# To be meaninful, first adjustment should be smaller than maturity unless loan
# termination can be delayed or modified. Don't remove them, but take this fact into account.
#loans[,adjust_firstyear:=pmin(adjust_firstyear,maturity)];
#loans[adjust_firstyear>=maturity, adjust_firstyear:=NA];

# Code original interest rate of adjustable loans. Also 
# No interest=-1
loans[adjust_origint==-1,adjust_origint:=0];
# Interest is reported as interest rate*10,000. Divide by 100 to get interest percentage.
# Example: interest rate=0.0124, interest percentage 1.24%, coding in scf=124.
loans[,adjust_origint:=adjust_origint/100];

# Rewrite *int* to include the interest rate closest to loan origination.
# Save reported interest rate in *lastint*.
loans[,lastint:=int];
loans[!is.na(adjust_origint),int:=adjust_origint];

# Code if adjustment of interest rates has been effective
# Yes=1, No =5
loans[,adjust_eff:= (adjust_eff==1)];

# Code for balloon payment dummy *balloon*
# Yes=2, No=1
loans[,balloon:= balloon==2]

# Code for balloon payment amount *balloon_amount*
# Amount, INAP=0

# Code for *fedguarant*
# Yes=1, No=5
loans[,fedguarant:= fedguarant==1];
loans[fedguarant==T,    fedguarant_c:=as.factor('yes'    )];
loans[fedguarant==F,    fedguarant_c:=as.factor('no'     )];
loans[is.na(fedguarant),fedguarant_c:=as.factor('unknown')];
loans[,fedguarant_c:=factor(fedguarant_c,levels=rev(levels(fedguarant_c)))];

# Code for *govprogram*
loans[,govprogram_c:=as.factor(govprogram)];

# Make sure that all institution codes have a name
stopifnot(all(levels(loans$govprogram_c) %in% govprogram.names[,1]));

# Map Government program codes to description
# Mapping method: map=setNames(values,names); map[names] -> values
levels(loans$govprogram_c) <- setNames(govprogram.names[,2],govprogram.names[,1])[levels(loans$govprogram_c)];
loans[fedguarant_c=='yes' & is.na(govprogram_c),govprogram_c:=as.factor('unknown')];
loans[is.na(govprogram_c),govprogram_c:=as.factor('none')];
loans[,govprogram_c:=factor(govprogram_c,levels=rev(levels(govprogram_c)))];

loans[,govprogram_d:=(govprogram_c!='none')];

# Code for Private Mortgage Insurance *pmi*
# Yes=1, No=5
loans[,pmi:= pmi==1];
loans[pmi==T,    pmi_c:=as.factor('yes'    )];
loans[pmi==F,    pmi_c:=as.factor('no'     )];
loans[is.na(pmi),pmi_c:=as.factor('unknown')];
loans[,pmi_c:=factor(pmi_c,levels=rev(levels(pmi_c)))];

# Code *finpur*
# Refinance=1, Borrow more=2, refinance and borrow more=3,
# Paid in cash but took loan later=4, mortgage assumed when inherited the house=8
# Check that only values of codebook exist
stopifnot(loans[,all(finpur %in% c(1:4,8,NA))]);
loans[finpur==1, finpur_c:=as.factor('refinance')];
loans[finpur==2, finpur_c:=as.factor('borrow more')];
loans[finpur==3, finpur_c:=as.factor('refinance and borrow more')];
loans[finpur==4, finpur_c:=as.factor('paid in cash, took loan later')];
loans[finpur==8, finpur_c:=as.factor('took loan after inheritance')];
loans[is.na(finpur),finpur_c:=as.factor('unknown')];
loans[,finpur_c:=factor(finpur_c,levels=rev(levels(finpur_c)))];

loans[finpur %in% c(1,3),                      refinance_c:=as.factor('yes')];
loans[(!is.na(finpur)) & (!finpur %in% c(1,3)),refinance_c:=as.factor('no')];
loans[is.na(finpur),refinance_c:=as.factor('unknown')];
loans[,refinance_c:=factor(refinance_c,levels=rev(levels(refinance_c)))];

loans[finpur %in% c(2,3),                      borrowmore_c:=as.factor('yes')];
loans[(!is.na(finpur)) & (!finpur %in% c(2,3)),borrowmore_c:=as.factor('no')];
loans[is.na(finpur),borrowmore_c:=as.factor('unknown')];
loans[,borrowmore_c:=factor(borrowmore_c,levels=rev(levels(borrowmore_c)))];

## Code for new or used car *new*
## New=1, Used=2
loans[,carnew:= carnew==1];
loans[carnew==T,    carnew_c:=as.factor('new'    )];
loans[carnew==F,    carnew_c:=as.factor('used'   )];
loans[is.na(carnew),carnew_c:=as.factor('unknown')];
loans[,carnew_c:=factor(carnew_c,levels=rev(levels(carnew_c)))];

# Code for Payment Includes *payincludes*
# Yes=1, No=5
loans[,payincludes_c:=as.factor(payincludes)];

levels(loans$payincludes_c) <- setNames(payincludes.names[,2],
                                 payincludes.names[,1])[levels(loans$payincludes_c)]
loans[is.na(payincludes_c),payincludes_c:=as.factor('unknown')];
loans[,payincludes_c:=factor(payincludes_c,levels=rev(levels(payincludes_c)))];

loans[,payincludes_cr:=payincludes_c];
levels(loans$payincludes_cr) <- replace(levels(loans$payincludes_cr),
                                        grep('nothing|unknown',
                                             levels(loans$payincludes_cr)),
                                        'unknown/nothing');

# Code for Accumulating interest that will have to be paid *accint*
# Yes=1, No=5
loans[,accint:= accint==1];
loans[accint==T,    accint_c:=as.factor('yes'    )];
loans[accint==F,    accint_c:=as.factor('no'     )];
loans[is.na(accint),accint_c:=as.factor('unknown')];
loans[,accint_c:=factor(accint_c,levels=rev(levels(accint_c)))];

# Code for paying loan right now *paying*
# Yes=1, No=5
loans[,paying:= paying==1];
loans[paying==T,    paying_c:=as.factor('yes'    )];
loans[paying==F,    paying_c:=as.factor('no'     )];
loans[is.na(paying),paying_c:=as.factor('unknown')];
loans[,paying_c:=factor(paying_c,levels=rev(levels(paying_c)))];

# Code for deferring loan payments *deferred*
# Yes=1, No=5
loans[,deferred:= deferred==1];
loans[deferred==T,    deferred_c:=as.factor('yes'    )];
loans[deferred==F,    deferred_c:=as.factor('no'     )];
loans[is.na(deferred),deferred_c:=as.factor('unknown')];
loans[,deferred_c:=factor(deferred_c,levels=rev(levels(deferred_c)))];

# Code for *payyear*, year loan will start getting repaid
# Year

## Categorize very flexible loans
loans[(loan_type != 'line') & is.na(npay) & (
        ((!is.na(reg)) & reg==F) |
        ((!is.na(deferred_c)) & deferred_c=='yes') |
        ((!is.na(paying_c)) & paying_c=='no') |
        ((!is.na(accint_c)) & accint_c=='yes')),
      c('flex','npay','maturity'):=.(as.factor('total'),0,0)]

loans[,log_maturity:=ifelse(maturity>0,log(maturity),0)];

# Categorize flexible loans
loans[(loan_type != 'line') & (!is.na(npay)) & (flex=='none') &
        Reduce(`|`,lapply(list(ypayfreq,pay),is.na)) & 
        (!(Reduce(`&`,lapply(list(ypayfreq,typicalypayfreq),is.na)) | 
           Reduce(`&`,lapply(list(pay,typicalpay),is.na)))),
      c('flex','ypayfreq','pay'):=
        .(as.factor('low'),
          ifelse(is.na(ypayfreq),typicalypayfreq,ypayfreq),
          ifelse(is.na(pay),typicalpay,pay))];

loans[loan_type != 'line' & (!is.na(npay)) & (flex=='none') &
        (Reduce(`&`,lapply(list(ypayfreq,typicalypayfreq),is.na)) |
         Reduce(`&`,lapply(list(pay,typicalpay),is.na))) &
         (paying_c=='no' | deferred_c=='yes' | accint_c=='yes'),
      flex:='high'];

## Create variable for type of educational loan
loans[,eduloan_type:=as.factor('non educational')]
loans[loan_type=='eduloan' & grepl('government|Direct student loan',insttype),
      eduloan_type:=as.factor('government')];
loans[loan_type=='eduloan' & (!grepl('government|Direct student loan',insttype)),
      eduloan_type:=as.factor('private')];

## Fix inconsistencies
source('./incons_fix.R',chdir=T);
