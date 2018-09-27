libs <- c('data.table');
for (i in libs) {
    if( !is.element(i, .packages(all.available=T))){
        capture.output(install.packages(i));
    }
    suppressMessages(require(i,character.only=T));
}
rm(i);

# Load auxiliary tools
if (!all(sapply(c('replace.na','pr.event.m'),exists))) {
  source('../aux.R',chdir=T);
}

# Load interest rate tools
if (!all(sapply(c('replace.na','int.conv','annuity.pv.c'),exists))) {
  source('../interests/tools.R',chdir=T);
}

# Load tables with code names
source('./names.R',chdir=T);

loan.cols <- colnames(dat)[which(sapply(strsplit(as.character(colnames(dat)), "_"),
                                        tail,1) %in%
                           c('home1','home2','home3','car1','car2','car3','otherreg1',
                             'otherreg2','otherreg3','othernonreg1',
                             'othernonreg2','othernonreg3','otherhouse1',
                             'otherhouse2'))];

# Create dataset for consumer loans, melt it for compactation
loans <- melt(dat[,mget(c('id',loan.cols))],id.vars='id');

# Create loan types from var. names efficiently (use factors)
loans[,loan_type:=variable];
levels(loans$loan_type) <- sapply(strsplit(as.character(levels(loans$loan_type)), '_'),tail,1);

# Store order loan within loan type
loans[,loan_type_order:=as.integer(gsub('[a-zA-Z]+([0-9]+)','\\1',levels(loans$loan_type),))[loan_type]];

# Clear loan type from reference to loan order
levels(loans$loan_type) <- gsub('([a-zA-Z]+)[0-9]+','\\1',levels(loans$loan_type),);

# Clear var. names
levels(loans$variable) <- sub('_[^_]+$','',levels(loans$variable));

# Recast data.table
loans <- dcast(loans, id + loan_type + loan_type_order ~ variable, value.var=c('value'));

loan.cols.new <- colnames(loans)[!(colnames(loans) %in% c('id','loan_type','loan_type_order'))];

## Cleaning
# 0 is no loan(INAP) (Inappropiate?, inapplicable?)
# -6 is zero
# -9 is Not Available(NA)
# -8 is Don't Know(DK)

# Convert 0,-8 and -9 to NAs
loans[is.na(loans) | (loans==0) | (loans==-8) | (loans==-9),] <- NA_integer_;

## Eliminate garbage rows, for which all data is NA or 0
# If a all row elemments are INAP,NA,DK or 0, remove row
loans <- loans[!Reduce(`&`,lapply(mget(loan.cols.new),
                                  function(x) return(is.na(x) | (x%in%c(0,-8,-9))))),]

# Eliminate loans for which interest is not known or doesnt apply
# Don't eliminate them, it may be inferred from other loan data like *pay*, *amount*, *maturity*,...
#loans <- loans[(!is.na(int)) & (!(int %in% c(0,-8,-9))),];
loans[is.na(int) | (int %in% c(0,-8,-9)),int:=NA];
# Code interest rates. -6 is zero.
loans <- loans[int==-6,int:=0];

# Scale interest
loans[,int:=int/10];

# Code missing date references
loans[ystart %in% c(0,-9), ystart:=NA];
loans[mstart %in% c(0,-9), mstart:=NA];
loans[yend %in% c(0,-8,-9),yend:=NA];
loans[mend %in% c(0,-8,-9),mend:=NA];

# Convert date information to dates, remove previous info
loans[(!is.na(mstart)) & (!is.na(ystart)) & (mstart>0) & (ystart>0),
      start:=as.Date(paste(ystart,mstart,'1',sep='-'))];
loans[(!is.na(mend)) & (!is.na(yend)) & (mend>0) & (yend>0),
      end:=as.Date(paste(yend,mend,'1',sep='-'))];
# Still useful for dummies/FE
#loans[,c('mstart','ystart','mend','yend'):=NULL];

# Create newtila dummy for loans originated after tila reform
# TILA reform effective since April 1981
newtilad <- as.Date('1981-4-1');
#loans[!is.na(start),newtila:=ifelse(start>=newtilad,T,F)];
loans[!is.na(start),newtila:=pr.event.m(event=newtilad,date=start)];

# Some parts effective since April 1982
newtila2d <- as.Date('1982-4-1');
#loans[!is.na(start),newtila2:=ifelse(start>=newtila2d,T,F)];
loans[!is.na(start),newtila2:=pr.event.m(event=newtila2d,date=start)];

# Convert purpose codes to factors
loans[,pur_c:=as.factor(pur)];
loans[,pur_cr:=as.factor(pur)];

# Stop if not all purpose codes had a valid description
stopifnot(all(levels(loans$pur_c) %in% as.character(pur.names[,1])));
stopifnot(all(levels(loans$pur_cr) %in% as.character(pur.names.simple[[1]])));
stopifnot(all(levels(loans$pur_cr) %in% as.character(pur.names.reduced[[1]])));

# Map purpose codes to description
# Mapping method: map=setNames(values,names); map[names] -> values
levels(loans$pur_c) <- setNames(pur.names[,2],pur.names[,1])[levels(loans$pur_c)];
levels(loans$pur_cr) <- setNames(pur.names.simple[[2]],pur.names.simple[[1]])[levels(loans$pur_cr)];
#levels(loans$pur_cr) <- setNames(pur.names.reduced[[2]],pur.names.reduced[[1]])[levels(loans$pur_cr)];
loans[is.na(pur_c),pur_c:=as.factor('unknown')];
loans[is.na(pur_cr),pur_cr:=as.factor('unknown')];

# Acceptable pur.names.simple results by R^2
#summary(lm(int ~ as.factor(pur),data=loans))
#summary(lm(int ~ pur_c,data=loans))
#loans[,.(int=mean(int)),by=pur][order(int)]
#ggplot(loans,aes(x=start,y=int,color=pur_c)) + geom_point();
#ggplot(loans[,.(int=mean(int)),by=pur_c],
#       aes(x=pur_c,y=int,color=pur_c)) + geom_bar(stat='identity');

# Amount outstanding on loan. Zero is code for INAP.
loans[!is.na(outstand),outstand:=ifelse(outstand==0,NA,outstand)];

# Loan original maturity. -9=NA, due prior to current date. 0=INAP.
loans[(!is.na(maturity)) & (maturity %in% c(-9,0)),maturity:=NA];
# Convert maturity to years
loans[,maturity:=maturity/12];

loans[,maturity_c:=as.factor(maturity)];
loans[is.na(maturity_c),maturity_c:=as.factor('unknown')];

# Standard omaturities, proxy for non-negotaition?
standard.maturities <- c(36,48,24,12,60,18,120,30,42,84,6,180,72);
#standard.maturities <- c(36,48,24,12,60,18,120);

# Variable that proxies for contract popularity
loans[,maturity_p:=0];
loans[maturity %in% standard.maturities,maturity_p := as.numeric(.N),by=maturity]
# Variable that indicates if a contract is standarized
loans[,maturity_s:=F];
loans[maturity %in% standard.maturities,maturity_s := T]

loans[maturity %in% standard.maturities,
      maturity_cr:=as.factor(maturity)];
## proxy for negotaition but without much room for it
loans[(!maturity %in% standard.maturities) & maturity < 60,maturity_cr:='non-standard <5'];
loans[(!maturity %in% standard.maturities) & maturity > 60 & maturity <= 120,maturity_cr:='non-standard 5-10'];
## proxy for negotiation and room for negotiation
loans[(!maturity %in% standard.maturities) & maturity > 120 & maturity <= 180,maturity_cr:='non-standard 10-15'];
#loans[(!maturity %in% standard.maturities) & maturity > 60 & maturity <= 180,maturity_cr:='non-standard 5-15'];
loans[(!maturity %in% standard.maturities) & maturity > 180,maturity_cr:='non-standard >15'];

# Original amount borrowed. -9=NA. 0=INAP.
loans[(!is.na(amount)) & (amount %in% c(-9,0)),amount:=NA];

# Project cost. 0=INAP. Not said in codebook for this var but -8=DK and -9=NA
loans[(!is.na(pcost)) & (pcost %in% c(0,-8,-9)),pcost:=NA];

# Size of each payment. -6=NA. 0=INAP.
loans[(!is.na(pay)) & (pay %in% c(-6,0)),pay:=NA];

# Frequency of payments. -9=NA. 0=INAP.
loans[(!is.na(payfreq)) & (payfreq %in% c(-9,0)),payfreq:=NA];

loans[,payfreq:=as.factor(payfreq)];
loans[,ypayfreq:=payfreq];

# Stop if not all names for payment frequencies are available
stopifnot(all(levels(loans$payfreq) %in% payfreq.names[,1]))
stopifnot(all(levels(loans$ypayfreq) %in% as.character(payfreq.ypayfreq[,1])))

# Conver payment frequency codes into names
# Mapping method: map=setNames(values,names); map[names] -> values
levels(loans$payfreq) <- setNames(payfreq.names[,2],
                                  payfreq.names[,1])[levels(loans$payfreq)];
levels(loans$ypayfreq) <- setNames(payfreq.ypayfreq[,2],
                                  payfreq.ypayfreq[,1])[levels(loans$ypayfreq)];
loans[,ypayfreq:=as.numeric(levels(ypayfreq))[ypayfreq]];
#loans[,.(setNames(payfreq.ypayfreq[,2],payfreq.ypayfreq[,1])[payfreq])]

# Original number of payments. 0=NA/INAP.
loans[(!is.na(npay)) & (npay==0),npay:=NA];

# Number of payments left. 0=NA/INAP.
loans[(!is.na(npayleft)) & (npayleft==0),npayleft:=NA];

# Amount of balloon payment. 0=INAP.
loans[(!is.na(balloon)) & (balloon==0),balloon:=NA];

# Institution that originated the loan, source of loan. -9=NA. 0=INAP.
loans[(!is.na(insttype)) & (insttype %in% c(-9,0)),insttype:=NA];

# Replace code for loan insttype with loan insttype name
# insttype as factor 
loans[,insttype_c:=as.factor(insttype)];
loans[,insttype_cr:=as.factor(insttype)];

# Stop if not all names for loan insttypes are available
stopifnot(all(levels(loans$insttype_c) %in% insttype.names[,1]))
stopifnot(all(levels(loans$insttype_cr) %in% insttype.names.simple[[1]]));

# Conver codes for loan insttypes into names
# Mapping method: map=setNames(values,names); map[names] -> values
levels(loans$insttype_c) <- setNames(insttype.names[,2],
                                     insttype.names[,1])[levels(loans$insttype_c)];
levels(loans$insttype_cr) <- setNames(insttype.names.simple[[2]],
                                      insttype.names.simple[[1]])[levels(loans$insttype_cr)];
loans[is.na(insttype_c),insttype_c:=as.factor('unknown')];
loans[is.na(insttype_cr),insttype_cr:=as.factor('unknown')];

# Place where papers where filled. -8=DK. -9=NA. 0=INAP.
loans[(!is.na(place)) & (place %in% c(-8,-9,0)),place:=NA];

# Replace code for paper place with paper place name
# place as factor 
loans[,place:=as.factor(place)];

# Stop if not all names for places where paper filled are available
stopifnot(all(levels(loans$place) %in% place.names[,1]))

# Conver codes for places where paper filled into names
# Mapping method: map=setNames(values,names); map[names] -> values
levels(loans$place) <- setNames(place.names[,2],
                                place.names[,1])[levels(loans$place)];

# TILA enforcement agency for each institution type
# Check that all institutions are classified
stopifnot(loans[(!is.na(insttype)) & (! insttype %in% unlist(enforce.insttype)),.N]==0)

for (aa in names(enforce.insttype)) {
  loans[insttype %in% enforce.insttype[[aa]], enforce:=as.factor(aa)];
}
loans[is.na(enforce), enforce:=as.factor('unknown')];

# Business type for each institution type
# Check that all institutions are classified
stopifnot(loans[(!is.na(insttype)) & (! insttype %in% unlist(biztype.insttype)),.N]==0)

for (aa in names(biztype.insttype)) {
  loans[insttype %in% biztype.insttype[[aa]], biztype:=as.factor(aa)];
}
loans[is.na(biztype), biztype:=as.factor('other')];

# Create finco. Equal to one for finance company and zero from bank
loans[, finco:=F];
loans[biztype=='financial', finco:=T];

#loans[!is.na(insttype),.N]
#loans[!is.na(finco),.N]


# Implied interest rate is inconsistent for some loans, fishy
# Use annuity formula. Allow tolerance in results
# amount = (pay/int_n)*(1-(1+int_n)^(-npay)) + balloon_amount/(1+int)^maturity

## Assume APR. Convert APR to continous and apply annuity formula
loans[,report_incons:=amount/annuity.pv.c(int=int.conv(int,ypayfreq,Inf),
                                          ypayfreq=ypayfreq,npay=npay,pay=pay,
                                          balloon=replace.na(balloon))]

# If interest rate is 0, annuity formula not valid
loans[int==0,report_incons:=amount/(replace.na(pay*npay)+replace.na(balloon))];
# Fix NaNs and Infs
loans[!is.finite(report_incons),report_incons:=NA];

# This ratio goes from 0 to Inf and equilibrium(nobias) is 1. Overweights from 1 to Inf.
# Use transformation similar to sigmoid to bound the value between 0 and 1 and
# in which nobias(1) becomes 1/2. f(x)=x/(1+x)=1/(1+1/x). Then substract 1/2 to center around 0.
# This way overshooting and undershooting have the same impact.
loans[,report_incons:=(1/(1/report_incons+1)-1/2)*2];
## Proportion of inconsistent loans. It's lower if APR is assumed instead of APY.
#loans[,sum(abs(report_incons)>0.01,na.rm=T)/.N]
#loans[,hist(report_incons,breaks=100)]

# Tag loans with inconsistent interest rate and characteristics
loans[,incons:=T];
loans[abs(report_incons)<0.01,incons:=F];

#loans[is.finite(report_incons),mean(abs(report_incons)<0.01,na.rm=T)];

setkey(loans,id,loan_type,loan_type_order);
