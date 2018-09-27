local({
  libs <- c('data.table','fitdistrplus','fBasics','ghyp');
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});

## Some loan payments include other concepts apart from interest and
## principal payments. Re-estimate payment amount.
#
## Payment ratio, ratio of (amount - pv(balloon))/(pv(pay))
loans[,payr:=log((amount - annuity.pv.c(int=int.conv(int,ypayfreq,Inf),
                                        ypayfreq=ypayfreq,npay=npay,pay=0,
                                        balloon=replace.na(balloon_amount)))/
                  annuity.pv.c(int=int.conv(int,ypayfreq,Inf),
                               ypayfreq=ypayfreq,npay=npay,pay=pay))]
loans[!is.finite(payr), payr:=NA_real_];

# Implied interest rate is inconsistent for some loans, fishy
# Use annuity formula. Allow tolerance in results
# amount = (pay/int_n)*(1-(1+int_n)^(-npay)) + balloon_amount/(1+int)^maturity

## Assume APR. Convert APR to continous and apply annuity formula
## Pick 2 of these (maturity, npay, ypayfreq). Maturity has low resolution, discard.
loans[,report_incons:=amount/annuity.pv.c(int=int.conv(int,ypayfreq,Inf),
                                          ypayfreq=ypayfreq,npay=npay,pay=pay,
                                          balloon=replace.na(balloon_amount))];

# Fix NaNs and Infs
loans[!is.finite(report_incons),report_incons:=NA];

# *report_incons* ratio goes from 0 to Inf and equilibrium(nobias) is 1. Overweights
# from 1 to Inf. log transformation solves it, but doesn't have finite boundaries.
#loans[,report_incons2:=log(report_incons)];
# Use transformation similar to sigmoid to bound the value between 0
# and 1 and in which nobias(1) becomes 1/2. f(x)=x/(1+x)=1/(1+1/x). Then substract
# 1/2 to center around 0. This way overshooting and undershooting have the same impact.
loans[,report_incons:=(1/(1/report_incons+1)-1/2)*2];
#loans[,report_incons:=log(report_incons)];
 
## Function that transform report incons to original amount/PV(payments) ratio
incons2bias <- function(x) {1/(2/(x+1)-1)}
#incons2bias(1/3)
#incons2bias(0.2)
#incons2bias(0.05)

# Tag loans with inconsistent interest rate and characteristics
loans[,incons:=T];
loans[abs(report_incons)<0.2,incons:=F];
#loans[abs(report_incons)<0.05,incons:=F];
