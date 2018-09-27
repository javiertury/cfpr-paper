# Load required libraries
local({
  libs <- c('pander','xtable','ggplot2');
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});

## Load main file with data
if (!all(sapply(c('regdat'),exists))) {
  source('creditcost_mixdata.R',chdir=T);
}

reg.dates.names <- matrix(byrow=T,ncol=2,
  data=c('heoa_pel','HEOAPEL',
         'heoa_calc','HEOACalc',
         'hoepa08','HOEPA08',
         'mdia','MDIA',
         'mdia2','MDIA2',
          'compban','CompBan',
         'safe_eff','SAFE',
         'ftc_ad','AdRuleFTC'#,
         # 'hfsth_act','Helping Families Save Their Home Act'
         ));

regulation.sd <- NULL
for (dd in c(# 'heoa_act','heoa_rule',
             'heoa_pel','heoa_calc',
             'hoepa08','mdia','mdia2','compban','safe_eff',
             'ftc_ad'#,'ftc_ad_warning','hfsth_act'
             )) {
  regulation.sd <- rbindlist(list(regulation.sd,
                    data.table(Regulation=setNames(reg.dates.names[,2],reg.dates.names[,1])[dd],
                               `SD(Pr)`=regdat[-union(exrows2,regdat[(flex!='none') | (year < 2005),which=T]),
                               ][get(dd) < 1 & get(dd) > 0,sd(get(dd))],
                               `SD(Pr|Loan-type)`=regdat[-union(exrows2,regdat[(flex!='none') |
                                                            (year < 2005),which=T]),
                               ][get(dd) < 1 & get(dd) > 0,.(sd=sd(get(dd))),by=loan_type][,mean(sd)])));
}

#sink('output/regulation.sd.tex');
##pandoc.table(regulation.sd,round=4,split.cells=40);
#print(xtable(regulation.sd,digits=4,
#             caption=''),include.rownames=F,comment=F);
#sink();

local({
  output <- capture.output(print(xtable(regulation.sd,digits=4,
                           caption=''),include.rownames=F,comment=F));
  write(output[min(which(grepl('begin\\{tabular\\}',output))):min(which(grepl('end\\{tabular\\}',output)))],
        file='./output/regulation.sd.tex');
})

pdf('output/termstruct.pdf',width=8,height=4);
ggplot(treasury.m[date > as.Date('1975-01-01'),
                  .(termstruct=cor(int,log(maturity),use='pairwise.complete.obs')),by=date],
       aes(x=date,y=termstruct)) + geom_line() +
       xlab('Date') + ylab('Correlation between log(maturity) and interest');
dev.off();

pdf('output/termstruct_corp.pdf',width=8,height=4);
ggplot(hqm.m[date > as.Date('1975-01-01'),
                  .(termstruct=cor(int,log(maturity),use='pairwise.complete.obs')),by=date],
       aes(x=date,y=termstruct)) + geom_line() +
       xlab('Date') + ylab('Correlation between log(maturity) and interest');
dev.off();

pdf('output/incons_mortgages.pdf',width=5,height=4.5);
ggplot(loans[is.finite(report_incons) & loan_type %in% c('mortgage','proploan') &
             insttype_cr != 'government' & int != 0,
             .(payincludes=(!payincludes_c %in% c('nothing','unknown')),report_incons)],
       aes(x=report_incons,linetype=payincludes)) + geom_density() +
       theme_bw() + theme(legend.position=c(0.8,0.85),plot.title=element_text(hjust=0.5)) +
       labs(#title='Annuity and balloon inconsistency for mortgages',
            x='Consistency',y='Density',
            linetype='Payment includes\nother concepts');
dev.off();


pdf('output/incons_student.pdf',width=5,height=4.5);
ggplot(loans[is.finite(report_incons) & loan_type=='eduloan' & insttype_cr != 'government' & int != 0,
             .(report_incons,
               group=ifelse(amount>quantile(amount,0.5,na.rm=T) &
                      maturity<quantile(maturity,0.5,na.rm=T),'Short maturity &\nhigh amount',
                      ifelse(maturity<quantile(maturity,0.5,na.rm=T),
                             'Short maturity','None')))],
       aes(x=report_incons,linetype=group)) + geom_density() +
       theme_bw() + theme(legend.position=c(0.8,0.85),plot.title=element_text(hjust=0.5)) +
       labs(#title='Annuity and balloon inconsistency for student loans',
            x='Consistency',y='Density',linetype='Characteristics');
dev.off();

stats.vars <- list(`Interest Rate`='int',
                   `Implicit Int. Rate`='newapr',
                   `Annuity Bias`='report_incons',
                   `Payment Amount`='pay',
                   `Yearly Pay Freq.`='ypayfreq',
                   `Balloon Payment`='balloon_amount',
                   `Eq. Risk Free Rate`='rfapr',
                   `Eq. Corporate Rate`='riskapr',
                   #`Mkt. Risk Premium`='mktrp',
                   `Maturity`='maturity',
                   `Adj. Amount`='amount_cpi');
regdat.stats <- NULL
for (vv in names(stats.vars)) {
  rowstoexclude <- exrows2;
  if (stats.vars[[vv]]=='newapr') { 
    rowstoexclude <- union(exrows2,regdat[newintc_reasonable==F,which=T])
  }

  regdat.stats <- rbindlist(list(regdat.stats,
      regdat[-rowstoexclude,
                .(`Variable`=vv,
                  `Mean`=mean(get(stats.vars[[vv]]),na.rm=T),
                  `Quartile 1`=quantile(get(stats.vars[[vv]]),probs=0.25,na.rm=T),
                  `Median`=quantile(get(stats.vars[[vv]]),probs=0.5,na.rm=T),
                  `Quartile 3`=quantile(get(stats.vars[[vv]]),probs=0.75,na.rm=T),
                  `Min`=min(get(stats.vars[[vv]]),na.rm=T),
                  `Max`=max(get(stats.vars[[vv]]),na.rm=T),
                  `SD`=sd(get(stats.vars[[vv]]),na.rm=T)
      )]),
    use.names=T);
}

local({
  output <- capture.output(print(xtable(regdat.stats,digits=2,
                           caption=''),include.rownames=F,comment=F));
  write(output[min(which(grepl('begin\\{tabular\\}',output))):min(which(grepl('end\\{tabular\\}',output)))],
        file='./output/regdat.stats.tex');
})
