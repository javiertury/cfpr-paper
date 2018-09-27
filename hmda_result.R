# Load required libraries
local({
  libs <- c('data.table','lmtest','multiwayvcov',
            # 'Matrix','SparseM','speedglm',
            # 'broom','stargazer','survtmle',
            'huxtable');
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});

# Load data
if (!all(sapply(c('lar.sample','lar.stats'),exists))) {
  source('./hmda_mixdata.R',chdir=T);
}

# Load reg_print
if (!all(sapply(c('get.vars.omit','clean.reg','write.reg'),exists))) {
  source('./aux_regprint.R',chdir=T);
}

lar_reg <- glm(approved ~ as.factor(year) + as.factor(loan_type) + as.factor(pur) +
                        as.factor(charter) +
                        as.factor(occupancy) +
                        as.factor(sex) + as.factor(race) +
                        as.factor(ethnicity) + coapplicant +
                        as.factor(preapproval) + as.factor(govprogram_d) +
                        as.factor(state) +
                        as.factor(prop_type) + as.factor(lien) +
                        log(amount_cpi) + log(income_cpi) +
                        #log_app_change +
                        #log_app_state_change +
                        mdia*govprogram_d*new_mdia +
                        respa08*govprogram_d +
                        mdia2*govprogram_d +
                        hoepa08*main_mortgage*newhome +
                        compban*other_mortgage +
                        hfsth_act*main_mortgage +
                        ftc_ad*ftc_mortgage +
                        safe_eff,
              #sparse=T,
              family=binomial(link='logit'),
              data=lar.sample);

gc()
lar_reg_dc <- coeftest(lar_reg,vcov=cluster.vcov(lar_reg,~ year + state))

lar_reg2 <- lm(log(1+n) ~ as.factor(year) + as.factor(state) +
                   as.factor(sex) + as.factor(race) + as.factor(charter) +
                   as.factor(ethnicity) + as.factor(loan_type) +
                   as.factor(pur) + as.factor(occupancy) +
                   as.factor(prop_type) + as.factor(lien) +
                   govprogram_d + coapplicant +
                   preapproval +
                   log_income_cpi + log_amount_cpi +
                   log_population_race + log_population_sex +
                   mdia*govprogram_d*new_mdia +
                   respa08*govprogram_d +
                   mdia2*govprogram_d +
                   hoepa08*main_mortgage*newhome +
                   compban*other_mortgage +
                   hfsth_act*main_mortgage +
                   ftc_ad*ftc_mortgage +
                   safe_eff,
               weights=wgt,
               #data=lar.stats[(year >= 2005) & (year <= 2013) & (state %in% states.in),]);
               data=lar.stats);

#lar.sample[is.na(preapproval),.N]
#lar.sample[is.na(log(income_cpi)),.N]
#lar.stats[,.(n=sum(n,na.rm=T)),by=charter]
#lar.sample[,table(charter)]

#summary(lar_reg2)
#coeftest(lar_reg2,vcov=cluster.vcov(lar_reg2,~ year + state))
#grep('as.factor',names(coef(lar_reg)),value=T,invert=T)

vars.omit <- get.vars.omit(list(lar_reg,
                                lar_reg2));

lar_reg_out <- huxreg(Approved=lar_reg,
                      `log(Applications)`=lar_reg2,
                      #Approved=coeftest(lar_reg,vcov=cluster.vcov(lar_reg,~ year + state)),
                      Approved=lar_reg_dc,
                      `log(Applications)`=coeftest(lar_reg2,vcov=cluster.vcov(lar_reg2,~ year + state)),
                      statistics=c('N'='nobs'),
                      stars=c(`***`=0.01,`**`=0.05,`*`=0.1),
                      omit_coefs=vars.omit);
lar_reg_out <- set_top_border(lar_reg_out,nrow(lar_reg_out)-1,1:ncol(lar_reg_out),1);
lar_reg_out <- set_bottom_border(lar_reg_out,nrow(lar_reg_out)-1,1:ncol(lar_reg_out),0);
lar_reg_out <- insert_row(lar_reg_out,'Model','Logit','WLS',
                          'Logit',
                          'WLS',
                         after=nrow(lar_reg_out)-1);
lar_reg_out <- set_top_border(lar_reg_out,nrow(lar_reg_out)-1,1:ncol(lar_reg_out),0);
lar_reg_out <- set_bottom_border(lar_reg_out,nrow(lar_reg_out)-1,1:ncol(lar_reg_out),0);
lar_reg_out <- insert_row(lar_reg_out,'Std. Errors','Simple','Simple',
                          'Double Cluster',
                          'Double Cluster',
                         after=nrow(lar_reg_out)-1);
lar_reg_out <- set_top_border(lar_reg_out,nrow(lar_reg_out)-1,1:ncol(lar_reg_out),0);
lar_reg_out <- set_bottom_border(lar_reg_out,nrow(lar_reg_out)-1,1:ncol(lar_reg_out),1);
lar_reg_out <- set_align(lar_reg_out,(nrow(lar_reg_out)-1-3)+1:3,2:ncol(lar_reg_out),'center');

lar_reg_out <- order.interactions(lar_reg_out,list(vars.main));
lar_reg_out <- clean.reg(lar_reg_out);

print(lar_reg_out)
write.reg(lar_reg_out,file='./output/hmda_regs.tex');
