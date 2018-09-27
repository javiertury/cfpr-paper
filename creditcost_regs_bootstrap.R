# Mail me any errors
options(error=function(...) {
  system(paste0('echo "Subject: Error on scf regression\n\n\'',geterrmessage(),'\'" | sendmail root'));
  recover(...);
});

# Load required libraries
local({
  libs <- c('parallel');
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});

## Load main file with data
if (!all(sapply(c('scf_design'),exists))) {
  source('creditcost_mixdata.R',chdir=T);
}

## Free up memory, multicore needs memory
rm(list=grep('^scf_design$',ls(),invert=T,value=T))
gc();

## multicore only picks up 2 cores, force to use all cores
options(mc.cores=detectCores());

vpsregs_folder <- './vpsregs/';

if (! dir.exists(vpsregs_folder)) {
  dir.create(vpsregs_folder);
}

scf_reg1 <- with(scf_design,
  svyglm(int ~ as.factor(id) +
               as.factor(loan_type) + as.factor(pur_c) +
               as.factor(year_c) +
               as.factor(insttype_cr) +
               as.factor(flex) +
               log_maturity + adjust_mortgage +
               log(amount_cpi) +
               rfapr +
               riskapr +
               mktrp +
               pmi_d + refinance_d + govprogram_d + borrowmore_d +
               payincludes_cr + newhome +
               hfsth_act + hfsth_act:main_mortgage +
               #mdia + mdia:any_mortgage + mdia:other_mortgage +
               #mdia:as.numeric(govprogram_d) + mdia:as.numeric(nonewhome) +
               ##as.numeric(govprogram_d):as.numeric(nonewhome) +
               ##mdia:as.numeric(govprogram_d):as.numeric(nonewhome) +
               # Can't estimate newcover_mdia alone
               mdia + mdia:any_mortgage + #newcover_mdia +
               mdia:newcover_mdia +
               hoepa08 + hoepa08:any_mortgage +
               hoepa08:as.numeric(main_mortgage) + hoepa08:as.numeric(newhome) +
               #as.numeric(main_mortgage):as.numeric(newhome) +
               hoepa08:as.numeric(main_mortgage):as.numeric(newhome) +
               ## Can't estimate respa08 alone because of collinearity
               #respa08 +
               respa08:as.numeric(any_mortgage) +
               respa08:as.numeric(govprogram_d) +
               #heoa_pel + pel + heoa_pel:pel + heoa_pel:no_flex + heoa_pel:pel:no_flex +
               heoa_pel + heoa_pel:eduloan + heoa_pel:no_flex + eduloan:no_flex +
               heoa_pel:eduloan:no_flex +
               mdia2 + mdia2:adjust_mortgage + mdia2:govprogram_d +
               adjust_mortgage:govprogram_d +
               mdia2:adjust_mortgage:govprogram_d +
               compban + compban:other_mortgage +
               safe_eff + safe_eff:any_mortgage +
               ftc_ad + ftc_mortgage + ftc_ad:ftc_mortgage +
               #heoa_calc + pel + eduinst + heoa_calc:pel + heoa_calc:eduinst +
               heoa_calc + eduinst + heoa_calc:eduloan + heoa_calc:eduinst,
         multicore=T));
saveRDS(scf_reg1, file=paste0(vpsregs_folder,'scf_reg1.rds'),compress='xz');
rm(scf_reg1);
gc();
system('echo "Subject: scf_reg1 finished" | sendmail root');

scf_reg2 <- with(scf_design,
  svyglm(report_incons ~ as.factor(id) +
               as.factor(loan_type) + as.factor(pur_c) +
               as.factor(year_c) +
               as.factor(insttype_cr) +
               as.factor(flex) +
               log_maturity + log_maturity:eduloan +
               log(amount_cpi) + log(amount_cpi):eduloan +
               adjust_mortgage +
               rfapr +
               riskapr +
               mktrp +
               pmi_d + refinance_d + govprogram_d + borrowmore_d +
               payincludes_cr + newhome +
               #### CCRA act
               #eduafter2008 +
               #heoa_rule + heoa_rule:education +
               hfsth_act + hfsth_act:main_mortgage +
               #mdia + mdia:any_mortgage + mdia:other_mortgage +
               #mdia:as.numeric(govprogram_d) + mdia:as.numeric(nonewhome) +
               ##as.numeric(govprogram_d):as.numeric(nonewhome) +
               ##mdia:as.numeric(govprogram_d):as.numeric(nonewhome) +
               # Can't estimate newcover_mdia alone
               mdia + mdia:any_mortgage + #newcover_mdia +
               mdia:newcover_mdia +
               hoepa08 + hoepa08:any_mortgage +
               hoepa08:as.numeric(main_mortgage) + hoepa08:as.numeric(newhome) +
               #as.numeric(main_mortgage):as.numeric(newhome) +
               hoepa08:as.numeric(main_mortgage):as.numeric(newhome) +
               ## Can't estimate respa08 alone because of collinearity
               #respa08 +
               respa08:as.numeric(any_mortgage) +
               respa08:as.numeric(govprogram_d) +
               #heoa_pel + pel + heoa_pel:pel + heoa_pel:no_flex + heoa_pel:pel:no_flex +
               heoa_pel + heoa_pel:eduloan +
               mdia2 + mdia2:adjust_mortgage + mdia2:govprogram_d +
               adjust_mortgage:govprogram_d +
               mdia2:adjust_mortgage:govprogram_d +
               compban + compban:other_mortgage +
               safe_eff + safe_eff:any_mortgage +
               #ftc_ad + ftc + ftc_ad:any_mortgage + ftc_ad:ftc +
               #ftc_ad:any_mortgage:ftc +
               ftc_ad + ftc_mortgage + ftc_ad:ftc_mortgage +
               #heoa_calc + pel + eduinst + heoa_calc:pel + heoa_calc:eduinst +
               heoa_calc + eduinst + heoa_calc:eduloan + heoa_calc:eduinst,
         multicore=T));
saveRDS(scf_reg2, file=paste0(vpsregs_folder,'scf_reg2.rds'),compress='xz');
rm(scf_reg2);
gc();
system('echo "Subject: scf_reg2 finished" | sendmail root');

scf_reg3 <- with(subset(scf_design,
                        is.finite(newintc_reasonable) & newintc_reasonable==T),
  svyglm(newapr ~ as.factor(id) +
               as.factor(loan_type) + as.factor(pur_c) +
               as.factor(year_c) +
               as.factor(insttype_cr) +
               as.factor(flex) +
               log_maturity + log_maturity:eduloan +
               log(amount_cpi) + log(amount_cpi):eduloan +
               adjust_mortgage +
               rfapr +
               riskapr +
               mktrp +
               pmi_d + refinance_d + govprogram_d + borrowmore_d +
               payincludes_cr + newhome +
               #### CCRA act
               #eduafter2008 +
               #heoa_rule + heoa_rule:education +
               hfsth_act + hfsth_act:main_mortgage +
               #mdia + mdia:any_mortgage + mdia:other_mortgage +
               #mdia:as.numeric(govprogram_d) + mdia:as.numeric(nonewhome) +
               ##as.numeric(govprogram_d):as.numeric(nonewhome) +
               ##mdia:as.numeric(govprogram_d):as.numeric(nonewhome) +
               # Can't estimate newcover_mdia alone
               mdia + mdia:any_mortgage + #newcover_mdia +
               mdia:newcover_mdia +
               hoepa08 + hoepa08:any_mortgage +
               hoepa08:as.numeric(main_mortgage) + hoepa08:as.numeric(newhome) +
               #as.numeric(main_mortgage):as.numeric(newhome) +
               hoepa08:as.numeric(main_mortgage):as.numeric(newhome) +
               ## Can't estimate respa08 alone because of collinearity
               #respa08 +
               respa08:as.numeric(any_mortgage) +
               respa08:as.numeric(govprogram_d) +
               #heoa_pel + pel + heoa_pel:pel + heoa_pel:no_flex + heoa_pel:pel:no_flex +
               heoa_pel + heoa_pel:eduloan +
               mdia2 + mdia2:adjust_mortgage + mdia2:govprogram_d +
               adjust_mortgage:govprogram_d +
               mdia2:adjust_mortgage:govprogram_d +
               compban + compban:other_mortgage +
               safe_eff + safe_eff:any_mortgage +
               #ftc_ad + ftc + ftc_ad:any_mortgage + ftc_ad:ftc +
               #ftc_ad:any_mortgage:ftc +
               ftc_ad + ftc_mortgage + ftc_ad:ftc_mortgage +
               #heoa_calc + pel + eduinst + heoa_calc:pel + heoa_calc:eduinst +
               #heoa_calc + eduinst + heoa_calc:eduloan + heoa_calc:eduinst,
               # Can't estimate eduinst interaction
               heoa_calc + eduinst + heoa_calc:eduloan,
         multicore=T));
saveRDS(scf_reg3, file=paste0(vpsregs_folder,'scf_reg3.rds'),compress='xz');
rm(scf_reg3);
gc();
system('echo "Subject: scf_reg3 finished" | sendmail root');

scf_reg4 <- with(scf_design,
  svyglm(I(abs(report_incons)) ~ as.factor(id) +
               as.factor(loan_type) + as.factor(pur_c) +
               as.factor(year_c) +
               as.factor(insttype_cr) +
               as.factor(flex) +
               log_maturity + log_maturity:eduloan +
               log(amount_cpi) + log(amount_cpi):eduloan +
               adjust_mortgage +
               rfapr +
               riskapr +
               mktrp +
               pmi_d + refinance_d + govprogram_d + borrowmore_d +
               payincludes_cr + newhome +
               #### CCRA act
               #eduafter2008 +
               #heoa_rule + heoa_rule:education +
               hfsth_act + hfsth_act:main_mortgage +
               #mdia + mdia:any_mortgage + mdia:other_mortgage +
               #mdia:as.numeric(govprogram_d) + mdia:as.numeric(nonewhome) +
               ##as.numeric(govprogram_d):as.numeric(nonewhome) +
               ##mdia:as.numeric(govprogram_d):as.numeric(nonewhome) +
               # Can't estimate newcover_mdia alone
               mdia + mdia:any_mortgage + #newcover_mdia +
               mdia:newcover_mdia +
               hoepa08 + hoepa08:any_mortgage +
               hoepa08:as.numeric(main_mortgage) + hoepa08:as.numeric(newhome) +
               #as.numeric(main_mortgage):as.numeric(newhome) +
               hoepa08:as.numeric(main_mortgage):as.numeric(newhome) +
               ## Can't estimate respa08 alone because of collinearity
               #respa08 +
               respa08:as.numeric(any_mortgage) +
               respa08:as.numeric(govprogram_d) +
               #heoa_pel + pel + heoa_pel:pel + heoa_pel:no_flex + heoa_pel:pel:no_flex +
               heoa_pel + heoa_pel:eduloan +
               mdia2 + mdia2:adjust_mortgage + mdia2:govprogram_d +
               adjust_mortgage:govprogram_d +
               mdia2:adjust_mortgage:govprogram_d +
               compban + compban:other_mortgage +
               safe_eff + safe_eff:any_mortgage +
               #ftc_ad + ftc + ftc_ad:any_mortgage + ftc_ad:ftc +
               #ftc_ad:any_mortgage:ftc +
               ftc_ad + ftc_mortgage + ftc_ad:ftc_mortgage +
               #heoa_calc + pel + eduinst + heoa_calc:pel + heoa_calc:eduinst +
               heoa_calc + eduinst + heoa_calc:eduloan + heoa_calc:eduinst,
         multicore=T));
saveRDS(scf_reg4, file=paste0(vpsregs_folder,'./scf_reg4.rds'),compress='xz');
rm(scf_reg4);
gc();
system('echo "Subject: scf_reg4 finished" | sendmail root');
