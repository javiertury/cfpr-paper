# Load required libraries
local({
  libs <- c('parallel','broom','huxtable','lmtest');
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

reg1 <- lm(int ~ as.factor(id) +
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
    hfsth_act*main_mortgage +
    #mdia*govprogram_d +
    #mdia*govprogram_d*newcover_mdia +
    #mdia*any_mortgage + mdia*other_mortgage + mdia*govprogram_d*nonewhome +
    mdia*any_mortgage + mdia*newcover_mdia +
    hoepa08*any_mortgage +
    hoepa08*main_mortgage*newhome +
    respa08*any_mortgage + respa08*govprogram_d +
    #heoa_pel*pel*no_flex +
    heoa_pel*eduloan*no_flex +
    mdia2*adjust_mortgage*govprogram_d +
    compban*other_mortgage +
    safe_eff*any_mortgage +
    ftc_ad*ftc_mortgage +
    #heoa_calc*pel*eduinst +
    heoa_calc*eduloan + heoa_calc*eduinst,
  #data=regdat[-exrows,],
  data=regdat[-exrows2,],
  weights=wgt);

reg2 <- lm(newapr ~ as.factor(id) +
    as.factor(loan_type) + as.factor(pur_c) +
    as.factor(year_c) +
    as.factor(insttype_cr) +
    as.factor(flex) +
    log_maturity*eduloan + adjust_mortgage +
    log(amount_cpi)*eduloan +
    rfapr +
    riskapr +
    mktrp +
    pmi_d + refinance_d + govprogram_d + borrowmore_d +
    payincludes_cr + newhome +
    hfsth_act*main_mortgage +
    #mdia*any_mortgage + mdia*other_mortgage + mdia*govprogram_d*nonewhome +
    mdia*any_mortgage + mdia*newcover_mdia +
    hoepa08*any_mortgage +
    hoepa08*main_mortgage*newhome +
    respa08*any_mortgage + respa08*govprogram_d +
    heoa_pel*eduloan*no_flex +
    mdia2*govprogram_d*adjust_mortgage +
    compban*other_mortgage +
    safe_eff*any_mortgage +
    ftc_ad*ftc_mortgage +
    #heoa_calc*eduloan*eduinst +
    heoa_calc*eduloan + heoa_calc*eduinst,
  #data=regdat[-exrows,],
  #data=regdat[-union(exrows,regdat[newintc_reasonable==F,which=T]),],
  #data=regdat[-exrows2,],
  data=regdat[-union(exrows2,regdat[newintc_reasonable==F,which=T]),],
  weights=wgt);

reg3 <- lm(report_incons ~ as.factor(id) +
    as.factor(loan_type) + as.factor(pur_c) +
    as.factor(year_c) +
    as.factor(insttype_cr) +
    as.factor(flex) +
    log_maturity*eduloan + mktrp + adjust_mortgage +
    log(amount_cpi)*eduloan +
    rfapr +
    riskapr +
    pmi_d + refinance_d + govprogram_d + borrowmore_d +
    payincludes_cr + newhome +
    hfsth_act*main_mortgage +
    #mdia*any_mortgage + mdia*other_mortgage + mdia*govprogram_d*nonewhome +
    mdia*any_mortgage + mdia*newcover_mdia +
    hoepa08*any_mortgage +
    hoepa08*main_mortgage*newhome +
    respa08*any_mortgage + respa08*govprogram_d +
    heoa_pel*eduloan*no_flex +
    mdia2*adjust_mortgage*govprogram_d +
    compban*other_mortgage +
    safe_eff*any_mortgage +
    ftc_ad*ftc_mortgage +
    #heoa_calc*eduloan*eduinst +
    heoa_calc*eduloan + heoa_calc*eduinst,
  #data=regdat[-exrows,],
  data=regdat[-exrows2,],
  weights=wgt);

reg4 <- lm(I(abs(report_incons)) ~ as.factor(id) +
    as.factor(loan_type) + as.factor(pur_c) +
    as.factor(year_c) +
    as.factor(insttype_cr) +
    as.factor(flex) +
    log_maturity*eduloan + mktrp + adjust_mortgage +
    log(amount_cpi)*eduloan +
    rfapr +
    riskapr +
    pmi_d + refinance_d + govprogram_d + borrowmore_d +
    payincludes_cr + newhome +
    hfsth_act*main_mortgage +
    #mdia*any_mortgage + mdia*other_mortgage + mdia*govprogram_d*nonewhome +
    mdia*any_mortgage + mdia*newcover_mdia +
    hoepa08*any_mortgage +
    hoepa08*main_mortgage*newhome +
    respa08*any_mortgage + respa08*govprogram_d +
    heoa_pel*eduloan*no_flex +
    mdia2*adjust_mortgage*govprogram_d +
    compban*other_mortgage +
    safe_eff*any_mortgage +
    ftc_ad*ftc_mortgage +
    #heoa_calc*eduloan*eduinst +
    heoa_calc*eduloan + heoa_calc*eduinst,
  #data=regdat[-exrows,],
  data=regdat[-exrows2,],
  weights=wgt);
