# Load required libraries
local({
  libs <- c('data.table','broom','huxtable','lmtest');
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});

# Load main regressions
if (!all(sapply(paste0('reg.',1:4),exists))) {
  source('./creditcost_regs_simple.R',chdir=T);
}

# Load reg_print
if (!all(sapply(c('get.vars.omit','clean.reg','write.reg'),exists))) {
  source('./aux_regprint.R',chdir=T);
}

vars.omit <- get.vars.omit(list(reg1,reg2,reg3,reg4));

reg.output <- huxreg(Int=coeftest(reg1,vcov=vcov(reg1)*5),
                     `Imp Int`=coeftest(reg2,vcov=vcov(reg2)*5),
                     `Bias`=coeftest(reg3,vcov=vcov(reg3)*5),
                     `abs(Bias)`=coeftest(reg4,vcov=vcov(reg4)*5),
              #statistics=c('Implicates'='m'),
              statistics=character(0),
              stars=c(`***`=0.01,`**`=0.05,`*`=0.1),
              #note=c('Regressions. {stars}.')
              omit_coefs=vars.omit);

reg.output <- insert_row(reg.output,'N',
                         as.integer(round(sapply(list(reg1,reg2,reg3,reg4),nobs)/5)),
                         after=nrow(reg.output)-1);
reg.output <- set_number_format(reg.output,nrow(reg.output)-1,2:ncol(reg.output),0);
reg.output <- set_top_border(reg.output,nrow(reg.output)-1,1:ncol(reg.output),1);
reg.output <- set_bottom_border(reg.output,nrow(reg.output)-1,1:ncol(reg.output),0);
reg.output <- insert_row(reg.output,'Std. Errors','Simple','Simple','Simple','Simple',
                         after=nrow(reg.output)-1);
reg.output <- set_top_border(reg.output,nrow(reg.output)-1,1:ncol(reg.output),0);
reg.output <- set_bottom_border(reg.output,nrow(reg.output)-1,1:ncol(reg.output),1);
reg.output <- set_align(reg.output,(nrow(reg.output)-1-2)+1:2,2:ncol(reg.output),'center');

latex_float(reg.output) <- 'H'
caption(reg.output) <- 'Regression'
position(reg.output) <- 'center'
font_size(reg.output) <- 11
# Set footnotes font size
reg.output <- set_font_size(reg.output,nrow(reg.output),1:ncol(reg.output),10)

reg.output <- order.interactions(reg.output,list(vars.main));
reg.output <- clean.reg(reg.output);

write.reg(reg.output,file='./output/regressions_simple.tex');

reg.parts <- split_reg(reg.output,parts=2,copyHeader=T);
for (ii in seq(1,length(reg.parts))) {
  write.reg(reg.parts[[ii]],file=paste0('./output/regressions_simple_part',ii,'.tex'));
}
