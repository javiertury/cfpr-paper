# Load required libraries
local({
  if(! library(lodown,logical.return=T)) {
    install.packages('devtools');
    require(devtools);
    install_github('ajdamico/lodown',dependencies=T);
  }
  libs <- c('data.table','haven','plm','stargazer','ggplot2','psych',
            'lubridate','multiwayvcov','survey','mitools','lodown',
            'broom','huxtable');
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});

# Load scf_MIcombine2
if (!all(sapply(c('scf_MIcombine2'),exists))) {
  source('./aux_scf.R',chdir=T);
}

# Load reg_print
if (!all(sapply(c('get.vars.omit','clean.reg','write.reg'),exists))) {
  source('./aux_regprint.R',chdir=T);
}

vpsregs_folder <- './vpsregs/';

if (! all(file.exists(paste0(vpsregs_folder,'scf_reg',1:4,'.rds')))) {
  # Run regressions if they aren't run already
  source('./creditcost_regs_bootstrap.R',chdir=T);
}

if (!all(sapply(paste0('scf_reg',1:4,'_comb'),exists))) {
  for (ii in 1:4) {
    reg.name <- paste0('scf_reg',ii);
    reg.name.comb <- paste0(reg.name,'_comb');
    assign(reg.name, readRDS(paste0(vpsregs_folder,'scf_reg',ii,'.rds')));
    assign(reg.name.comb, scf_MIcombine2(get(reg.name),force.names=T));
    rm(list=c(reg.name));
  }
}

vars.omit <- get.vars.omit(list(scf_reg1_comb,scf_reg2_comb,scf_reg3_comb,scf_reg4_comb
                                ));

reg.output <- huxreg(Int=scf_reg1_comb,
                     `Imp Int`=scf_reg3_comb,
                     `Bias`=scf_reg2_comb,
                     `abs(Bias)`=scf_reg4_comb,
              #statistics=c('Implicates'='m'),
              statistics=character(0),
              stars=c(`***`=0.01,`**`=0.05,`*`=0.1),
              #note=c('Regressions. {stars}.')
              omit_coefs=vars.omit);

reg.output <- insert_row(reg.output,'Std. Errors','Bootstrap','Bootstrap','Bootstrap', 'Bootstrap',
                         after=nrow(reg.output)-1);
reg.output <- set_top_border(reg.output,nrow(reg.output)-1,1:ncol(reg.output),1);
reg.output <- set_align(reg.output,(nrow(reg.output)-1-1)+1,2:ncol(reg.output),'center');

latex_float(reg.output) <- 'H'
caption(reg.output) <- 'Regression'
position(reg.output) <- 'center'
font_size(reg.output) <- 11
# Set footnotes font size
reg.output <- set_font_size(reg.output,nrow(reg.output),1:ncol(reg.output),10)

reg.output <- order.interactions(reg.output,list(vars.main));
reg.output <- clean.reg(reg.output);

print(reg.output)
write.reg(reg.output,file='./output/regressions_bootstrap.tex');

reg.parts <- split_reg(reg.output,parts=2,copyHeader=T);
for (ii in seq(1,length(reg.parts))) {
  write.reg(reg.parts[[ii]],file=paste0('./output/regressions_bootstrap_part',ii,'.tex'));
}
