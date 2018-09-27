# Load required libraries
local({
  libs <- c('pander','xtable','ggplot2','Hmisc');
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});

## Load main file with data
if (!all(sapply(c('lar.sample','lar.stats'),exists))) {
  source('hmda_mixdata.R',chdir=T);
}

## Numeric Statistics for aggregated data
stats.vars <- list(`Approval Rate`='approved',
                   `Origination Rate`='originated',
                   `Coapplicant`='coapplicant',
                   `Adj. Income`='income_cpi',
                   `Adj. Amount`='amount_cpi');
table.lar.stats <- NULL
for (vv in names(stats.vars)) {
  table.lar.stats <- rbindlist(list(table.lar.stats,
      lar.stats[is.finite(get(stats.vars[[vv]])) & is.finite(wgt),
                .(`Variable`=vv,
                  `Mean`=wtd.mean(get(stats.vars[[vv]]),wgt),
                  `Quartile 1`=wtd.quantile(get(stats.vars[[vv]]),
                                             weights=wgt,probs=0.25),
                  `Median`=wtd.quantile(get(stats.vars[[vv]]),
                                        weights=wgt,probs=0.5),
                  `Quartile 3`=wtd.quantile(get(stats.vars[[vv]]),
                                             weights=wgt,probs=0.75),
                  `Min`=min(get(stats.vars[[vv]]),na.rm=T),
                  `Max`=max(get(stats.vars[[vv]]),na.rm=T),
                  `SD`=sqrt(wtd.var(get(stats.vars[[vv]]),
                                    weights=wgt))
      )]),
    use.names=T);
}

local({
  output <- capture.output(print(xtable(table.lar.stats,digits=2,
                           caption=''),include.rownames=F,comment=F));
  write(output[min(which(grepl('begin\\{tabular\\}',output))):min(which(grepl('end\\{tabular\\}',output)))],
        file='./output/table.lar.stats.tex');
})

## Numeric statistics for sample data
table.lar.sample <- NULL
for (vv in names(stats.vars)) {
  table.lar.sample <- rbindlist(list(table.lar.sample,
      lar.sample[is.finite(get(stats.vars[[vv]])),
                .(`Variable`=vv,
                  `Mean`=mean(get(stats.vars[[vv]])),
                  `Quartile 1`=quantile(get(stats.vars[[vv]]),probs=0.25),
                  `Median`=quantile(get(stats.vars[[vv]]),probs=0.5),
                  `Quartile 3`=quantile(get(stats.vars[[vv]]),probs=0.75),
                  `Min`=min(get(stats.vars[[vv]])),
                  `Max`=max(get(stats.vars[[vv]])),
                  `SD`=sd(get(stats.vars[[vv]]))
      )]),
    use.names=T);
}

local({
  output <- capture.output(print(xtable(table.lar.sample,digits=2,
                           caption=''),include.rownames=F,comment=F));
  write(output[min(which(grepl('begin\\{tabular\\}',output))):min(which(grepl('end\\{tabular\\}',output)))],
        file='./output/table.lar.sample.tex');
})
