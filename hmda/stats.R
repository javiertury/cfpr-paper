# Load required libraries
local({
  libs <- c('data.table','readr'#,'stringi','iotools'
            );
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});

if (!all(sapply(c('lar.vars','load_lar','larfiles'),exists))) {
  source('./load.R',chdir=T);
}

hmda_stats_file <- '../data/HMDA/lar_stats.rds';

if (file.exists(hmda_stats_file)) {
  lar.stats <- readRDS(hmda_stats_file);
} else {
  # Batch size
  bsize <- 5*10^6;

  lar.catvars <- c('year','sex','race','ethnicity','state',
                   'occupancy','pur','charter',
                   'loan_type','relincomecat',
                   'prop_type','lien','govprogram_d');
  lar.logvars <- c('amount','income','income_cpi','amount_cpi');
  lar.numvars <- c(lar.logvars,'preapproval','hoepa','coapplicant',
                   'approved','originated');

  lar.stats <- NULL;
  for (larfile in larfiles) {
    print(larfile$file);
    larfile.n <- as.numeric(system(paste0('zcat "',larfile$file,'" | wc -l'),intern=T));

    for(bb in (1:ceiling(larfile.n/bsize))) {
      gc();
      
      # Read lar data
      lar <- load_lar(larfile$file,larfile$vars,skip=(bb-1)*bsize,nrows=bsize,
                      format=larfile$format);
      
      lar.stats <- rbindlist(list(
          lar.stats,
          lar[,c(setNames(lapply(mget(lar.logvars),
                                 function(x) mean(Filter(is.finite,log(x)),na.rm=T)),
                          paste0('log_',lar.logvars)),
                 list(n=.N),
                 lapply(mget(lar.numvars),mean,na.rm=T)),
              by=mget(lar.catvars)]),
        use.names=T);
    }

    # So far, stats are aggregated by year and bsize. Reaggregate stats by year.
    lar.stats <- rbindlist(list(
        lar.stats[year != larfile$year,],
        lar.stats[year == larfile$year,
                  c(lapply(mget(paste0('log_',lar.logvars)),
                           function(x) sum(Filter(is.finite,log(x)*n),na.rm=T)/sum(n,na.rm=T)),
                    list(n=sum(n,na.rm=T)),
                    lapply(mget(lar.numvars),
                           function(x) sum(Filter(is.finite,x*n),na.rm=T)/sum(n,na.rm=T))),
            by=mget(lar.catvars)]),
      use.names=T)
  }

  #lar.stats
  #object.size(lar.stats)
  saveRDS(lar.stats,file=hmda_stats_file,compress='xz');
}
