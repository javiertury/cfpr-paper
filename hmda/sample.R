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

hmda_sample_file <- '../data/HMDA/lar_sample.rds';

if (file.exists(hmda_sample_file)) {
  lar.sample <- readRDS(hmda_sample_file);
} else {
  # Batch size
  bsize <- 5000000

  # Sample percentage and seed
  samp.per <- 0.05;
  samp.seed <- 1234;

  lar.sample <- NULL;
  for (larfile in Filter(function(x) ((x$year >= 2005) & (x$year <= 2013)),
                         larfiles)) {
    print(larfile$file);
    larfile.n <- as.numeric(system(paste0('zcat "',larfile$file,'" | wc -l'),intern=T));

    for(bb in (1:ceiling(larfile.n/bsize))) {
      gc();
      
      # Read lar data
      lar <- load_lar(larfile$file, larfile$vars, skip=(bb-1)*bsize,
                      nrows=bsize,format=larfile$format);
      
      set.seed(samp.seed);
      lar.sample <- rbindlist(list(lar.sample,lar[sample(.N,.N*samp.per,replace=F),]),use.names=T);
    }
  }

  #lar.sample[,.N]
  #object.size(lar.sample)
  saveRDS(lar.sample,file=hmda_sample_file,compress='xz');
}
