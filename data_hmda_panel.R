# Load required libraries
local({
  libs <- c('data.table');
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});

# Utility lo load flat data
if (!all(sapply(c('load_flat'),exists))) {
  source('./aux.R',chdir=T);
}

larpanel.vars <- data.table(
  var_name=c('rid',
             # 'msa',
             'agency',
             # 'agency_group',
             'rssd'),
  b_s=c(1,
        #11,
        16,
        #17,
        160),
  b_e=c(10,
        #15,
        16,
        #18,
        169),
  var_type=c('character',
             # 'integer',
             'integer',
             # 'character',
             'integer'));

larpanel.vars.2014 <- data.table(
  var_name=c('year',
             'rid',
             'agency',
             # 'parent_rid',
             'rssd'
             # 'parent_rssd', 'state_fips'
             ),
  b_s=c(1,5,15,
        #16
        320
        # 330,340
        ),
  b_e=c(4,14,15,
        #25
        329
        # 339,341
        ),
  var_type=c('integer','character','integer',
             # 'character',
             'integer'
             # 'integer','integer'
             ));

# Check that all of the bytes are present and none are duplicated
#stopifnot(larpanel.vars[,.(seq=seq(b_s,b_e)),
#          by=seq_len(nrow(larpanel.vars))][order(seq),seq]==c(1:larpanel.vars[,max(b_e)]));
#stopifnot(larpanel.vars.2014[,.(seq=seq(b_s,b_e)),
#          by=seq_len(nrow(larpanel.vars.2014))][order(seq),seq]==c(1:larpanel.vars.2014[,max(b_e)]));

# Byte length of variable
larpanel.vars[,b_l:=b_e-b_s+1];
larpanel.vars.2014[,b_l:=b_e-b_s+1];

larpanel.files <- list(
  list(file='u2004pan.public.dat',vars=larpanel.vars,
       format='fwf',compression='none',year=2004),
  list(file='PANEL.U.2005.DAT',vars=larpanel.vars,
       format='fwf',compression='none',year=2005),
  list(file='PANEL.U.2006.DAT',vars=larpanel.vars,
       format='fwf',compression='none',year=2006),
  list(file='panel.u.2007.dat',vars=larpanel.vars,
       format='fwf',compression='none',year=2007),
  list(file='panel.u.2008.dat',vars=larpanel.vars,
       format='fwf',compression='none',year=2008),
  list(file='2009_Ultimate_PUBLIC_Panel.dat',vars=larpanel.vars,
       format='fwf',compression='none',year=2009),
  list(file='Panel.ultimate.2010.dat',vars=larpanel.vars,
       format='fwf',compression='none',year=2010),
  list(file='Panel.ultimate.2011.dat',vars=larpanel.vars,
       format='fwf',compression='none',year=2011),
  list(file='Panel.ultimate.2012.dat',vars=larpanel.vars,
       format='fwf',compression='none',year=2012),
  list(file='Panel.ultimate.2013.dat',vars=larpanel.vars,
       format='fwf',compression='none',year=2013),
  list(file='2014HMDAReporterPanel.zip',vars=larpanel.vars.2014,
       format='fwf',compression='zip',year=2014)
  );
# Set files directory
larpanel.files <- Map(function(x) {
                        x$file <- paste0('./data/HMDAPanel/',x$file);
                        return(x);
                      },larpanel.files);

larpanel <- NULL;
local({
for (ff in larpanel.files) {
  newlarpanel <- load_flat(ff$file,vars=ff$vars,format=ff$format,
                           compression=ff$compression);
  if (! 'year' %in% colnames(newlarpanel)) {
    newlarpanel[,year:=ff$year];
  }

  larpanel <<- rbindlist(list(larpanel,newlarpanel),use.names=T);
}
});
