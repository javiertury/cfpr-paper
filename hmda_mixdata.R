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

## Load hmda sample file
#lar.sample <- readRDS('./data/HMDA/lar_sample.rds');
if (!all(sapply(c('lar.sample'),exists))) {
  source('hmda/sample.R',chdir=T);
}

# Load auxiliary tools
if (!all(sapply(c('pr.event.y','pr.event.d2y','spline.y2d','spline.m2d'),exists))) {
  source('aux.R',chdir=T);
}

# Load dates of interest
if (!all(sapply(c('reg.dates'),exists))) source('study_dates.R',chdir=T);

# Load hmda stats file
#lar.stats <- readRDS('./data/HMDA/lar_stats.rds');
if (!all(sapply(c('lar.stats'),exists))) {
  source('hmda/stats.R',chdir=T);
}

# Load census
if (!all(sapply(c('census'),exists))) source('./data_census.R',chdir=T);

# Load census
if (!all(sapply(c('cpi.y.f'),exists))) source('./data_cpi.R',chdir=T);

# Load information about aggregate loan volumes
if (!all(sapply(c('cfpb.vol','cfpb.vol.inc'),exists))) source('loan_aggregate/cfpb.orig.R',chdir=T);

states.in <- c(01,02,04,05,06,08:13,15:42,44:51,53:56)
years.in <- c(2005:2013)
years.interp <- c(2004:2014)

## Remove non-state territories from sample
lar.sample <- lar.sample[state %in% states.in,];

lar2census.sex <- matrix(byrow=T,ncol=2,
                         data=c('male',  'Male',
                                'female','Female',
                                'all',   'Not applicable',
                                'all',   'Unknown'));
lar2census.ethnicity <- matrix(byrow=T,ncol=2,
                               data=c('hispanic',   'Hispanic',
                                      'nonhispanic','Not Hispanic',
                                      'all',        'Not applicable',
                                      'all',        'Unknown'));
lar2census.race <- matrix(byrow=T,ncol=2,
                          data=c('white',   'White',
                                 'black',   'Black',
                                 'asian',   'Asian',
                                 'native',  'American Indian',
                                 'islander','Islander',
                                 'all',     'Not applicable',
                                 'all',     'Unknown'));
stopifnot(all(levels(lar.sample$sex) %in% lar2census.sex[,2]))
stopifnot(all(levels(lar.sample$ethnicity) %in% lar2census.ethnicity[,2]))
stopifnot(all(levels(lar.sample$race) %in% lar2census.race[,2]))
stopifnot(all(levels(lar.stats$sex) %in% lar2census.sex[,2]))
stopifnot(all(levels(lar.stats$ethnicity) %in% lar2census.ethnicity[,2]))
stopifnot(all(levels(lar.stats$race) %in% lar2census.race[,2]))

stopifnot(all(lar2census.sex[,1] %in% levels(census$sex)))
stopifnot(all(lar2census.ethnicity[,1] %in% levels(census$ethnicity)))
stopifnot(all(lar2census.race[,1] %in% levels(census$race)))

lar.stats[,census_sex:=as.factor(setNames(lar2census.sex[,1],lar2census.sex[,2])[sex])];
lar.stats[,census_race:=as.factor(setNames(lar2census.race[,1],lar2census.race[,2])[race])];
lar.stats[,census_ethnicity:=as.factor(setNames(lar2census.ethnicity[,1],
                                               lar2census.ethnicity[,2])[ethnicity])];
lar.sample[,census_sex:=as.factor(setNames(lar2census.sex[,1],lar2census.sex[,2])[sex])];
lar.sample[,census_race:=as.factor(setNames(lar2census.race[,1],lar2census.race[,2])[race])];
lar.sample[,census_ethnicity:=as.factor(setNames(lar2census.ethnicity[,1],
                                               lar2census.ethnicity[,2])[ethnicity])];

gc();

for (ee in names(reg.dates)) {
  # Create event date
  event.d <- reg.dates[[ee]]
  # Assign probability of event happening for each loan in that age cohort
  lar.sample[,c(ee):=pr.event.y(event=event.d,date=as.Date(ISOdate(year,1,1)))];
  lar.stats[,c(ee):=pr.event.y(event=event.d,date=as.Date(ISOdate(year,1,1)))];
}

## For population changes

print('calculating log population according to sex');
lar.stats <- merge(lar.stats,
  census[ethnicity=='all' & race=='all',
         .(log_population_sex=log(1+sum(population,na.rm=T))),
         by=.(year,state,census_sex=sex)],
  by=c('year','state','census_sex'),all.x=T);
                  
print('calculating log population according to ethnicity and race');
lar.stats <- merge(lar.stats,
  census[sex=='all',
          .(log_population_race=log(1+sum(population,na.rm=T))),
         by=.(year,state,census_ethnicity=ethnicity,
              census_race=race)],
  by=c('year','state','census_race','census_ethnicity'),all.x=T);

## Create weigths for lar.stats
lar.catvars <- c('year','sex','race','ethnicity','loan_type','pur',
                 'relincomecat','charter',
                 'occupancy','state','prop_type','lien','govprogram_d');
lar.stats[,wgt:=sum(n,na.rm=T),
          by=mget(setdiff(lar.catvars,'year'))];

#object.size(lar.sample)
#lar.sample[,.N]
#colnames(lar.sample)
#str(lar.sample)
#nrow(unique(lar.sample,by=c('year','rid2','sex','race','ethnicity',
#                        'loan_type','pur','occupancy','state',
#                        'prop_type','lien','agency')))

lar.sample[,main_mortgage:=(occupancy=='Owner-occupied as principal dwelling')];
lar.stats[,main_mortgage:=(occupancy=='Owner-occupied as principal dwelling')];
lar.sample[,other_mortgage:=(occupancy=='Not owner-occupied as principal dwelling')];
lar.stats[,other_mortgage:=(occupancy=='Not owner-occupied as principal dwelling')];
lar.sample[,newhome:=(pur=='Home purchase')];
lar.stats[,newhome:=(pur=='Home purchase')];
lar.sample[,new_mdia:=(other_mortgage | (!newhome))];
lar.stats[,new_mdia:=(other_mortgage | (!newhome))];

lar.sample[,ftc_mortgage:=F];
lar.sample[grepl('non-deposit|edge or agreement|holding|broker|stock ownership plan|unknown',charter),ftc_mortgage:=T];
lar.stats[,ftc_mortgage:=F];
lar.stats[grepl('non-deposit|edge or agreement|holding|broker|stock ownership plan|unknown',charter),ftc_mortgage:=T];

lar.sample[,income_cpi:=income*100/cpi.y.f(year,basket='all')];
lar.sample[,amount_cpi:=amount*100/cpi.y.f(year,basket='all')];

set.seed(1234)
chosen.rows <- sample(nrow(lar.sample),0.3*nrow(lar.sample));

## Work with these data
lar.sample <- lar.sample[chosen.rows,];
lar.stats <- lar.stats[(year >= 2005) & (year <= 2013) & (state %in% states.in),];
## For stats use only complete time series
## Wrong, this approach doesn't include other variables 
#lar.stats <- lar.stats[,if(all(2005:2013 %in% year)) .SD, by=mget(setdiff(lar.catvars,'year'))]
## Correct approach
lar.stats[,complete:=all(2005:2013 %in% year), by=mget(setdiff(lar.catvars,'year'))]
lar.stats <- lar.stats[complete==T,];

gc()

#object.size(lar.sample)
#object.size(lar.stats)
#lar.stats[,.N]
#lar.sample[,.N]
