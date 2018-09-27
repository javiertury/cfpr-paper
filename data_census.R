# Load required libraries
local({
  libs <- c('data.table','readr','stringi','iotools');
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});
  
## Many bytes per line: these are the usable ones
## variable name, byte pos. start, byte pos. end, variable type
census.vars <- data.table(
  var_name=c('year','msa','state','county','tract','principal_city',
             'small_county','split_tract','demographic_data','population_type',
             'msa_median_family_income','msa_median_household_income',
             'tract2msa_median_family_income_percentage',
             'hud_msa_median_family_income','population','families',
             'households','female_population','male_population',
             'minority_population','minority_percentage',
             'total_hispanic_nonhispanic','total_hispanic','total_nonhispanic',
             'total_nonhispanic_one_race','total_nonhispanic_white',
             'total_nonhispanic_black','total_nonhispanic_native',
             'total_nonhispanic_asian','total_nonhispanic_islander',
             'total_nonhispanic_other','total_nonhispanic_two_races',
             'total_nonhispanic_two_races',
             'total_nonhispanic_white_black','total_nonhispanic_white_native',
             'total_nonhispanic_white_asian',
             'total_nonhispanic_white_islander',
             'total_nonhispanic_white_other','total_nonhispanic_black_native',
             # 'total_omb_nonhispanic_black','total_omb_nonhispanic_native',
             # 'total_omb_nonhispanic_asian','total_omb_nonhispanic_islander',
             # 'total_omb_nonhispanic_other','total_omb_nonhispanic_two_races',
             'total_hispanic_population','total_hispanic_white',
             'total_hispanic_black','total_hispanic_native',
             'total_hispanic_asian','total_hispanic_islander',
             'total_hispanic_other','total_hispanic_two_races',
             'total_population','total_population_one_race',
             'total_population_white','total_population_black',
             'total_population_native','total_population_asian',
             'total_population_islander','total_population_other',
             'total_population_two_races','total_population_two_races',
             'total_population_white_black','total_population_white_native',
             'total_population_white_asian','total_population_white_islander',
             'total_population_white_other','total_population_black_native'#,
             # 'all_all_income_population','all_all_income_0_10000','all_all_income_10000_15000',
             # 'all_all_income_15000_20000','all_all_income_20000_25000','all_all_income_25000_30000',
             # 'all_all_income_30000_35000','all_all_income_35000_40000','all_all_income_40000_45000',
             # 'all_all_income_45000_50000','all_all_income_50000_60000','all_all_income_60000_75000',
             # 'all_all_income_75000_100000','all_all_income_100000_125000','all_all_income_125000_150000',
             # 'all_all_income_150000_200000','all_all_income_200000_Inf','all_all_median_income',
             # 'all_white_income_population','all_white_income_0_10000','all_white_income_10000_15000',
             # 'all_white_income_15000_20000','all_white_income_20000_25000','all_white_income_25000_30000',
             # 'all_white_income_30000_35000','all_white_income_35000_40000','all_white_income_40000_45000',
             # 'all_white_income_45000_50000','all_white_income_50000_60000','all_white_income_60000_75000',
             # 'all_white_income_75000_100000','all_white_income_100000_125000','all_white_income_125000_150000',
             # 'all_white_income_150000_200000','all_white_income_200000_Inf','all_black_income_population',
             # 'all_black_income_0_10000','all_black_income_10000_15000','all_black_income_15000_20000',
             # 'all_black_income_20000_25000','all_black_income_25000_30000','all_black_income_30000_35000',
             # 'all_black_income_35000_40000','all_black_income_40000_45000','all_black_income_45000_50000',
             # 'all_black_income_50000_60000','all_black_income_60000_75000','all_black_income_75000_100000',
             # 'all_black_income_100000_125000','all_black_income_125000_150000','all_black_income_150000_200000',
             # 'all_black_income_200000_Inf','all_native_income_population','all_native_income_0_10000',
             # 'all_native_income_10000_15000','all_native_income_15000_20000','all_native_income_20000_25000',
             # 'all_native_income_25000_30000','all_native_income_30000_35000','all_native_income_35000_40000',
             # 'all_native_income_40000_45000','all_native_income_45000_50000','all_native_income_50000_60000',
             # 'all_native_income_60000_75000','all_native_income_75000_100000','all_native_income_100000_125000',
             # 'all_native_income_125000_150000','all_native_income_150000_200000','all_native_income_200000_Inf',
             # 'all_asian_income_population','all_asian_income_0_10000','all_asian_income_10000_15000',
             # 'all_asian_income_15000_20000','all_asian_income_20000_25000','all_asian_income_25000_30000',
             # 'all_asian_income_30000_35000','all_asian_income_35000_40000','all_asian_income_40000_45000',
             # 'all_asian_income_45000_50000','all_asian_income_50000_60000','all_asian_income_60000_75000',
             # 'all_asian_income_75000_100000','all_asian_income_100000_125000','all_asian_income_125000_150000',
             # 'all_asian_income_150000_200000','all_asian_income_200000_Inf','all_islander_income_population',
             # 'all_islander_income_0_10000','all_islander_income_10000_15000','all_islander_income_15000_20000',
             # 'all_islander_income_20000_25000','all_islander_income_25000_30000',
             # 'all_islander_income_30000_35000','all_islander_income_35000_40000',
             # 'all_islander_income_40000_45000','all_islander_income_45000_50000',
             # 'all_islander_income_50000_60000','all_islander_income_60000_75000',
             # 'all_islander_income_75000_100000','all_islander_income_100000_125000',
             # 'all_islander_income_125000_150000','all_islander_income_150000_200000',
             # 'all_islander_income_200000_Inf','all_other_income_population',
             # 'all_other_income_0_10000','all_other_income_10000_15000','all_other_income_15000_20000',
             # 'all_other_income_20000_25000','all_other_income_25000_30000','all_other_income_30000_35000',
             # 'all_other_income_35000_40000','all_other_income_40000_45000','all_other_income_45000_50000',
             # 'all_other_income_50000_60000','all_other_income_60000_75000','all_other_income_75000_100000',
             # 'all_other_income_100000_125000','all_other_income_125000_150000','all_other_income_150000_200000',
             # 'all_other_income_200000_Inf','all_tworaces_income_population','all_tworaces_income_0_10000',
             # 'all_tworaces_income_10000_15000','all_tworaces_income_15000_20000',
             # 'all_tworaces_income_20000_25000','all_tworaces_income_25000_30000',
             # 'all_tworaces_income_30000_35000','all_tworaces_income_35000_40000',
             # 'all_tworaces_income_40000_45000','all_tworaces_income_45000_50000',
             # 'all_tworaces_income_50000_60000','all_tworaces_income_60000_75000',
             # 'all_tworaces_income_75000_100000','all_tworaces_income_100000_125000',
             # 'all_tworaces_income_125000_150000','all_tworaces_income_150000_200000',
             # 'all_tworaces_income_200000_Inf','hispanic_all_income_population',
             # 'hispanic_all_income_0_10000','hispanic_all_income_10000_15000',
             # 'hispanic_all_income_15000_20000','hispanic_all_income_20000_25000',
             # 'hispanic_all_income_25000_30000','hispanic_all_income_30000_35000',
             # 'hispanic_all_income_35000_40000','hispanic_all_income_40000_45000',
             # 'hispanic_all_income_45000_50000','hispanic_all_income_50000_60000',
             # 'hispanic_all_income_60000_75000','hispanic_all_income_75000_100000',
             # 'hispanic_all_income_100000_125000','hispanic_all_income_125000_150000',
             # 'hispanic_all_income_150000_200000','hispanic_all_income_200000_Inf',
             # 'nonhispanic_white_income_population','nonhispanic_white_income_0_10000',
             # 'nonhispanic_white_income_10000_15000','nonhispanic_white_income_15000_20000',
             # 'nonhispanic_white_income_20000_25000','nonhispanic_white_income_25000_30000',
             # 'nonhispanic_white_income_30000_35000','nonhispanic_white_income_35000_40000',
             # 'nonhispanic_white_income_40000_45000','nonhispanic_white_income_45000_50000',
             # 'nonhispanic_white_income_50000_60000','nonhispanic_white_income_60000_75000',
             # 'nonhispanic_white_income_75000_100000','nonhispanic_white_income_100000_125000',
             # 'nonhispanic_white_income_125000_150000','nonhispanic_white_income_150000_200000',
             # 'nonhispanic_white_income_200000_Inf'
             ),
  b_s=c(1,5,10,12,15,21,22,23,24,25,26,34,42,48,56,64,72,80,88,96,104,110,118,
        126,134,142,150,158,166,174,182,190,198,206,214,222,230,238,246,
        #254,262,270,278,286,294,
        302,310,318,326,334,342,350,358,366,374,382,390,398,406,414,422,
        430,438,446,454,462,470,478,486#,
        #2814,2822,2830,2838,2846,2854,2862,2870,2878,2886,2894,2902,2910,2918,2926,2934,
        #2942,2950,2958,2966,2974,2982,2990,2998,3006,3014,3022,3030,3038,3046,3054,3062,
        #3070,3078,3086,3094,3102,3110,3118,3126,3134,3142,3150,3158,3166,3174,3182,3190,
        #3198,3206,3214,3222,3230,3238,3246,3254,3262,3270,3278,3286,3294,3302,3310,3318,
        #3326,3334,3342,3350,3358,3366,3374,3382,3390,3398,3406,3414,3422,3430,3438,3446,
        #3454,3462,3470,3478,3486,3494,3502,3510,3518,3526,3534,3542,3550,3558,3566,3574,
        #3582,3590,3598,3606,3614,3622,3630,3638,3646,3654,3662,3670,3678,3686,3694,3702,
        #3710,3718,3726,3734,3742,3750,3758,3766,3774,3782,3790,3798,3806,3814,3822,3830,
        #3838,3846,3854,3862,3870,3878,3886,3894,3902,3910,3918,3926,3934,3942,3950,3958,
        #3966,3974,3982,3990,3998,4006,4014,4022,4030,4038,4046,4054,4062,4070,4078,4086,
        #4094,4102,4110,4118,4126,4134,4142,4150,4158,4166,4174
        ),
  b_e=c(4,9,11,14,20,21,22,23,24,25,33,41,47,55,63,71,79,87,95,103,109,117,125,
        133,141,149,157,165,173,181,189,197,205,213,221,229,237,245,253,
        #261,269,277,285,293,301,
        309,317,325,333,341,349,357,365,373,381,389,397,405,413,421,429,
        437,445,453,461,469,477,485,493#,
        #2821,2829,2837,2845,2853,2861,2869,2877,2885,2893,2901,2909,2917,2925,2933,2941,
        #2949,2957,2965,2973,2981,2989,2997,3005,3013,3021,3029,3037,3045,3053,3061,3069,
        #3077,3085,3093,3101,3109,3117,3125,3133,3141,3149,3157,3165,3173,3181,3189,3197,
        #3205,3213,3221,3229,3237,3245,3253,3261,3269,3277,3285,3293,3301,3309,3317,3325,
        #3333,3341,3349,3357,3365,3373,3381,3389,3397,3405,3413,3421,3429,3437,3445,3453,
        #3461,3469,3477,3485,3493,3501,3509,3517,3525,3533,3541,3549,3557,3565,3573,3581,
        #3589,3597,3605,3613,3621,3629,3637,3645,3653,3661,3669,3677,3685,3693,3701,3709,
        #3717,3725,3733,3741,3749,3757,3765,3773,3781,3789,3797,3805,3813,3821,3829,3837,
        #3845,3853,3861,3869,3877,3885,3893,3901,3909,3917,3925,3933,3941,3949,3957,3965,
        #3973,3981,3989,3997,4005,4013,4021,4029,4037,4045,4053,4061,4069,4077,4085,4093,
        #4101,4109,4117,4125,4133,4141,4149,4157,4165,4173,4181
        ),
  var_type=c('integer','integer','integer','integer','numeric','character',
             'character','character','character','character','integer',
             'integer','numeric','integer','integer','integer','integer',
             'integer','integer','integer','numeric','integer','integer',
             'integer','integer','integer','integer','integer','integer',
             'integer','integer','integer','integer','integer','integer',
             'integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer',
             'integer','integer','integer','integer','integer','integer',
             'integer','integer','integer','integer','integer','integer',
             'integer','integer','integer','integer','integer','integer',
             'integer','integer','integer','integer','integer'#,
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer','integer','integer','integer','integer','integer',
             # 'integer','integer','integer'
             )
  )[order(b_s),];

# Check that byte ranges are valid and no byte is duplicated
stopifnot(census.vars[,all(b_s<=b_e)]);
stopifnot(census.vars[,.(seq=seq(b_s,b_e)),
          by=seq_len(nrow(census.vars))][order(seq),!anyDuplicated(seq)]);

# Byte length of variable
census.vars[,b_l:=b_e-b_s+1];

# Files and info
census.files <- list(
  list(archive='2004.zip',      file='2004/census_2004.dat',format='fwf',year=2004),
  list(archive='2005.zip',      file='2005/census_2005.dat',format='fwf',year=2005),
  list(archive='2006.zip',      file='2006/census_2006.dat',format='fwf',year=2006),
  list(archive='2007.zip',      file='2007/census_2007.dat',format='fwf',year=2007),
  list(archive='census2008.zip',file='census2008.DAT',      format='fwf',year=2008),
  list(archive='census2009.zip',file='census2009.DAT',      format='fwf',year=2009),
  list(archive='census2010.zip',file='census2010.DAT',      format='fwf',year=2010),
  list(archive='census2011.zip',file='census2011.DAT',      format='fwf',year=2011),
  list(archive='Census2012.zip',file='Census2012.csv',      format='csv',year=2012),
  list(archive='Census2013.zip',file='Census2013.csv',      format='csv',year=2013),
  list(archive='CENSUS2014.ZIP',file='Census2014.csv',      format='csv',year=2014));

# Set archive directory
census.files<- Map(function(x) {
                     x$archive <- paste0('./data/census/',x$archive);
                     return(x);
                   },census.files);

custom.convert <- function(x,type=NULL) {
  # Validate types
  stopifnot(type %in% c('numeric','integer','factor','character','logical',
                        'complex','raw','POSIXct'));

  # Guess automatically if type not supplied
  if(is.null(type)) {
    converter <- type.convert;
  } else {
    converter <- get(paste0('as.',type));
  }

  return(converter(x));
}

load_census <- function(archive,file,var.map,skip=0,nrows=Inf,format='fwf') {
  # Read LAR data
  if (format=='fwf') {
    ## Use readr
    #content <- as.data.table(read_fwf(unz(archive,file),
    #          col_positions=fwf_widths(widths=var.map$b_l,col_names=var.map$var_name),
    #          col_types=paste0(substr(var.map$var_type,1,1),collapse=''),
    #          progress=F,na=c('NA','na','Na'),skip=skip,n_max=nrows));
    ## Use iotools
    #content <- as.data.table(input.file(unz(archive,file),
    #          formatter = dstrfw, 
    #          col_types = var.map$var_type, 
    #          widths=var.map$b_l,skip=skip,nrows=nrows));
    # Use fread
    content <- fread(paste0('unzip -p "',archive,'" "',file,'"'),header=F,sep='\n',
                  skip=skip,nrows=nrows)[,
              setNames(lapply(seq_len(nrow(var.map)),
                              function(x) 
                                custom.convert(gsub(paste0(paste0('^\\s*',c('NA','na','Na'),'\\s*$'),
                                                           collapse='|'),'',
                                                    stri_sub(V1, var.map$b_s[x], var.map$b_e[x]))
                                               ,var.map$var_type[x])),
                       var.map$var_name)];
  } else if (format=='csv') {
    content <- fread(paste0('unzip -p "',archive,'" "',file,'"'),header=F,sep=',',
                  skip=skip,nrows=nrows,na.strings=c('NA','na','Na'))[,
              setNames(lapply(seq_len(nrow(var.map)),
                              function(x) custom.convert(.SD[[x]],var.map$var_type[x])),
                       var.map$var_name)];
  } else {
    stop(paste('load_content: unknown format ',format));
  }
  ## state>county>Census, msa only for cities?

  #melt(content[,mget(union(c('year','msa','state','county','tract',
  #                          'principal_city','small_county','split_tract',
  #                          'demographic_data','population_type','population'),
  #                          grep('hispanic',colnames(content),value=T)))],
  #     id.vars=c('year','msa','state','county','tract',
  #               'principal_city','small_county','split_tract',
  #               'demographic_data','population_type'))

  content <- melt(content[,mget(c('year','msa','state','county','tract',
                                  'population',
                                  c('total_hispanic','total_nonhispanic'),
                                  paste0('total_',
                                         rep(c('hispanic','nonhispanic'),each=6),'_',
                                         rep(c('white','black','native','asian','islander','other'),2)),
                                  paste0('total_population_',
                                         c('white','black','native','asian','islander','other')),
                                  'male_population','female_population',
                                  'tract2msa_median_family_income_percentage'))],
                 id.vars=c('year','msa','state','county','tract',
                           'tract2msa_median_family_income_percentage'));

  # Race
  levels(content$variable) <- gsub('^total_population_([^_]+)$','total_all_\\1_all',
                                   levels(content$variable));
  # Ethnicity
  levels(content$variable) <- gsub('^total_(hispanic|nonhispanic)_([^_]+)$','total_\\1_\\2_all',
                                   levels(content$variable));
  levels(content$variable) <- gsub('^total_(hispanic|nonhispanic)$','total_\\1_all_all',
                                   levels(content$variable));

  levels(content$variable) <- gsub('^population$','total_all_all_all',levels(content$variable));
  levels(content$variable) <- gsub('^(male|female)_population$','total_all_all_\\1',
                                   levels(content$variable));

  levels(content$variable) <- gsub('^total_','',levels(content$variable));
  content[,c('ethnicity','race','sex'):=variable]
  levels(content$ethnicity) <- tstrsplit(levels(content$ethnicity),split='_')[[1]];
  levels(content$race) <- tstrsplit(levels(content$race),split='_')[[2]];
  levels(content$sex) <- tstrsplit(levels(content$sex),split='_')[[3]];

  content[is.na(value),value:=0];
  content[,variable:=NULL]
  setnames(content,'value','population');

  return(content);
}

local({
# Batch size
bsize <- 5000000

census <<- NULL;
for (file in census.files) {
  print(paste0(file$archive,':  ',file$file));
  file.n <- as.numeric(system(paste0('unzip -p "',file$archive,'" "',file$file,'" | wc -l'),intern=T));

  for(bb in (1:ceiling(file.n/bsize))) {
    gc();
    
    # Read census data
    tmp <- load_census(file$archive, file$file, census.vars, skip=(bb-1)*bsize,
                    nrows=bsize,format=file$format);
    
    census <<- rbindlist(list(census,tmp),use.names=T);
  }
}
})

census.relincome <- census[,
   .(relincome=mean(tract2msa_median_family_income_percentage,na.rm=T)),
   by=.(tract,year)][,
   relincomecat:=as.factor(ifelse(!is.finite(relincome),'Unknown',
                           ifelse(relincome<50,'Low',
                           ifelse(relincome<80,'Moderate',
                           ifelse(relincome<120,'Middle','High')))))];

if(!file.exists('data/census/census.relincome.rds')) {
  saveRDS(census.relincome,
          file='data/census/census.relincome.rds',compress='xz');
}
