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

# Load names
if (!all(sapply(c('loan_type.names','prop_type.names','pur.names',
                  'occupancy.names','preapproval.names','action.names',
                  'ethnicity.names','race.names','sex.names',
                  'purchaser_type.names','denial_reason.names','hoepa.names',
                  'lien.names','edit.names','agency.names'),exists))) {
  source('./names.R',chdir=T);
}

# Utility to load flat data
if (!all(sapply(c('load_flat'),exists))) {
  source('../aux.R',chdir=T);
}

# Load CPI data
if (!all(sapply(c('cpi.y.f'),exists))) {
  source('../data_cpi.R',chdir=T);
}

# Load LAR panel data
if (!all(sapply(c('larpanel'),exists))) {
  source('../data_hmda_panel.R',chdir=T);
}

# Load NIC data
if (!all(sapply(c('nic'),exists))) {
  source('../data_nic.R',chdir=T);
}

if (!all(sapply(c('census.relincome'),exists))) {
  census.relincome <- readRDS(file='../data/census/census.relincome.rds');
}

# Check that all agency and rid combination have a unique rssd
stopifnot(unique(larpanel,by=c('agency','rid','rssd'))[,
            length(unique(rssd))==1,by=.(rid,rssd)][,all(V1)]);

charter.names <- matrix(byrow=T,ncol=2,
  data=c(0,'',
         110,'government',
         200,'commercial bank',
         250,'non-deposit trust',
         300,'savings bank',
         310,'savings & loan association',
         320,'cooperative bank',
         330,'credit union',
         340,'industrial bank',
         400,'edge or agreement corporation',
         500,'holding company only',
         550,'insurance broker or agent and/or insurance company',
         610,'employee stock ownership plan or trust',
         700,'securities broker or dealer',
         700,'utility company or electric power co-generator',
         720,'other non-depository institution'));

nic[,CHTR_TYPE:=as.factor(CHTR_TYPE_CD)];
levels(nic$CHTR_TYPE) <- setNames(charter.names[,2],
                                  charter.names[,1])[levels(nic$CHTR_TYPE)];

rid.enforce <- merge(
  unique(larpanel,by=c('agency','rid'))[,.(
    rssd,rid2=interaction(agency,rid,sep='-'))],
  setnames(copy(nic[,.(ID_RSSD,CHTR_TYPE#,ENTITY_TYPE
                       )]),
           c('ID_RSSD','CHTR_TYPE'),c('rssd','charter')),
  all.x=T,by=c('rssd'));
  
## 82 bytes per line: 80 data bytes and 2 newline bytes(0a 0d, \n \r)
## variable name, byte pos. start, byte pos. end, variable type
lar.vars <- data.table(
  var_name=c('year','rid','agency','loan_type','pur','occupancy','amount',
             'action','msa','state','county','tract','sex','sex_co','income',
             'purchaser_type','denial_reason1','denial_reason2',
             'denial_reason3','edit','prop_type','preapproval',
             'ethnicity','ethnicity_co','race1','race2','race3','race4',
             'race5','race_co1','race_co2','race_co3','race_co4','race_co5',
             'rate_spread','hoepa','lien','sequence'),
  b_s=c(1,5,15,16,17,18,19,24,25,30,32,35,42,43,44,48,49,50,51,52,53,54,55,56,
        57,58,59,60,61,62,63,64,65,66,67,72,73,74),
  b_e=c(4,14,15,16,17,18,23,24,29,31,34,41,42,43,47,48,49,50,51,52,53,54,55,56,
        57,58,59,60,61,62,63,64,65,66,71,72,73,80),
  var_type=c('integer','character','integer','integer','integer','integer',
             'integer','integer','integer','integer','integer','numeric',
             'integer','integer','integer','integer','integer','integer',
             'integer','integer','integer','integer','integer','integer',
             'integer','integer','integer','integer','integer','integer',
             'integer','integer','integer','integer','numeric','integer',
             'integer','integer'))[order(b_s),];
lar.vars.2014 <- data.table(
  var_name=c('year','rid','agency','loan_type','prop_type','pur','occupancy',
             'amount','preapproval','action','msa','state','county','tract',
             'ethnicity','ethnicity_co','race1','race2','race3','race4',
             'race5','race_co1','race_co2','race_co3','race_co4','race_co5',
             'sex','sex_co','income','purchaser_type','denial_reason1',
             'denial_reason2','denial_reason3','rate_spread','hoepa','lien',
             'edit','sequence','population','minority','median_income',
             'tract2msa_income','owner_units','family_units','date_indicator'),
  b_s=c(1,5,15,16,17,18,19,20,25,26,27,32,34,37,44,45,46,47,48,49,50,51,52,53,
        54,55,56,57,58,62,63,64,65,66,71,72,73,74,81,89,95,103,109,117,125),
  b_e=c(4,14,15,16,17,18,19,24,25,26,31,33,36,43,44,45,46,47,48,49,50,51,52,53,
        54,55,56,57,61,62,63,64,65,70,71,72,73,80,88,94,102,108,116,124,125),
  var_type=c('integer','character','integer','integer','integer','integer',
             'integer','integer','integer','integer','integer','integer',
             'integer','numeric','integer','integer','integer','integer',
             'integer','integer','integer','integer','integer','integer',
             'integer','integer','integer','integer','integer','integer',
             'integer','integer','integer','numeric','integer','integer',
             'integer','integer','integer','numeric','integer','numeric',
             'integer','integer','integer'))[order(b_s),];

# Check that all of the bytes are present and none are duplicated
stopifnot(lar.vars[,.(seq=seq(b_s,b_e)),
          by=seq_len(nrow(lar.vars))][order(seq),seq]==c(1:lar.vars[,max(b_e)]));
stopifnot(lar.vars.2014[,.(seq=seq(b_s,b_e)),
          by=seq_len(nrow(lar.vars.2014))][order(seq),seq]==c(1:lar.vars.2014[,max(b_e)]));

# Byte length of variable
lar.vars[,b_l:=b_e-b_s+1];
lar.vars.2014[,b_l:=b_e-b_s+1];

# Files and info
larfiles <- list(list(file='u2004lar.public.dat.zip',vars=lar.vars,
                      format='fwf',year=2004),
                 list(file='LARS.ULTIMATE.2005.DAT.zip',vars=lar.vars,
                      format='fwf',year=2005),
                 list(file='LARS.ULTIMATE.2006.DAT.zip',vars=lar.vars,
                      format='fwf',year=2006),
                 list(file='lars.ultimate.2007.dat.zip',vars=lar.vars,
                      format='fwf',year=2007),
                 list(file='lars.ultimate.2008.dat.zip',vars=lar.vars,
                      format='fwf',year=2008),
                 list(file='2009_Ultimate_PUBLIC_LAR.dat.zip',vars=lar.vars,
                      format='fwf',year=2009),
                 list(file='Lars.ultimate.2010.dat.zip',vars=lar.vars,
                      format='fwf',year=2010),
                 list(file='Lars.ultimate.2011.dat.zip',vars=lar.vars,
                      format='fwf',year=2011),
                 list(file='Lars.ultimate.2012.dat.zip',vars=lar.vars,
                      format='fwf',year=2012),
                 list(file='Lars.ultimate.2013.dat.zip',vars=lar.vars,
                      format='fwf',year=2013),
                 list(file='2014HMDALAR - National.zip',vars=lar.vars.2014,
                      format='csv',year=2014));

# Set files directory
larfiles<- Map(function(x) {
                 x$file <- paste0('../data/HMDA/',x$file);
                 return(x);
               },larfiles);
       
## Keep only these columns & drop census info
lar.vars.core <- c('year','occupancy','county','prop_type','hoepa',
                  'denial_reason','originated','agency','amount','tract',
                  'preapproval','lien','rid2','loan_type','msa','sex',
                  'ethnicity','coapplicant','govprogram_d','pur','state',
                  'income','rate_spread','race','approved');

load_lar <- function(file,lar.vars,skip=0,nrows=Inf,format='fwf') {
  lar <- load_flat(file=file,vars=lar.vars,skip=skip,nrows=nrows,
                   format=format,compression='zip');

  ## state>county>Census, msa only for cities
  
  ## Clean data
  ## Remove applications withdrawn by borrower before lender decision
  ## Remove incomplete/malformed applications by borrower mistake
  ## Remove loans that were purchased(not originated, possibly duplicated)
  lar <- lar[! (action %in% c(4,5,6)),];
  
  ## Remove applicants that are not natural persons
  ## Race is cumbersome, there are 5 of them
  lar <- lar[!((ethnicity==4) & (sex==4)),]
  
  ## Remove applicants not in the main states
  states.in <- c(01,02,04,05,06,08:13,15:42,44:51,53:56)
  lar <- lar[state %in% states.in,]

  ## Remove sequence number
  lar[,sequence:=NULL];
  
  ## Remove edit information
  lar[,edit:=NULL];
  
  ## *rid*(respondent id) and *agency* identify each unique reporting
  ## organization(bank,credit union, financial company...)
  ## Convert respondent id from character to factor
  ## Make respondent id unique regardless of regulatory agency
  lar[,rid:=as.factor(rid)];
  lar[,c('rid2','rid'):=.(interaction(agency,rid,sep='-'),NULL)];
  #lar[,c('rid2','rid'):=.(rid*10+agency,NULL)]
  
  ## If there is a coappliant, code that information
  lar[,coapplicant:=F];
  lar[(sex_co %in% c(1:3)) | (ethnicity_co %in% c(1:3)),coapplicant:=T];
  ## But remove extra information about co-appliant
  lar[,c(grep('_co[0-9]*$',colnames(lar),value=T)):=NULL];
  
  ## Keep only first first race
  lar[,race:=race1];
  lar[,c(grep('race[0-9]+$',colnames(lar),value=T)):=NULL];
  
  ## Keep only first denial reason
  lar[,denial_reason:=denial_reason1];
  lar[,c(grep('denial_reason[0-9]+$',colnames(lar),value=T)):=NULL];
  
  ## Code HOEPA loans
  # hoepa triggered=1, otherwise=2
  lar[,hoepa:=(hoepa==1)];
  
  ## Code dummy for loans related to government programs. Either orignated
  ## for a government program, or later sold to government.
  lar[,govprogram_d:=F];
  lar[(loan_type %in% 2:4) | (purchaser_type %in% c(1:4)),govprogram_d:=T];
  
  ## Drop purchaser type information
  lar[,purchaser_type:=NULL];
  
  ## Pre-approval requested dummy
  lar[,preapproval:=(preapproval==1)];
  
  ## Application approved
  lar[action %in% c(3,7),approved:=F];
  lar[action %in% c(1,2,8),approved:=T];
  
  ## Loan originated
  lar[,originated:=F];
  lar[action==1,originated:=T];
  
  ## Remove action and preapproval variables
  lar[,action:=NULL];
  
  ## *rate_spread* is NA for most observations, can be dropped

  lar <- merge(lar,rid.enforce,
                all.x=T,by=c('rid2'));
  lar[is.na(charter),charter:=as.factor('unknown')];
  
  # Format income and amount
  lar[,income:=income*1000];
  lar[,amount:=amount*1000];

  lar[,income_cpi:=income*100/cpi.y.f(year=year,basket='all')];
  lar[,amount_cpi:=amount*100/cpi.y.f(year=year,basket='all')];

  lar <- merge(lar,census.relincome,by=c('year','tract'));

  # Application date indicator, *date_indicator*
  # 0 -- application date >= 01-01-2004; 1 -- Applicaiton Date < 01-01-2004; 2 -- NA
  # Remove it
  if ('date_indicator' %in% colnames(lar)) lar[,date_indicator:=NULL];
  
  lar.names <- list(
    loan_type      = loan_type.names     , 
    prop_type      = prop_type.names     , 
    pur            = pur.names           , 
    occupancy      = occupancy.names     , 
    #preapproval    = preapproval.names   , 
    #action         = action.names        , 
    ethnicity      = ethnicity.names     , 
    race           = race.names          , 
    sex            = sex.names           , 
    #purchaser_type = purchaser_type.names, 
    denial_reason  = denial_reason.names , 
    #hoepa          = hoepa.names         , 
    lien           = lien.names          , 
    #edit           = edit.names          , 
    agency         = agency.names        );
  
  # For each variable type, tranform codes into names
  #for (vv in Filter(function(vv) any(grepl(paste0('^',vv,'(_co)?[0-9]*$'),colnames(lar))),
  #                  names(lar.names))) {
  for (vv in names(lar.names)) {
    # Select all variable of the same type
    allvv <- grep(paste0('^',vv,'(_co)?[0-9]*$'),colnames(lar),value=T);
    # Check that there is at least one matched variable
    stopifnot(length(allvv)>0)
  
    # Format these variables as factors
    lar[,c(allvv):=lapply(mget(allvv),as.factor)];
  
    # Transform codes into names for each variable
    for (vv2 in allvv) {
      # Check that all codes can be mapped into names
      stopifnot(levels(lar[[vv2]]) %in% lar.names[[vv]][,1]);
      # Transform codes into names
      levels(lar[[vv2]]) <- setNames(lar.names[[vv]][,2],
                                      lar.names[[vv]][,1])[levels(lar[[vv2]])]
      lar[is.na(get(vv2)),c(vv2):=as.factor('NA')];
    }
  }

  return(lar);
}
