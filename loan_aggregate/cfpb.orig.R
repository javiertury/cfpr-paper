# Load required libraries
libs <- c('data.table','openxlsx','tempdisagg');
for (i in libs) {
    if( !is.element(i, .packages(all.available=T))){
        capture.output(install.packages(i));
    }
    suppressMessages(require(i,character.only=T));
}
rm(i);

# Load auxiliary tools
if (!all(sapply(c('mseq'),exists))) {
  source('../aux.R',chdir=T);
}

# Automobile    AUT
# Credit Card   CRC
# Mortgage      MTG
# Student       STU

cfpb.num <- rbindlist(list(
    fread('../data/cfpb/num_data_AUT.csv',stringsAsFactor=T)[,credit_type:=as.factor('automobile' )],
    fread('../data/cfpb/num_data_CRC.csv',stringsAsFactor=T)[,credit_type:=as.factor('credit_card')],
    fread('../data/cfpb/num_data_MTG.csv',stringsAsFactor=T)[,credit_type:=as.factor('mortgage'   )],
    fread('../data/cfpb/num_data_STU.csv',stringsAsFactor=T)[,credit_type:=as.factor('student'    )]),
  use.names=T);
cfpb.num[,date:=as.Date(paste0(levels(date),'-01'))[date]];
cfpb.num[,month:=NULL];
cfpb.num <- melt(cfpb.num, measure.vars=c('num','num_unadj'),
                 variable.name='season_adj',value.name='num');
cfpb.num[,season_adj:= season_adj!='num_unadj'];
cfpb.num <- rbindlist(list(cfpb.num,
                           cfpb.num[,.(credit_type='all',num=sum(num)),
                                    by=.(date,season_adj)],
                           cfpb.num[credit_type %in% c('automobile','mortgage','student'),
                                    .(credit_type='all_nrev',num=sum(num)),
                                    by=.(date,season_adj)]),
                      use.names=T);

cfpb.vol <- rbindlist(list(
    fread('../data/cfpb/vol_data_AUT.csv',stringsAsFactor=T)[,credit_type:=as.factor('automobile' )],
    fread('../data/cfpb/vol_data_CRC.csv',stringsAsFactor=T)[,credit_type:=as.factor('credit_card')],
    fread('../data/cfpb/vol_data_MTG.csv',stringsAsFactor=T)[,credit_type:=as.factor('mortgage'   )],
    fread('../data/cfpb/vol_data_STU.csv',stringsAsFactor=T)[,credit_type:=as.factor('student'    )]),
  use.names=T);
cfpb.vol[,date:=as.Date(paste0(levels(date),'-01'))[date]];
cfpb.vol[,month:=NULL];
cfpb.vol <- melt(cfpb.vol, measure.vars=c('vol','vol_unadj'),
                 variable.name='season_adj',value.name='vol');
cfpb.vol[,season_adj:= season_adj!='vol_unadj'];
cfpb.vol <- rbindlist(list(cfpb.vol,
                           cfpb.vol[,.(credit_type='all',vol=sum(vol)),
                                    by=.(date,season_adj)],
                           cfpb.vol[credit_type %in% c('automobile','mortgage','student'),
                                    .(credit_type='all_nrev',vol=sum(vol)),
                                    by=.(date,season_adj)]),
                      use.names=T);

cfpb.vol.age <- rbindlist(list(
    fread('../data/cfpb/volume_data_Age_Group_AUT.csv',stringsAsFactor=T)[,credit_type:=as.factor('automobile' )],
    fread('../data/cfpb/volume_data_Age_Group_CRC.csv',stringsAsFactor=T)[,credit_type:=as.factor('credit_card')],
    fread('../data/cfpb/volume_data_Age_Group_MTG.csv',stringsAsFactor=T)[,credit_type:=as.factor('mortgage'   )],
    fread('../data/cfpb/volume_data_Age_Group_STU.csv',stringsAsFactor=T)[,credit_type:=as.factor('student'    )]),
  use.names=T);
cfpb.vol.age[,date:=as.Date(paste0(levels(date),'-01'))[date]];
cfpb.vol.age[,month:=NULL];
cfpb.vol.age <- melt(cfpb.vol.age, measure.vars=c('vol','vol_unadj'),
                     variable.name='season_adj',value.name='vol');
cfpb.vol.age[,season_adj:= season_adj!='vol_unadj'];
cfpb.vol.age <- rbindlist(list(cfpb.vol.age,
                               cfpb.vol.age[,.(credit_type='all',vol=sum(vol)),
                                            by=.(date,season_adj,age_group)],
                               cfpb.vol.age[credit_type %in% c('automobile','mortgage','student'),
                                            .(credit_type='all_nrev',vol=sum(vol)),
                                            by=.(date,season_adj,age_group)]),
                          use.names=T);

age_group.map <- matrix(ncol=3,byrow=T,
  data=c( 'Younger than 30',  0, 29,
                'Age 30-44', 30, 44,
                'Age 45-64', 45, 64,
         'Age 65 and older', 65, Inf
         ));
colnames(age_group.map) <- c('age_group','age_l','age_h')

cfpb.vol.age[,c('age_l','age_h'):=
  .(setNames(as.numeric(age_group.map[,2]),
                        age_group.map[,1])[levels(cfpb.vol.age$age_group)][age_group],
    setNames(as.numeric(age_group.map[,3]),
                        age_group.map[,1])[levels(cfpb.vol.age$age_group)][age_group])];

cfpb.vol.inc <- rbindlist(list(
    fread('../data/cfpb/volume_data_Income_Level_AUT.csv',stringsAsFactor=T)[,credit_type:=as.factor('automobile' )],
    fread('../data/cfpb/volume_data_Income_Level_CRC.csv',stringsAsFactor=T)[,credit_type:=as.factor('credit_card')],
    fread('../data/cfpb/volume_data_Income_Level_MTG.csv',stringsAsFactor=T)[,credit_type:=as.factor('mortgage'   )],
    fread('../data/cfpb/volume_data_Income_Level_STU.csv',stringsAsFactor=T)[,credit_type:=as.factor('student'    )]),
  use.names=T);
cfpb.vol.inc[,date:=as.Date(paste0(levels(date),'-01'))[date]];
cfpb.vol.inc[,month:=NULL];
cfpb.vol.inc <- melt(cfpb.vol.inc, measure.vars=c('vol','vol_unadj'),
                     variable.name='season_adj',value.name='vol');
cfpb.vol.inc[,season_adj:= season_adj!='vol_unadj'];
cfpb.vol.inc <- rbindlist(list(cfpb.vol.inc,
                               cfpb.vol.inc[,.(credit_type='all',vol=sum(vol)),
                                            by=.(date,season_adj,income_level_group)],
                               cfpb.vol.inc[credit_type %in% c('automobile','mortgage','student'),
                                            .(credit_type='all_nrev',vol=sum(vol)),
                                            by=.(date,season_adj,income_level_group)]),
                          use.names=T);

# Neighborhood income relative to metropolitan area or county(rural zones)
income_group.map <- matrix(ncol=3,byrow=T,
  data=c(     'Low',   0,  49,
         'Moderate',  50,  79,
           'Middle',  80, 119,
             'High', 120, Inf
         ));
colnames(income_group.map) <- c('income_level_group','inc_l','inc_h')

cfpb.vol.inc[,c('inc_l','inc_h'):=
  .(setNames(as.numeric(income_group.map[,2]),
                        income_group.map[,1])[levels(cfpb.vol.inc$income_level_group)][income_level_group],
    setNames(as.numeric(income_group.map[,3]),
                        income_group.map[,1])[levels(cfpb.vol.inc$income_level_group)][income_level_group])];

cfpb.vol.cs <- rbindlist(list(
    fread('../data/cfpb/volume_data_Score_Level_AUT.csv',stringsAsFactor=T)[,credit_type:=as.factor('automobile' )],
    fread('../data/cfpb/volume_data_Score_Level_CRC.csv',stringsAsFactor=T)[,credit_type:=as.factor('credit_card')],
    fread('../data/cfpb/volume_data_Score_Level_MTG.csv',stringsAsFactor=T)[,credit_type:=as.factor('mortgage'   )],
    fread('../data/cfpb/volume_data_Score_Level_STU.csv',stringsAsFactor=T)[,credit_type:=as.factor('student'    )]),
  use.names=T);
cfpb.vol.cs[,date:=as.Date(paste0(levels(date),'-01'))[date]];
cfpb.vol.cs[,month:=NULL];
cfpb.vol.cs <- melt(cfpb.vol.cs, measure.vars=c('vol','vol_unadj'),
                    variable.name='season_adj',value.name='vol');
cfpb.vol.cs[,season_adj:= season_adj!='vol_unadj'];
cfpb.vol.cs <- rbindlist(list(cfpb.vol.cs,
                              cfpb.vol.cs[,.(credit_type='all',vol=sum(vol)),
                                          by=.(date,season_adj,credit_score_group)],
                              cfpb.vol.cs[credit_type %in% c('automobile','mortgage','student'),
                                          .(credit_type='all_nrev',vol=sum(vol)),
                                          by=.(date,season_adj,credit_score_group)]),
                         use.names=T);

# Neighborhood income relative to metropolitan area or county(rural zones)
creditscore_group.map <- matrix(ncol=3,byrow=T,
  data=c('Deep subprime', 300, 579,
              'Subprime', 580, 619,
            'Near-prime', 620, 659,
                 'Prime', 660, 719,
           'Super-prime', 720, 850
         ));
colnames(creditscore_group.map) <- c('credit_score_group','cs_l','cs_h')
levels(cfpb.vol.cs$credit_score_group)

cfpb.vol.cs[,c('cs_l','cs_h'):=
  .(setNames(as.numeric(creditscore_group.map[,2]),
                        creditscore_group.map[,1])[levels(cfpb.vol.cs$credit_score_group)][credit_score_group],
    setNames(as.numeric(creditscore_group.map[,3]),
                        creditscore_group.map[,1])[levels(cfpb.vol.cs$credit_score_group)][credit_score_group])];
