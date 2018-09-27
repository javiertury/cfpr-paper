# Pick dates. Create variable with prob. that loan is taken after those dates
reg.dates <- list(# Education loans
                  'heoa_pel'=as.Date('2010-2-14'),
                  'heoa_calc'=as.Date('2011-10-29'),
                  # Mortgages
                  # 'hoepa01'=as.Date('2002-10-1'),
                  'hoepa08'=as.Date('2009-10-1'),
                  'mdia'=as.Date('2009-07-30'),
                  'mdia2'=as.Date('2011-01-30'),
                  ## Rule banning compensation to mortgage originators for non-volume terms
                  'compban'=as.Date('2011-04-06'),
                  # SAFE act, registration of mortgage loan originators
                  'safe_eff'=as.Date('2011-07-29'),
                  # RESPA 2008 rule
                  'respa08'=as.Date('2010-01-01'),
                  # FTC advertising rule
                  'ftc_ad'=as.Date('2011-08-19'),
                  'ftc_ad_warning'=as.Date('2011-11-19'),
                  # Helping Families Save Their Homes Act of 2009
                  'hfsth_act'=as.Date('2009-05-20')
                  );
