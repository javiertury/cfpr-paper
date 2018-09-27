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

# Load SCF 2013
load('../data/scf2013.rda');
# imp1, imp2, imp3, imp4, rw

# Load description names of variables
source('./names.R',chdir=T);

imp1 <- as.data.table(imp1);
imp2 <- as.data.table(imp2);
imp3 <- as.data.table(imp3);
imp4 <- as.data.table(imp4);
imp5 <- as.data.table(imp5);
rw <- as.data.table(rw);

#colnames(imp1)
cols <- matrix(ncol=2, byrow=T,
       data=c('yy1',   'id',          # Id of household
               'y1',   'repid',       # Replication id = id*10 + rep number
              'wgt',   'wgt',         # Replication? weight
              'networth', 'networth', # Net worth
              'x33001', 'wilshire',   # Wilshire stock index at interview
              'x42001', 'weight',     # Kennickell-Woodburn corrected weight
              'x8021', 'sex',         # Sex of respondent
               'x103', 'sex_partner', # Sex of respondents partner
                'x14', 'age',         # Age of respondent
                'x19', 'age_partner', # Age of respondents partner
              'x5702', 'income',      # Main income of household
              'x5729', 'income_total',      # Income from all sources(total)
              'x7362', 'income_regular',    # Income (total) in regular years
              'x7578', 'showcards_avail',
              'x7579', 'showcards_confirm',
              'x7100', 'search_effort',     # Effort put in comparing financial products
              'x7101', 'search_source_1',   # Use search source in this order to compare
              'x7102', 'search_source_2',
              'x7103', 'search_source_3',
              'x7104', 'search_source_4',
              'x7105', 'search_source_5',
              'x7106', 'search_source_6',
              'x7107', 'search_source_7',
              'x7108', 'search_source_8',
              'x7109', 'search_source_9',
              'x7110', 'search_source_10',
              'x6849', 'search_source_11',
              'x6861', 'search_source_12',
              'x6862', 'search_source_13',
              'x6863', 'search_source_14',
              'x6864', 'search_source_15',
               'x305', 'search_software',   # Use search software
              'x7111', 'search2_effort',    # Effort put in comparing financial products
              'x7112', 'search2_source_1',  # Use search source in this order to compare
              'x7113', 'search2_source_2',
              'x7114', 'search2_source_3',
              'x7115', 'search2_source_4',
              'x7116', 'search2_source_5',
              'x7117', 'search2_source_6',
              'x7118', 'search2_source_7',
              'x7119', 'search2_source_8',
              'x7120', 'search2_source_9',
              'x7121', 'search2_source_10',
              'x6865', 'search2_source_11',
              'x6866', 'search2_source_12',
              'x6867', 'search2_source_13',
              'x6868', 'search2_source_14',
              'x6869', 'search2_source_15',
              'x7131', 'credit_apply',      # Applied for credit in last 5yr
               'x407', 'credit_down',       # Turned down for credit
               'x408', 'credit_reapply_full',     # Able to obtain full amount reapplying somewhere
              'x7585', 'credit_down_reason',      # Reason for credit turn down
              'x7584', 'credit_apply_type', # Type of credit applied for
               'x409', 'credit_pesimist',   # Didn't apply because fear of been turned down
              'x7583', 'credit_pesimist_reason',  # Reason for credit pessimism
               'x724', 'mortgage1_fedguarant',    # First mortgage is federally guaranteed
               'x726', 'mortgage1_govprogram', # Program under which first mortgage was granted
             #  'x727', 'mortgage1_why',     # Why did you choose this type of loan?
               'x725', 'mortgage1_pmi',     # Private mortgage insurance
              'x7137', 'mortgage1_finpur',  # Financial purpose, refinance and/or borrow more?
               'x802', 'mortgage1_year',
               'x902', 'mortgage2_year',
              'x1002', 'mortgage3_year',
             #  'x803', 'mortgage1_prev',    # Mortgage assumed from previous owner
             #  'x903', 'mortgage2_prev',    # Mortgage assumed from previous owner
             # 'x7138', 'mortgage1_additional',    # Additional money borrowed
              'x6723', 'mortgage1_pur',     # Purpose
               'x918', 'mortgage2_pur',
              'x1018', 'mortgage3_pur',
               'x804', 'mortgage1_amount',  # Land Contract amount
               'x904', 'mortgage2_amount',
              'x1004', 'mortgage3_amount',
               'x805', 'mortgage1_left',    # Amount left
               'x905', 'mortgage2_left',
              'x1005', 'mortgage3_left',
               'x806', 'mortgage1_maturity',      # Original maturity in years
               'x906', 'mortgage2_maturity',
              'x1006', 'mortgage3_maturity',
               'x807', 'mortgage1_npay',    # Number of payments
               'x907', 'mortgage2_npay',
              'x1007', 'mortgage3_npay',
               'x808', 'mortgage1_pay',     # Payment amount
               'x908', 'mortgage2_pay',
              'x1008', 'mortgage3_pay',
               'x809', 'mortgage1_payfreq', # Frequency of payments
               'x909', 'mortgage2_payfreq',
              'x1009', 'mortgage3_payfreq',
               'x813', 'mortgage1_typicalpay',    # Typical payment
               'x913', 'mortgage2_typicalpay',
              'x1013', 'mortgage3_typicalpay',
               'x814', 'mortgage1_typicalpayfreq',   # Typical payment frequency
               'x914', 'mortgage2_typicalpayfreq',
              'x1014', 'mortgage3_typicalpayfreq',
               'x810', 'mortgage1_payincludes',   # Includes insurance or taxes in payment
               'x811', 'mortgage1_balloon', # There is a balloon payment
               'x911', 'mortgage2_balloon',
              'x1011', 'mortgage3_balloon',
               'x812', 'mortgage1_balloon_amount',   # Amount of balloon payment
               'x912', 'mortgage2_balloon_amount',
              'x1012', 'mortgage3_balloon_amount',
              'x7571', 'mortgage1_schedule',       # Are payments on schedule?
              'x7570', 'mortgage2_schedule',
              'x7569', 'mortgage3_schedule',
               'x815', 'mortgage1_repay',   # Year Loan is expected to be repaid 
               'x915', 'mortgage2_repay',
              'x1015', 'mortgage3_repay',
               'x816', 'mortgage1_int',     # Current Interest rate
               'x916', 'mortgage2_int',
              'x1016', 'mortgage3_int',
               'x817', 'mortgage1_inst',    # Institution that granted loan
               'x917', 'mortgage2_inst',
              'x1017', 'mortgage3_inst',
              'x9083', 'mortgage1_insttype',      # Institution type
              'x9084', 'mortgage2_insttype',
              'x9085', 'mortgage3_insttype',
             # 'x7568', 'mortgage3_sameinst',      # Same instituion as original loan
             # 'x7580', 'mortgage3_originst',      # Original loan institution
             # 'x6442', 'mortgage3_originsttype',  # Original loan institution type
             #  'x819', 'mortgage3_origreason',    # Reason to go with original lender
               'x820', 'mortgage1_adjust',  # Adjustable rate mortgage
               'x920', 'mortgage2_adjust',
              'x1020', 'mortgage3_adjust',
             #  'x821', 'mortgage1_adjust_int',    # Adjust depends on other interest rate
              'x7053', 'mortgage1_adjust_eff',    # Has interest rate changed ever?
              'x7054', 'mortgage1_adjust_origint',# Original interest rate
              'x7055', 'mortgage1_adjust_firstyear',     # 1st year that interest chage was possible
             # 'x7056', 'mortgage1_adjust_limit',  # After first change, times interest can change
              'x7057', 'mortgage1_adjust_freq',   # Possible change frequency
             # 'x7058', 'mortgage1_adjust_ceil',   # Ceiling for interest rate
             # 'x7060', 'mortgage1_adjust_convertible',   # Can convert to a fixed interest rate
             # 'x7061', 'mortgage1_adjust_other',  # Other terms scheduled to change
             # 'x7062', 'mortgage1_adjust_otherspec',  # Specify other terms scheduled to change
              'x1032', 'mproploan',  # Other loan used to purchase main property(mprop)
              'x1034', 'mproploan1_year',
              'x1035', 'mproploan1_amount',
              'x1036', 'mproploan1_reg',           # Regular installments
              'x1038', 'mproploan1_maturity',      # Maturity in number of years
              'x1037', 'mproploan1_npay',          # Number of payments
              'x1039', 'mproploan1_pay',           # Payment amount
              'x7567', 'mproploan1_payfreq',       # Payment frequency
              'x1040', 'mproploan1_typicalpay',    # Typical payment
              'x1041', 'mproploan1_typicalpayfreq',   # Typical payment frequency
              'x7566', 'mproploan1_schedule',      # Is it being paid on schedule?
              'x1043', 'mproploan1_repay',         # Year it will be repaid
              'x1044', 'mproploan1_left',          # Amount left to pay
              'x1045', 'mproploan1_int',           # Interest rate
              'x1046', 'mproploan1_inst',          # Institution
              'x9086', 'mproploan1_insttype',      # Institution type
              'x1101', 'lines',      # Line of credit Y/N
              'x1102', 'lines_num',  # Number of lines of credit
              'x1103', 'line1_sechome', # Line of credit secured by home
              'x1114', 'line2_sechome',
              'x1125', 'line3_sechome',
              'x1105', 'line1_inuse',       # Currently owe money
              'x1116', 'line2_inuse',
              'x1127', 'line3_inuse',
              'x7141', 'line1_lastamount',  # Last amount borrowed
              'x7142', 'line2_lastamount',
              'x7143', 'line3_lastamount',
              'x1106', 'line1_pur',  # Purpose
              'x1117', 'line2_pur',
              'x1128', 'line3_pur',
              'x1108', 'line1_amount',      # Current amount borrowed
              'x1119', 'line2_amount',
              'x1130', 'line3_amount',
              'x1109', 'line1_typicalpay',  # Typical payment
              'x1120', 'line2_typicalpay',
              'x1131', 'line3_typicalpay',
              'x1110', 'line1_typicalpayfreq',    # Typical payment frequency
              'x1121', 'line2_typicalpayfreq',
              'x1132', 'line3_typicalpayfreq',
              'x1111', 'line1_int',  # Annual Rate of Interest
              'x1122', 'line2_int',
              'x1133', 'line3_int',
              'x1112', 'line1_inst', # Institution
              'x1123', 'line2_inst',
              'x1134', 'line3_inst',
              'x9087', 'line1_insttype',    # Institution type
              'x9088', 'line2_insttype',
              'x9089', 'line3_insttype',
              'x1104', 'line1_limit',       # Credit limit
              'x1115', 'line2_limit',
              'x1126', 'line3_limit',
             # 'x1136', 'lineo_amount',      # Amount currently owed in other lines
              'x1201', 'remodel',    # Remodeling Y/N
              'x1202', 'remodel_totalcost', # Cost of all remodellings
              'x1203', 'remodloan1',         # Took a loan to remodel? Y/N
              'x1205', 'remodloan1_year',    # Year of last remodelling loan
              'x1206', 'remodloan1_amount',  # Amount of last remodelling loan
              'x1207', 'remodloan1_reg',     # Regular installment loan Y/N
              'x1209', 'remodloan1_maturity',      # Maturity in years
              'x1208', 'remodloan1_npay',    # Number of payments
              'x1210', 'remodloan1_pay',     # Payment amount
              'x7565', 'remodloan1_payfreq', # Payment frequency
              'x1211', 'remodloan1_typicalpay',    # Typical payment
              'x1212', 'remodloan1_typicalpayfreq',    # Typical payment frequency
              'x7564', 'remodloan1_schedule',      # Are you making payments on schedule?
              'x1214', 'remodloan1_repay',   # Year expected to be repaid
              'x1215', 'remodloan1_left',    # Amount still owed
              'x1216', 'remodloan1_int',     # Interest rate
              'x1217', 'remodloan1_inst',    # Institution 
              'x9090', 'remodloan1_insttype',      # Type of institution 
            # 'x1218', 'remodloano', # More loans than this one?
            # 'x1219', 'remodloano_amount', # Amount owed on other loans
            # 'x1220', 'remodloano_pay',    # Payments on other loans
            # 'x1221', 'remodloano_payfreq',      # Payment frequency on other loans
              'x1701', 'prop',       # Investment/vacation real state not owned by business. Includes home.
              'x1703', 'prop1_type', # Type of real state
              'x1803', 'prop2_type',
              'x1704', 'prop1_owntype',     # Ownership type
              'x1804', 'prop2_owntype',
              'x1705', 'prop1_own',  # Ownership percentage
              'x1805', 'prop2_own',
              'x1706', 'prop1_value',       # Value of real state, total
              'x1806', 'prop2_value',
              'x1708', 'prop1_year', # Year obtained
              'x1808', 'prop2_year',
              'x1710', 'prop1_gift', # Was it a gift/inheritance?
              'x1810', 'prop2_gift',
              'x1709', 'prop1_valuepast',   # Value when obtained
              'x1809', 'prop2_valuepast',
              'x1711', 'proploan1',  # Outstanding loan to finance real stat investment?
              'x1811', 'proploan2',
              'x1713', 'proploan1_year',    # Year loon was took
              'x1813', 'proploan2_year',
              'x1714', 'proploan1_amount',  # Amount borrowed
              'x1814', 'proploan2_amount',
              'x1715', 'proploan1_left',    # Amount left to pay
              'x1815', 'proploan2_left',
              'x1716', 'proploan1_maturity',      # Maturity of loan at origination, years
              'x1816', 'proploan2_maturity',
              'x1717', 'proploan1_npay',    # Number of payments
              'x1817', 'proploan2_npay',
              'x1718', 'proploan1_pay',     # Payment amount
              'x1818', 'proploan2_pay',
              'x1719', 'proploan1_payfreq', # Payment frequency
              'x1819', 'proploan2_payfreq',
              'x1723', 'proploan1_typicalpay',    # Typical payment amount
              'x1823', 'proploan2_typicalpay',
              'x1724', 'proploan1_typicalpayfreq',   # Typical payment frequency
              'x1824', 'proploan2_typicalpayfreq',
              'x1720', 'proploan1_payincludes',      # Payment includes insurance/taxes
              'x1820', 'proploan2_payincludes',
              'x1721', 'proploan1_balloon',       # Loan includes balloon payment
              'x1821', 'proploan2_balloon',
              'x1722', 'proploan1_balloon_amount',   # Balloon amount
              'x1822', 'proploan2_balloon_amount',
              'x7554', 'proploan1_schedule',      # Are payments being made on schedule?
              'x7553', 'proploan2_schedule',
              'x1725', 'proploan1_repay',   # Expected repayment year
              'x1825', 'proploan2_repay',
              'x1726', 'proploan1_int',     # Interest rate
              'x1826', 'proploan2_int',
              'x1727', 'proploan1_adjust',  # Adjustable interest rate Y/N
              'x1827', 'proploan2_adjust',
              'x1728', 'proploan1_inst',    # Institution
              'x1828', 'proploan2_inst',
              'x9099', 'proploan1_insttype',      # Institution type
              'x9100', 'proploan2_insttype',
             # 'x2001', 'vaco',       # Rest of vacation properties
             # 'x2002', 'vaco_own_value',    # Ownership percentage
             # 'x2003', 'vaco_own_pastvalue',      # Original value
             # 'x2004', 'vaco_gift',  # Gift/inherit? Y/N
             # 'x2005', 'vacloano',   # Loan on this property
             # 'x2006', 'vacloano_amount',   # Howe much is owed on share of loans
             # 'x2007', 'vacloano_pay',      # Payment amount
             # 'x2008', 'vacloano_payfreq',  # Payment frequency
             # 'x2011', 'propo',      # Other property
             # 'x2012', 'propo_own_value',   # Value of other properties
             # 'x2013', 'propo_own_pastvalue',     # Family ownership
             # 'x2014', 'propo_gift', # Gift/inherit? Y/N
             # 'x2015', 'proploano',  # Loan on those properties
             # 'x2016', 'proploano_amount',  # Amount owed
             # 'x2017', 'proploano_pay',     # Payment amount
             # 'x2018', 'proploano_payfreq', # Payment frequency
             # 'x3107', 'biz1_industry',      # Business industry code
             # 'x3207', 'biz2_industry',
             # 'x3108', 'biz1_acquisition',   # How was this business acquired/started?
             # 'x3208', 'biz2_acquisition',
             # 'x3110', 'biz1_year',  # Year business was started/acquired
             # 'x3210', 'biz2_year',
             # 'x3111', 'biz1_emp',   # Number of employees
             # 'x3211', 'biz2_emp',
             # 'x3119', 'biz1_legalform',     # Legal form of business
             # 'x3219', 'biz2_legalform',
             # 'x3120', 'biz1_secloan',       # Used personal assets to secure a loan for business
             # 'x3220', 'biz2_secloan',
             # 'x7144', 'biz1_sec',   # How was it secured?
             # 'x7145', 'biz2_sec',
             # 'x3121', 'biz1_secamount',     # How much was collaterilized or guaranteed?
             # 'x3221', 'biz2_secamount',
             # 'x3121', 'biz1_dupe',  # Already recorded, duplicated
             # 'x3221', 'biz2_dupe',
             # 'x7551', 'biz1_dupewhich',     # Which is duplicated?
             # 'x7550', 'biz2_dupewhich',
             # 'x3123', 'biz1_nonsec',        # Business owes you non-secured money
             # 'x3223', 'biz2_nonsec',
             # 'x3124', 'biz1_nonsecamount',  # Amount owed by business
             # 'x3224', 'biz2_nonsecamount',
             # 'x3125', 'biz1_owetobiz',      # Do you owe money to the business?
             # 'x3225', 'biz2_owetobiz',
             # 'x3126', 'biz1_owetobiz_amount',     # Amount owed to business
             # 'x3226', 'biz2_owetobiz_amount',
             # 'x3127', 'biz1_owetobiz_dupe', # Owed money to business already recorded, duplicated
             # 'x3227', 'biz2_owetobiz_dupe',
             # 'x3127', 'biz1_owetobiz_dupe_which', # Which duplicate?
             # 'x3227', 'biz2_owetobiz_dupe_which',
             # 'x3141', 'biz1_startsource1', # Sources of money used to start business
             # 'x3142', 'biz1_startsource2',
             # 'x3143', 'biz1_startsource3',
             # 'x3144', 'biz1_startsource4',
             # 'x3145', 'biz1_startsource5',
             # 'x3146', 'biz1_startsource6',
             # 'x3147', 'biz1_startsource7',
             # 'x3148', 'biz1_startsource8',
             # 'x3149', 'biz1_startsource9',
             # 'x3241', 'biz2_startsource1',
             # 'x3242', 'biz2_startsource2',
             # 'x3243', 'biz2_startsource3',
             # 'x3244', 'biz2_startsource4',
             # 'x3245', 'biz2_startsource5',
             # 'x3246', 'biz2_startsource6',
             # 'x3247', 'biz2_startsource7',
             # 'x3248', 'biz2_startsource8',
             # 'x3249', 'biz2_startsource9',
             # 'x3151', 'biz1_ongoing_source1', # Sources of money used to finance ongoing business
             # 'x3152', 'biz1_ongoing_source2',
             # 'x3153', 'biz1_ongoing_source3',
             # 'x3154', 'biz1_ongoing_source4',
             # 'x3155', 'biz1_ongoing_source5',
             # 'x3156', 'biz1_ongoing_source6',
             # 'x3157', 'biz1_ongoing_source7',
             # 'x3158', 'biz1_ongoing_source8',
             # 'x3159', 'biz1_ongoing_source9',
             # 'x3251', 'biz2_ongoing_source1',
             # 'x3252', 'biz2_ongoing_source2',
             # 'x3253', 'biz2_ongoing_source3',
             # 'x3254', 'biz2_ongoing_source4',
             # 'x3255', 'biz2_ongoing_source5',
             # 'x3256', 'biz2_ongoing_source6',
             # 'x3257', 'biz2_ongoing_source7',
             # 'x3258', 'biz2_ongoing_source8',
             # 'x3259', 'biz2_ongoing_source9',
             # 'x3161', 'biz1_inst',  # Primary financial institution
             # 'x3261', 'biz2_inst',
             # 'x3161', 'biz1_insttype',        # Financial institution type
             # 'x3261', 'biz2_insttype',
             # 'x3161', 'biz1_instdist',        # Distance to office of financial institution
             # 'x3261', 'biz2_instdist',
             ## biz leads nowhere
              'x2201', 'carloan',    # Car loan
              'x2203', 'car1_type',
              'x2303', 'car2_type',
              'x2403', 'car3_type',
              'x7150', 'car4_type',
              'x2205', 'car1_modelyear',
              'x2305', 'car2_modelyear',
              'x2405', 'car3_modelyear',
              'x7152', 'car4_modelyear',
              'x8166', 'car1_value',
              'x8167', 'car2_value',
              'x8168', 'car3_value',
              'x8188', 'car4_value',
              'x7543', 'car1_new',   # Was it new or used?
              'x7542', 'car2_new',
              'x7541', 'car3_new',
              'x7153', 'car4_new',
              'x7540', 'car1_year',  # Purchase year
              'x7539', 'car2_year',
              'x7538', 'car3_year',
              'x7154', 'car4_year',
              'x2206', 'carloan1',   # Loan to obtain car?
              'x2306', 'carloan2',
              'x2406', 'carloan3',
              'x7155', 'carloan4',
              'x2208', 'carloan1_year',     # Year loan was took
              'x2308', 'carloan2_year',
              'x2408', 'carloan3_year',
              'x7157', 'carloan4_year',
              'x2209', 'carloan1_amount',   # Loan amount
              'x2309', 'carloan2_amount',
              'x2409', 'carloan3_amount',
              'x7158', 'carloan4_amount',
              'x2210', 'carloan1_reg',      # Regular installment
              'x2310', 'carloan2_reg',
              'x2410', 'carloan3_reg',
              'x7159', 'carloan4_reg',
              'x2212', 'carloan1_maturity', # Original maturity in years, years
              'x2312', 'carloan2_maturity',
              'x2412', 'carloan3_maturity',
              'x7161', 'carloan4_maturity',
              'x2211', 'carloan1_npay',     # Number of payments
              'x2311', 'carloan2_npay',
              'x2411', 'carloan3_npay',
              'x7160', 'carloan4_npay',
              'x2213', 'carloan1_pay',      # Payment amount
              'x2313', 'carloan2_pay',
              'x2413', 'carloan3_pay',
              'x7162', 'carloan4_pay',
              'x7537', 'carloan1_payfreq',  # Payment frequency
              'x7536', 'carloan2_payfreq',
              'x7535', 'carloan3_payfreq',
              'x7163', 'carloan4_payfreq',
              'x2214', 'carloan1_typicalpay',     # Typical payment amount
              'x2314', 'carloan2_typicalpay',
              'x2414', 'carloan3_typicalpay',
              'x7164', 'carloan4_typicalpay',
              'x2215', 'carloan1_typicalpayfreq', # Typical payment frequency
              'x2315', 'carloan2_typicalpayfreq',
              'x2415', 'carloan3_typicalpayfreq',
              'x7165', 'carloan4_typicalpayfreq',
              'x7534', 'carloan1_schedule', # Are you making payments on schedule?
              'x7533', 'carloan2_schedule',
              'x7532', 'carloan3_schedule',
              'x7166', 'carloan4_schedule',
              'x2217', 'carloan1_repay',    # Repayment year
              'x2317', 'carloan2_repay',
              'x2417', 'carloan3_repay',
              'x7168', 'carloan4_repay',
              'x2218', 'carloan1_left',     # Amount left to repay
              'x2318', 'carloan2_left',
              'x2418', 'carloan3_left',
              'x7169', 'carloan4_left',
              'x2219', 'carloan1_int',      # Interest rate
              'x2319', 'carloan2_int',
              'x2419', 'carloan3_int',
              'x7170', 'carloan4_int',
              'x2220', 'carloan1_inst',     # Institution
              'x2320', 'carloan2_inst',
              'x2420', 'carloan3_inst',
              'x7171', 'carloan4_inst',
              'x9102', 'carloan1_insttype', # Institution type
              'x9103', 'carloan2_insttype',
              'x9104', 'carloan3_insttype',
              'x9215', 'carloan4_insttype',
             # 'x2422', 'caro_value', # Other car value
             # 'x2423', 'carloano',   # Other car loans amount
             # 'x2424', 'carloano_left',     # Amount still owed (left?)
             # 'x2425', 'carloano_pay',      # Payment amount
             # 'x2426', 'carloano_payfreq',  # Payment frequency
              'x2503', 'vec',        # Non-car vehicles owned
              'x2504', 'vec_num',           # Number of non-car vehicles owned
              'x2505', 'vec1_type',         # Type of vehicle
              'x2605', 'vec2_type',
              'x2506', 'vec1_value',        # Value of vehicle
              'x2606', 'vec2_value',
              'x2507', 'vecloan1',          # Loan on vehicle Y/N
              'x2607', 'vecloan2',
              'x2509', 'vecloan1_year',     # Year loan was taken
              'x2609', 'vecloan2_year',
              'x2510', 'vecloan1_amount',   # Amount of loan
              'x2610', 'vecloan2_amount',
              'x2511', 'vecloan1_reg',      # Regular installments Y/N
              'x2611', 'vecloan2_reg',
              'x2513', 'vecloan1_maturity', # Maturity in years
              'x2613', 'vecloan2_maturity',
              'x2512', 'vecloan1_npay',     # Number of payments
              'x2612', 'vecloan2_npay',
              'x2514', 'vecloan1_pay',      # Payment amount
              'x2614', 'vecloan2_pay',
              'x7531', 'vecloan1_payfreq',  # Payment frequency
              'x7530', 'vecloan2_payfreq',
              'x2515', 'vecloan1_typicalpay',     # Typical payment amount
              'x2615', 'vecloan2_typicalpay',
              'x2516', 'vecloan1_typicalpayfreq', # Typical payment frequency
              'x2616', 'vecloan2_typicalpayfreq',
              'x7529', 'vecloan1_schedule', # Do you make payments on schedule?
              'x7528', 'vecloan2_schedule',
              'x2518', 'vecloan1_repay',    # Expected year of repayment
              'x2618', 'vecloan2_repay',
              'x2519', 'vecloan1_left',     # Amount left for repay
              'x2619', 'vecloan2_left',
              'x2520', 'vecloan1_int',      # Interest rate
              'x2620', 'vecloan2_int',
              'x2521', 'vecloan1_inst',     # Institution
              'x2621', 'vecloan2_inst',
              'x9105', 'vecloan1_insttype', # Institution type
              'x9106', 'vecloan2_insttype',
             # 'x2623', 'veco_value', # Value of all other non-car vehicles
             # 'x2624', 'vecloano',   # Loan on those other non-car vehicles?
             # 'x2625', 'vecloano_left',     # Amount still owned
             # 'x2626', 'vecloano_pay',      # Payment amount
             # 'x2627', 'vecloano_payfreq',  # Payment frequency
              'x7804', 'eduloan1_year',     # Year education loan was taken
              'x7827', 'eduloan2_year',
              'x7850', 'eduloan3_year',
              'x7904', 'eduloan4_year',
              'x7927', 'eduloan5_year',
              'x7950', 'eduloan6_year',
              'x7805', 'eduloan1_amount',   # Amount borrowed
              'x7828', 'eduloan2_amount',
              'x7851', 'eduloan3_amount',
              'x7905', 'eduloan4_amount',
              'x7928', 'eduloan5_amount',
              'x7951', 'eduloan6_amount',
              'x7806', 'eduloan1_paying',   # Paying loan right now?
              'x7829', 'eduloan2_paying',
              'x7852', 'eduloan3_paying',
              'x7906', 'eduloan4_paying',
              'x7929', 'eduloan5_paying',
              'x7952', 'eduloan6_paying',
              'x7173', 'eduloan1_deferred', # Are payments being deferred?
              'x7174', 'eduloan2_deferred',
              'x7175', 'eduloan3_deferred',
              'x7176', 'eduloan4_deferred',
              'x7177', 'eduloan5_deferred',
              'x7178', 'eduloan6_deferred',
              'x7808', 'eduloan1_payyear',  # Year loan will start being paid
              'x7831', 'eduloan2_payyear',
              'x7854', 'eduloan3_payyear',
              'x7908', 'eduloan4_payyear',
              'x7931', 'eduloan5_payyear',
              'x7954', 'eduloan6_payyear',
              'x7809', 'eduloan1_accint',   # Accumulates interest that will have to be paid
              'x7832', 'eduloan2_accint',
              'x7855', 'eduloan3_accint',
              'x7909', 'eduloan4_accint',
              'x7932', 'eduloan5_accint',
              'x7955', 'eduloan6_accint',
              'x7811', 'eduloan1_lastpay',  # Year started paying this loan
              'x7834', 'eduloan2_lastpay',
              'x7857', 'eduloan3_lastpay',
              'x7911', 'eduloan4_lastpay',
              'x7934', 'eduloan5_lastpay',
              'x7957', 'eduloan6_lastpay',
              'x7812', 'eduloan1_reg',      # Year started paying this loan
              'x7835', 'eduloan2_reg',
              'x7858', 'eduloan3_reg',
              'x7912', 'eduloan4_reg',
              'x7935', 'eduloan5_reg',
              'x7958', 'eduloan6_reg',
              'x7813', 'eduloan1_maturity', # Original maturity of loan, years
              'x7836', 'eduloan2_maturity',
              'x7859', 'eduloan3_maturity',
              'x7913', 'eduloan4_maturity',
              'x7936', 'eduloan5_maturity',
              'x7959', 'eduloan6_maturity',
              'x7814', 'eduloan1_npay',     # Number of payments
              'x7837', 'eduloan2_npay',
              'x7860', 'eduloan3_npay',
              'x7914', 'eduloan4_npay',
              'x7937', 'eduloan5_npay',
              'x7960', 'eduloan6_npay',
              'x7815', 'eduloan1_pay',      # Payment amount
              'x7838', 'eduloan2_pay',
              'x7861', 'eduloan3_pay',
              'x7915', 'eduloan4_pay',
              'x7938', 'eduloan5_pay',
              'x7961', 'eduloan6_pay',
              'x7816', 'eduloan1_payfreq',  # Payment frequency
              'x7839', 'eduloan2_payfreq',
              'x7862', 'eduloan3_payfreq',
              'x7916', 'eduloan4_payfreq',
              'x7939', 'eduloan5_payfreq',
              'x7962', 'eduloan6_payfreq',
              'x7817', 'eduloan1_typicalpay',     # Typical payment amount
              'x7840', 'eduloan2_typicalpay',
              'x7863', 'eduloan3_typicalpay',
              'x7917', 'eduloan4_typicalpay',
              'x7940', 'eduloan5_typicalpay',
              'x7963', 'eduloan6_typicalpay',
              'x7818', 'eduloan1_typicalpayfreq', # Typical payment amount
              'x7841', 'eduloan2_typicalpayfreq',
              'x7864', 'eduloan3_typicalpayfreq',
              'x7918', 'eduloan4_typicalpayfreq',
              'x7941', 'eduloan5_typicalpayfreq',
              'x7964', 'eduloan6_typicalpayfreq',
              'x7821', 'eduloan1_schedule', # Do you make payments on schedule?
              'x7844', 'eduloan2_schedule',
              'x7867', 'eduloan3_schedule',
              'x7921', 'eduloan4_schedule',
              'x7944', 'eduloan5_schedule',
              'x7967', 'eduloan6_schedule',
              'x7820', 'eduloan1_repay',    # Year Expected to be repaid
              'x7843', 'eduloan2_repay',
              'x7866', 'eduloan3_repay',
              'x7920', 'eduloan4_repay',
              'x7943', 'eduloan5_repay',
              'x7966', 'eduloan6_repay',
              'x7822', 'eduloan1_int',      # Interest rate
              'x7845', 'eduloan2_int',
              'x7868', 'eduloan3_int',
              'x7922', 'eduloan4_int',
              'x7945', 'eduloan5_int',
              'x7968', 'eduloan6_int',
              'x7823', 'eduloan1_inst',     # Institution
              'x7846', 'eduloan2_inst',
              'x7869', 'eduloan3_inst',
              'x7923', 'eduloan4_inst',
              'x7946', 'eduloan5_inst',
              'x7969', 'eduloan6_inst',
              'x9203', 'eduloan1_insttype', # Institution type
              'x9204', 'eduloan2_insttype',
              'x9205', 'eduloan3_insttype',
              'x9206', 'eduloan4_insttype',
              'x9207', 'eduloan5_insttype',
              'x9208', 'eduloan6_insttype',
              'x7824', 'eduloan1_left',     # Amount still owed
              'x7847', 'eduloan2_left',
              'x7870', 'eduloan3_left',
              'x7924', 'eduloan4_left',
              'x7947', 'eduloan5_left',
              'x7970', 'eduloan6_left',
             # 'x7179', 'eduloano_left',     # Owed to other education loans
             # 'x7180', 'eduloano_pay',      # Payment amount
             # 'x7181', 'eduloano_payfreq',  # Payment frequency
              'x7182', 'consloan',   # Other consumer loans
              'x2709', 'consloan_num',      # Number of other consumer loans
              'x2710', 'consloan1_pur',     # Purpose
              'x2727', 'consloan2_pur',
              'x2810', 'consloan3_pur',
              'x2827', 'consloan4_pur',
              'x2910', 'consloan5_pur',
              'x2927', 'consloan6_pur',
              'x6842', 'consloan1_biz',     # Business loan
              'x6843', 'consloan2_biz',
              'x6844', 'consloan3_biz',
              'x6845', 'consloan4_biz',
              'x6846', 'consloan5_biz',
              'x6847', 'consloan6_biz',
              'x2713', 'consloan1_year',    # Year loan was taken
              'x2730', 'consloan2_year',
              'x2813', 'consloan3_year',
              'x2830', 'consloan4_year',
              'x2913', 'consloan5_year',
              'x2930', 'consloan6_year',
              'x2714', 'consloan1_amount',  # Amount financed
              'x2731', 'consloan2_amount',
              'x2814', 'consloan3_amount',
              'x2831', 'consloan4_amount',
              'x2914', 'consloan5_amount',
              'x2931', 'consloan6_amount',
              'x2715', 'consloan1_reg',     # Regular installments Y/N
              'x2732', 'consloan2_reg',
              'x2815', 'consloan3_reg',
              'x2832', 'consloan4_reg',
              'x2915', 'consloan5_reg',
              'x2932', 'consloan6_reg',
              'x2717', 'consloan1_maturity',      # Maturity in years
              'x2734', 'consloan2_maturity',
              'x2817', 'consloan3_maturity',
              'x2834', 'consloan4_maturity',
              'x2917', 'consloan5_maturity',
              'x2934', 'consloan6_maturity',
              'x2716', 'consloan1_npay',    # number of payments
              'x2733', 'consloan2_npay',
              'x2816', 'consloan3_npay',
              'x2833', 'consloan4_npay',
              'x2916', 'consloan5_npay',
              'x2933', 'consloan6_npay',
              'x2718', 'consloan1_pay',     # Payment amount
              'x2735', 'consloan2_pay',
              'x2818', 'consloan3_pay',
              'x2835', 'consloan4_pay',
              'x2918', 'consloan5_pay',
              'x2935', 'consloan6_pay',
              'x7527', 'consloan1_payfreq', # Payment amount
              'x7526', 'consloan2_payfreq',
              'x7525', 'consloan3_payfreq',
              'x7524', 'consloan4_payfreq',
              'x7523', 'consloan5_payfreq',
              'x7522', 'consloan6_payfreq',
              'x2719', 'consloan1_typicalpay',    # Typical payment amount
              'x2736', 'consloan2_typicalpay',
              'x2819', 'consloan3_typicalpay',
              'x2836', 'consloan4_typicalpay',
              'x2919', 'consloan5_typicalpay',
              'x2936', 'consloan6_typicalpay',
              'x2720', 'consloan1_typicalpayfreq',    # Typical payment frequency
              'x2737', 'consloan2_typicalpayfreq',
              'x2820', 'consloan3_typicalpayfreq',
              'x2837', 'consloan4_typicalpayfreq',
              'x2920', 'consloan5_typicalpayfreq',
              'x2937', 'consloan6_typicalpayfreq',
              'x7521', 'consloan1_schedule',      # Are you making payments on schedule?
              'x7520', 'consloan2_schedule',
              'x7519', 'consloan3_schedule',
              'x7518', 'consloan4_schedule',
              'x7517', 'consloan5_schedule',
              'x7516', 'consloan6_schedule',
              'x2722', 'consloan1_repay',   # Expected year of repay
              'x2739', 'consloan2_repay',
              'x2822', 'consloan3_repay',
              'x2839', 'consloan4_repay',
              'x2922', 'consloan5_repay',
              'x2939', 'consloan6_repay',
              'x2723', 'consloan1_left',    # Amount still owed
              'x2740', 'consloan2_left',
              'x2823', 'consloan3_left',
              'x2840', 'consloan4_left',
              'x2923', 'consloan5_left',
              'x2940', 'consloan6_left',
              'x2724', 'consloan1_int',     # Current interest
              'x2741', 'consloan2_int',
              'x2824', 'consloan3_int',
              'x2841', 'consloan4_int',
              'x2924', 'consloan5_int',
              'x2941', 'consloan6_int',
              'x2725', 'consloan1_inst',    # Institution
              'x2742', 'consloan2_inst',
              'x2825', 'consloan3_inst',
              'x2842', 'consloan4_inst',
              'x2925', 'consloan5_inst',
              'x2942', 'consloan6_inst',
              'x9107', 'consloan1_insttype',      # Institution type
              'x9108', 'consloan2_insttype',
              'x9109', 'consloan3_insttype',
              'x9110', 'consloan4_insttype',
              'x9111', 'consloan5_insttype',
              'x9112', 'consloan6_insttype',
             # 'x7183', 'consloano_left',    # Owed to other loans
             # 'x7184', 'consloano_pay',     # Payment amount
             # 'x7185', 'consloano_payfreq', # Payment frequency
              'x7063', 'payday',     # During past year, took payday loan?
              'x7064', 'payday_reason1',    # Purpose
              'x6365', 'payday_reason2'    # Purpose
              ));

# Stop if not all original or named columns are unique
stopifnot(all(table(cols[,1])==1));
stopifnot(all(table(cols[,2])==1));

# Merge implicates and select useful columns
dat <- setnames(rbindlist(list(imp1,imp2,imp3,imp4,imp5),
                          use.names=T)[,mget(cols[,1])],
                cols[,2]);
# Strip id from repid
dat[,repid:=repid-id*10];
# Check that all repid are valid
stopifnot(dat[,all(repid %in% 1:5)])

# Code sex
# male=1, female=2, inappropiate=0
dat[,sex:=as.factor(sex)];
dat[,sex_partner:=as.factor(sex_partner)];
dat[sex==0,sex:=NA];
dat[sex_partner==0,sex_partner:=NA];

levels(dat$sex) <- setNames(sex.names[,2],sex.names[,1])[levels(dat$sex)];
levels(dat$sex_partner) <- setNames(sex.names[,2],sex.names[,1])[levels(dat$sex_partner)];

# Code age
# Inappropiate=0
dat[age==0,age:=NA];
dat[age_partner==0,age_partner:=NA];

# Code income
# Nothing=-1, Negative=-9, inappropiate=0
dat[income %in% c(0,-9),income:=NA];
dat[income==-1,income:=0];
dat[income_total %in% c(0,-9),income_total:=NA];
dat[income_total==-1,income_total:=0];
dat[income_regular %in% c(0,-9),income_regular:=NA];
dat[income_regular==-1,income_regular:=0];

# Check that income is coded correctly
stopifnot(dat[!is.na(income),all(income>=0)]);
stopifnot(dat[!is.na(income_total),all(income_total>=0)]);
stopifnot(dat[!is.na(income_regular),all(income_regular>=0)]);

setkey(dat,id,repid);
