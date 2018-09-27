# *sex*, code sex
sex.names <- matrix(ncol=2,byrow=T,
  data=c(1, 'male',
         2, 'female'));

# *pur*, code purpose from MASTER LOAN PURPOSE LIST
pur.names <- matrix(ncol=2,byrow=T,
  data=c(
          1,     'Own home purchase/construction',
          3,     'Home improvements or additions (incl. assessments for sewer/sidewalk, etc.)',
          4,     'Home repairs/maintenance/upkeep',
         10,     'Car, including repossessed car',
         11,     'Refrigerator',
         12,     'Stove/range; microwave oven',
         13,     'Dishwasher',
         14,     'Freezer',
         15,     'Air conditioner; furnace',
         16,     'Washing machine (incl. washer/dryer combination)',
         17,     'Dryer',
         18,     'Furniture (excluding pianos and organs -- see code 34); lamps; mattress and spring combinations; rug and/or carpet; other household furnishings',
         20,     'Vacuum cleaners',
         23,     'Home computer; calculator; computer terminal',
         24,     'Truck/jeep/utility vehicle',
         25,     'Combination of appliances (incl. TV); appliances -- NA type',
         26,     'Combination of furniture and appliances',
         29,     'Other appliances or durable goods; sewing machine; typewriter',
         31,     'Stereo; phonograph (may include radio); include sound equipment; amplifiers here; radio (AM or FM); tuner; CB equipment; tape recorder, tape player (cassette or reel-to-reel); CD player',
         34,     'Piano; Organ',
         35,     'Musical instruments (excl. piano and organ)',
         36,     'TV -- color or black and white; home entertainment center (including combination TV, radio, phonograph); video cassette recorder/player (VCR); video camera (Cam-corder); satellite dish',
         49,     'Other small/indoor hobby, recreation, and entertainment items (incl. pool tables and regular cameras)',
         50,     'Power tools and yard equipment',
         61,     'Boat; boating equipment (incl. trailer), airplane, airplane equipment',
         63,     'Motorcycles; bicycle; moped; snowmobiles; off-road vehicles',
         65,     'Camper-trailers; RV, n.f.s.',
         67,     'Cottage; vacation property; mobile homes -- seasonal residence (if current residence, code 01); motor home; second home',
         69,     'Other outdoor recreation items; horse',
         74,     'Invest in own business',
         75,     'Business investment (exc. 74), incl. businesses now defunct',
         76,     'Other asset investment; bought stocks/bonds; IRA deposit; gold; investment, n.f.s.',
         78,     'Investment real estate (incl. cemetery plots and additions and repairs to investment property); farmland (exc. 74); vacation property',
         79,     'To have cash reserve',
         80,     'Divorce/separation expenses',
         81,     'Travel/vacation expenses',
         82,     'Medical/dental/veterinary expenses; attorneys fees',
         83,     'Education/school expenses',
         84,     'Tax and insurance expenses (exc. vehicle, code 93)',
         85,     'Weddings/funerals/other occasions',
         86,     'Legal judgment against R; money owed on overpayment of benefits',
         88,     'Moving expenses',
         89,     'Other special expenses; encyclopedia; health membership',
         90,     'Personal loan--NA what for',
         91,     'Bill/debt consolidation; bills',
         92,     'Personal items, incl. clothing, jewelry',
         93,     'Vehicle repair/upkeep (incl. insurance)',
         94,     'Gifts; goods or gifts of money; Christmas',
         95,     'Living/general expenses',
         96,     'Loans made to others; loaned friend/son money for a house',
         97,     'Charitable or political contributions',
         -7,     'Other (including combinations)'
));
colnames(pur.names) <- c('code','purpose');

pur.names.public <- matrix(ncol=2,byrow=T,
  data=c(
          # home purchase/construction, cottage, vacation property, mobile homes, seasonal residence, motor home, second home
          1,    'home',
          # home improvement or repairs
          2,    'home reform',
          # car, truck, jeep or utility vehicle
          3,    'vehicles',
          # furniture, home appliances or durable goods
          4,    'home appliances',
          # home entertainment, computer, musical instruments, tv, hobby, power tools or outdoor recreation
          5,    'entertainment',
          # boat, airplane, motorcycle, bycicle, moped, snowmobile, off-road vehicle, camper-trailer, or RV
          6,    'other vehicles',
          # business investment, assets, stock/bonds, gold, real state investment, cash reserve
          7,    'investment',
          # divorce, travel/vacation, weddings/funerals/occasions, moving, special/encyclopedia/health membership
          8,    'family expenses',
          # medical/dental/veterinary or education/school
          9,    'important expenses',
          # tax and insurance, legal, personal, debt consolidation, vehicle rapir, gift, living/general, loans made to others, charities/polical
         10,    'other expenses'
         ));
colnames(pur.names.public) <- c('code','purpose');

# Custom purpose codes, splitting education and medical purposes if possible
pur.names.custom <- rbind(pur.names.public,
                          c(83,'education'),
                          c(82,'medical'));
  
# Frequency code to description
payfreq.names <- matrix(ncol=2,byrow=T,
  data=c(
          2,    'Week',
          3,    'Every two weeks',
          4,    'Month',
          5,    'Quarter',
          6,    'Year',
          8,    'Lump sum/one payment only',
         11,    'Twice per year',
         12,    'Every two months',
         22,    'Varies',
         23,    '13 times a year; every 4 weeks',
         24,    'Every 6 weeks',
         31,    'Twice a month',
         -1,    'None',
         -2,    'NO TYPICAL PAYMENT',
         -7,    '*Other'
));
colnames(payfreq.names) <- c('code','frequency');
  
# Payment on schedule code to description
schedule.names <- matrix(ncol=2,byrow=T,
  data=c(
         1,    'On schedule',
         2,    'Ahead of schedule',
         3,    'Behind schedule '
));

# Institution code to description
inst.names <- matrix(ncol=2,byrow=T,
  data=c(
          1, 'Institution 1',
          2, 'Institution 2',
          3, 'Institution 3',
          4, 'Institution 4',
          5, 'Institution 5',
          6, 'Institution 6',
          7, 'Institution 7',
         11, 'COMMERCIAL BANK; trust company',
         12, 'SAVINGS AND LOAN OR SAVINGS BANK',
         13, 'CREDIT UNION',
         14, 'FINANCE OR LOAN COMPANY',
         15, 'Store or other business; dealer; utility company',
         16, 'BROKERAGE; mutual fund, hedge fund, n.f.s.; also include general financial service companies that have group membership restrictions (e.g., TIAA/CREF)',
         17, 'Insurance company',
         18, 'MORTGAGE COMPANY; mortgage broker',
         19, 'Contractor or developer; trailer park owner',
         20, 'Prior owner',
         21, 'Automobile finance company; GMAC, Ford Credit',
         22, 'Doctor or hospital; dentist; veterinarian',
         23, 'Lawyer',
         24, 'Accountant',
         25, 'Employer; former employer',
         26, 'Friend or Relative (not codeable above)',
         27, 'Individual formal lender/adviser (not codeable above)',
         28, 'Pension Administrator',
         29, 'BROAD FINANCIAL SERVICES COMPANY n.e.c.',
         30, 'Internet-based businesses, n.e.c. (note: excludes code 101)',
         31, 'Real estate (investment) company; includes land trusts',
         32, 'School/college/university',
         33, 'Local/county/state government (except Courts code 42)',
         34, 'Special federal government agency; FMHA, SBA, VA, FHA, HUD, NDSL',
         35, 'Federal government general or NA agency; IRS',
         36, 'Fiduciary/advisor, n.e.c.',
         37, 'Self/spouse/partner (manages own trust)',
         38, 'Bank or general purpose credit card company; Visa, Carte Blanche, Master Card (except American Express code 51)',
         39, 'Union',
         40, 'Church',
         41, 'American Association of Retired Persons (AARP)',
         42, 'Courts',
         43, 'Collection agency; loan liquidator',
         44, 'Cooperative organization; co-op; agricultural cooperative lending associations (FCS)',
         45, 'Specialized education lender, n.e.c.',
         46, 'Family trust; trust fund; charitable remainder trust',
         47, 'Fraternal organization',
         50, 'Discover card/Novus (for X415 etc. only; Sears only, use code 15)',
         51, 'American Express/Optima card',
         52, 'AT&T card',
         53, 'Gasoline company',
         56, 'Leasing company',
         57, 'Airline, hotel',
         61, 'Other membership organization; AAA, NEA, NTA (X415 etc. only)',
         62, 'Tribal and similar organizations',
         75, 'Foreign institution type',
         80, 'Direct student loan, n.e.c. (include references to Stafford, Perkins, Ford, etc. student loans when a more specific institution reference is not available).',
         81, 'Nonprofit credit counseling service',
         85, 'Ex-spouse',
         92, 'Money market (mutual) funds, n.f.s.',
         93, 'Farm-related lenders (not codeable above)',
         94, 'Investment/management companies or consultants, n.e.c.; include specialized institutions providing private banking and investment services to individuals',
         95, 'Non-financial institution (except codes 40-42 and 61)',
         96, 'Individual, n.e.c.',
        101, 'Internet-based or other bill paying service',
         -1, 'NO FINANCIAL INSTITUTIONS LISTED',
         -7, 'A PERSON OR OTHER NON-INSTITUTION'
));
colnames(inst.names) <- c('code','institution');

## Public dataset combinations
## Debt Collection, counseling
#42,43,81 -> 95
## Membership orgs.
#39,40,41,62 -> 61
## Acountant
#23,36 -> 24
## Government
#33,34 -> 35
## Bank or credit card
#50:53 -> 38
## Broad financial services -> Commercial bank
# 29 -> 11
## Internet business
# 101 -> 30

# Simplified institution types
inst.names.simple <- rbindlist(list(
  # Depository institution(Banks)
  data.table(code=c(11,12),
             name='depository'),
  data.table(code=c(13),
             name='credit union'),
  # Financial company. Standard financing, not leasing, cc or others
  data.table(code=c(14,18,21,29),
             name='financial'),
  # Specialized education lender
  data.table(code=c(45),
             name='specialized education lender'),
  # Credit cards(issued by bank or others)
  data.table(code=c(38,50:53),
             name='credit card'),
  # Leasing
  data.table(code=c(56),
             name='leasing'),
  # Investment
  data.table(code=c(16,31,92,94),
             name='investment'),
  # Government institutions and programs
  data.table(code=c(33:35,80),
             name='government'),
  # Business, loan to finance a product/service provided to consumers
  data.table(code=c(15,19,22,32,57),
             name='business-sales'),
  # Business, other. Extend loans but not to consumers, debt collection,...
  # Likely that foreign institutions are for-profit
  data.table(code=c(17,23,24,36,25,27,28,30,101,42,43,81,75),
             name='business'),
  # Other. Individuals, non-profits...
  data.table(code=c(20,26,37,39:41,61:62,95,44,46,47,81,85,93,95,96,-1,-7),
             name='other')
  ), use.names=T);

inst.names.reduced <- inst.names.simple;
inst.names.reduced[name %in% c('credit card','leasing','investment'),
                   name:='other financial'];
inst.names.reduced[,unique(name)];

payfreq.ypayfreq <- matrix(ncol=2,byrow=T,
data=c(2, 365.25/7      , # Weekly
       3, (365.25/7)/2  , # Every two weeks
       4, 12            , # Monthly
       5, 4             , # Quarterly
       6, 1             , # Yearly
       # Careful with this, rely on npay or maturity
       8, -1            , # Lum sum/one payment only.
       11, 2            , # Twice per year; semiannual
       12, 12/2         , # Every two months
       #22, 13           , # Varies
       23, 13           , # 13 times a year; every 4 weeks
       24, (365.25/7)/6 , # Every six weeks
       31, 12*2           # Twice a month
       ));
colnames(payfreq.ypayfreq) <- c('code','ypayfreq');

payincludes.names <- matrix(ncol=2, byrow=T,
  data=c(1,'taxes only',
         2,'insurance only',
         3,'taxes and insurance',
         4,'nothing'));
colnames(payincludes.names) <- c('code','includes');

govprogram.names <- matrix(ncol=2, byrow=T,
  data=c( 1,    'Federal Housing Administration(FHA)',
          2,    'Veterans Administration(VA)',
          3,    'Federal land bank',
          4,    'Federal National Mortgage Association (Fannie Mae)',
          5,    'Federal Home Loan Mortgage Corp. (Freddie Mac)',
         10,    'State housing programs',
         11,    'First-time buyer program, n.e.c.',
         12,    'Other Federal loan program',
         -7,    'Other'));
colnames(govprogram.names) <- c('code','program');
govprogram.names.public <- matrix(ncol=2, byrow=T,
  data=c( 1,    'Federal Housing Administration(FHA)',
          5,    'Fannie Mae or Freddie Mac',
         12,    'Other Federal loan programs',
         -7,    'Other'));
colnames(govprogram.names.public) <- c('code','program');

enforce.insttype <- list(
  #Federal Reserve Board(FRB): member banks of the federal reserve and foreign banks
  # Office of the Comptroller of the Currency(OCC): national banks
  'frb or occ'=c(11),
  # Office of Thrift Supervision(OTS): Saving associations
  ots=c(12),
  # National Credit Union Administration(NCUA): Federal credit unions
  ncua=c(13),
  # Secretary of Transportation: air carriers
  st=c(57),
  ## Government, maybe unenforceable?
  #gov=c(23:25,32),
  ## Federal Trade Comission(FTC): the rest(default)
  'ftc'=c(14:56,58:101,-1,-7)
  );
