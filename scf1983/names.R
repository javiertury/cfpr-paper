libs <- c('data.table');
for (i in libs) {
    if( !is.element(i, .packages(all.available=T))){
        capture.output(install.packages(i));
    }
    suppressMessages(require(i,character.only=T));
}
rm(i);

# Change purpose codes to description
pur.names <- matrix(ncol=2,byrow=T,
  data=c( 0, 'INAP, no loan',
          1, 'purchase home (current residence)',
          2, 'purchase home (current residence) if mobile home',
          3, 'home improvement or addition (including assessment for sewer)',
          4, 'home repairs, upkeep, maintenance',
          6, 'new automobile',
          7, 'used automobile',
          8, 'new truck or utility vehicle',
          9, 'used truck or utility vehicle',
         11, 'refrigerator',
         12, 'stove-range, microwave oven',
         14, 'freezer',
         15, 'air conditioner',
         16, 'washing machine, washer dryer combination',
         17, 'dryer',
         18, 'furniture, lamps, mattress and spring combos',
         19, 'rug, carpet',
         20, 'vacuum cleaner',
         21, 'sewing machine',
         22, 'typewriter (manual or electric)',
         23, 'home computer, calculator, computer terminal',
         25, 'combination of appliances (including TV); appliance NA type',
         26, 'furniture and appliance combinations',
         27, 'furniture and carpet combinations',
         28, 'curtains, drapes, china, other small household goods/furnishings',
         29, 'other appliances or durable goods',
         31, 'stereo, phonograph (may include radio), sound equipment, amp',
         34, 'piano, organ',
         35, 'musical instruments (except pianos or organs)',
         36, 'TV -- NA color or black and white',
         37, 'color TV',
         39, 'home entertainment center, (incl. combination TV, radio, phonograph, etc.); beta max, video cassette recorder/player',
         41, 'camera; camera equipment (incl. lighting apparatus,enlarger)',
         49, 'other small/indoor hobby or entertainment items (e.g. pool tables)',
         51, 'power tools (hand held or stationary) -- electric drill radial arm or chain saw, belt sander, router',
         52, 'yard equipment, lawn mower, snow blower, roto-tiller',
         53, 'tractor, self propelled construction/farming devices (non-busi.)',
         59, 'other tools',
         61, 'boat; boating equipment (incl. trailer)',
         62, 'bicycle, moped',
         63, 'motorcycle',
         64, 'snowmobile, off-road vehicles',
         65, 'camper-trailers (excluding self-propelled campers)',
         66, 'mobile homes (not current residence); self-propelled campers',
         67, 'cottage, vacation property',
         69, 'other outdoor recreation items',
         71, 'stamp/coin collection; antique-classic car (incl. other similar asset collections)',
         72, 'investment real estate (incl. cemetery plots)',
         79, 'other investments',
         81, 'travel/vacation expenses',
         82, 'medical/dental expenses',
         83, 'education/school expenses',
         84, 'tax and insurance expenses (except for vehicles)',
         85, 'weddings, funerals, combinations',
         86, 'encyclopedias, health clubs, spas',
         90, 'personal loans - NA what for',
         91, 'living/general expenses; bill consolidation; moving expenses',
         92, 'personal items (incl. clothing or jewelry)',
         93, 'vehicle repair/upkeep (incl. insurance)',
         94, 'gifts; goods or gifts of money; christmas',
         97, 'other purpose'));

# Purpose codes grouped in the same way as public purpose codes of SCF 2013
pur.names.simple <- rbindlist(list(
  # home purchase/construction, cottage, vacation property, mobile homes, seasonal residence, motor home, second home
  data.table(code=c(1:2,67),
             name='home'),
  # home improvement or repairs
  data.table(code=c(3:4),
             name='other home'),
  # car, truck, jeep or utility vehicle
  data.table(code=c(6:9),
             name='vehicles'),
  # furniture, home appliances or durable goods
  data.table(code=c(11:19,20:22,24:29),
             name='home appliances'),
  # home entertainment, computer, musical instruments, tv, hobby, power tools or outdoor recreation
  data.table(code=c(23,31:49,51:59,69),
             name='entertainment'),
  # boat, airplane, motorcycle, bycicle, moped, snowmobile, off-road vehicle, camper-trailer, or RV
  data.table(code=c(61:66),
             name='other vehicles'),
  # business investment, assets, stock/bonds, gold, real state investment, cash reserve
  data.table(code=c(71:79),
             name='investment'),
  # divorce, travel/vacation, weddings/funerals/occasions, moving, special/encyclopedia/health membership
  data.table(code=c(81,85:86),
             name='family expenses'),
  # medical/dental/veterinary or education/school
  data.table(code=c(82:83),
             name='important expenses'),
  # tax and insurance, legal, personal, debt consolidation, vehicle rapir, gift, living/general, loans made to others, charities/polical
  data.table(code=c(84,90:97),
             name='other')
  ), use.names=T);

pur.names.reduced <- rbindlist(list(
  data.table(code=c(1),
             name='home'),
  data.table(code=c(2:4,67),
             name='other home'),
  # Items that can be productive to earn money
  data.table(code=c(6:9,93,51:59),
             name='vehicles and tools'),
  # Items that likely won't earn money
  data.table(code=c(10:29,31:49,61:66,69,81,86),
             name='hosehold appliances, durable goods, entertainment and consumption'),
  data.table(code=c(91:92,94),
             name='living'),
  data.table(code=c(71:79),
             name='investment'),
  data.table(code=c(83:84),
             name='major predictable exp: education, taxes, insurance'),
  # Other includes: major unpredictable exp like medic and family
  data.table(code=c(82,85,90,97,0),
             name='other')
  ), use.names=T);

# Payment frequency code to description or number
payfreq.names <- matrix(ncol=2,byrow=T,
  data=c(3, 'weekly',
         4, 'bi-weekly',
         5, 'monthly',
         6, 'yearly',
         7, 'every three months',
         8, 'no regular payments'));

payfreq.ypayfreq <- matrix(ncol=2,byrow=T,
data=c(3, 365.25/7    , # Weekly
       4, (365.25/7)/2, # Bi-weekly
       5, 12       , # Monthly
       6, 1        , # Yearly
       7, 4        , # Every 3 months
       8, NA_real_  ));

#payfreq.ypayfreq <- matrix(ncol=2,byrow=T,
#  data=c('weekly'             , 365/7    , 
#         'bi-weekly'          , (365/7)*2, 
#         'monthly'            , 12       , 
#         'yearly'             , 1        , 
#         'every three months' , 4        , 
#         'no regular payments', NA_real_  ));

# Code of source/insttype to description
insttype.names <- matrix(ncol=2,byrow=T,
  data=c( 1, 'commercial bank',
          2, 'savings and loan association or savings bank',
          3, 'credit union',
          4, 'finance or loan company',
          5, 'store or dealer',
          6, 'brokerage company',
          7, 'insurance company',
          8, 'mortgage company',
          9, 'contractor or developer',
         10, 'prior owner',
         11, 'automobile finance company',
         12, 'doctor or hospital, dentist',
         13, 'lawyer',
         15, 'employer',
         16, 'friend or relative (not codable above)',
         17, 'individual lender (not codable above)',
         21, 'real estate investment company',
         22, 'school, college, university',
         23, 'local, county, state government',
         24, 'Federal government: FMHA, SBA, VA, FHA, HUD, NDSL',
         25, 'other Federal government, IRS',
         30, 'church',
         32, 'courts',
         93, 'farm related lenders (not codable above)',
         94, 'investment, management, or consulting companies',
         96, 'combinations',
         97, 'other lender'));

insttype.names.simple <- rbindlist(list(
  data.table(code=c(1:3),
             name='bank'),
  data.table(code=c(4,6,7,8,11,21,93:96),
             name='finco'),
  data.table(code=c(5,9,10,12:17,22:25,30:32,97),
             name='other')
  ), use.names=T);

# Savings institutions: Savings bank, commercial bank,
# credit unions and savings/loan associations 

# Financial credit company
# Some insurance companies allow to borrow against life insurance benefits. It's
# equivalent to borrow from yourself. However this doesn't seem to be very common.
# Most insurance companies offer fixed rate commercial loans(mortgages) to
# compensate potential insurance liability claims.

enforce.insttype <- list(
  #Federal Reserve Board(FRB): member banks of the federal reserve and foreign banks
  # Office of the Comptroller of the Currency(OCC): national banks
  'frb or occ'=c(1),
  # Office of Thrift Supervision(OTS): Saving associations
  ots=c(2),
  # National Credit Union Administration(NCUA): Federal credit unions
  ncua=c(3),
  # Secretary of Transportation: air carriers
  #st=c(),
  ## Government, maybe unenforceable?
  #gov=c(23:25,32),
  ## Federal Trade Comission(FTC): the rest(default)
  # 'ftc'=c(4,8,11),
  # 'ftc(nonfin)'=c(5,6,7,9:10,12:15,21,22,93:96),
  # 'ftc(nonbiz)'=c(16:17,30,97)
  'ftc'=c(4:97)
  );

biztype.insttype <- list(
  # Depository institutions
  depository=c(1:3),
  # Financial companies
  financial=c(4,8,11),
  # Businesses that provide loans for the consumption of standard
  # products and services they produce.
  consumer=c(5,9,12,22),
  # Government
  gov=c(23:25,32),
  # Borker(profesional) and insurance(risk mgmt) loans are very special.
  # Other categories are small or not conventional loans.
  other=c(16:17,30,96,97,6,7,13,15,10,21,93,94)
  );

# Code of place to description
place.names <- matrix(ncol=2,byrow=T,
  data=c( 1, 'commercial bank',
          2, 'savings and loan association or savings bank',
          3, 'credit union',
          4, 'finance or loan company',
          5, 'store or dealer',
          7, 'insurance company (summed separately for irregular payments)',
          9, 'contractor or developer',
         13, 'lawyer',
         16, 'friend or relative (not codable above)',
         17, 'individual lender (not codable above)',
         22, 'school, college, university',
         23, 'local, county, state government',
         26, 'at home',
         97, 'other location'));
