# Load required libraries
libs <- c('data.table');
for (i in libs) {
    if( !is.element(i, .packages(all.available=T))){
        capture.output(install.packages(i));
    }
    suppressMessages(require(i,character.only=T));
}
rm(i);

## High Quality Market Corporate Bond
# This is a zero-coupon rate.
# From the information supplied at treasury.gov, it seems to be semiannually compounded.
# https://www.treasury.gov/resource-center/economic-policy/corp-bond-yield/Pages/Corp-Yield-Bond-Curve-Papers.aspx
local({
  # Structure
  # Var name/security group by freq = (file of sec, maturity of sec, grade of sec)
  tmp.datafiles <- list(
    'hqm.p.m'=list( # Monthly, Par yield, by maturity
      c('../data/CORPBNDS_csv_2/data/HQMCB2YRP.csv'  ,  2 ),   #  2yr
      c('../data/CORPBNDS_csv_2/data/HQMCB5YRP.csv'  ,  5 ),   #  5yr
      c('../data/CORPBNDS_csv_2/data/HQMCB10YRP.csv'  , 10 ),  # 10yr
      c('../data/CORPBNDS_csv_2/data/HQMCB30YRP.csv'  , 30 )), # 30yr
    'hqm.m'=list( # Monthly, Spot rate, by maturity
      c('../data/CORPBNDS_csv_2/data/HQMCB6MT.csv'  ,   0.5 ), #   6   Month
      c('../data/CORPBNDS_csv_2/data/HQMCB1YR.csv'  ,   1   ), #   1   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB1Y6M.csv' ,   1.5 ), #   1.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB2YR.csv'  ,   2   ), #   2   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB2Y6M.csv' ,   2.5 ), #   2.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB3YR.csv'  ,   3   ), #   3   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB3Y6M.csv' ,   3.5 ), #   3.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB4YR.csv'  ,   4   ), #   4   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB4Y6M.csv' ,   4.5 ), #   4.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB5YR.csv'  ,   5   ), #   5   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB5Y6M.csv' ,   5.5 ), #   5.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB6YR.csv'  ,   6   ), #   6   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB6Y6M.csv' ,   6.5 ), #   6.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB7YR.csv'  ,   7   ), #   7   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB7Y6M.csv' ,   7.5 ), #   7.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB8YR.csv'  ,   8   ), #   8   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB8Y6M.csv' ,   8.5 ), #   8.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB9YR.csv'  ,   9   ), #   9   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB9Y6M.csv' ,   9.5 ), #   9.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB10Y6M.csv',  10.5 ), #  10.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB10YR.csv' ,  10   ), #  10   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB11Y6M.csv',  11.5 ), #  11.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB11YR.csv' ,  11   ), #  11   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB12Y6M.csv',  12.5 ), #  12.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB12YR.csv' ,  12   ), #  12   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB13Y6M.csv',  13.5 ), #  13.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB13YR.csv' ,  13   ), #  13   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB14Y6M.csv',  14.5 ), #  14.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB14YR.csv' ,  14   ), #  14   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB15Y6M.csv',  15.5 ), #  15.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB15YR.csv' ,  15   ), #  15   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB16Y6M.csv',  16.5 ), #  16.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB16YR.csv' ,  16   ), #  16   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB17Y6M.csv',  17.5 ), #  17.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB17YR.csv' ,  17   ), #  17   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB18Y6M.csv',  18.5 ), #  18.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB18YR.csv' ,  18   ), #  18   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB19Y6M.csv',  19.5 ), #  19.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB19YR.csv' ,  19   ), #  19   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB20Y6M.csv',  20.5 ), #  20.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB20YR.csv' ,  20   ), #  20   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB21Y6M.csv',  21.5 ), #  21.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB21YR.csv' ,  21   ), #  21   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB22Y6M.csv',  22.5 ), #  22.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB22YR.csv' ,  22   ), #  22   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB23Y6M.csv',  23.5 ), #  23.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB23YR.csv' ,  23   ), #  23   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB24Y6M.csv',  24.5 ), #  24.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB24YR.csv' ,  24   ), #  24   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB25Y6M.csv',  25.5 ), #  25.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB25YR.csv' ,  25   ), #  25   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB26Y6M.csv',  26.5 ), #  26.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB26YR.csv' ,  26   ), #  26   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB27Y6M.csv',  27.5 ), #  27.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB27YR.csv' ,  27   ), #  27   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB28Y6M.csv',  28.5 ), #  28.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB28YR.csv' ,  28   ), #  28   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB29Y6M.csv',  29.5 ), #  29.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB29YR.csv' ,  29   ), #  29   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB30Y6M.csv',  30.5 ), #  30.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB30YR.csv' ,  30   ), #  30   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB31Y6M.csv',  31.5 ), #  31.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB31YR.csv' ,  31   ), #  31   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB32Y6M.csv',  32.5 ), #  32.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB32YR.csv' ,  32   ), #  32   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB33Y6M.csv',  33.5 ), #  33.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB33YR.csv' ,  33   ), #  33   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB34Y6M.csv',  34.5 ), #  34.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB34YR.csv' ,  34   ), #  34   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB35Y6M.csv',  35.5 ), #  35.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB35YR.csv' ,  35   ), #  35   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB36Y6M.csv',  36.5 ), #  36.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB36YR.csv' ,  36   ), #  36   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB37Y6M.csv',  37.5 ), #  37.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB37YR.csv' ,  37   ), #  37   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB38Y6M.csv',  38.5 ), #  38.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB38YR.csv' ,  38   ), #  38   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB39Y6M.csv',  39.5 ), #  39.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB39YR.csv' ,  39   ), #  39   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB40Y6M.csv',  40.5 ), #  40.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB40YR.csv' ,  40   ), #  40   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB41Y6M.csv',  41.5 ), #  41.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB41YR.csv' ,  41   ), #  41   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB42Y6M.csv',  42.5 ), #  42.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB42YR.csv' ,  42   ), #  42   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB43Y6M.csv',  43.5 ), #  43.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB43YR.csv' ,  43   ), #  43   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB44Y6M.csv',  44.5 ), #  44.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB44YR.csv' ,  44   ), #  44   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB45Y6M.csv',  45.5 ), #  45.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB45YR.csv' ,  45   ), #  45   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB46Y6M.csv',  46.5 ), #  46.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB46YR.csv' ,  46   ), #  46   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB47Y6M.csv',  47.5 ), #  47.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB47YR.csv' ,  47   ), #  47   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB48Y6M.csv',  48.5 ), #  48.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB48YR.csv' ,  48   ), #  48   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB49Y6M.csv',  49.5 ), #  49.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB49YR.csv' ,  49   ), #  49   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB50Y6M.csv',  50.5 ), #  50.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB50YR.csv' ,  50   ), #  50   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB51Y6M.csv',  51.5 ), #  51.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB51YR.csv' ,  51   ), #  51   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB52Y6M.csv',  52.5 ), #  52.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB52YR.csv' ,  52   ), #  52   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB53Y6M.csv',  53.5 ), #  53.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB53YR.csv' ,  53   ), #  53   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB54Y6M.csv',  54.5 ), #  54.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB54YR.csv' ,  54   ), #  54   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB55Y6M.csv',  55.5 ), #  55.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB55YR.csv' ,  55   ), #  55   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB56Y6M.csv',  56.5 ), #  56.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB56YR.csv' ,  56   ), #  56   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB57Y6M.csv',  57.5 ), #  57.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB57YR.csv' ,  57   ), #  57   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB58Y6M.csv',  58.5 ), #  58.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB58YR.csv' ,  58   ), #  58   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB59Y6M.csv',  59.5 ), #  59.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB59YR.csv' ,  59   ), #  59   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB60Y6M.csv',  60.5 ), #  60.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB60YR.csv' ,  60   ), #  60   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB61Y6M.csv',  61.5 ), #  61.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB61YR.csv' ,  61   ), #  61   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB62Y6M.csv',  62.5 ), #  62.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB62YR.csv' ,  62   ), #  62   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB63Y6M.csv',  63.5 ), #  63.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB63YR.csv' ,  63   ), #  63   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB64Y6M.csv',  64.5 ), #  64.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB64YR.csv' ,  64   ), #  64   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB65Y6M.csv',  65.5 ), #  65.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB65YR.csv' ,  65   ), #  65   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB66Y6M.csv',  66.5 ), #  66.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB66YR.csv' ,  66   ), #  66   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB67Y6M.csv',  67.5 ), #  67.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB67YR.csv' ,  67   ), #  67   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB68Y6M.csv',  68.5 ), #  68.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB68YR.csv' ,  68   ), #  68   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB69Y6M.csv',  69.5 ), #  69.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB69YR.csv' ,  69   ), #  69   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB70Y6M.csv',  70.5 ), #  70.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB70YR.csv' ,  70   ), #  70   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB71Y6M.csv',  71.5 ), #  71.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB71YR.csv' ,  71   ), #  71   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB72Y6M.csv',  72.5 ), #  72.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB72YR.csv' ,  72   ), #  72   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB73Y6M.csv',  73.5 ), #  73.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB73YR.csv' ,  73   ), #  73   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB74Y6M.csv',  74.5 ), #  74.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB74YR.csv' ,  74   ), #  74   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB75Y6M.csv',  75.5 ), #  75.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB75YR.csv' ,  75   ), #  75   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB76Y6M.csv',  76.5 ), #  76.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB76YR.csv' ,  76   ), #  76   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB77Y6M.csv',  77.5 ), #  77.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB77YR.csv' ,  77   ), #  77   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB78Y6M.csv',  78.5 ), #  78.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB78YR.csv' ,  78   ), #  78   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB79Y6M.csv',  79.5 ), #  79.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB79YR.csv' ,  79   ), #  79   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB80Y6M.csv',  80.5 ), #  80.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB80YR.csv' ,  80   ), #  80   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB81Y6M.csv',  81.5 ), #  81.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB81YR.csv' ,  81   ), #  81   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB82Y6M.csv',  82.5 ), #  82.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB82YR.csv' ,  82   ), #  82   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB83Y6M.csv',  83.5 ), #  83.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB83YR.csv' ,  83   ), #  83   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB84Y6M.csv',  84.5 ), #  84.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB84YR.csv' ,  84   ), #  84   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB85Y6M.csv',  85.5 ), #  85.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB85YR.csv' ,  85   ), #  85   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB86Y6M.csv',  86.5 ), #  86.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB86YR.csv' ,  86   ), #  86   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB87Y6M.csv',  87.5 ), #  87.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB87YR.csv' ,  87   ), #  87   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB88Y6M.csv',  88.5 ), #  88.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB88YR.csv' ,  88   ), #  88   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB89Y6M.csv',  89.5 ), #  89.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB89YR.csv' ,  89   ), #  89   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB90Y6M.csv',  90.5 ), #  90.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB90YR.csv' ,  90   ), #  90   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB91Y6M.csv',  91.5 ), #  91.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB91YR.csv' ,  91   ), #  91   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB92Y6M.csv',  92.5 ), #  92.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB92YR.csv' ,  92   ), #  92   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB93Y6M.csv',  93.5 ), #  93.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB93YR.csv' ,  93   ), #  93   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB94Y6M.csv',  94.5 ), #  94.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB94YR.csv' ,  94   ), #  94   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB95Y6M.csv',  95.5 ), #  95.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB95YR.csv' ,  95   ), #  95   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB96Y6M.csv',  96.5 ), #  96.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB96YR.csv' ,  96   ), #  96   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB97Y6M.csv',  97.5 ), #  97.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB97YR.csv' ,  97   ), #  97   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB98Y6M.csv',  98.5 ), #  98.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB98YR.csv' ,  98   ), #  98   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB99Y6M.csv',  99.5 ), #  99.5 Year
      c('../data/CORPBNDS_csv_2/data/HQMCB99YR.csv' ,  99   ), #  99   Year
      c('../data/CORPBNDS_csv_2/data/HQMCB100YR.csv', 100   )) # 100   Year
    );

  for (dd.group in names(tmp.datafiles)) {
    # Load and merge security data
    tmp <- NULL;
    for (dd in tmp.datafiles[[dd.group]]) {
      tmp <- rbindlist(list(tmp, setnames(fread(dd[[1]],na.strings=c('.'))[,
          maturity:=as.factor(dd[[2]])],
        c('date','int','maturity'))), use.names=T);
    }
    # Remove missing values and reorder columns
    tmp <- setcolorder(tmp[!is.na(int),],c('date','maturity','int'));
    # Correct format and order
    tmp[,c('date','maturity','int'):=
         .(as.Date(date),as.numeric(maturity),as.numeric(int))];

    # This is a zero-coupon rate. Annual rate but semiannually compounded.

    # Order data
    setkey(tmp,date,maturity);

    # Export merged file to the global namespace
    assign(dd.group,tmp,envir=.GlobalEnv)
  }
});
