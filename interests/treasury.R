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

## Treasury Securities, Constant Maturity
# Constant maturity data from the FED was already interpolated with
# 1mo=30days and 1yr=360days as reference.
# https://www.treasury.gov/resource-center/faqs/Interest-Rates/Pages/faq.aspx
# These rates are based on semianual interest payments, even those maturing in 1 year or less.
# Annual rates semi-annually compounded.
local({
  # Structure
  # Var name/security group by freq = (file of sec, maturity of sec in days)
  tmp.datafiles <- list(
    ## Constant Maturity Securities
    'treasury.d'=list( # Daily
      c('../data/IRTCM_csv_2/data/DGS1MO.csv',1/12),  #  1mo
      c('../data/IRTCM_csv_2/data/DGS3MO.csv',3/12),  #  3mo
      c('../data/IRTCM_csv_2/data/DGS6MO.csv',6/12),  #  6mo
      c('../data/IRTCM_csv_2/data/DGS1.csv'  ,   1),  #  1yr
      c('../data/IRTCM_csv_2/data/DGS2.csv'  ,   2),  #  2yr
      c('../data/IRTCM_csv_2/data/DGS3.csv'  ,   3),  #  3yr
      c('../data/IRTCM_csv_2/data/DGS5.csv'  ,   5),  #  5yr
      c('../data/IRTCM_csv_2/data/DGS7.csv'  ,   7),  #  7yr
      c('../data/IRTCM_csv_2/data/DGS10.csv' ,  10),  # 10yr
      c('../data/IRTCM_csv_2/data/DGS20.csv' ,  20),  # 20yr
      c('../data/IRTCM_csv_2/data/DGS30.csv' ,  30)), # 30yr
    'treasury.w'=list( # Weekly
      c('../data/IRTCM_csv_2/data/WGS1MO.csv' ,1/12),  #  1mo
      c('../data/IRTCM_csv_2/data/WGS3MO.csv' ,3/12),  #  3mo
      c('../data/IRTCM_csv_2/data/WGS6MO.csv' ,6/12),  #  6mo
      c('../data/IRTCM_csv_2/data/WGS1YR.csv' ,   1),  #  1yr
      c('../data/IRTCM_csv_2/data/WGS2YR.csv' ,   2),  #  2yr
      c('../data/IRTCM_csv_2/data/WGS3YR.csv' ,   3),  #  3yr
      c('../data/IRTCM_csv_2/data/WGS5YR.csv' ,   5),  #  5yr
      c('../data/IRTCM_csv_2/data/WGS7YR.csv' ,   7),  #  7yr
      c('../data/IRTCM_csv_2/data/WGS10YR.csv',  10),  # 10yr
      c('../data/IRTCM_csv_2/data/WGS20YR.csv',  20),  # 20yr
      c('../data/IRTCM_csv_2/data/WGS30YR.csv',  30)), # 30yr
    'treasury.m'=list( # Monthly
      c('../data/IRTCM_csv_2/data/GS1M.csv',1/12), #  1mo
      c('../data/IRTCM_csv_2/data/GS3M.csv',3/12), #  3mo
      c('../data/IRTCM_csv_2/data/GS6M.csv',6/12), #  6mo
      c('../data/IRTCM_csv_2/data/GS1.csv' ,   1), #  1yr
      c('../data/IRTCM_csv_2/data/GS2.csv' ,   2), #  2yr
      c('../data/IRTCM_csv_2/data/GS3.csv' ,   3), #  3yr
      c('../data/IRTCM_csv_2/data/GS5.csv' ,   5), #  5yr
      c('../data/IRTCM_csv_2/data/GS7.csv' ,   7), #  7yr
      c('../data/IRTCM_csv_2/data/GS10.csv',  10), # 10yr
      c('../data/IRTCM_csv_2/data/GS20.csv',  20), # 20yr
      c('../data/IRTCM_csv_2/data/GS30.csv',  30)) # 30yr
    ## Inflation Indexed Treasury Securities, Constant Maturity
   # 'treasury.ii.d'=list( # Daily
   #   c('../data/IRTCM_csv_2/data/DFII5.csv' ,  5),  #  5yr
   #   c('../data/IRTCM_csv_2/data/DFII7.csv' ,  7),  #  7yr
   #   c('../data/IRTCM_csv_2/data/DFII10.csv', 10),  # 10yr
   #   c('../data/IRTCM_csv_2/data/DFII20.csv', 20),  # 20yr
   #   c('../data/IRTCM_csv_2/data/DFII30.csv', 30)), # 30yr
   # 'treasury.ii.w'=list( # Weekly
   #   c('../data/IRTCM_csv_2/data/WFII5.csv' ,  5),  #  5yr
   #   c('../data/IRTCM_csv_2/data/WFII7.csv' ,  7),  #  7yr
   #   c('../data/IRTCM_csv_2/data/WFII10.csv', 10),  # 10yr
   #   c('../data/IRTCM_csv_2/data/WFII20.csv', 20),  # 20yr
   #   c('../data/IRTCM_csv_2/data/WFII30.csv', 30)), # 30yr
   # 'treasury.ii.m'=list( # Monthly
   #   c('../data/IRTCM_csv_2/data/FII5.csv' ,  5), #  5yr
   #   c('../data/IRTCM_csv_2/data/FII7.csv' ,  7), #  7yr
   #   c('../data/IRTCM_csv_2/data/FII10.csv', 10), # 10yr
   #   c('../data/IRTCM_csv_2/data/FII20.csv', 20), # 20yr
   #   c('../data/IRTCM_csv_2/data/FII30.csv', 30)) # 30yr
    );

  for (dd.group in names(tmp.datafiles)) {
    # Load and merge security data
    tmp <- NULL;
    for (dd in tmp.datafiles[[dd.group]]) {
      tmp <- rbindlist(list(tmp, setnames(fread(dd[[1]],na.strings=c('.'))[,maturity:=dd[[2]]],
                                          c('date','int','maturity'))),
                       use.names=T);
    }
    # Remove missing values and reorder columns
    tmp <- setcolorder(tmp[!is.na(int),],c('date','maturity','int'));
    # Correct format
    tmp[,c('date','maturity','int'):=.(as.Date(date),as.numeric(maturity),as.numeric(int))];
    # Set order
    setkey(tmp,date,maturity);

    # Export merged file to the global namespace
    assign(dd.group,tmp,envir=.GlobalEnv)
  }
});
