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

## Average Prime Offer Rate(APOR)
# It's an average of Annual Percentage Rates(APR).
# APR is a simple/nominal annual rate, without adjustment for full compounding.
# Calculations for APOR assume fully amortized loan with monthly compounding, but legal comparison
# of APRs doesn't take into account the compounding frequency.

# Fixed interest loans
apor.f <- fread('../data/apor/YieldTableFixed.CSV',stringsAsFactors=T,header=T);
# Adjustable interest loans
apor.a <- fread('../data/apor/YieldTableAdjustable.CSV',stringsAsFactors=T,header=T);

# Rename date column
setnames(apor.f,'Term of Loan in Years','date');
setnames(apor.a,'Years to First Adjustment','date');
# Check that date column was found and renamed
stopifnot(('date' %in% colnames(apor.f)) & ('date' %in% colnames(apor.a)))

apor.f <- melt(apor.f,measure.vars=grep('^[0-9]+$',colnames(apor.f),value=T),
              value.name='int', variable.name='maturity');
apor.a <- melt(apor.a,measure.vars=grep('^[0-9]+$',colnames(apor.a),value=T),
              value.name='int', variable.name='first_adjust');

# Fix column formats
apor.f[,date:=as.Date(date,format='%m/%d/%Y')];
apor.f[,maturity:=as.numeric(maturity)];
apor.a[,date:=as.Date(date,format='%m/%d/%Y')];
apor.a[,first_adjust:=as.numeric(first_adjust)];
