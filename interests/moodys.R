# Load required libraries
libs <- c('data.table');
for (i in libs) {
    if( !is.element(i, .packages(all.available=T))){
        capture.output(install.packages(i));
    }
    suppressMessages(require(i,character.only=T));
}
rm(i);

local({
  # Structure
  # Var name/security group by freq = (file of sec, maturity of sec, grade of sec)
  tmp.datafiles <- list(
    'moodys.d'=list( # Daily
      c('../data/CORPBNDS_csv_2/data/DAAA.csv', 'seasoned', 'aaa' ),
      c('../data/CORPBNDS_csv_2/data/DBAA.csv', 'seasoned', 'baa' )),
    'moodys.w'=list( # Weekly
      c('../data/CORPBNDS_csv_2/data/WAAA.csv', 'seasoned', 'aaa' ),
      c('../data/CORPBNDS_csv_2/data/WBAA.csv', 'seasoned', 'baa' )),
    'moodys.m'=list( # Monthly
      c('../data/CORPBNDS_csv_2/data/AAA.csv', 'seasoned', 'aaa' ),
      c('../data/CORPBNDS_csv_2/data/BAA.csv', 'seasoned', 'baa' ))
    );

  for (dd.group in names(tmp.datafiles)) {
    # Load and merge security data
    tmp <- NULL;
    for (dd in tmp.datafiles[[dd.group]]) {
      tmp <- rbindlist(list(tmp, setnames(fread(dd[[1]],na.strings=c('.'))[,
          c('maturity', 'grade'):=.(as.factor(dd[[2]]), as.factor(dd[[3]]))],
        c('date','int','maturity','grade'))),
                       use.names=T);
    }
    # Remove missing values and reorder columns
    tmp <- setcolorder(tmp[!is.na(int),],c('date','maturity','grade','int'));
    # Correct format and order
    tmp[,c('date','maturity','grade','int'):=
         .(as.Date(date),as.factor(maturity),as.factor(grade),as.numeric(int))];

    # Corporate bonds are usually quoted on an annual rate compounded semi-annually(SABB)

    # Order
    setkey(tmp,date,maturity,grade);

    # Export merged file to the global namespace
    assign(dd.group,tmp,envir=.GlobalEnv)
  }
});
