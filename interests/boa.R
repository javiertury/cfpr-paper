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
    'boa.mat.d'=list( # Daily, by maturity
      c('../data/CORPBNDS_csv_2/data/BAMLC1A0C13YEY.csv'  , '1-3'   ),  #  1-3yr
      c('../data/CORPBNDS_csv_2/data/BAMLC2A0C35YEY.csv'  , '3-5'   ),  #  3-5yr
      c('../data/CORPBNDS_csv_2/data/BAMLC3A0C57YEY.csv'  , '5-7'   ),  #  5-7yr
      c('../data/CORPBNDS_csv_2/data/BAMLC4A0C710YEY.csv' , '7-10'  ),  #  7-10yr
      c('../data/CORPBNDS_csv_2/data/BAMLC7A0C1015YEY.csv', '10-15' ),  # 10-15yr
      c('../data/CORPBNDS_csv_2/data/BAMLC8A0C15PYEY.csv' , '15+'   ))  # 15+yr  ,
    );

  for (dd.group in names(tmp.datafiles)) {
    # Load and merge security data
    tmp <- NULL;
    for (dd in tmp.datafiles[[dd.group]]) {
      tmp <- rbindlist(list(tmp, setnames(fread(dd[[1]],na.strings=c('.'))[,
          maturity:=as.factor(dd[[2]])],
        c('date','int','maturity'))),
                       use.names=T);
    }
    # Remove missing values and reorder columns
    tmp <- setcolorder(tmp[!is.na(int),],c('date','maturity','int'));
    # Correct format and order
    tmp[,c('date','maturity','int'):=
         .(as.Date(date),as.numeric(maturity),as.numeric(int))];

    # Order
    setkey(tmp,date,maturity);

    # Corporate bonds are usually quoted on an annual rate compounded semi-annually(SABB)

    # Export merged file to the global namespace
    assign(dd.group,tmp,envir=.GlobalEnv)
  }
});

local({
  # Structure
  # Var name/security group by freq = (file of sec, maturity of sec, grade of sec)
  tmp.datafiles <- list(
    'boa.grade.d'=list( # Daily, by grade
      c('../data/CORPBNDS_csv_2/data/BAMLC0A1CAAAEY.csv', 'aaa' ),  # AAA
      c('../data/CORPBNDS_csv_2/data/BAMLC0A2CAAEY.csv' , 'aa'  ),  # AA
      c('../data/CORPBNDS_csv_2/data/BAMLC0A3CAEY.csv'  , 'a'   ),  # A
      c('../data/CORPBNDS_csv_2/data/BAMLC0A4CBBBEY.csv', 'bbb' ),  # BBB
      c('../data/CORPBNDS_csv_2/data/BAMLH0A1HYBBEY.csv', 'bb'  ),  # BB
      c('../data/CORPBNDS_csv_2/data/BAMLH0A2HYBEY.csv' , 'b'   ),  # B
      c('../data/CORPBNDS_csv_2/data/BAMLH0A3HYCEY.csv' , 'ccc' )), # CCC
    'boa.grade.simp.d'=list( # Daily, by grade
      c('../data/CORPBNDS_csv_2/data/BAMLC0A0CMEY.csv'  , 'corporate'  ), # Corporate: AAA,AA,A,BBB
      c('../data/CORPBNDS_csv_2/data/BAMLH0A0HYM2EY.csv', 'high yield' )) # High Yield: BB, B, CCC
    );

  for (dd.group in names(tmp.datafiles)) {
    # Load and merge security data
    tmp <- NULL;
    for (dd in tmp.datafiles[[dd.group]]) {
      tmp <- rbindlist(list(tmp, setnames(fread(dd[[1]],na.strings=c('.'))[,
                                                'grade':=as.factor(dd[[2]])],
        c('date','int','grade'))),
                       use.names=T);
    }
    # Remove missing values and reorder columns
    tmp <- setcolorder(tmp[!is.na(int),],c('date','grade','int'));
    # Correct format and order
    tmp[,c('date','grade','int'):=
         .(as.Date(date),as.factor(grade),as.numeric(int))];

    # Order
    setkey(tmp,date,grade);

    # Corporate bonds are usually quoted on an annual rate compounded semi-annually(SABB)

    # Export merged file to the global namespace
    assign(dd.group,tmp,envir=.GlobalEnv)
  }
});
