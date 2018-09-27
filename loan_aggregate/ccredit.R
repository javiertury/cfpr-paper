# Load required libraries
libs <- c('data.table','stringr');
for (i in libs) {
    if( !is.element(i, .packages(all.available=T))){
        capture.output(install.packages(i));
    }
    suppressMessages(require(i,character.only=T));
}
rm(i);

local({
  # Load ccredit data and info headers
  ccredit <- fread('../data/FRB_G19.csv',skip=6,stringsAsFactors=T,header=F,
                     na.strings=c('ND'));
  ccredit.header <- fread('../data/FRB_G19.csv',nrow=6,stringsAsFactors=T,header=F);

  # Column names are the unique idenfier row (from header)
  setnames(ccredit, c('date', as.character(t(ccredit.header[5,]))[-1]));

  # Set date column as date
  ccredit[,date:=as.Date(paste0(date,'-01'))];

  # Put ccredit header vertically and name info columns
  ccredit.header <- transpose(ccredit.header);
  ccredit.header <- setnames(ccredit.header[-1,],
                               gsub(':\\s*$','',as.character(ccredit.header[1,])));

  # Format header data
  ccredit.header[,Currency:=as.factor(Currency)];
  ccredit.header[,Unit:=as.factor(Unit)];
  ccredit.header[,Multiplier:=as.numeric(Multiplier)];

  # Rename columns
  setnames(ccredit.header,
           c('Unique Identifier','Unit','Multiplier','Currency','Series Description','Time Period'),
           c('serie','unit','multiplier','currency','description','time_period'));

  # Order columns
  setorderv(ccredit.header,c('serie','unit','multiplier','currency','description','time_period'));

  ## Procedure to classify variables. Move strings from description to variables until none is left.
  ccredit.header[,class_words:=description]
  # Order in list is important, each term is removed before the next one is evaluated
  # Types of operations variables: binary(bin), capturing(cap), remove(rmv) and substitute(sub)
  # bin and cap store match on a column
  class_words.rules <- list(
       # type ,     var name    ,      regex     , ...                 
       # Obvious things to remove
       c('bin', 'discontinued'  , '[,.;]? \\(discontinued[^)]*\\)'        ),
       # Non-ambigous binaries and words that could confuse if not removed
       c('bin', 'total'         , '(T|t)otal'                             ), 
       c('bin', 'all_loans'     , '(O|o)wned and securitized'             ),
       c('bin', 'securitized'   , '(S|s)ecuritized'                       ), 
       c('bin', 'nonrevolving'  , '(N|n)onrevolving'                      ),
       # Must be after revolving
       c('bin', 'revolving'     , '(R|r)evolving'                         ),
       c('bin', 'maturity'      , '((A|a)verage )?(M|m)aturity'           ),
       # This one is nasty, get rid of it as soon as possible.
       # Make sure car doesn't get card(context). Also auto doesn't match 'auto finance companies'.
       c('bin', 'vehicle'       , '(((M|m)otor )?(V|v)ehicle|((^N| n)ew)( car| autos))'),
       c('bin', 'student'       , '(^S| s)tudent'                         ),
       c('bin', 'percent'       , '(^P| p)ercent (change (of)?)?'         ),
       c('bin', 'rate'          , '((^A| a)verage)?((^F| f)inance|(^I| i)nterest) rate'   ),
       # Careful with amount and weight. Make sure this is not 'amount of finance weighted' or similar
       c('bin', 'amount'        , '((^A| a)verage)?(^A| a)mount financed' ),
       # Properly delimited string capturing
       #c('cap', 'season_adj'    , '[,.;] ((?:not )?seasonally adjusted)(?: at [^,.;]+ rate)?'),
       c('bin', 'not_season_adj', '[,.;] not seasonally adjusted( at [^,.;]+ rate)?'),
       # Must be after not_season_adj
       c('bin', 'season_adj'    , '[,.;] seasonally adjusted( at [^,.;]+ rate)?'),
       # Must be after vehicle
       c('cap', 'loan_maturity' , '[,.;]\\s*([0-9]*)\\s*month loan'       ),
       c('cap', 'freq'          , '[,.;] ([a-zA-Z]+) rate'                ),
       c('cap', 'weight'        , '[,.;] ([^,.;]+) weighted'              ),
       c('bin', 'weighted'      , '((W|w)eighted)(( a|-a)verage)?'          ),
       c('cap', 'account_type'  , '[,.;] ([^,.;]*account[^,.;]*)'         ),
       # Not well delimited string capturing
       # Try to start from the end, and from more specific to less
       c('cap', 'level_or_flow' , ' (level|flow)'                         ),
       # Must be before purpose 
       c('cap', 'holder'        , '(?: owned by| at) (.*)'                ),
       # Must be after holder. Otherwise conflicts with 'credit unions'.
       c('cap', 'product'       , ' (loans|credit card plans|credit)'     ),
       c('bin', 'consumer_inst' , '(of)?(^C| c)onsumer installment'       ),
       # Must be after vehicle
       c('cap', 'purpose'       , ' (?:on|of) ([^,.;]*)'                  ),
       c('cap', 'ratio'         , '([^,.;]*) ratio'                       ),
       # Must be after purpose
       c('bin', 'consumer'      , '(^C| c)onsumer'                        ),
       # Miscellanea that got through
       c('bin', 'com_bank'      , '(^C| c)ommercial bank'                 ),
       c('bin', 'average'       , '(A|a)verage'                           ),
       # Things to delete, obtain NAs
       c('rmv', 'on'            , '\\s*on'                                ),
       c('rmv', 'of'            , '\\s*of'                                ),
       c('rmv', 'for'           , '\\s*for'                               ),
       c('rmv', 'and'           , '\\s*and'                               ),
       c('sub', 'empty_na'      , '^[ ,.;-]*$', NA                        )
       );
  # Check that var name is unique in list

  # Apply rules
  for (rr in class_words.rules) {
    if(rr[1]=='bin') {
      ccredit.header[,c(rr[2],'class_words'):=
                       .(grepl(rr[3],   class_words),
                          sub(rr[3],'',class_words))];
    } else if(rr[1]=='rmv') {
      ccredit.header[,c('class_words'):=sub(rr[3],'',class_words)];
    } else if(rr[1]=='sub') {
      ccredit.header[,c('class_words'):=sub(rr[3],rr[4],class_words)];
    # capturing
    } else {
      ccredit.header[,c(rr[2],'class_words'):=
                       # Extract matched groups from column 2 onwards
                       # paste data.frame by columns, careful not to collapse to a vector when subset
                       .(as.factor(tolower(apply(str_match(class_words, rr[3])[,-1,drop=F],
                                                 1,function(x) {paste(na.omit(x),collapse='_')}))),
                         sub(rr[3],'',class_words))];
      # Removing empty strings from factors
      ccredit.header[,c(rr[2]):=as.factor(sub('^\\s*$',NA,levels(get(rr[2])))[get(rr[2])]) ];
    }
  }
  # Check that all information from description has been classified or discarded
  stopifnot(ccredit.header[!is.na(class_words),.N]==0)
  # Proceed to delete class_words column
  ccredit.header[,class_words:=NULL];

  ## Workaround on extraction process

  # Commercial bank exception. Check no override
  stopifnot(ccredit.header[com_bank==T, all(is.na(holder))]);
  ccredit.header[com_bank==T, holder:='commercial banks'];
  ccredit.header[,com_bank:=NULL];

  # Check holders fall in these categories
  ccredit.holders <- c('auto finance companies',
    'commercial banks', 'credit unions', 'depository institutions',
    'federal government', 'finance companies', 'nonfinancial business',
    'nonprofit and educational institutions','savings institutions')
  stopifnot(ccredit.header[!(holder %in% c(NA, ccredit.holders)),.N]==0);

  # All vars with weight are weighted
  ccredit.header[!is.na(weight),weighted:=T];

  # Season ajusted
  ccredit.header[season_adj==F, season_adj:=NA];
  ccredit.header[not_season_adj==T, season_adj:=F];
  ccredit.header[,not_season_adj:=NULL];

  # Revolving
  ccredit.header[revolving==F, revolving:=NA];
  ccredit.header[nonrevolving==T, revolving:=F];
  ccredit.header[,nonrevolving:=NULL];

  # Consumer installment means consumer=T
  ccredit.header[grepl('(C|c)onsumer',purpose), consumer:=T];
  ccredit.header[consumer_inst==T, consumer:=T];

  # Student is 'educaiton' purpose. Check no override
  stopifnot(ccredit.header[student==T, all(is.na(purpose))]);
  ccredit.header[student==T , purpose:='education'];
  ccredit.header[, student:=NULL];

  # All 'average' keywords should have been captured by maturity, rate or amount 
  stopifnot(ccredit.header[average==T, .N]==0);
  ccredit.header[, average:=NULL];

  # Classify according to variable type
  stopifnot(ccredit.header[prod(maturity,percent,rate,amount,!is.na(ratio))>0,.N]==0);
  ccredit.header[maturity==T  , measure_type:='maturity'];
  ccredit.header[percent==T   , measure_type:='percent' ];
  ccredit.header[rate==T      , measure_type:='rate'    ];
  ccredit.header[amount==T    , measure_type:='amount'  ];
  ccredit.header[!is.na(ratio), measure_type:='ratio'   ];
  ccredit.header[,c('maturity','percent','rate','amount'):=NULL,];
  # The rest are asigned to volume
  ccredit.header[is.na(measure_type), measure_type:='volume'];

  # Check acceptable override. Vehicle is more specific that consumer installment.
  stopifnot(ccredit.header[vehicle==T, all(is.na(purpose))]);
  ccredit.header[vehicle==T, purpose:='vehicle'];
  ccredit.header[,vehicle:=NULL];

  # Check that all variables with loan maturity have 'loan' as product
  stopifnot(ccredit.header[!is.na(loan_maturity),all(product=='loans')]);

  # Securitized loans are categorized as their own holder, they are held by the public
  stopifnot(ccredit.header[securitized==T & (!is.na(holder)),.N]==0);
  ccredit.header[securitized==T, holder:='securitized',];
  ccredit.header[, securitized:=NULL];

  # 'Owned and securitized' refers to all loan holders and securitized loans.
  stopifnot(ccredit.header[all_loans==T & (!is.na(holder)),.N]==0);
  ccredit.header[all_loans==T, holder:='all',];
  ccredit.header[, all_loans:=NULL];

  # Export data to global env
  ccredit <<- ccredit;
  ccredit.header <<- ccredit.header;
});

## Create partiton of credit volume. (level or flow)/revolving/holder.
local({
  # Total credit volume level by holder
  creditvol.hld.s <- ccredit.header[discontinued==F & measure_type=='volume' &
    level_or_flow=='level' & season_adj==F & total==T, .(serie,holder)];
  # Non revolving credit volume level by holder
  creditvol.nrev.hld.s <- ccredit.header[discontinued==F & measure_type=='volume' &
    level_or_flow=='level' & season_adj==F & revolving==F, .(serie,holder)];
  # Revolving credit volume level by holder
  creditvol.rev.hld.s <- ccredit.header[discontinued==F & measure_type=='volume' &
    level_or_flow=='level' & season_adj==F & revolving==T, .(serie,holder)];

  # Total credit volume flow by holder
  creditvol.flw.hld.s <- ccredit.header[discontinued==F & measure_type=='volume' &
    level_or_flow=='flow' & season_adj==F & total==T, .(serie,holder)]
  # Nonrevolving credit volume flow by holder
  creditvol.flw.nrev.hld.s <- ccredit.header[discontinued==F & measure_type=='volume' &
    level_or_flow=='flow' & season_adj==F & revolving==F, .(serie,holder)];
  # Revolving credit volume flow by holder
  creditvol.flw.rev.hld.s <- ccredit.header[discontinued==F & measure_type=='volume' &
    level_or_flow=='flow' & season_adj==F & revolving==T, .(serie,holder)];

  # Check that holders are unique across (levels or flows)/(all, revolving or non-revolving)
  for (gg in c('creditvol.hld.s','creditvol.nrev.hld.s','creditvol.rev.hld.s',
               'creditvol.flw.hld.s','creditvol.flw.nrev.hld.s',
               'creditvol.flw.rev.hld.s')) {
    stopifnot(nrow(get(gg))==nrow(unique(get(gg),by='holder')));
  }

  ## Create credit volume dataset
  # Assemble data
  creditvol.hld <- ccredit[,mget(c('date', creditvol.hld.s$serie, creditvol.nrev.hld.s$serie,
    creditvol.rev.hld.s$serie, creditvol.flw.hld.s$serie, creditvol.flw.nrev.hld.s$serie,
    creditvol.flw.rev.hld.s$serie))];
  creditvol.hld <- melt(creditvol.hld,id.vars='date');
  # Remove NAs
  creditvol.hld <- creditvol.hld[!is.na(value),];
  # Fix names
  creditvol.hld <- setnames(creditvol.hld, c('variable','value'), c('serie','vol'));

  # Pass info from headers
  creditvol.hld <- merge(creditvol.hld, ccredit.header[,.(serie,level_or_flow,total,revolving)],
                         by='serie', all.x=T);

  # Export to global env
  creditvol.hld <<- creditvol.hld;
});
