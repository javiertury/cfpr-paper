vars.main <- c('hfsth_act','mdia','hoepa08','respa08','heoa_pel','mdia2',
               'compban','safe_eff','ftc_ad','heoa_calc')

get.vars.omit <- function(regs) {
  vars <- Reduce(union,lapply(regs , function(x) {
                                      return(names(coef(x)));
                                    }));
  vars.omit <- grep('as.factor',vars,value=T);

  vars.omit <- c(vars.omit,grep('payincludes_c',vars,value=T));
  vars.omit <- c(vars.omit,grep('charter',vars,value=T));
  #vars.omit <- c(vars.omit,grep('heoa_calc',vars,value=T));
  vars.omit <- c(vars.omit,grep('hfsth_act',vars,value=T));
  vars.omit <- c(vars.omit,'(Intercept)',
                 'log_maturity','log(amount_cpi)','adjust_mortgageTRUE','rfapr',
                 'riskapr','mktrp','pmi_dTRUE','refinance_dTRUE','any_flexTRUE',
                 'any_flex','no_flex','no_flexTRUE',
                 'govprogram_dTRUE','borrowmore_dTRUE','any_mortgageTRUE',
                 'other_mortgageTRUE','main_mortgageTRUE','eduloanTRUE',
                 'newhome','nonewhome','newhomeTRUE','nonewhomeTRUE',
                 'mdia','mdia2','compban','hfsth_act','respa08',
                 'safe_eff','ftc_ad','ftcTRUE','ftc_mortgage','ftc_mortgageTRUE','hoepa08',
                 'newcover_hoepa08TRUE','heoa_pel','heoa_calc','eduinstTRUE',
                 'adjust_mortgageTRUE:govprogram_dTRUE',
                 'govprogram_dTRUE:any_mortgageTRUE','any_mortgageTRUE:ftcTRUE',
                 'main_mortgageTRUE:newcover_hoepa08TRUE','eduloanTRUE:any_flexTRUE',
                 'eduloanTRUE:no_flexTRUE',
                 'eduloanTRUE:eduinstTRUE','log_maturity:eduloanTRUE',
                 'eduloanTRUE:log(amount_cpi)','govprogram_d:nonewhome',
                 'newhome:main_mortgage','govprogram_dTRUE:nonewhomeTRUE',
                 'newhomeTRUE:main_mortgageTRUE','coapplicant',
                 'coapplicantTRUE','log(income_cpi)','log(amount_cpi)',
                 'log_app_change','new_mdiaTRUE','govprogram_dTRUE',
                 'preapproval','govprogram_d','govprogram_dTRUE',
                 'log_income_cpi','log_amount_cpi','log_population_race',
                 'log_population_sex','newcover_mdia','newcover_mdiaTRUE',
                 'any_mortgage:newcover_mdia','any_mortgageTRUE:newcover_mdiaTRUE',
                 'govprogram_d:new_mdiaTRUE',
                 'govprogram_dTRUE:new_mdiaTRUE','main_mortgageTRUE:newhomeTRUE');
  #vars.omit <- c(vars.omit,'govprogram_dTRUE:mdia:any_mortgageTRUE',
  #               'govprogram_dTRUE:any_mortgageTRUE:respa08',
  #               'main_mortgageTRUE:hoepa08:newcover_hoepa08TRUE',
  #               'eduloanTRUE:heoa_calc:eduinstTRUE','heoa_pel:any_flexTRUE'
  #               );
  return(vars.omit);
}

order.interactions <- function(reg,priority) {
  stopifnot(class(priority)=='list');

  # Order internal row name 
  for (vars.main in rev(priority)) {
    reg$names <- unlist(lapply(strsplit(reg$names,'\\s*:\\s*'),
                        function(x) {
                          paste(c(x[x %in% vars.main],
                                  x[!(x %in% vars.main)]),collapse=' : ');
                        }));
  }

  ## Order rows with respect other rows
  var.rows <- NULL;
  for (xx in priority[[1]]) {
    candidates <- which(grepl(paste0('^',xx,'($|\\s|:)'),reg$names))
    var.rows <- c(var.rows,unlist(sapply(candidates[!candidates %in% var.rows],
                                  function(x) return(c(x,x+1)))));
  };

  head.rows <- seq(1,(min(var.rows)-1));
  bottom.rows <- seq((max(var.rows)+1),nrow(reg));

  reg <- reg[c(head.rows,var.rows,bottom.rows),];

  head.rows <- seq(1,length(head.rows));
  bottom.rows <- seq(nrow(reg)-length(bottom.rows)+1,nrow(reg));
  var.rows <- seq(max(head.rows)+1,min(bottom.rows)-1);

  # Remove bottom border in var cells
  bottom_border(reg)[var.rows,] <- 0;
  bottom_border(reg)[max(var.rows),] <- 1;

  return(reg);
}

clean.reg <- function(reg) {
  reg$names <- gsub('TRUE','',reg$names);
  reg$names <- gsub('I\\((?:\\()?([a-zA-Z0-9_]+)(?:\\)\\s*\\*\\s*1)?\\)','\\1',reg$names);
  reg$names <- gsub('as\\.numeric\\(([a-zA-Z0-9_]+)\\)','\\1',reg$names);
  reg$names <- gsub('_d(\\s*:|$)','\\1',reg$names);
  reg$names <- gsub('any_mortgage','AnyMortgage',reg$names);
  reg$names <- gsub('main_mortgage','MainMortgage',reg$names);
  reg$names <- gsub('other_mortgage','OtherMortgage',reg$names);
  reg$names <- gsub('adjust_mortgage','AdjMortgage',reg$names);
  reg$names <- gsub('ftc_mortgage','FTCMortgage',reg$names);
  reg$names <- gsub('hfsth_act','HFSTH',reg$names);
  reg$names <- gsub('ftc_ad','AdRuleFTC',reg$names);
  reg$names <- gsub('ftc','FTC',reg$names);
  reg$names <- gsub('any_flex','Flex',reg$names);
  reg$names <- gsub('no_flex','NoFlex',reg$names);
  reg$names <- gsub('heoa_calc','HEOACalc',reg$names);
  reg$names <- gsub('heoa_pel','HEOAPEL',reg$names);
  reg$names <- gsub('safe_eff','SAFE',reg$names);
  reg$names <- gsub('newcover_hoepa08','NewCoverHOEPA08',reg$names);
  reg$names <- gsub('govprogram','GovRel',reg$names);
  reg$names <- gsub('respa08','RESPA08',reg$names);
  reg$names <- gsub('hoepa08','HOEPA08',reg$names);
  reg$names <- gsub('compban','CompBan',reg$names);
  reg$names <- gsub('newcover_mdia','NewCoverMDIA',reg$names);
  reg$names <- gsub('new_mdia','NewCoverMDIA',reg$names);
  reg$names <- gsub('mdia','MDIA',reg$names);
  reg$names <- gsub('eduloan','EduLoan',reg$names);
  reg$names <- gsub('eduinst','EduInst',reg$names);
  reg$names <- gsub('nonewhome','NoNewHome',reg$names);
  reg$names <- gsub('newhome','NewHome',reg$names);

  ## Merge Duplicates
  rep.rows <- reg$names[which(duplicated(reg$names) & reg$names != '')]
  for (x in rep.rows) {
    dupes <- which(reg$names == x);
    reg[dupes[1],] <- apply(reg[dupes,],2,
                            function(y) return(y[y != ''][1]))
    reg[dupes[1]+1,] <- apply(reg[dupes+1,],2,
                              function(y) return(y[y != ''][1]))
    if (length(dupes)>1) reg <- reg[-(dupes[2:length(dupes)]+0:1),];
  };

  return(reg)
}

split_reg <- function(reg,rows=NULL,parts=NULL,copyHeader=F) {
  n <- nrow(reg);

  if ((! is.null(rows)) & exists('rows')) {
    stopifnot(is.finite(rows) & (rows > 0));
    if (n <= rows) return(list(reg));

    parts <- ifelse((n > rows) & (n <= (2*rows-2)),2,
                    ceiling((n-2)/(rows-2)));
  } else {
    stopifnot(exists('parts') & (!is.null(parts)) & is.finite(parts) & (parts > 0));
    if (parts==1) return(list(reg));

    rows <- ifelse(parts==2,ceiling(n/2+1),
                   ceiling((n-2)/parts+2));
  }

  fill_row <- function(ht,after) {
    return(insert_row(ht,'...',rep('',ncol(ht)-1),after=after));
  }
  continue_caption <- function(ht) {
    set_caption(ht,paste(caption(ht),'(Continued)'));
  }

  out <- list(fill_row(reg[1:(rows-1),],after=rows-1));
  if (parts > 2) {
    for (ii in 2:(parts-1)) {
      tmp <- fill_row(reg[((ii-1)*(rows-2)+2):(ii*(rows-2)+1),],after=0);
      tmp <- fill_row(tmp,after=rows-1);
      tmp <- continue_caption(tmp);
      out <- c(out,list(tmp));
    }
  }
  tmp <- fill_row(reg[((parts-1)*(rows-2)+2):n,],after=0)
  tmp <- continue_caption(tmp);
  out <- c(out,list(tmp));

  if (copyHeader==T) {
    for (ii in 2:length(out)) {
      out[[ii]] <- rbind(reg[1,],out[[ii]]);
    }
  }

  return(out);
}

write.reg <- function(reg,file) {
  #sink(file)
  #  print_latex(reg)
  #sink()

  # Fix table width manually
  #output <- capture.output(print_latex(reg));
  #write(gsub('begin\\{tabularx\\}\\{[^}]+\\}',
  #           'begin\\{tabularx\\}\\{\\\\textwidth\\}',output),
  #      file=file);
  #quick_pdf(set_latex_float(reg,'h'),file=file,open=F);

  output <- capture.output(print_latex(reg));
  write(output[seq(min(which(grepl('begin\\{tabularx\\}',output))),
                   min(which(grepl('end\\{tabularx\\}',output))))],
        file=file);
}
