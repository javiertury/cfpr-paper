# Load required libraries
libs <- c('data.table','haven');
for (i in libs) {
    if( !is.element(i, .packages(all.available=T))){
        capture.output(install.packages(i));
    }
    suppressMessages(require(i,character.only=T));
}
rm(i);

## Survey of Consumer Finances, 1983
scf.imp.orig <- as.data.table(read_dta('../data/1983_scf83bs.zip'));
#scf.raw.orig <- as.data.table(read_dta('../data/1983_scf83as.zip'));

cols <- matrix(byrow=T, ncol=2,
  data=c('v1', 'id',
         'b3001', 'sample_type', # Sample type, high income(1), clean(2) and excluded(3)
         'b3002', 'nonres_w', # Non reponse adjustement factor
         'b3003', 'area80_w', # Post-stratification weight 1980
         'b3004', 'area83_w', # Post-stratification weight 1983
         'b3005', 'nonres_area83_w', # Non-respones and stratification 1983 weight
         'b3006', 'inclu_p', # Inclusion probit predicted value
         'b3007', 'clean_w', # Cleaned sample inclusion weight
         'b3008', 'clean_area80_w', # Cleaned area and 1980 stratification area weight
         'b3009', 'clean_area83_w', # Cleaned area and 1983 stratification area weight
         'b3010', 'nonres_clean_area83_w', # Non-respones, cleaned area and
                                            # 1983 stratification weight
         'b3011', 'inerr', # Inclusion error expectation, control var. for bias
         'b3012', 'hincome_w', # High income sample weight
         'b3013', 'psu', # Full sample PSU code
         'b3014', 'full_raw_w', # Full sample SRC composite weight
                                # Non-response + 1980 strat + high income + income adj
         'b3015', 'full_clean_w', # Full cleaned sample SRC composite weight
                                  # Non-response + clean + 1983 strat + high income + income adj
         'b3016', 'full_final_w', # Extended income FRB weight
                                  # Non-response + clean + 1983 strat + high income + income adj + IRS extended income
         'b3017', 'area83_new_w', # Revised SRC area probability weight
         'b3018', 'hincome_new_w', # Revised SRC high income weight
         'b3019', 'strat83_hincome_new_w', # Revised SRC composite weight
         'b3111', 'race', # Race of household
         'b3126', 'sex', # Sex of head
         'b3112', 'marital', # Marital status of head
         'b3165', 'docs', # Used docs to answer
         'b3166', 'docs_loan', # Appropiate docs for loans
         'b3167', 'docs_deposit', # Appropiate docs for checking, savings , or investment questions
         'b3168', 'docs_pension', # Appropiate docs for pensions
         'b3169', 'docs_tax', # Appropiate docs for income tax returns 
         # Balance Sheet
         # b3708-onwards Individual Asset Totals
         # b4001-onwards Individual Liability totals
         # b3701-onwards Individual net worth totals
         # b3501-onwards business assets
         # b4207 loan payment problems
         # b5522 turned down for credit or discouraged from borrowing
         'b4201', 'total_consdebt', # Total closed-end consumer debt outstanding
         'b4202', 'total_regdebt', # Total regular payment debt outstanding
         'b4203', 'total_nonregdebt', # Total non-regular payment debt outstanding
         'b4204', 'total_insurancedebt', # Total debt against the life insurance
         'b4205', 'total_cardebt', # Total amount of debt for car purchase
         'b4206', 'total_noncardebt', # Total amount of debt for non-car purchase
         'b4211', 'pur_home1', # Purpose of home1 loan
         'b4231', 'pur_home2',        # home2
         'b4251', 'pur_home3',        # home3
         'b4271', 'pur_car1',         # car1
         'b4291', 'pur_car2',         # car2
         'b4311', 'pur_car3',         # car3
         'b4331', 'pur_otherreg1',    # otherreg1
         'b4351', 'pur_otherreg2',    # otherreg2
         'b4371', 'pur_otherreg3',    # otherreg3
         'b4391', 'pur_othernonreg1', # othernonreg1
         'b4411', 'pur_othernonreg2', # othernonreg2
         'b4431', 'pur_othernonreg3', # othernonreg3
         'b4451', 'pur_otherhouse1',  # otherhouse1
         'b4471', 'pur_otherhouse2',  # otherhouse2
         'b4212', 'outstand_home1', # Outstanding of home1 loan
         'b4232', 'outstand_home2',        # home2
         'b4252', 'outstand_home3',        # home3
         'b4272', 'outstand_car1',         # car1
         'b4292', 'outstand_car2',         # car2
         'b4312', 'outstand_car3',         # car3
         'b4332', 'outstand_otherreg1',    # otherreg1
         'b4352', 'outstand_otherreg2',    # otherreg2
         'b4372', 'outstand_otherreg3',    # otherreg3
         'b4392', 'outstand_othernonreg1', # othernonreg1
         'b4412', 'outstand_othernonreg2', # othernonreg2
         'b4432', 'outstand_othernonreg3', # othernonreg3
         'b4452', 'outstand_otherhouse1',  # otherhouse1
         'b4472', 'outstand_otherhouse2',  # otherhouse2
         'b4213', 'mstart_home1', # Start month of home1 loan or refinancing
         'b4233', 'mstart_home2',        # home2
         'b4253', 'mstart_home3',        # home3
         'b4273', 'mstart_car1',         # car1
         'b4293', 'mstart_car2',         # car2
         'b4313', 'mstart_car3',         # car3
         'b4333', 'mstart_otherreg1',    # otherreg1
         'b4353', 'mstart_otherreg2',    # otherreg2
         'b4373', 'mstart_otherreg3',    # otherreg3
         'b4393', 'mstart_othernonreg1', # othernonreg1
         'b4413', 'mstart_othernonreg2', # othernonreg2
         'b4433', 'mstart_othernonreg3', # othernonreg3
         'b4453', 'mstart_otherhouse1',  # otherhouse1
         'b4473', 'mstart_otherhouse2',  # otherhouse2
         'b4214', 'ystart_home1', # Start year of home1 loan
         'b4234', 'ystart_home2',        # home2
         'b4254', 'ystart_home3',        # home3
         'b4274', 'ystart_car1',         # car1
         'b4294', 'ystart_car2',         # car2
         'b4314', 'ystart_car3',         # car3
         'b4334', 'ystart_otherreg1',    # otherreg1
         'b4354', 'ystart_otherreg2',    # otherreg2
         'b4374', 'ystart_otherreg3',    # otherreg3
         'b4394', 'ystart_othernonreg1', # othernonreg1
         'b4414', 'ystart_othernonreg2', # othernonreg2
         'b4434', 'ystart_othernonreg3', # othernonreg3
         'b4454', 'ystart_otherhouse1',  # otherhouse1
         'b4474', 'ystart_otherhouse2',  # otherhouse2
         'b4215', 'mend_home1', # End month of home1 loan
         'b4235', 'mend_home2',        # home2
         'b4255', 'mend_home3',        # home3
         'b4275', 'mend_car1',         # car1
         'b4295', 'mend_car2',         # car2
         'b4315', 'mend_car3',         # car3
         'b4335', 'mend_otherreg1',    # otherreg1
         'b4355', 'mend_otherreg2',    # otherreg2
         'b4375', 'mend_otherreg3',    # otherreg3
         'b4395', 'mend_othernonreg1', # othernonreg1
         'b4415', 'mend_othernonreg2', # othernonreg2
         'b4435', 'mend_othernonreg3', # othernonreg3
         'b4455', 'mend_otherhouse1',  # otherhouse1
         'b4475', 'mend_otherhouse2',  # otherhouse2
         'b4216', 'yend_home1', # End year of home1 loan
         'b4236', 'yend_home2',        # home2
         'b4256', 'yend_home3',        # home3
         'b4276', 'yend_car1',         # car1
         'b4296', 'yend_car2',         # car2
         'b4316', 'yend_car3',         # car3
         'b4336', 'yend_otherreg1',    # otherreg1
         'b4356', 'yend_otherreg2',    # otherreg2
         'b4376', 'yend_otherreg3',    # otherreg3
         'b4396', 'yend_othernonreg1', # othernonreg1
         'b4416', 'yend_othernonreg2', # othernonreg2
         'b4436', 'yend_othernonreg3', # othernonreg3
         'b4456', 'yend_otherhouse1',  # otherhouse1
         'b4476', 'yend_otherhouse2',  # otherhouse2
         'b4217', 'maturity_home1', # Original maturity of home1 loan, months
         'b4237', 'maturity_home2',        # home2
         'b4257', 'maturity_home3',        # home3
         'b4277', 'maturity_car1',         # car1
         'b4297', 'maturity_car2',         # car2
         'b4317', 'maturity_car3',         # car3
         'b4337', 'maturity_otherreg1',    # otherreg1
         'b4357', 'maturity_otherreg2',    # otherreg2
         'b4377', 'maturity_otherreg3',    # otherreg3
         'b4397', 'maturity_othernonreg1', # othernonreg1
         'b4417', 'maturity_othernonreg2', # othernonreg2
         'b4437', 'maturity_othernonreg3', # othernonreg3
         'b4457', 'maturity_otherhouse1',  # otherhouse1
         'b4477', 'maturity_otherhouse2',  # otherhouse2
         'b4218', 'amount_home1', # Original amount of home1 loan
         'b4238', 'amount_home2',        # home2
         'b4258', 'amount_home3',        # home3
         'b4278', 'amount_car1',         # car1
         'b4298', 'amount_car2',         # car2
         'b4318', 'amount_car3',         # car3
         'b4338', 'amount_otherreg1',    # otherreg1
         'b4358', 'amount_otherreg2',    # otherreg2
         'b4378', 'amount_otherreg3',    # otherreg3
         'b4398', 'amount_othernonreg1', # othernonreg1
         'b4418', 'amount_othernonreg2', # othernonreg2
         'b4438', 'amount_othernonreg3', # othernonreg3
         'b4458', 'amount_otherhouse1',  # otherhouse1
         'b4478', 'amount_otherhouse2',  # otherhouse2
         'b4219', 'pay_home1', # Amount of each payment for home1 loan
         'b4239', 'pay_home2',        # home2
         'b4259', 'pay_home3',        # home3
         'b4279', 'pay_car1',         # car1
         'b4299', 'pay_car2',         # car2
         'b4319', 'pay_car3',         # car3
         'b4339', 'pay_otherreg1',    # otherreg1
         'b4359', 'pay_otherreg2',    # otherreg2
         'b4379', 'pay_otherreg3',    # otherreg3
         'b4399', 'pay_othernonreg1', # othernonreg1
         'b4419', 'pay_othernonreg2', # othernonreg2
         'b4439', 'pay_othernonreg3', # othernonreg3
         'b4459', 'pay_otherhouse1',  # otherhouse1
         'b4479', 'pay_otherhouse2',  # otherhouse2
         'b4220', 'payfreq_home1', # Frequency of payments for home1 loan
         'b4240', 'payfreq_home2',        # home2
         'b4260', 'payfreq_home3',        # home3
         'b4280', 'payfreq_car1',         # car1
         'b4300', 'payfreq_car2',         # car2
         'b4320', 'payfreq_car3',         # car3
         'b4340', 'payfreq_otherreg1',    # otherreg1
         'b4360', 'payfreq_otherreg2',    # otherreg2
         'b4380', 'payfreq_otherreg3',    # otherreg3
         'b4400', 'payfreq_othernonreg1', # othernonreg1
         'b4420', 'payfreq_othernonreg2', # othernonreg2
         'b4440', 'payfreq_othernonreg3', # othernonreg3
         'b4460', 'payfreq_otherhouse1',  # otherhouse1
         'b4480', 'payfreq_otherhouse2',  # otherhouse2
         'b4221', 'npay_home1', # Original number of payments for home1 loan
         'b4241', 'npay_home2',        # home2
         'b4261', 'npay_home3',        # home3
         'b4281', 'npay_car1',         # car1
         'b4301', 'npay_car2',         # car2
         'b4321', 'npay_car3',         # car3
         'b4341', 'npay_otherreg1',    # otherreg1
         'b4361', 'npay_otherreg2',    # otherreg2
         'b4381', 'npay_otherreg3',    # otherreg3
         'b4401', 'npay_othernonreg1', # othernonreg1
         'b4421', 'npay_othernonreg2', # othernonreg2
         'b4441', 'npay_othernonreg3', # othernonreg3
         'b4461', 'npay_otherhouse1',  # otherhouse1
         'b4481', 'npay_otherhouse2',  # otherhouse2
         'b4222', 'npayleft_home1', # Number of payments left for home1 loan
         'b4242', 'npayleft_home2',        # home2
         'b4262', 'npayleft_home3',        # home3
         'b4282', 'npayleft_car1',         # car1
         'b4302', 'npayleft_car2',         # car2
         'b4322', 'npayleft_car3',         # car3
         'b4342', 'npayleft_otherreg1',    # otherreg1
         'b4362', 'npayleft_otherreg2',    # otherreg2
         'b4382', 'npayleft_otherreg3',    # otherreg3
         'b4402', 'npayleft_othernonreg1', # othernonreg1
         'b4422', 'npayleft_othernonreg2', # othernonreg2
         'b4442', 'npayleft_othernonreg3', # othernonreg3
         'b4462', 'npayleft_otherhouse1',  # otherhouse1
         'b4482', 'npayleft_otherhouse2',  # otherhouse2
         'b4223', 'int_home1', # Annual rate of interest for home1 loan
         'b4243', 'int_home2',        # home2
         'b4263', 'int_home3',        # home3
         'b4283', 'int_car1',         # car1
         'b4303', 'int_car2',         # car2
         'b4323', 'int_car3',         # car3
         'b4343', 'int_otherreg1',    # otherreg1
         'b4363', 'int_otherreg2',    # otherreg2
         'b4383', 'int_otherreg3',    # otherreg3
         'b4403', 'int_othernonreg1', # othernonreg1
         'b4423', 'int_othernonreg2', # othernonreg2
         'b4443', 'int_othernonreg3', # othernonreg3
         'b4463', 'int_otherhouse1',  # otherhouse1
         'b4483', 'int_otherhouse2',  # otherhouse2
         'b4224', 'insttype_home1', # Source for home1 loan
         'b4244', 'insttype_home2',        # home2
         'b4264', 'insttype_home3',        # home3
         'b4284', 'insttype_car1',         # car1
         'b4304', 'insttype_car2',         # car2
         'b4324', 'insttype_car3',         # car3
         'b4344', 'insttype_otherreg1',    # otherreg1
         'b4364', 'insttype_otherreg2',    # otherreg2
         'b4384', 'insttype_otherreg3',    # otherreg3
         'b4404', 'insttype_othernonreg1', # othernonreg1
         'b4424', 'insttype_othernonreg2', # othernonreg2
         'b4444', 'insttype_othernonreg3', # othernonreg3
         'b4464', 'insttype_otherhouse1',  # otherhouse1
         'b4484', 'insttype_otherhouse2',  # otherhouse2
         'b4225', 'place_home1', # Place where papers for home1 loan were filled
         'b4245', 'place_home2',        # home2
         'b4265', 'place_home3',        # home3
         'b4285', 'place_car1',         # car1
         'b4305', 'place_car2',         # car2
         'b4325', 'place_car3',         # car3
         'b4345', 'place_otherreg1',    # otherreg1
         'b4365', 'place_otherreg2',    # otherreg2
         'b4385', 'place_otherreg3',    # otherreg3
         # 'b4405', 'place_othernonreg1', # othernonreg1
         # 'b4425', 'place_othernonreg2', # othernonreg2
         # 'b4445', 'place_othernonreg3', # othernonreg3
         # 'b4465', 'place_otherhouse1',  # otherhouse1
         # 'b4485', 'place_otherhouse2',  # otherhouse2
         'b4226', 'pcost_home1', # Project cost of home1 loan
         'b4246', 'pcost_home2',        # home2
         'b4266', 'pcost_home3',        # home3
         'b4286', 'pcost_car1',         # car1
         'b4306', 'pcost_car2',         # car2
         'b4326', 'pcost_car3',         # car3
         'b4346', 'pcost_otherreg1',    # otherreg1
         'b4366', 'pcost_otherreg2',    # otherreg2
         # 'b4386', 'pcost_otherreg3',    # otherreg3
         'b4406', 'pcost_othernonreg1', # othernonreg1
         'b4426', 'pcost_othernonreg2', # othernonreg2
         # 'b4446', 'pcost_othernonreg3', # othernonreg3
         # 'b4466', 'pcost_otherhouse1',  # otherhouse1
         # 'b4486', 'pcost_otherhouse2',  # otherhouse2
         'b4465', 'balloon_othernonreg1', # Amount of balloon payment
         'b5332', 'credit_union', # Mentions to credit union (sophistication)
         'b5516', 'q1',   # Question 1: amount payment (if knowleadgeble)
         'b5517', 'q1_g', # Question 1 insisted (regardless of knowledge)
         'b5518', 'q2',   # Question 2: Interest rate of question 1
         'b5519', 'q2_g', # Question 2: Interest rate of question 1
         'b5520', 'q2_i', # Implied amount of payment
         'b5521', 'q1_i' # Implied amount of interest rate
         ));

# Stop if not all orignal or name column names are unique
stopifnot(all(table(cols[,1])));
stopifnot(all(table(cols[,2])));

# Copy selected column to new data.table and rename
dat <- setnames(scf.imp.orig[,mget(cols[,1])],cols[,2]);

# Do not remove households without sophistication questions
#dat <- dat[!(is.na(q1_i) | is.na(q2_g) | q1_i<0 | q2_g <0),];

## Scale variables
# Scale weights? Not necessary usually
#dat[,nonres_w:=nonres_w/10000] 
#dat[,strat80_w:=strat80_w/10000] 
#dat[,strat83_w:=strat83_w/10000] 
dat[,inclu_p:=inclu_p/10000];

# Scale percentages
dat[,c('q1_i','q2','q2_g'):=
       lapply(.(q1_i,q2,q2_g),
              function(x) ifelse(x<0,x,x/10))];

## Create variables
# Bias as q2_g-q1_i. Less if in lowest quintile.
# More biased quintiles 2-5 or did not answer one or both quesitons
dat[(!is.na(q1_i)) & (!is.na(q2_g)) & q1_i>=0 & q2_g>=0,
     bias:=q2_g-q1_i];

dat[!is.na(bias),lessbias:=ifelse(bias>=quantile(bias,0.75),T,F)];
dat[is.na(bias),lessbias:=F];
dat[,morebias:=.(!lessbias)];

## Documents used for anwsering questions
# Format NAs. NA=-9.
dat[docs==-9, docs:=NA];

# Create map of answer code to description
docs.names <- matrix(ncol=2,byrow=T,
  data=c( 1, 'frequently',
          3, 'sometimes',
          4, 'rarely',
          5, 'never'));

# Convert variable to factor.
dat[, docs:=as.factor(docs)];

# Check that all codes are described
stopifnot(all(levels(dat$docs) %in% as.character(docs.names[,1])));

# Map codes to description
# Mapping method: map=setNames(values,names); map[names] -> values
levels(dat$docs) <- setNames(docs.names[,2],docs.names[,1])[levels(dat$docs)];

# Dummy equal to TRUE if docs were ever used.
dat[, docs_d:=.(docs %in% c('frequently','sometimes','rarely'))];

## Were documents used to answer questions regarding these topics?
dat[docs_loan %in% c(-9,0),docs_loan:=NA];
dat[docs_deposit %in% c(-9,0),docs_deposit:=NA]
dat[docs_pension %in% c(-9,0),docs_pension:=NA]
dat[docs_tax %in% c(-9,0),docs_tax:=NA]
dat[,docs_loan:=.(docs_loan==1)];
dat[,docs_deposit:=.(docs_deposit==1)]
dat[,docs_pension:=.(docs_pension==1)]
dat[,docs_tax:=.(docs_tax==1)]

setkey(dat,id);
