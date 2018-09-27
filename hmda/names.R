loan_type.names <- matrix(byrow=T,ncol=2,
  data=c(1,'Conventional',
         2,'FHA',
         3,'VA',
         4,'FSA/RHS'));

prop_type.names <- matrix(byrow=T,ncol=2,
  data=c(0,'0', # Undocumented, 2007 data singularity
         1,'One to four-family dwelling',
         2,'Manufactured housing',
         3,'Multifamily dwelling'));

pur.names <- matrix(byrow=T,ncol=2,
  data=c(1,'Home purchase',
         2,'Home improvement',
         3,'Refinancing'));

occupancy.names <- matrix(byrow=T,ncol=2,
  data=c(1,'Owner-occupied as principal dwelling',
         ## second homes, vacation homes or rental properties
         2,'Not owner-occupied as principal dwelling',
         ## Not a multifamily dwelling
         3,'Not applicable'));

preapproval.names <- matrix(byrow=T,ncol=2,
  data=c(1,'Preapproval requested',
         2,'Preapproval not requested',
         3,'Not applicable'));

action.names <- matrix(byrow=T,ncol=2,
  data=c(1,'Loan originated',
         2,'Application approved but not accepted',
         3,'Application denied',
         4,'Application withdrawn',
         5,'File closed for incompleteness',
         6,'Loan purchased by your institution',
         7,'Preapproval request denied',
         8,'Preapproval request approved but not accepted'));

ethnicity.names <- matrix(byrow=T,ncol=2,
  data=c(1,'Hispanic',
         2,'Not Hispanic',
         3,'Unknown',
         4,'Not applicable',
         5,'No co-applicant'));

race.names <- matrix(byrow=T,ncol=2,
  data=c(1,'American Indian',
         2,'Asian',
         3,'Black',
         4,'Islander',
         5,'White',
         6,'Unknown',
         7,'Not applicable',
         8,'No co-applicant'));

sex.names <- matrix(byrow=T,ncol=2,
  data=c(1,'Male',
         2,'Female',
         3,'Unknown',
         4,'Not applicable',
         5,'No co-applicant'));

purchaser_type.names <- matrix(byrow=T,ncol=2,
  data=c(0,'Loan was not originated',
         1,'Fannie Mae',
         2,'Ginnie Mae',
         3,'Freddie Mac',
         4,'Farmer Mac',
         5,'Private securitization',
         6,'Commercial bank, savings bank or savings association',
         7,'Life insurance company, credit union, mortgage bank, or finance company',
         8,'Affiliate institution',
         9,'Other'));

denial_reason.names <- matrix(byrow=T,ncol=2,
  data=c(1,'Debt-to-income ratio',
         2,'Employment history',
         3,'Credit history',
         4,'Collateral',
         5,'Insufficient cash',
         6,'Unverifiable information',
         7,'Credit application incomplete',
         8,'Mortgage insurance denied',
         9,'Other'));

hoepa.names <- matrix(byrow=T,ncol=2,
  data=c(1,'High-cost mortgage under HOEPA',
         2,'Not high-cost mortgage under HOEPA'));

lien.names <- matrix(byrow=T,ncol=2,
  data=c(1,'first-lien',
         2,'subordinate-lien',
         3,'Not secured by a lien',
         4,'Not applicable'));

edit.names <- matrix(byrow=T,ncol=2,
  data=c(5,'Validity edit failure',
         6,'Quality edit failure',
         7,'Validity and Quality edit failures'));

agency.names <- matrix(byrow=T,ncol=2,
  data=c(1,'OCC',
         2,'FRS',
         3,'FDIC',
         4,'OTS',
         5,'NCUA',
         7,'HUD',
         8, '8', # Undocumented, 2010 data singularity
         9,'CFPB'));
