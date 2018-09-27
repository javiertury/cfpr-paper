This repository hosts the code used in the following paper.

Garcia Gonzalez, Javier, Effects of Consumer Financial Protection Introduced after the Financial Crisis of 2007-2008 (September 27, 2018). Available at: https://ssrn.com/abstract=3256291 and https://mpra.ub.uni-muenchen.de/id/eprint/89226

The main purpose is to make the results replicable. Additionally I would like that this code can serve as a useful example to researches working with these datasets.

- [Survey of Consumer Finances(SCF)](https://www.federalreserve.gov/econres/scfindex.htm)
- [Home Mortgage Disclosure Act(HMDA) Loan Application Registry(LAR)](https://www.ffiec.gov/hmda/hmdaproducts.htm)

This code uses with multiply imputed survey data with bootstrap replication weights. This data was manipulated using R thanks to [*lodown*](https://github.com/ajdamico/lodown) and *survey* packages. Next you will find the abstract of the paper.

# Abstract

This paper studies the effects of consumer financial protection regulation introduced in the US after the financial crisis of 2007-2008. It starts with a review of bounded rationality in the context of retail financial markets. I analyze the survey of consumer finances using diffs-in-diffs, paying special attention to the singularities of this dataset. The main goal is to assess the effectiveness of regulatory changes. Secondarily, the paper tries to find out if deception was occurring in the marketplace. There is support for the effectiveness of the 2011 FTC advertising rule. Results reject effectiveness of HOEPA rule of 2008 and HEOA provisions about private education loans.

# Most important files

These are the files that produce regression tables and output figures.

-----------------------------  --------------------------------------------------
creditcost_result_simple.R     Outputs simple regression tables from SCF data    
creditcost_result_bootstrap.R  Outputs bootstrap regression tables from SCF data 
creditcost_figures.R           Outputs figures and description tables of SCF data
hmda_result.R                  Outputs regression tables from HMDA data          
hmda_description.R             Outputs description tables of HMDA data           
-----------------------------  --------------------------------------------------

# Populate Data

I obtained data from public data sources. Data is not included with code, but you can download it yourself. Next, I will detail which datasets you have to place in each directory.

### Survey of Consumer Finance(SCF) 1983 data

https://www.federalreserve.gov/econres/scf_1983.htm

Download file `1983_scf83bs.zip` and place it under the directory `data/`.

### Survey of Consumer Finance(SCF) 2013 data

Use the script provided by *ajdamico* in *asdfree* to download this data. The script has the name `asdfree_scf_download.R`. Otherwise, modify the code to download data with *lodown*. Place the resulting `scf2013.rda` file inside the directory `data/`.

### Home Mortgage Disclosure Act(HMDA) data

https://catalog.archives.gov
https://www.ffiec.gov/hmda/hmdaflat.htm

Download the HMDA LAR ultimate series(2004-2014) files and place them inside the directory `data/HMDA`. Download HMDA (Reporter) Panel ultimate series files for the same period and place them under `data/HMDAPanel/`.

### CFPB Origination Statistics

https://www.consumerfinance.gov/data-research/consumer-credit-trends/

Download all `num_data_*.csv`, `vol_data_*.csv`, `volume_data_Age_Group_*.csv`, `volume_data_Income_Level_*.csv`, `volume_data_Score_Level_*.csv`. The download links can be found under the figures displayed for each credit product. Place those files inside `data/cfpb/`.

### Census Flat Files

https://www.ffiec.gov/censusapp.htm
	
Download census flat files(2004-2014). Place them inside `data/census/`

### Corporate Bond data

https://fred.stlouisfed.org/categories/32348
	
Download the whole category and place it under `data/CORPBNDS_csv_2/`. If you decide to download one by one, download all `HQMCB*.csv` files, `AAA.csv`, `DAAA.csv`, `WAAA.csv`, `BAA.csv`, `DBAA.csv` and `WBAA.csv`.

Includes zero rates as High Quality Market(HQM) rates, and constant principal interest rates.

### Treasury Constant Maturity data, constant principal

https://fred.stlouisfed.org/categories/115

Download the whole category into `data/IRTCM_csv_2/`.

### Treasury spot rates

https://www.federalreserve.gov/pubs/feds/2006/200628/index.html
https://www.quandl.com/api/v3/datasets/FED/SVENY.csv
https://www.quandl.com/api/v3/datasets/FED/PARAMS.csv

Put both `FED-PARAMS.csv` and `FED-SVENY.csv` in `data/`

#### Consumer Price Index(CPI) data

https://fred.stlouisfed.org

Download the following series into `data/CPI_csv_2/`:

- `CPIAUCSL.csv`
- `CPIHOSNS.csv`
- `CPIMEDNS.csv`
- `CPIRECNS.csv`
- `CUUR0000SAD.csv`
- `CUUR0000SAE1.csv`
- `CUUR0000SAH3.csv`
- `CUUR0000SAT1.csv`
- `CUUR0000SEEB.csv`
- `CUUR0000SETA01.csv`
- `CUUR0000SETA02.csv`
- `CUUR0000SETA.csv`

### FRB National Information Center(NIC) data

https://www.ffiec.gov/nicpubweb/nicweb/DataDownload.aspx

Download the following series into `data/NIC/`

- `20180630_ATTRIBUTES_ACTIVE.zip`
- `20180630_ATTRIBUTES_BRANCH.zip`
- `20180630_ATTRIBUTES_INACTIVE.zip`
- `20180630_RELATIONSHIPS.zip`
- `20180630_TRANSFORMATIONS.zip`

# Structure

The file `study_dates.R` contains the dates of the events of interest for this study. `aux*.R` files and `intersts/tools.R` provide auxiliary code functions.

The folder `interests/` contains files processing different interest rate datasets. The files inside `loan_aggregate/` process datasets related to loan origination statistics. The directories `scf2013` and `scf1983` contain files to process the SCF datasets. `data_*.R` files process other datasets.

The remaining set of files, `creditcost_*.R` and `hmda_*.R`, are used to create the regressions of the study using the SCF 2013 and HMDA LAR datasets respectively. The first step is to combine those datasets with complementary datasets, this is done in `*_mixdata.R` files.

`creditcost_figures.R` and `hmda_description.R` produce tables and figures used in the paper to illustrate the datasets.

`creditcost_regs_*.R` and `hmda_result.R` run the regressions. While `hmda_result.R` outputs the latex regression tables directly, you have to run `creditcost_result_*.R` to obtain the latex regression tables for the SCF dataset.

# How to run bootstrap regressions

These regressions take around 3 days. It's preferable to rent a VPS and run regressions there. Copy the whole directory with populated data to the VPS. Run the file `creditcost_regs_bootstrap.R` to run the regressions. These regressions will be stored in the folder `vpsregs/` and each of them will weight around 2.5GB. You can transfer that directory from the server to your desktop or obtain latex tables directly in the server. Run `creditcost_result_bootstrap.R` to obtain latex tables.

# Non-used datasets

Although I didn't use this datasets in the final paper, I had already built utilities to process them.

### FRB of NY, origination statistics

https://www.newyorkfed.org/microeconomics/databank.html
https://www.newyorkfed.org/medialibrary/interactives/householdcredit/data/xls/HHD_C_Report_2018Q1.xlsx
http://newyorkfed.org/medialibrary/media/research/blog/2016/LSE_2016q3_new-autos-scally_data.xlsx

Go to the databank and download the following files, place them under "data/frbny_orig/"

- *Quarterly report on hoursehold debt* > *Data underlying report* `HHD_C_Report_2018Q1.xlsx`
- *Quarterly report on hoursehold debt* > *Historical Data (Pre-2003)* `pre2003_data.xlsx`
- *Auto Loans* > *Originations* > *Subprime Auto Debt Grows Despite Rising Delinquencies* `LSE_2016q3_new-autos-scally_data.xlsx`

It's processed in the file `loan_aggregate/nyfed.orig.R`

### APOR tables

https://www.ffiec.gov/ratespread/aportables.htm

Download `YieldTableAdjustable.CSV` and `YieldTableFixed.CSV`. Place them under `data/apor/`.

It's processed in the file `interests/apor.R`

### HMDA Mortgage Statistics

https://www.consumerfinance.gov/documents/6492/bcfp_hmda_2017-mortgage-market-activity-trends_tables.xlsx

Place the file under `data/`

It's processed in the file `loan_aggregate/hmda.R`

### Consumer Credit Volume Outstanding Statistics

https://www.federalreserve.gov/datadownload/Choose.aspx?rel=g19

Download all(`FRB_G19.csv`) and place it in `data/`

It's processed in the file `loan_aggregate/ccredit.R`

### BOA interest rates

https://fred.stlouisfed.org/

For ICE BofAML US Corporate interest rate series, `BAMLC0*.csv` and `BAMLH0*.csv`

It's processed in the file `interests/boa.R`
