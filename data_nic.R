# Load required libraries
local({
  libs <- c('xml2','data.table');
  for (i in libs) {
      if( !is.element(i, .packages(all.available=T))){
          capture.output(install.packages(i));
      }
      suppressMessages(require(i,character.only=T));
  }
});

nic.rds <- './data/NIC/nic.rds';

if (file.exists(nic.rds)) {
  nic <- readRDS(nic.rds);
} else {
  nic.files <- list(list(archive='./data/NIC/20180630_ATTRIBUTES_ACTIVE.zip',
                         file='20180630_ATTRIBUTES_ACTIVE.XML',
                         schema='ATTRIBUTES_VERSION001.xsd'),
                    list(archive='./data/NIC/20180630_ATTRIBUTES_INACTIVE.zip',
                         file='20180630_ATTRIBUTES_INACTIVE.XML',
                         schema='ATTRIBUTES_VERSION001.xsd')#,
                    #list(archive='./data/NIC/20180630_ATTRIBUTES_BRANCH.zip',
                    #     file='20180630_ATTRIBUTES_BRANCH.XML',
                    #     schema='ATTRIBUTES_VERSION001.xsd')
                    );

  nic <- NULL
  local({
  for (ff in nic.files) {
    # Read xml and schema
    dat <- read_xml(unz(ff$archive, ff$file));
    #schema <- read_xml(unz(ff$archive, ff$schema));
    #xml_validate(dat,schema);

    # Convert xml to data.table
    dat.parsed <- as_list(xml_find_all(dat, '//ATTRIBUTES'));
    out <- rbindlist(lapply(dat.parsed,function(x) {
      # Remove redundant sublists and trim whitespace
      x <- lapply(x,function(y) trimws(unlist(y)));
      # Use only columns with data
      x <- x[lapply(x,length)>0];
      # Convert to data.table
      return(as.data.table(x));
    }), fill=T,use.names=T);

    out[,ACT_PRIM_CD:=as.factor(ACT_PRIM_CD)];
    out[,AUTH_REG_DIST_FRS:=as.integer(AUTH_REG_DIST_FRS)];
    out[,BANK_CNT:=as.integer(BANK_CNT)];
    out[,BHC_IND:=as.integer(BHC_IND)];
    out[,BNK_TYPE_ANALYS_CD:=as.integer(BNK_TYPE_ANALYS_CD)];
    out[,BROAD_REG_CD:=as.integer(BROAD_REG_CD)];
    out[,CHTR_AUTH_CD:=as.integer(CHTR_AUTH_CD)];
    out[,CHTR_TYPE_CD:=as.integer(CHTR_TYPE_CD)];
    out[,CITY:=as.factor(CITY)];
    out[,CNSRVTR_CD:=as.integer(CNSRVTR_CD)];
    out[,CNTRY_CD:=as.numeric(CNTRY_CD)];
    out[,CNTRY_INC_CD:=as.integer(CNTRY_INC_CD)];
    out[,CNTRY_INC_NM:=as.factor(CNTRY_INC_NM)];
    out[,CNTRY_NM:=as.factor(CNTRY_NM)];
    out[,COUNTY_CD:=as.integer(COUNTY_CD)];
    out[,D_DT_END:=as.POSIXct(D_DT_END)];
    out[,D_DT_EXIST_CMNC:=as.Date(D_DT_EXIST_CMNC)];
    out[,D_DT_EXIST_TERM:=as.POSIXct(D_DT_EXIST_TERM)];
    out[,D_DT_INSUR:=as.POSIXct(D_DT_INSUR)];
    out[,D_DT_OPEN:=as.Date(D_DT_OPEN)];
    out[,D_DT_START:=as.POSIXct(D_DT_START)];
    out[,DIST_FRS:=as.integer(DIST_FRS)];
    # Dummy
    out[,DOMESTIC_IND:=as.factor(DOMESTIC_IND)];
    out[,DT_END:=as.Date(DT_END,format='%Y%m%d')];
    out[,DT_EXIST_CMNC:=as.Date(DT_EXIST_CMNC,format='%Y%m%d')];
    out[,DT_EXIST_TERM:=as.Date(DT_EXIST_TERM,format='%Y%m%d')];
    out[,DT_INSUR:=as.Date(DT_INSUR,format='%Y%m%d')];
    out[,DT_OPEN:=as.Date(DT_OPEN,format='%Y%m%d')];
    out[,DT_START:=as.Date(DT_START,format='%Y%m%d')];
    out[,ENTITY_TYPE:=as.factor(ENTITY_TYPE)];
    out[,EST_TYPE_CD:=as.integer(EST_TYPE_CD)];
    out[,FBO_4C9_IND:=as.integer(FBO_4C9_IND)];
    out[,FHC_IND:=as.integer(FHC_IND)];
    out[,FISC_YREND_MMDD:=as.numeric(FISC_YREND_MMDD)];
    out[,FNCL_SUB_HOLDER:=as.integer(FNCL_SUB_HOLDER)];
    out[,FNCL_SUB_IND:=as.integer(FNCL_SUB_IND)];
    out[,FUNC_REG:=as.integer(FUNC_REG)];
    out[,IBA_GRNDFTHR_IND:=as.integer(IBA_GRNDFTHR_IND)];
    out[,IBF_IND:=as.integer(IBF_IND)];
    out[,ID_ABA_PRIM:=as.integer(ID_ABA_PRIM)];
    #out[,ID_CUSIP:=as.factor(ID_CUSIP)];
    out[,ID_FDIC_CERT:=as.integer(ID_FDIC_CERT)];
    #out[,ID_LEI:=as.factor(ID_LEI)];
    out[,ID_NCUA:=as.integer(ID_NCUA)];
    out[,ID_OCC:=as.integer(ID_OCC)];
    out[,ID_RSSD:=as.integer(ID_RSSD)];
    out[,ID_RSSD_HD_OFF:=as.integer(ID_RSSD_HD_OFF)];
    out[,ID_TAX:=as.integer(ID_TAX)];
    out[,ID_THRIFT:=as.integer(ID_THRIFT)];
    #out[,ID_THRIFT_HC:=as.factor(ID_THRIFT_HC)];
    out[,INSUR_PRI_CD:=as.integer(INSUR_PRI_CD)];
    # Dummy
    out[,MBR_FHLBS_IND:=as.integer(MBR_FHLBS_IND)];
    out[,MBR_FRS_IND:=as.integer(MBR_FRS_IND)];
    out[,MJR_OWN_MNRTY:=as.integer(MJR_OWN_MNRTY)];
    #out[,NM_LGL:=as.factor(NM_LGL)];
    #out[,NM_SHORT:=as.factor(NM_SHORT)];
    out[,NM_SRCH_CD:=as.integer(NM_SRCH_CD)];
    out[,ORG_TYPE_CD:=as.integer(ORG_TYPE_CD)];
    out[,PLACE_CD:=as.integer(PLACE_CD)];
    out[,PRIM_FED_REG:=as.factor(PRIM_FED_REG)];
    out[,PROV_REGION:=as.factor(PROV_REGION)];
    out[,REASON_TERM_CD:=as.integer(REASON_TERM_CD)];
    out[,SEC_RPTG_STATUS:=as.integer(SEC_RPTG_STATUS)];
    out[,SLHC_IND:=as.integer(SLHC_IND)];
    out[,SLHC_TYPE_IND:=as.integer(SLHC_TYPE_IND)];
    out[,STATE_ABBR_NM:=as.factor(STATE_ABBR_NM)];
    out[,STATE_CD:=as.integer(STATE_CD)];
    out[,STATE_HOME_CD:=as.integer(STATE_HOME_CD)];
    out[,STATE_INC_ABBR_NM:=as.factor(STATE_INC_ABBR_NM)];
    out[,STATE_INC_CD:=as.integer(STATE_INC_CD)];
    # STREET_LINE1
    # STREET_LINE2
    # URL
    out[,ZIP_CD:=as.factor(ZIP_CD)];

    nic <<- rbindlist(list(nic,out),use.names=T,fill=T);
  }
  });

  #nrow(nic)
  #object.size(nic)

  saveRDS(nic,'./data/NIC/nic.rds',compress='xz');

  # 'ID_RSSD'
  # 'CHTR_TYPE_CD'
  # 'ENTITY_TYPE'
}
