#!/usr/bin/env Rscript
# Small synthetic tests only. Disposable rasters stay in the approved task scratch.
source("localhost/scripts/postprocessing_sourcing/2post_runtime_sourcing_v1.R")
.rs_require()
library(data.table)
library(terra)

check <- function(ok,label) { if(!isTRUE(ok))stop(label,call.=FALSE); cat("PASS ",label,"\n",sep="") }
expect_error <- function(expr,pattern) {
  error <- tryCatch({force(expr);NULL},error=conditionMessage)
  check(!is.null(error)&&grepl(pattern,error),paste("rejects",pattern))
}
encode <- function(x) {
  if(x==0)return(c(0,0,0))
  e<-max(-1022,min(1023,floor(log(abs(x))/log(2))))
  z<-x*2^(-e)*2^26; hi<-floor(z); c(e,hi,(z-hi)*2^27)
}
x<-c(0,1,pi,1e-15,1e12,12.345678901234567)
triples<-vapply(x,encode,numeric(3))
check(identical(.rs_decode(triples[1,],triples[2,],triples[3,]),x),"binary64 codec round trip")
check(identical(.rs_f32(c(1/3,1,NA_real_)),as.double(c(.rs_f32(1/3),1,NA_real_))),"float32 memory cast")
s<-c(denominator=2,demand=100,weeks=1,slices=1,adjustment=0,source_step=1)
check(identical(.rs_normalize(rep(1,4),c(0,1,2,255),s),c(0,0,50,NA_real_)),"four-state mask reconstruction")
s_increase<-s;s_increase[["adjustment"]]<- -25
check(.rs_normalize(1,2,s_increase)==62.5,"negative demand knob preserves demand increase")
check(identical(.rs_accumulate(list(c(NA_real_,1),c(NA_real_,2)),TRUE,c(0,0)),c(0,3)),"null-union retains initial zero accumulator")
expect_error(.rs_match_pressure(c(1,2),c(1,3),"synthetic",c(TRUE,TRUE)),"differs")

scratch<-file.path("E:/MoFuSS_Active/ecsa_sourcing_and_speed_v1/runtime_sourcing_tests",
                   paste0(format(Sys.time(),"%Y%m%d_%H%M%S"),"_",Sys.getpid()))
dir.create(file.path(scratch,"Sourcing","static"),recursive=TRUE)
dir.create(file.path(scratch,"Sourcing","MC001"),recursive=TRUE)
dir.create(file.path(scratch,"debugging_1"),recursive=TRUE)
template<-rast(nrows=2,ncols=2,xmin=0,xmax=2,ymin=0,ymax=2,crs="EPSG:4326")
put<-function(values,path,datatype="FLT4S") {
  r<-template; values(r)<-values
  writeRaster(r,path,overwrite=TRUE,datatype=datatype)
}
zones<-template; values(zones)<-c(1,1,2,2)
zonepath<-file.path(scratch,"zones.tif");writeRaster(zones,zonepath,datatype="INT1U")
zones<-rast(zonepath)
put(rep(0,4),file.path(scratch,"Sourcing","static","accumulator_domain.tif"),"INT1U")
cw<-data.table(source_id=1:2,source_iso3=c("AAA","BBB"),source_name=c("A","B"))
indices<-list(W=data.table(ComponentIndex=1:2,DemandISO3=c("AAA","BBB"),AllowedSourceISO3=c("AAA","BBB")),
              V=data.table(ComponentIndex=1:2,DemandISO3=c("AAA","BBB"),AllowedSourceISO3=c("AAA;BBB","BBB")))
bases<-list(W1=c(1,1,0,0),W2=c(0,0,1,1),V1=c(1,0,1,0),V2=c(0,0,0,1))
scalar_values<-list(W1=c(2,100,1,1,0,1),W2=c(2,100,1,1,0,1),V1=c(2,40,1,1,0,1),V2=c(1,20,1,1,0,1))
save_scalars<-function(ch,id,values) {
  bits<-as.vector(vapply(values,encode,numeric(3)))
  fwrite(data.table(Key=seq_along(bits),Value=bits),
         file.path(scratch,"Sourcing","MC001",sprintf("%s_scalars%03d_01.csv",ch,id)))
}
for(ch in c("W","V")) {
  put(rep(2,4),file.path(scratch,"Sourcing","MC001",sprintf("mask_%s01.tif",ch)),"INT1U")
  for(i in 1:2) {
    put(bases[[paste0(ch,i)]],file.path(scratch,"Sourcing","static",sprintf("%s_base%03d_01.tif",ch,i)))
    save_scalars(ch,i,scalar_values[[paste0(ch,i)]])
  }
}
maps<-list(Proj_harv_Wtot=rep(50,4),Proj_harv_Vtot=c(20,0,20,20),
 Proj_harv_Wdef=rep(50,4),Proj_harv_Vdef=c(20,0,20,20),Non_harv_AGR=c(10,0,0,0),
 Ex_agr_harv=c(0,5,0,5),harv_AGR=c(40,50,50,50),Expect_harv_tot=c(60,55,70,75),Harvest_tot=c(30,55,35,75))
save_maps<-function() for(stem in names(maps))put(maps[[stem]],file.path(scratch,"debugging_1",paste0(stem,"01.tif")))
save_maps()
meta<-list(run=scratch,run_name="synthetic_bau",start=2020,end=2021,mc=1,regrowth="capped")
out<-.rs_year(meta,1,2020,zones,cw,indices,block_mb=1)
check(abs(sum(out$matrix$realised_harvest_tonnes)-195)<1e-10,"v12 harvest reconciles")
check(out$qa$crossborder_pooled_W_realised_tonnes==5,"v12 pooled W crossing is explicit")
check(out$qa$forbidden_direct_W_realised_tonnes==0,"domestic direct W stays domestic")
summary<-.rs_summarize(out$matrix,out$demand)
ken<-summary$origin[origin_iso3=="AAA" & channel=="W"]
check(ken$domestic_harvest_tonnes==75 && ken$imported_harvest_tonnes==5,"origin domestic/import volumes")
check(ken$domestic_share_pct==93.75 && ken$import_share_pct==6.25,"percentage is bounded ratio of harvest")
combined<-summary$origin[origin_iso3=="AAA" & channel=="W+V"]
check(combined$realised_harvest_tonnes==100 && combined$domestic_harvest_tonnes==85 &&
      combined$import_share_pct==15,"combined W+V sums volumes before percentages")

# Period shares must divide summed volumes, not average annual percentages.
m2<-copy(out$matrix); m2[,year:=2021]
d2<-copy(out$demand);d2[,year:=2021]
m2[origin_iso3=="AAA" & channel=="W" & source_iso3=="BBB",realised_harvest_tonnes:=15]
period<-.rs_summarize(rbindlist(list(out$matrix,m2)),rbindlist(list(out$demand,d2)),.rs_periods("2020:2021"))
p<-period$origin[origin_iso3=="AAA" & channel=="W"]
check(abs(p$import_share_pct-100*p$imported_harvest_tonnes/p$realised_harvest_tonnes)<1e-12,"period ratio uses summed mass")
expect_error(.rs_summarize(out$matrix,out$demand,.rs_periods("2020:2021")),"Incomplete annual")

# Preserve a negative W adjustment; never turn it into positive physical trade.
maps$Proj_harv_Wdef[1]<-0;maps$harv_AGR[1]<- -10
maps$Expect_harv_tot[1]<-10;maps$Harvest_tot[1]<-5;save_maps()
signed<-.rs_year(meta,1,2020,zones,cw,indices,block_mb=1)
ss<-.rs_summarize(signed$matrix,signed$demand)$origin[origin_iso3=="AAA" & channel=="W"]
check(ss$negative_realised_tonnes==5 && is.na(ss$import_share_pct),"signed W retained and trade shares withheld")
expect_error(.rs_year(meta,1,2020,zones,cw,indices,block_mb=1,signed_policy="error"),"Signed negative W")

# v13 has each W origin's own exact TOF shortfall and eligible forest sum.
save_scalars("W",1,c(scalar_values$W1,10,1));save_scalars("W",2,c(scalar_values$W2,0,2))
put(c(0,1,1,1),file.path(scratch,"Sourcing","MC001","forest_state01.tif"),"INT1U")
maps$Proj_harv_Wdef<-rep(50,4);maps$harv_AGR<-c(40,50,50,50)
maps$Ex_agr_harv<-c(0,10,0,0);maps$Expect_harv_tot<-c(60,60,70,70)
maps$Harvest_tot<-maps$Expect_harv_tot/2;save_maps()
v13<-.rs_year(meta,1,2020,zones,cw,indices,block_mb=1)
check(v13$qa$tof_mechanism=="origin_preserving_v13","v13 schema detection")
check(v13$qa$crossborder_origin_preserving_W_realised_tonnes==0,"v13 domestic W redistribution")
check(abs(sum(v13$matrix$realised_harvest_tonnes)-sum(maps$Harvest_tot))<1e-10,"v13 harvest reconciliation")
check(!any(v13$matrix$term=="pooled_TOF_redistribution"),"v13 never mislabeled pooled")
# Exercise the public CLI entry point against the synthetic one-year capture.
dir.create(file.path(scratch,"LULCC","TempTables"),recursive=TRUE)
dir.create(file.path(scratch,"In","DemandScenarios"),recursive=TRUE)
fwrite(data.table(Var=c("start_year","end_year","monte_carlo_runs","uncapped_regrowth"),
                 ParCHR=c(2020,2020,1,0)),file.path(scratch,"LULCC","TempTables","parameters_dinamica.csv"))
for(ch in c("W","V"))fwrite(indices[[ch]],file.path(scratch,"In","DemandScenarios",paste0(ch,"_origin_component_index.csv")))
crosswalk_path<-file.path(scratch,"crosswalk.csv");fwrite(cw,crosswalk_path)
output_path<-file.path(scratch,"output")
public<-rs_main(c(paste0("--run-dir=",scratch),paste0("--zones=",zonepath),
                  paste0("--crosswalk=",crosswalk_path),paste0("--output-dir=",output_path),
                  "--periods=2020:2020","--mc-runs=1","--block-mb=1"))
check(file.exists(file.path(output_path,"period_origin_balance.csv")),"CLI writes period balances")
check(all(abs(public$qa$origin_source_reconciliation_residual_tonnes)<1e-10),"CLI QA reconciles origin-source ledger")
check(grepl("no_complete_frozen",public$qa$metadata_provenance),"missing provenance is explicit")
relative_index<-file.path("In","DemandScenarios","W_origin_component_index.csv")
frozen_index<-file.path(scratch,"Sourcing","metadata","input_snapshot",relative_index)
dir.create(dirname(frozen_index),recursive=TRUE)
file.copy(file.path(scratch,relative_index),frozen_index)
check(identical(.rs_index(scratch,"W")$DemandISO3,c("AAA","BBB")),"frozen index used after identity verification")
changed<-copy(indices$W);changed[1,DemandISO3:="CCC"]
fwrite(changed,file.path(scratch,relative_index))
expect_error(.rs_index(scratch,"W"),"differs from frozen runtime snapshot")
file.copy(frozen_index,file.path(scratch,relative_index),overwrite=TRUE)
cat("ALL RUNTIME SOURCING TESTS PASSED\nScratch: ",scratch,"\n",sep="")
