"""Native Windows regression for v13-compatible v14 pixel mechanics.

Copies exact production expression nodes into isolated miniature graphs. Tests
Fixed-cover cases (numeric zero, positive stock, NULL stock, TOF/forest and
missing LUC), three years, actual capped logistic and uncapped Chapman-Richards
branches, positive/zero demand, sold-fuelwood domains, and four transition codes.
The stock inputs are model states, not raw AGB preprocessing. Category parameter
lookup is supplied explicitly; full input initialization and sourcing are covered
by run_woodman_dinamica_v14_full_regression.py instead.

All maps, compiler TEMP and source snapshots are saved to an explicitly supplied
new scratch directory under MoFuSS_Active. No production folder is modified.
"""
from __future__ import annotations
import argparse
from pathlib import Path


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--engine", type=Path, default=Path(r"C:/Program Files/Dinamica EGO/DinamicaConsole.exe"))
    parser.add_argument("--rscript", type=Path, default=Path(r"C:/Program Files/R/R-4.6.0/bin/Rscript.exe"))
    parser.add_argument("--v13", type=Path)
    parser.add_argument("--v14", type=Path)
    args = parser.parse_args()
    import copy, csv, hashlib, json, os, subprocess, sys, xml.etree.ElementTree as E
    ROOT=args.scratch.resolve()
    if not any(p.lower()=="mofuss_active" for p in ROOT.parts) or ROOT.parent==ROOT:
     raise ValueError("Use a named scratch subfolder below MoFuSS_Active")
    if ROOT.exists() and any(ROOT.iterdir()):
     raise FileExistsError("Refusing to overwrite nonempty runtime evidence")
    SRC=Path(__file__).resolve().parents[1]
    R=args.rscript
    ENGINE=args.engine
    ROOT.mkdir(parents=True,exist_ok=True)
    r_temp=ROOT/'r_temp';r_temp.mkdir(exist_ok=True)
    os.environ.update(TEMP=str(r_temp),TMP=str(r_temp),TMPDIR=str(r_temp))
    sys.dont_write_bytecode = True
    sys.path.insert(0,str(SRC/'tools'))
    from build_dinamica_sourcing_v12 import node,port,calculate
    models={}; hashes={}
    for ver in (13,14):
     p=(args.v13 if ver==13 else args.v14) or SRC/f'10_dyn_Sc17_webmofuss_ctrees_g_v{ver}.egoml'
     b=p.read_bytes();hashes[f'v{ver}']=hashlib.sha256(b).hexdigest()
     (ROOT/f'source_v{ver}.egoml').write_bytes(b)
     r=E.fromstring(b); models[ver]={op.get('id'):n for n in r.iter() for op in n.findall('outputport')}
    (ROOT/'source_hashes.json').write_text(json.dumps(hashes,indent=2))

    def load(root,path,out):
     n=node(root,'LoadMap','Input '+out);port(n,'filename','"'+str(path.as_posix())+'"')
     for k,v in [('nullValue','.none'),('loadAsSparse','.no'),('suffixDigits','0'),('step','.none'),('workdir','.none')]:port(n,k,v)
     E.SubElement(n,'outputport',name='map',id=out)
    def save(root,peer,path):
     n=node(root,'SaveMap','Save '+path);port(n,'map',peer=peer);port(n,'filename','"'+path+'"')
     for k,v in [('suffixDigits','0'),('step','.none'),('useCompression','.yes'),('workdir','.none')]:port(n,k,v)
    def clone(root,ver,peer,mapping,out):
     n=copy.deepcopy(models[ver][peer]);n.find('outputport').set('id',out)
     for p in n.iter('inputport'):
      old=p.get('peerid')
      if old:
       if old not in mapping:raise KeyError((ver,peer,old))
       p.set('peerid',mapping[old])
     for p in n.findall('property'):
      if p.get('key')=='dff.functor.alias':p.set('value',p.get('value')+' '+out)
     root.append(n);return out

    def runmodel(root,work):
     work.mkdir(exist_ok=True);E.indent(root,space='    ')
     model=work/'model.egoml';E.ElementTree(root).write(model,encoding='utf-8',xml_declaration=True)
     temp=work/'native_temp';temp.mkdir(exist_ok=True)
     env=os.environ.copy();env['TEMP']=str(temp);env['TMP']=str(temp)
     p=subprocess.run([str(ENGINE),'-processors','1','-predefined-seed','-log-level','3',str(model)],cwd=work,env=env,capture_output=True,text=True,timeout=90)
     (work/'engine.log').write_text(p.stdout+p.stderr)
     if p.returncode:raise RuntimeError(p.stdout+p.stderr)

    def scriptroot():
     root=E.Element('script');E.SubElement(root,'property',key='dff.version',value='2.4.1.20140602');return root

    setup=r"""suppressPackageStartupMessages(library(terra));root<-commandArgs(TRUE)[1];dir.create(file.path(root,'inputs'),showWarnings=FALSE)
    x<-data.frame(case=c('forest_zero','forest_positive','forest_null','forest_zero_K0','forest_above_K','tof_zero','tof_positive','null_luc','forest_null_baseline_positive_current','forest_gain','forest_loss','tof_gain','tof_loss'),initial=c(0,50,NA,0,150,0,100,NA,NA,50,50,0,100),previous=c(0,50,NA,0,150,0,100,NA,50,50,50,0,100),luc=c(2,2,2,2,2,3,3,NA,2,2,3,3,2),tof=c(0,0,0,0,0,1,1,NA,0,0,1,1,0),category_k=c(100,100,100,0,100,0,100,NA,100,100,100,100,100),transition=c(0,0,0,0,0,0,0,0,0,2,1,3,4))
    x<-rbind(x,data.frame(case='forest_return_NULL_K',initial=NA,previous=0,luc=2,tof=0,category_k=100,transition=2))
    for(code in 1:4)x<-rbind(x,data.frame(case=paste0('initial_NULL_transition',code),initial=NA,previous=NA,luc=if(code%in%c(1,3))3 else 2,tof=as.numeric(code%in%c(1,3)),category_k=100,transition=code))
    x<-rbind(x,data.frame(case='forest_missing_CR',initial=50,previous=50,luc=2,tof=0,category_k=100,transition=0))
x<-rbind(transform(x,demand=0),transform(x,demand=30));x$case<-paste0(x$case,'_demand',x$demand);x$rate<-ifelse(x$tof==1,20,.1);x$zero<-0
    x$A<-100;x$k<-.05;x$m<-2;x$baseline_luc<-x$luc;x$baseline_luc[x$transition==1]<-2;x$baseline_luc[x$transition==2]<-3;x$baseline_luc[x$transition==4]<-3;x$baseline_luc[grepl('^forest_return_NULL_K_',x$case)]<-2
    x[grepl('^forest_missing_CR_',x$case),c('A','k','m')]<-NA
    wr<-function(stem,x){r<-rast(nrows=1,ncols=length(x),xmin=0,xmax=length(x),ymin=0,ymax=1,crs='EPSG:4326');values(r)<-x;writeRaster(r,file.path(root,'inputs',paste0(stem,'.tif')),overwrite=TRUE,datatype='FLT4S',NAflag=-2147483648)}
    for(n in setdiff(names(x),'case'))wr(n,x[[n]]);write.csv(x,file.path(root,'cases.csv'),row.names=FALSE,na='')
    wr('mask_luc',c(1,2,3,1,2,3,NA,2));wr('mask_tof',c(0,0,0,1,1,1,NA,NA));wr('mask_initial',rep(50,8))
wr('k_luc0',c(2,2,2,NA));wr('k_initial',c(0,50,NA,NA));wr('k_luc1',c(2,2,2,2));wr('k_luc2',c(3,3,3,2));wr('k_luc3',c(2,2,2,2));wr('k_category',rep(100,4))
    """
    (ROOT/'setup.R').write_text(setup)
    subprocess.run([str(R),str(ROOT/'setup.R'),str(ROOT)],cwd=ROOT,capture_output=True,text=True,check=True)
    for ver in (13,14):
     for mode in ('capped','uncapped_CR'):
      root=scriptroot()
      for stem in ('initial','previous','luc','baseline_luc','tof','category_k','transition','rate','zero','demand','A','k','m'):load(root,ROOT/'inputs'/f'{stem}.tif','in_'+stem)
      clone(root,ver,'v211',{'v210':'in_initial','v203':'in_category_k'},'effective_k')
      if ver==14:
       clone(root,ver,'v90003',{'v90020':'in_luc','v200':'in_initial'},'current_luc')
       clone(root,ver,'v90005',{'v90021':'in_tof','v200':'in_initial'},'current_tof')
      prev='in_previous';prev_k='effective_k';prev_luc='in_baseline_luc'
      for yr in range(1,4):
       out=lambda s:f'y{yr}_{s}'
       transition='in_transition' if yr==1 else 'in_zero'
       base={'v40':prev,'v200':'in_initial','v298':'in_baseline_luc','v204':'in_tof','v209':'effective_k','v213':'in_rate','v112':'in_zero','v114':'in_zero','v95':'in_demand','v90':'in_zero','v90003':'current_luc' if ver==14 else 'in_luc','v90005':'current_tof' if ver==14 else 'in_tof','v90010':'in_category_k','v90018':transition,'v172':'in_k','v173':'in_m','v174':'in_A','v90019':prev_luc}
       if ver==14:
        for src in ('v90012','v90008'):
         clone(root,ver,src,base,out(src));base[src]=out(src)
        calculate(root,'Map','Transition annual rate','if isNull(i1) or isNull(i2) then null else if i2=1 or i2=2 or i2=4 then 0 else i3',out('rate'),maps=('current_luc',transition,'in_rate'))
        base['v90011']=out('rate');save(root,base['v90008'],f'y{yr}_start.tif');prev_k=base['v90012'];prev_luc='current_luc'
       else:save(root,prev,f'y{yr}_start.tif')
       seq=('v177','v178') if mode=='uncapped_CR' else ('v180','v353')
       for src in (*seq,'v130','v131','v98'):
        clone(root,ver,src,base,out(src));base[src]=out(src)
        if src==seq[-1]:base['v171']=out(src)
       for key,src in [('available',seq[-1]),('harvest','v130'),('end','v98')]:save(root,base[src],f'y{yr}_{key}.tif')
       prev=base['v98']
      runmodel(root,ROOT/f'v{ver}_{mode}')
    root=scriptroot();load(root,ROOT/'inputs/mask_luc.tif','luc');load(root,ROOT/'inputs/mask_tof.tif','tof')
    clone(root,13,'v317',{'v316':'tof','v298':'luc'},'old_mask');clone(root,14,'v90013',{'v90003':'luc','v90005':'tof'},'new_mask')
    clone(root,13,'v192',{'v317':'old_mask'},'old_domain');clone(root,14,'v90015',{'v90013':'new_mask'},'new_domain')
    for s in ('old_mask','new_mask','old_domain','new_domain'):save(root,s,s+'.tif')
    runmodel(root,ROOT/'domain_probe')
    # A return to the baseline class restores its calibrated K, including 0/NULL.
    root=scriptroot()
    for stem in ('k_luc0','k_initial','k_luc1','k_luc2','k_luc3','k_category'):
        load(root,ROOT/'inputs'/f'{stem}.tif',stem)
    for yr in range(1,4):
        clone(root,14,'v90003',{'v90020':f'k_luc{yr}','v200':'k_initial'},f'eligible_luc{yr}')
        clone(root,14,'v90012',{'v90003':f'eligible_luc{yr}','v90010':'k_category','v298':'k_luc0','v209':'k_initial'},f'k{yr}')
        save(root,f'k{yr}',f'k{yr}.tif')
    runmodel(root,ROOT/'k_return_probe')
    check=r"""suppressPackageStartupMessages(library(terra));root<-commandArgs(TRUE)[1];x<-read.csv(file.path(root,'cases.csv'));val<-function(dir,name)as.numeric(values(rast(file.path(root,dir,paste0(name,'.tif')))))
    res<-list();i<-0
    for(mode in c('capped','uncapped_CR'))for(ver in c(13,14))for(yr in 1:3){i<-i+1;a<-x;a$version<-ver;a$mode<-mode;a$year<-yr;for(k in c('start','available','harvest','end'))a[[k]]<-val(paste0('v',ver,'_',mode),paste0('y',yr,'_',k));res[[i]]<-a}
    a<-do.call(rbind,res);write.csv(a,file.path(root,'pixel_results.csv'),row.names=FALSE,na='')
    fixed<-x$transition==0 & !grepl('^forest_null_baseline_positive_current_',x$case);parity<-list();for(mode in c('capped','uncapped_CR'))for(yr in 1:3)for(k in c('start','available','harvest','end')){v13<-val(paste0('v13_',mode),paste0('y',yr,'_',k))[fixed];v14<-val(paste0('v14_',mode),paste0('y',yr,'_',k))[fixed];stopifnot(identical(is.na(v13),is.na(v14)),identical(v13[!is.na(v13)],v14[!is.na(v14)]))}
    m<-data.frame(luc=val('inputs','mask_luc'),tof=val('inputs','mask_tof'));for(k in c('old_mask','new_mask','old_domain','new_domain'))m[[k]]<-val('domain_probe',k);write.csv(m,file.path(root,'domain_results.csv'),row.names=FALSE,na='');stopifnot(identical(m$old_mask,m$new_mask),identical(m$old_domain,m$new_domain))
    stopifnot(identical(val('k_return_probe','k1'),c(0,50,NaN,NaN)),identical(val('k_return_probe','k2'),c(100,100,NaN,NaN)),identical(val('k_return_probe','k3'),c(0,50,NaN,NaN)))
    v<-a[a$version==14 & a$year==1 & a$transition %in% c(1,2,4) & is.finite(a$initial),];stopifnot(all(v$available==0),all(v$harvest==0),all(v$end==0))
    excluded<-a[a$version==14 & !is.finite(a$initial),];stopifnot(all(is.na(excluded$start)),all(is.na(excluded$available)),all(excluded$harvest==0),all(is.na(excluded$end)))
    cr<-a[a$version==14 & grepl('^forest_missing_CR_',a$case),];stopifnot(all(is.finite(cr$available[cr$mode=='capped'])),all(is.na(cr$available[cr$mode=='uncapped_CR'])))
    cat('PASS: fixed-LUC v13/v14 exact finite values and NULL masks across',sum(fixed),'cases x 3 years x 2 modes x 4 states; all mask cases equal; eligible conversion1/2/4 availability, harvest, end stock zero; immutable missing-initial exclusion including every transition and counterfactual finite prior stock; CR gap unchanged.\n');print(a[a$version==14 & a$case %in% c('forest_zero_demand0','forest_null_demand0','forest_gain_demand30','tof_loss_demand30'),c('case','mode','year','start','available','harvest','end')],row.names=FALSE)
    """
    (ROOT/'read_results.R').write_text(check)
    p=subprocess.run([str(R),str(ROOT/'read_results.R'),str(ROOT)],cwd=ROOT,capture_output=True,text=True)
    (ROOT/'summary.txt').write_text(p.stdout+p.stderr);print(p.stdout+p.stderr)
    if p.returncode:raise SystemExit(p.returncode)


if __name__ == "__main__":
    main()
