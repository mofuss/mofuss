"""Four-cell actual-engine proof of the domestic-W TOF correction.

Uses the production graph transformer, with two origin countries and three
explicit edge cases. Outputs are limited to the caller-supplied scratch root.
"""
from __future__ import annotations
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import xml.etree.ElementTree as E

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "tools"))
from build_dinamica_sourcing_v12 import add_domestic_tof_correction, node, port, calculate, filename


def load(parent, path, output, dynamic=False):
    n=node(parent,"LoadMap","Fixture "+output)
    port(n,"filename",peer=path) if dynamic else port(n,"filename",'"'+path+'"')
    for key,value in (("nullValue",".none"),("loadAsSparse",".no"),("suffixDigits","0"),("step",".none"),("workdir",".none")):
        port(n,key,value)
    E.SubElement(n,"outputport",name="map",id=output)


def attrs(parent, source, output):
    n=node(parent,"ExtractMapAttributes","Fixture attributes "+output)
    port(n,"map",peer=source);port(n,"extractDynamicAttributes",".yes");port(n,"extractStatisticalAttributes",".no")
    E.SubElement(n,"outputport",name="attributes",id=output)


def make_model(inputs):
    root=E.Element("script")
    E.SubElement(root,"property",key="dff.version",value="2.4.1.20140602")
    for stem,peer in (("landcover","v204"),("capacity","v213"),("zero","v191"),("forest_aggregate","v91")):
        load(root,(inputs/f"{stem}.tif").as_posix(),peer)
    loop=node(root,"ForEach","Two domestic W origins",True)
    port(loop,"elements",'[\n"Key" "Value"\n1 1\n2 1\n]')
    E.SubElement(loop,"internaloutputport",name="step",id="v356")
    step=node(loop,"Step","Origin index");port(step,"step",peer="v356");E.SubElement(step,"outputport",name="step",id="v357")
    for stem,peer,path in (("pressure","v366","v600"),("weights","v364","v601")):
        filename(loop,(inputs/(stem+"<v1>.tif")).as_posix(),("v357",),path)
        load(loop,path,peer,True)
    mux=node(loop,"MuxMap","Original W accumulator");port(mux,"initial",peer="v191");port(mux,"feedback",peer="v368");E.SubElement(mux,"outputport",name="map",id="v87")
    calculate(loop,"Map","Original W sum","i1+i2","v368",maps=("v87","v366"))
    calculate(root,"Map","Original regional TOF deficit","if i1 = 1 then if i2 > i3 then i2-i3 else 0 else 0","v89",maps=("v204","v368","v213"))
    attrs(root,"v89","v42");attrs(root,"v91","v43")
    calculate(root,"Map","Original pooled TOF redistribution","i1 * t1[12] / t2[12]","v93",maps=("v91",),tables=("v42","v43"))
    calculate(root,"Map","Total requested harvest","if isNull(i3) then i1-i2 else i1-i2+i3","v95",maps=("v368","v89","v93"))
    calculate(root,"Map","Realized harvest with abundant stock","max(0,min(i1,1000))","v130",maps=("v95",))
    for stem,peer in (("redistribution","v93"),("requested","v95"),("realized","v130")):
        n=node(root,"SaveMap","Save "+stem);port(n,"map",peer=peer);port(n,"filename",'"'+stem+'.tif"')
        for key,value in (("suffixDigits","0"),("step",".none"),("useCompression",".yes"),("workdir",".none")):
            port(n,key,value)
    E.indent(root,space="    ")
    return E.tostring(root,encoding="unicode")


def main():
    ap=argparse.ArgumentParser();ap.add_argument("--scratch",type=Path,required=True)
    ap.add_argument("--engine",type=Path,default=Path(r"C:\Program Files\Dinamica EGO\DinamicaConsole.exe"))
    ap.add_argument("--rscript",type=Path,default=Path(r"C:\Program Files\R\R-4.6.0\bin\Rscript.exe"))
    args=ap.parse_args();args.scratch.mkdir(parents=True,exist_ok=True)
    setup='''suppressPackageStartupMessages(library(terra))
args<-commandArgs(TRUE);root<-args[1]
cases<-c("crossborder","no_domestic_forest","zero_deficit")
for(case in cases){
 d<-file.path(root,case,"inputs");dir.create(d,recursive=TRUE,showWarnings=FALSE)
 write<-function(name,x){r<-rast(nrows=1,ncols=4,xmin=0,xmax=4,ymin=0,ymax=1,crs="EPSG:4326");values(r)<-x;writeRaster(r,file.path(d,paste0(name,".tif")),overwrite=TRUE,datatype="FLT4S",NAflag=-2147483648)}
 write("landcover",c(1,0,1,0));write("capacity",c(if(case=="zero_deficit")1000 else 20,0,1000,0));write("zero",rep(0,4))
 write("weights1",c(1,if(case=="no_domestic_forest")0 else 1,0,0));write("weights2",c(0,0,1,1))
 write("pressure1",c(100,if(case=="no_domestic_forest")0 else 100,0,0));write("pressure2",c(0,0,10,10))
 write("forest_aggregate",c(NA,if(case=="no_domestic_forest")0 else 1,NA,1))
}
'''
    setup_path=args.scratch/"setup.R";setup_path.write_text(setup,encoding="utf-8")
    subprocess.run([str(args.rscript),str(setup_path),str(args.scratch)],check=True,capture_output=True,text=True)
    for case in ("crossborder","no_domestic_forest","zero_deficit"):
        baseline=make_model(args.scratch/case/"inputs")
        corrected,_=add_domestic_tof_correction(baseline)
        for version,text in (("v11",baseline),("v13",corrected)):
            work=args.scratch/case/version;work.mkdir(parents=True,exist_ok=True)
            model=work/"model.egoml";model.write_text(text,encoding="utf-8")
            tmp=work/"native_temp";tmp.mkdir(exist_ok=True)
            env=os.environ.copy();env["TEMP"]=str(tmp);env["TMP"]=str(tmp)
            p=subprocess.run([str(args.engine),"-processors","1","-log-level","3",str(model)],cwd=work,env=env,capture_output=True,text=True,timeout=120)
            (work/"engine.log").write_text(p.stdout+p.stderr,encoding="utf-8")
            if p.returncode:raise RuntimeError(p.stdout+p.stderr)
    check='''suppressPackageStartupMessages(library(terra));root<-commandArgs(TRUE)[1]
val<-function(case,version,stem)as.numeric(values(rast(file.path(root,case,version,paste0(stem,".tif")))))
stopifnot(identical(val("crossborder","v11","realized"),c(20,140,10,50)))
stopifnot(identical(val("crossborder","v13","realized"),c(20,180,10,10)))
stopifnot(identical(val("no_domestic_forest","v11","realized"),c(20,0,10,90)))
stopifnot(identical(val("no_domestic_forest","v13","realized"),c(20,0,10,10)))
stopifnot(identical(val("zero_deficit","v11","realized"),val("zero_deficit","v13","realized")))
for(case in c("crossborder","no_domestic_forest","zero_deficit"))for(version in c("v11","v13"))cat(case,version,paste(val(case,version,"realized"),collapse=","),"\\n")
'''
    check_path=args.scratch/"check.R";check_path.write_text(check,encoding="utf-8")
    p=subprocess.run([str(args.rscript),str(check_path),str(args.scratch)],check=True,text=True,capture_output=True)
    def sha(path):return hashlib.sha256(path.read_bytes()).hexdigest()
    old=sha(args.scratch/"zero_deficit/v11/realized.tif");new=sha(args.scratch/"zero_deficit/v13/realized.tif")
    report=dict(all_cases_passed=True,engine="actual native Dinamica",crossborder_export_removed=40,
                no_domestic_forest_unmet=80,zero_deficit_output_sha256_old=old,
                zero_deficit_output_sha256_new=new,zero_deficit_byte_identical=old==new,values=p.stdout)
    (args.scratch/"bugproof_report.json").write_text(json.dumps(report,indent=2),encoding="utf-8")
    assert old==new,"No-deficit realized raster differs at the byte level"
    print(json.dumps(report,indent=2))


if __name__=="__main__":main()
