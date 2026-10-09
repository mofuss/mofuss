"""Exercise exact production input routing in native Dinamica on tiny rasters.

Fixtures cover the inclusive transition, unchanged calendar, frozen LUC/TOF,
post-freeze growth resumption, missing optional parameter, named CSV lookup,
default-2050 parity, and LUC=1 with no annual files. No real run is modified.
"""
from __future__ import annotations
import argparse
import copy
import csv
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import xml.etree.ElementTree as E

sys.dont_write_bytecode = True


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--engine", type=Path, default=Path("C:/Program Files/Dinamica EGO/DinamicaConsole.exe"))
    parser.add_argument("--rscript", type=Path, default=Path("C:/Program Files/R/R-4.6.0/bin/Rscript.exe"))
    args = parser.parse_args()
    work = args.scratch.resolve()
    if "mofuss_active" not in {p.lower() for p in work.parts}:
        raise ValueError("Use a named scratch subfolder below MoFuSS_Active")
    if work.exists() and any(work.iterdir()):
        raise FileExistsError("Refusing to overwrite runtime evidence")
    work.mkdir(parents=True, exist_ok=True)
    scripts = Path(__file__).resolve().parents[1]
    sys.path.insert(0, str(scripts / "tools"))
    from build_dinamica_sourcing_v12 import node, port, calculate, filename, save
    from dinamica_v12_transform import _producers
    from add_woodman_freeze_year import validate_freeze_graph
    model_path = scripts / "10_dyn_Sc17_webmofuss_ctrees_g_v14.egoml"
    content = model_path.read_bytes()
    (work / "source_v14.egoml").write_bytes(content)
    original = E.fromstring(content)
    validate_freeze_graph(original)
    prod = _producers(original)
    parents = {child: parent for parent in original.iter() for child in parent}
    (work / "source_sha256.txt").write_text(hashlib.sha256(content).hexdigest())
    temp = work / "native_temp"
    temp.mkdir()
    env = os.environ.copy()
    env.update(TEMP=str(temp), TMP=str(temp), TMPDIR=str(temp))

    def load(root, relative, ident):
        item = node(root, "LoadMap", ident)
        port(item, "filename", '"' + relative + '"')
        for key, value in (("nullValue", ".none"), ("loadAsSparse", ".no"),
                           ("suffixDigits", "0"), ("step", ".none"), ("workdir", ".none")):
            port(item, key, value)
        E.SubElement(item, "outputport", name="map", id=ident)

    def constant(root, value, ident):
        item = node(root, "Int", ident)
        port(item, "constant", str(value))
        E.SubElement(item, "outputport", name="object", id=ident)

    def clone(root, ident):
        item = copy.deepcopy(prod[ident])
        root.append(item)
        return item

    cases = [("freeze2026", 3, 2026), ("default2050", 3, 2050),
             ("legacy_missing", 3, None), ("fixed_luc1", 1, 2026),
             ("freeze2000", 3, 2000)]
    setup = r'''suppressPackageStartupMessages(library(terra))
root<-commandArgs(TRUE)[1]
wr<-function(path,x){dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE);r<-rast(nrows=1,ncols=3,xmin=0,xmax=3,ymin=0,ymax=1,crs="EPSG:4326");values(r)<-x;writeRaster(r,path,overwrite=TRUE,datatype="FLT4S",NAflag=-2147483648)}
for(case in c("freeze2026","default2050","legacy_missing","fixed_luc1","freeze2000")){
 d<-file.path(root,case);dir.create(d,recursive=TRUE);dir.create(file.path(d,"debugging_1"))
 wr(file.path(d,"initial.tif"),c(50,20,NA));wr(file.path(d,"luc.tif"),c(2,5,2));wr(file.path(d,"tof.tif"),c(0,1,0));wr(file.path(d,"k.tif"),c(100,100,100))
 if(case=="fixed_luc1")next
 ys<-if(case=="freeze2000")2000 else if(case=="freeze2026")2025:2026 else 2025:2028
 for(y in ys){k<-as.character(y);lc<-switch(k,"2000"=c(3,2,2),"2025"=c(2,5,2),"2026"=c(3,2,2),"2027"=c(4,3,2),"2028"=c(2,4,2));tf<-switch(k,"2000"=c(0,0,0),"2025"=c(0,1,0),"2026"=c(0,0,0),"2027"=c(1,0,0),"2028"=c(0,1,0));tr<-switch(k,"2000"=c(2,4,0),"2025"=c(0,0,0),"2026"=c(2,4,0),"2027"=c(1,0,0),"2028"=c(2,3,0))
 wr(file.path(d,"LULCC/TempRaster",paste0("LULCt3_c_",y,".tif")),lc);wr(file.path(d,"LULCC/TempRaster",paste0("TOFvsFOR_mask3_",y,".tif")),tf);wr(file.path(d,"LULCC/TempRaster",paste0("LULCt3_transition_",y,".tif")),tr)
 }}'''
    (work / "setup.R").write_text(setup)
    subprocess.run([str(args.rscript), str(work / "setup.R"), str(work)],
                   capture_output=True, text=True, check=True, env=env)
    for label, luc, freeze in cases:
        case_dir = work / label
        start_year = 2000 if freeze == 2000 else 2025
        with (case_dir / "parameters.csv").open("w", newline="") as file:
            writer = csv.writer(file)
            writer.writerow(("Var", "ParCHR"))
            # Deliberately put the new parameter between unrelated rows.
            writer.writerow(("npa_ease", 10))
            if freeze is not None:
                writer.writerow(("woodman_luc_freeze_year", freeze))
            writer.writerow(("start_year", start_year))
        root = E.Element("script")
        E.SubElement(root, "property", key="dff.version", value="2.4.1.20140602")
        for value, ident in ((start_year, "v247"), (luc, "v302"), (1, "v38")):
            constant(root, value, ident)
        table = node(root, "LoadTable", "Named parameters")
        port(table, "filename", '"parameters.csv"')
        for key, value in (("suffixDigits", "0"), ("step", ".none"), ("workdir", ".none")):
            port(table, key, value)
        E.SubElement(table, "outputport", name="table", id="v267")
        for ident in ("v94000", "v94001", "v94002", "v94006", "v94004", "v94005"):
            clone(root, ident)
        for item in original.iter("functor"):
            if item.get("name") == "SaveLookupTable" and item.find("inputport[@name='filename']") is not None and item.find("inputport[@name='filename']").get("peerid") == "v94005":
                root.append(copy.deepcopy(item))
        for path, ident in (("initial.tif", "v200"), ("luc.tif", "v298"),
                            ("tof.tif", "v204"), ("k.tif", "v90010")):
            load(root, path, ident)
        annual = node(root, "Repeat", "Four true calendar years", True)
        port(annual, "iterations", "4")
        E.SubElement(annual, "internaloutputport", name="step", id="v39")
        for ident in ("v90001", "v94003", "v90030"):
            clone(annual, ident)
        # Clone both complete mutually exclusive input branches from production.
        annual.append(copy.deepcopy(parents[prod["v90032"]]))
        annual.append(copy.deepcopy(parents[prod["v90031"]]))
        for ident in ("v90020", "v90021", "v90007", "v90003", "v90005", "v90018", "v90019"):
            clone(annual, ident)
        stock = node(annual, "MuxMap", "Previous biomass")
        port(stock, "initial", peer="v200")
        port(stock, "feedback", peer="test_end")
        E.SubElement(stock, "outputport", name="map", id="v40")
        clone(annual, "v90008")
        # Transparent growth pulse makes replayed resets directly observable.
        calculate(annual, "Map", "Ten-tonne diagnostic growth pulse", "i1 + 10", "test_end", maps=("v90008",))
        calculate(annual, "Map", "Actual calendar raster", "if isNull(i1) then null else v1", "calendar", maps=("v200",), values=("v90001",))
        for stem, ident in (("luc", "v90003"), ("tof", "v90005"), ("transition", "v90018"),
                            ("start", "v90008"), ("end", "test_end"), ("calendar", "calendar")):
            filename(annual, stem + "_<v1>.tif", ("v90001",), "file_" + stem)
            save(annual, "Map", ident, "file_" + stem)
        E.indent(root, space="    ")
        fixture = case_dir / "model.egoml"
        E.ElementTree(root).write(fixture, encoding="utf-8", xml_declaration=True)
        result = subprocess.run([str(args.engine), "-processors", "1", "-predefined-seed", "-log-level", "3", str(fixture)],
                                cwd=case_dir, env=env, capture_output=True, text=True, timeout=90)
        (case_dir / "engine.log").write_text(result.stdout + result.stderr)
        if result.returncode:
            raise RuntimeError(label + ": " + result.stdout + result.stderr)
    check = r'''suppressPackageStartupMessages(library(terra));root<-commandArgs(TRUE)[1]
v<-function(c,s,y)as.numeric(values(rast(file.path(root,c,paste0(s,"_",y,".tif")))))[1:2]
for(y in 2025:2028){
 stopifnot(identical(v("freeze2026","calendar",y),rep(as.numeric(y),2)))
 for(s in c("luc","tof","transition","start","end","calendar"))stopifnot(identical(v("default2050",s,y),v("legacy_missing",s,y)))
 stopifnot(identical(v("fixed_luc1","luc",y),c(2,5)),identical(v("fixed_luc1","tof",y),c(0,1)),identical(v("fixed_luc1","transition",y),c(0,0)))
}
stopifnot(identical(v("freeze2000","transition",2000),c(2,4)),identical(v("freeze2000","start",2000),c(0,0)))
for(y in 2001:2003)stopifnot(identical(v("freeze2000","luc",y),c(3,2)),identical(v("freeze2000","transition",y),c(0,0)),identical(v("freeze2000","start",y),rep((y-2000)*10,2)))
for(y in 2025:2026)for(s in c("luc","tof","transition","start","end","calendar"))stopifnot(identical(v("freeze2026",s,y),v("default2050",s,y)))
stopifnot(identical(v("freeze2026","transition",2026),c(2,4)),identical(v("freeze2026","start",2026),c(0,0)))
for(y in 2027:2028){stopifnot(identical(v("freeze2026","luc",y),c(3,2)),identical(v("freeze2026","tof",y),c(0,0)),identical(v("freeze2026","transition",y),c(0,0)),identical(v("freeze2026","start",y),rep((y-2026)*10,2)))}
stopifnot(identical(v("default2050","luc",2027),c(4,3)),identical(v("default2050","tof",2027),c(1,0)),identical(v("default2050","transition",2027),c(1,0)))
for(c in c("freeze2026","default2050","legacy_missing","fixed_luc1","freeze2000"))for(y in if(c=="freeze2000")2000:2003 else 2025:2028)for(s in c("luc","tof","transition","start","end","calendar"))stopifnot(is.na(as.numeric(values(rast(file.path(root,c,paste0(s,"_",y,".tif")))))[3]))
for(c in c("freeze2026","default2050","legacy_missing","fixed_luc1","freeze2000")){x<-read.csv(file.path(root,c,"debugging_1/woodman_luc_execution.csv"),check.names=FALSE);stopifnot(identical(trimws(names(x)[1:2]),c("Key*","Value")),identical(x[[1]],1:4));expected<-c(if(c=="fixed_luc1")1 else 3,if(c=="freeze2000")2000 else if(c%in%c("freeze2026","fixed_luc1"))2026 else 2050,1,if(c=="freeze2000")2000 else 2025);stopifnot(all(x[[2]]==expected))}
cat("PASS: 5 native fixtures x 4 years; inclusive freeze; no replay; class and TOF held; calendar unchanged; optional default and named lookup; LUC1 requires no annual inputs; initial NoData preserved; actual per-MC settings recorded.\n")'''
    (work / "check.R").write_text(check)
    result = subprocess.run([str(args.rscript), str(work / "check.R"), str(work)],
                            capture_output=True, text=True, env=env)
    (work / "summary.txt").write_text(result.stdout + result.stderr)
    print(result.stdout + result.stderr)
    if result.returncode:
        raise SystemExit(result.returncode)


if __name__ == "__main__":
    main()
