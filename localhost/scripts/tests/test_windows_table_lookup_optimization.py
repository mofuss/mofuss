"""Native exact-equality microprobe for Windows MC row lookup optimization.

Fixtures and engine outputs are written only under the explicit --scratch path
inside MoFuSS_Active. The production graph is read only. No canonical model or
simulation folder is changed. Timings include native compilation and file I/O.
"""
from __future__ import annotations
import argparse
import copy
import csv
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time
import xml.etree.ElementTree as ET

TABLE_INPUTS = {
    "v201": {1: "v242", 2: "v244"}, "v203": {1: "v244"},
    "v207": {2: "v244"}, "v213": {1: "v243"},
    "v90010": {1: "v244"}, "v90011": {1: "v243"},
}


def matrix_reference(node, output):
    """Keep the test baseline available after canonical graphs are optimized."""
    node = copy.deepcopy(node)
    expression = node.find("inputport[@name='expression']")
    expression.text = re.sub(r"t(\d+)\[i1\s*\+\s*1\]", r"t\1[[v1][i1 + 1]]", expression.text)
    for hook in node.findall("functor"):
        if hook.get("name") == "NumberTable":
            slot = int(hook.find("inputport[@name='tableNumber']").text)
            hook.find("inputport[@name='table']").set("peerid", TABLE_INPUTS[output][slot])
    if not any(n.get("name") == "NumberValue" and n.find("inputport[@name='valueNumber']").text == "1"
               for n in node.findall("functor")):
        hook = ET.SubElement(node, "functor", name="NumberValue")
        ET.SubElement(hook, "inputport", name="value", peerid="v10")
        ET.SubElement(hook, "inputport", name="valueNumber").text = "1"
    return node


def static_checks(scripts):
    sys.path.insert(0, str(scripts / "tools"))
    from optimize_dinamica_windows import optimize_table_lookups
    source = (scripts / "10_dyn_Sc17_webmofuss_ctrees_g_v14.egoml").read_text(encoding="utf-8")
    once, _ = optimize_table_lookups(source)
    twice, report = optimize_table_lookups(once)
    assert once == twice
    assert all(x.get("already_optimized") for x in report["targets"])
    root = ET.fromstring(once)
    annual = []
    for parent in root.iter():
        for child in list(parent):
            ids = [p.get("id") for p in child.findall("outputport") if p.get("id") in ("v90010", "v90011")]
            if ids:
                annual.append(matrix_reference(child, ids[0]))
                parent.remove(child)
    mc = next(n for n in root.iter("containerfunctor") if any(p.get("value") == "repeat775" for p in n.findall("property")))
    mc.extend(annual)
    result, report = optimize_table_lookups(ET.tostring(root, encoding="unicode"))
    assert len(report["selected_rows"]) == 3
    assert sum(x.get("already_optimized", False) for x in report["targets"]) == 4
    assert optimize_table_lookups(result)[0] == result

    def rejects(changed):
        try:
            optimize_table_lookups(ET.tostring(changed, encoding="unicode"))
        except ValueError:
            return
        raise AssertionError("Altered graph was not rejected")

    broken = ET.fromstring(once)
    next(p for p in broken.iter("property") if p.get("value") == "repeat775").set("value", "unexpected repeat")
    rejects(broken)
    broken = ET.fromstring(once)
    node = next(n for n in broken.iter() if any(p.get("id") == "v201" for p in n.findall("outputport")))
    next(n for n in node.findall("functor") if n.get("name") == "NumberTable").find("inputport[@name='table']").set("peerid", "v244")
    rejects(broken)
    broken = ET.fromstring(once)
    for parent in broken.iter():
        for child in list(parent):
            if any(p.get("id") == "v201" for p in child.findall("outputport")):
                reference = matrix_reference(child, "v201")
                expression = reference.find("inputport[@name='expression']")
                expression.text = expression.text[:-1] + " + t1[[v1][i2 + 1]]]"
                parent.remove(child)
                parent.append(reference)
                break
    rejects(broken)
    print("PASS: idempotence, incremental v13-to-v14 transformation, and three altered-graph rejection checks")


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--scratch", type=Path)
    ap.add_argument("--static", action="store_true")
    ap.add_argument("--cells", type=int, default=20000)
    ap.add_argument("--table-nodata", action="store_true", help="Include blank parameter cells in the source tables")
    ap.add_argument("--engine", type=Path, default=Path("C:/Program Files/Dinamica EGO/DinamicaConsole.exe"))
    ap.add_argument("--rscript", type=Path, default=Path("C:/Program Files/R/R-4.6.0/bin/Rscript.exe"))
    args = ap.parse_args()
    scripts = Path(__file__).resolve().parents[1]
    sys.dont_write_bytecode = True
    static_checks(scripts)
    if args.static:
        return
    if args.scratch is None:
        ap.error("--scratch is required for native probes")
    scratch = args.scratch.resolve()
    if "mofuss_active" not in [p.lower() for p in scratch.parts] or scratch.name.lower() == "mofuss_active":
        raise ValueError("Use a named MoFuSS_Active scratch subfolder")
    if scratch.exists() and any(scratch.iterdir()):
        raise FileExistsError("Refusing nonempty scratch folder")
    scratch.mkdir(parents=True, exist_ok=True)
    sys.path.insert(0, str(scripts / "tools"))
    sys.dont_write_bytecode = True
    from optimize_dinamica_windows import optimize_table_lookups, TARGET_OUTPUTS
    source_path = scripts / "10_dyn_Sc17_webmofuss_ctrees_g_v14.egoml"
    source = source_path.read_bytes()
    production = ET.fromstring(source)
    producers = {p.get("id"): n for n in production.iter() for p in n.findall("outputport")}
    (scratch / "source_sha256.txt").write_text(hashlib.sha256(source).hexdigest())
    environment = os.environ.copy()
    environment.update(TEMP=str(scratch), TMP=str(scratch), TMPDIR=str(scratch))
    setup = r'''suppressPackageStartupMessages(library(terra))
args<-commandArgs(TRUE);root<-args[1];ncells<-as.integer(args[2]);terraOptions(tempdir=root)
wr<-function(name,x){nr<-ceiling(ncells/1000);nc<-min(ncells,1000);r<-rast(nrows=nr,ncols=nc,xmin=0,xmax=nc,ymin=0,ymax=nr,crs='EPSG:3395');values(r)<-rep(x,length.out=ncell(r));writeRaster(r,file.path(root,paste0(name,'.tif')),datatype='FLT4S',NAflag=-2147483648,overwrite=TRUE)}
wr('luc',c(NA,0,1,2,3,4,5,6,7,8,6,1,2,3,4,5,6,7,8))
wr('tof',c(NA,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1))
wr('stock',c(NA,0,0,1,0.1,1e8,NA,99.99999,13,14,15,0,NA,44,55,66,77,88,99))
wr('percent',c(NA,0,12.34,50,100,99.9999,1e-5))
wr('transition',c(NA,0,0,0,1,2,3,4,0,0,0,0,1,2,3,4,0,0,0))
'''
    (scratch / "setup.R").write_text(setup)
    p = subprocess.run([str(args.rscript), "--vanilla", str(scratch / "setup.R"), str(scratch), str(args.cells)],
                       env=environment, capture_output=True, text=True)
    (scratch / "setup.log").write_text(p.stdout + p.stderr)
    if p.returncode:
        raise RuntimeError(p.stdout + p.stderr)
    for stem, factor in (("initial", 0.625), ("capacity", 1.0), ("rate", 0.000123456789012345)):
        with (scratch / f"{stem}.csv").open("w", newline="") as handle:
            writer = csv.writer(handle)
            writer.writerow(["Key"] + [f"category{i}" for i in range(1, 9)])
            for row in (1, 2, 3):
                vals = [0, row * 0.10000000000000003, row * 99999.99999, row * 1e-20,
                        -row * 0.3333333333333333, row * 16777217.00001, row * 7.125,
                        row * 42.123456789012345]
                serialized = [format(v * factor, ".17g") for v in vals]
                if args.table_nodata and row == 2:
                    serialized[2] = ""
                writer.writerow([row] + serialized)

    def node(parent, kind, alias, container=False):
        n = ET.SubElement(parent, "containerfunctor" if container else "functor", name=kind)
        ET.SubElement(n, "property", key="dff.functor.alias", value=alias)
        return n
    def port(n, name, text=None, peer=None):
        p = ET.SubElement(n, "inputport", name=name)
        p.text = text
        if peer: p.set("peerid", peer)
        return p
    def load(root, peer, stem):
        n = node(root, "LoadMap", stem)
        port(n, "filename", '"' + (scratch / (stem + ".tif")).as_posix() + '"')
        for k,v in (("nullValue",".none"),("loadAsSparse",".no"),("suffixDigits","0"),("step",".none"),("workdir",".none")):
            port(n,k,v)
        ET.SubElement(n,"outputport",name="map",id=peer)
    def constant(root, peer, value):
        n=node(root,"Double",peer);port(n,"constant",str(value));ET.SubElement(n,"outputport",name="object",id=peer)
    def model(initial_mode):
        root=ET.Element("script");ET.SubElement(root,"property",key="dff.version",value="2.4.1.20140602")
        for peer,stem in (("v298","luc"),("v90003","luc"),("v204","tof"),("v206","stock"),("v202","percent"),("v90018","transition")):
            load(root,peer,stem)
        for peer,val in (("v270",initial_mode),("v268",73.123456789),("v6",48),("v5",7)):
            constant(root,peer,val)
        for peer,stem in (("v242","initial"),("v244","capacity"),("v243","rate")):
            n=node(root,"LoadTable",stem);port(n,"filename",'"'+(scratch/(stem+".csv")).as_posix()+'"')
            for k,v in (("suffixDigits","0"),("step",".none"),("workdir",".none")):port(n,k,v)
            ET.SubElement(n,"outputport",name="table",id=peer)
        mc=node(root,"Repeat","repeat775",True);port(mc,"iterations","3");ET.SubElement(mc,"internaloutputport",name="step",id="v8")
        n=node(mc,"Step","MC row");port(n,"step",peer="v8");ET.SubElement(n,"outputport",name="step",id="v10")
        for output in TARGET_OUTPUTS:
            mc.append(matrix_reference(producers[output], output))
            n=node(mc,"SaveMap","Save "+output);port(n,"map",peer=output);port(n,"filename",'"'+output+'.tif"')
            for k,v in (("suffixDigits","2"),("useCompression",".yes"),("workdir",".none")):port(n,k,v)
            port(n,"step",peer="v8")
        ET.indent(root,space="    ")
        return ET.tostring(root,encoding="unicode")

    timings=[]
    for initial_mode in (0,1):
        original=model(initial_mode)
        candidate,report=optimize_table_lookups(original)
        assert len(report["selected_rows"])==3 and len(report["targets"])==6
        (scratch/f"transform_mode{initial_mode}.json").write_text(json.dumps(report,indent=2))
        for variant,xml in (("original",original),("optimized",candidate)):
            folder=scratch/f"{variant}_mode{initial_mode}";folder.mkdir()
            temp=folder/"native_temp";temp.mkdir()
            env=environment.copy();env.update(TEMP=str(temp),TMP=str(temp))
            path=folder/"model.egoml";path.write_text(xml)
            before=time.perf_counter()
            p=subprocess.run([str(args.engine),"-processors","1","-predefined-seed","-log-level","4",str(path)],
                             cwd=folder,env=env,capture_output=True,text=True,timeout=300)
            elapsed=time.perf_counter()-before
            (folder/"engine.log").write_text(p.stdout+p.stderr)
            if args.table_nodata:
                assert p.returncode and "Failed to parse table" in p.stdout + p.stderr
            elif p.returncode:
                raise RuntimeError(f"{variant} mode{initial_mode}:\n"+p.stdout+p.stderr)
            debug=(folder/"debug.txt").read_text(errors="replace")
            warnings=[line for line in debug.splitlines() if "Unable to generate a native version" in line]
            timings.append(dict(variant=variant,initial_mode=initial_mode,cells=args.cells,
                                seconds=elapsed,returncode=p.returncode,native_compile_failure_lines=warnings))
            (scratch/"timings.json").write_text(json.dumps(timings,indent=2))
            print(variant,initial_mode,"seconds",round(elapsed,3),"compile failure lines",len(warnings),flush=True)

    if args.table_nodata:
        print("PASS: original and optimized models both reject blank table values at LoadTable")
        return

    check=r'''suppressPackageStartupMessages(library(terra));root<-commandArgs(TRUE)[1];rows<-list()
for(mode in 0:1)for(id in c('v201','v203','v207','v213','v90010','v90011'))for(mc in 1:3){
  name<-sprintf('%s%02d.tif',id,mc);a<-as.numeric(values(rast(file.path(root,paste0('original_mode',mode),name))));b<-as.numeric(values(rast(file.path(root,paste0('optimized_mode',mode),name))))
  stopifnot(identical(is.na(a),is.na(b)),identical(a[!is.na(a)],b[!is.na(b)]))
  rows[[length(rows)+1L]]<-data.frame(mode=mode,node=id,mc=mc,cells=length(a),null=sum(is.na(a)),exact=TRUE)
}
write.csv(do.call(rbind,rows),file.path(root,'exact_comparison.csv'),row.names=FALSE)
cat('PASS: all six production expressions, both initialization modes, three MC rows; all float32 values and NoData masks exactly equal.\n')
'''
    (scratch/"check.R").write_text(check)
    p=subprocess.run([str(args.rscript),"--vanilla",str(scratch/"check.R"),str(scratch)],
                     env=environment,capture_output=True,text=True)
    (scratch/"comparison.log").write_text(p.stdout+p.stderr)
    print(p.stdout+p.stderr)
    if p.returncode:raise RuntimeError("Exact native comparison failed")


if __name__ == "__main__":
    main()
