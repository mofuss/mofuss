"""Annual-cache regression; production models are never edited.

Without arguments, check the graph contract. With --scratch PATH, additionally
execute cold and warm production cache fragments in Windows Dinamica for two
MC draws, two IDW snapshots and fixed/changing LUC. R/terra makes tiny inputs
and compares exact float32 values and NoData masks to annual recomputation.
"""
from __future__ import annotations

import copy
import sys
import unittest
from pathlib import Path
import xml.etree.ElementTree as E

SCRIPTS = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(SCRIPTS / "tools"))
from fix_woodman_annual_sourcing_cache import correct_annual_sourcing_cache
from build_woodman_dinamica_v14 import build_model
from dinamica_v12_transform import _producers
from build_dinamica_sourcing_v12 import node, port, calculate


def canonical(n):
    return (n.tag, tuple(sorted(n.attrib.items())), (n.text or "").strip(),
            tuple(canonical(c) for c in n))


def baseline_graph():
    # Generate the intentionally uncorrected reference explicitly, so this test
    # remains useful after the canonical production v14 includes the fix.
    return build_model((SCRIPTS / "10_dyn_Sc17_webmofuss_ctrees_g_v13.egoml").read_text(encoding="utf-8"),
                       annual_cache=False)


class TestAnnualCacheCandidate(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.source = baseline_graph()
        cls.candidate, cls.report = correct_annual_sourcing_cache(cls.source)
        cls.old = _producers(E.fromstring(cls.source))
        cls.new = _producers(E.fromstring(cls.candidate))

    def test_only_cache_and_observer_nodes_change(self):
        permitted = set(self.report["changed_producer_ids"])
        for ident, n in self.old.items():
            if ident in permitted or n.get("name") in ("Repeat", "ForEach"):
                continue
            self.assertEqual(canonical(n), canonical(self.new[ident]), ident)

    def test_annual_mask_is_outside_cache_and_static_capture_is_unmasked(self):
        tree = E.fromstring(self.candidate)
        producers = _producers(tree)
        parents = {child: parent for parent in tree.iter() for child in parent}
        for cold, base, raw, domain, capture in (
            ("v6000", "v362", "v91000", "v90003", "v4005"),
            ("v6010", "v377", "v91010", "v90013", "v4025"),
        ):
            self.assertEqual(self.new[cold].findtext("inputport[@name='expression']"), "[i1]")
            self.assertEqual(self.new[raw].get("name"), "MapJunction")
            peers = {n.get("peerid") for n in self.new[base].iter() if n.get("peerid")}
            self.assertEqual(peers, {raw, domain})
            self.assertIn("_npa_base", self.new[capture].findtext("inputport[@name='format']"))
            self.assertEqual(parents[producers[cold]].get("name"), "IfThen")
            self.assertEqual(parents[producers[base]].get("name"), "ForEach")
            writer = next(n for n in tree.iter("functor") if n.get("name") == "SaveMap"
                          and n.find("inputport[@name='filename']").get("peerid") == capture)
            self.assertEqual(writer.find("inputport[@name='map']").get("peerid"), raw)
        self.assertIn("accumulator_domain<v2,2>", self.new["v91020"].findtext("inputport[@name='format']"))
        self.assertIs(parents[producers["v91020"]], producers["v39"])

    def test_reapplication_is_exact_and_incomplete_marker_is_rejected(self):
        again, report = correct_annual_sourcing_cache(self.candidate)
        self.assertEqual(again, self.candidate)
        self.assertTrue(report["already_applied"])
        # Remove the marker while retaining all reserved correction IDs.
        tree = E.fromstring(self.candidate)
        tree.remove(tree.find("property[@key='mofuss.sourcing.capture.contract']"))
        with self.assertRaisesRegex(ValueError, "marker missing"):
            correct_annual_sourcing_cache(E.tostring(tree, encoding="unicode"))
        tree = E.fromstring(self.candidate)
        _producers(tree)["v6000"].find("inputport[@name='expression']").text = "[i1 * 2]"
        with self.assertRaisesRegex(ValueError, "float32 identity"):
            correct_annual_sourcing_cache(E.tostring(tree, encoding="unicode"))

    def test_runtime_marker_replaces_the_obsolete_static_accumulator(self):
        tree = E.fromstring(self.candidate)
        writers = [n for n in tree.iter("functor") if n.get("name") == "SaveLookupTable"
                   and "annual_domain_after_static_npa_cache_v1.csv" in n.findtext("inputport[@name='filename']", "")]
        self.assertEqual(len(writers), 1)
        parents = {child: parent for parent in tree.iter() for child in parent}
        self.assertEqual(parents[writers[0]].get("name"), "IfThen")
        self.assertEqual(parents[writers[0]].find("inputport[@name='condition']").get("peerid"), "v4000")
        self.assertNotIn('"Sourcing/static/accumulator_domain.tif"', self.candidate)


def native_probe(scratch, engine, rscript):
    import json
    import os
    import subprocess

    scratch = scratch.resolve()
    if "mofuss_active" not in [p.lower() for p in scratch.parts] or scratch.name.lower() == "mofuss_active":
        raise ValueError("Use a named scratch folder below MoFuSS_Active")
    if scratch.exists() and any(scratch.iterdir()):
        raise FileExistsError("Refusing to replace existing probe evidence")
    scratch.mkdir(parents=True, exist_ok=True)
    temp = scratch / "temporary"
    temp.mkdir()
    env = os.environ.copy()
    env.update(TEMP=str(temp), TMP=str(temp), TMPDIR=str(temp))
    source = baseline_graph()
    candidate, report = correct_annual_sourcing_cache(source)
    (scratch / "candidate.egoml").write_text(candidate, encoding="utf-8")
    (scratch / "candidate_report.json").write_text(json.dumps(report, indent=2))
    trees = {"old": E.fromstring(source), "candidate": E.fromstring(candidate)}
    models = {key: _producers(tree) for key, tree in trees.items()}
    parents = {key: {child: parent for parent in tree.iter() for child in parent}
               for key, tree in trees.items()}

    setup = r'''suppressPackageStartupMessages(library(terra)); root<-commandArgs(TRUE)[1]
wr<-function(name,x){r<-rast(nrows=2,ncols=4,xmin=0,xmax=4000,ymin=0,ymax=2000,crs='EPSG:3395');values(r)<-x;writeRaster(r,file.path(root,paste0(name,'.tif')),datatype='FLT4S',NAflag=-9999,overwrite=TRUE)}
wr('initial',c(50,50,50,NA,50,0,50,50));wr('npa',c(NA,1,NA,1,NA,1,NA,NA));wr('selection',c(1,1,1,1,1,1,0,1));wr('biomass',c(100,100,100,100,100,100,100,0))
raw<-c(1.333333,2.222222,3.141592,4,5,0,NA,7.5);wr('raw01',raw);wr('raw11',raw*1.271234)
for(case in c('fixed','dynamic'))for(j in 1:5){luc<-c(11,22,11,11,NA,11,11,11);tof<-c(0,1,0,0,NA,0,0,0)
 if(case=='dynamic'){
  if(j%in%c(2,4,5)){luc[2]<-11;tof[2]<-0}
  if(j%in%c(2,4)){luc[3]<-NA;tof[3]<-NA}
  if(j%in%c(2,3,5)){luc[5]<-11;tof[5]<-0}
 };wr(paste0(case,'_luc',j),luc);wr(paste0(case,'_tof',j),tof)
}
'''
    (scratch / "setup.R").write_text(setup)
    p = subprocess.run([str(rscript), str(scratch / "setup.R"), str(scratch)],
                       cwd=scratch, env=env, capture_output=True, text=True)
    if p.returncode:
        raise RuntimeError(p.stdout + p.stderr)

    def load(root, path, out):
        n = node(root, "LoadMap", "Fixture " + out)
        port(n, "filename", '"' + path.as_posix() + '"')
        for key, value in (("nullValue", ".none"), ("loadAsSparse", ".no"),
                           ("suffixDigits", "0"), ("step", ".none"), ("workdir", ".none")):
            port(n, key, value)
        E.SubElement(n, "outputport", name="map", id=out)

    def write(root, peer, path):
        n = node(root, "SaveMap", "Probe " + peer)
        port(n, "map", peer=peer)
        port(n, "filename", '"' + path.as_posix() + '"')
        for key, value in (("suffixDigits", "0"), ("step", ".none"),
                           ("useCompression", ".yes"), ("workdir", ".none")):
            port(n, key, value)

    def clone(root, element, remap=None, capture_root=None):
        n = copy.deepcopy(element)
        for x in n.iter():
            for key in ("id", "peerid"):
                if x.get(key) and remap and x.get(key) in remap:
                    x.set(key, remap[x.get(key)])
            if capture_root and x.text and "Sourcing/" in x.text:
                x.text = x.text.replace("Sourcing/", capture_root.as_posix() + "/Sourcing/")
        root.append(n)
        return n

    years = (1, 2, 3, 11, 12)
    for case in ("fixed", "dynamic"):
        for variant in ("old", "candidate"):
            (scratch / case / variant / "Sourcing/static").mkdir(parents=True)
        for mc in (1, 2):
            for j, step in enumerate(years, 1):
                snapshot = 1 if step < 11 else 11
                run = scratch / case / f"mc{mc}_step{step:02d}"
                run.mkdir()
                root = E.Element("script")
                E.SubElement(root, "property", key="dff.version", value="2.4.1.20140602")
                for stem, ident in (("initial", "v200"), ("npa", "v299"), ("selection", "selection"),
                                    ("biomass", "v90008"), (f"{case}_luc{j}", "v90020"),
                                    (f"{case}_tof{j}", "v90021")):
                    load(root, scratch / (stem + ".tif"), ident)
                for ident, value in {"v38": mc, "v39": step, "v354": snapshot, "v285": 37,
                                     "v357": 1, "v372": 1, "v279": 1, "v278": 1,
                                     "v358": 100, "v373": 100, "v6": 48, "v5": 48, "v4": 0}.items():
                    calculate(root, "Value", "Fixture " + ident, str(value), ident)
                for ident in ("v359", "v374"):
                    n = node(root, "CreateString", "Raw component filename", True)
                    port(n, "format", '"' + (scratch / f"raw{snapshot:02d}.tif").as_posix() + '"')
                    E.SubElement(n, "outputport", name="result", id=ident)
                for ident in ("v4000", "v90003", "v90005", "v90013", "v90016"):
                    clone(root, models["old"][ident])
                for ident, domain in (("v181", "v90003"), ("v184", "v90013")):
                    calculate(root, "Map", "Annual Patcher fixture", "if isNull(i1) then null else i2",
                              ident, maps=(domain, "selection"))

                for channel, cold, warm, base, unmasked, capture, follow, eligibility in (
                    ("W", "v6000", "v6001", "v362", "v91000", "v4005",
                     ("v363", "v364", "v365", "v366"), "v4001"),
                    ("V", "v6010", "v6011", "v377", "v91010", "v4025",
                     ("v378", "v379", "v380", "v381"), "v4021"),
                ):
                    for variant in ("old", "candidate"):
                        prod = models[variant]
                        cold_group, warm_group = parents[variant][prod[cold]], parents[variant][prod[warm]]
                        writer = next(n for n in trees[variant].iter("functor") if n.get("name") == "SaveMap"
                                      and n.find("inputport[@name='filename']").get("peerid") == capture)
                        capture_group = parents[variant][writer]
                        pieces = [cold_group, warm_group]
                        if variant == "candidate":
                            pieces.append(prod[unmasked])
                        pieces += [prod[base], *(prod[k] for k in follow), prod[eligibility], capture_group]
                        identifiers = {x.get("id") for piece in pieces for x in piece.iter() if x.get("id")}
                        remap = {ident: variant + "_" + ident for ident in identifiers}
                        for piece in pieces:
                            clone(root, piece, remap, scratch / case / variant)
                        for label, ident in (("base", base), ("eligible", follow[1]),
                                             ("normalized", follow[3]), ("mask", eligibility)):
                            write(root, remap[ident], run / f"{variant}_{channel}_{label}.tif")

                    # Independent graph: recompute the original NPA and domain stages every year.
                    npa = "v361" if channel == "W" else "v376"
                    raw = "v360" if channel == "W" else "v375"
                    parts = [models["old"][raw], models["old"][npa], models["old"][cold],
                             *(models["old"][k] for k in follow)]
                    remap = {k: "reference_" + k for k in (raw, npa, cold, *follow)}
                    remap[base] = remap[cold]
                    for part in parts:
                        clone(root, part, remap)
                    for label, ident in (("base", cold), ("eligible", follow[1]), ("normalized", follow[3])):
                        write(root, remap[ident], run / f"reference_{channel}_{label}.tif")
                write(root, "v90016", run / "annual_accumulator.tif")
                E.indent(root, space="    ")
                model_path = run / "probe.egoml"
                E.ElementTree(root).write(model_path, encoding="utf-8", xml_declaration=True)
                p = subprocess.run([str(engine), "-processors", "1", "-predefined-seed", "-log-level", "3", str(model_path)],
                                   cwd=run, env=env, capture_output=True, text=True, timeout=90)
                (run / "engine.log").write_text(p.stdout + p.stderr)
                if p.returncode:
                    raise RuntimeError(p.stdout + p.stderr)
                print(f"Native cache probe {case} MC{mc} step{step:02d} passed execution", flush=True)

    check = r'''suppressPackageStartupMessages(library(terra));args<-commandArgs(TRUE);root<-args[1];source(args[2])
v<-function(p)as.numeric(values(rast(p)));same<-function(x,y)identical(is.na(x),is.na(y))&&identical(x[!is.na(x)],y[!is.na(y)])
rows<-list();z<-0
for(case in c('fixed','dynamic'))for(mc in 1:2)for(step in c(1,2,3,11,12))for(ch in c('W','V')){
 run<-file.path(root,case,sprintf('mc%d_step%02d',mc,step));snapshot<-if(step<11)1 else 11
 for(metric in c('base','eligible','normalized')){
  old<-v(file.path(run,paste0('old_',ch,'_',metric,'.tif')));new<-v(file.path(run,paste0('candidate_',ch,'_',metric,'.tif')));ref<-v(file.path(run,paste0('reference_',ch,'_',metric,'.tif')))
  stopifnot(same(new,ref));if(case=='fixed')stopifnot(same(old,new))
  z<-z+1;rows[[z]]<-data.frame(case=case,mc=mc,step=step,channel=ch,metric=metric,candidate_matches_recompute=same(new,ref),old_matches_recompute=same(old,ref),different_cells=sum(xor(is.na(old),is.na(ref))|(!is.na(old)&!is.na(ref)&old!=ref),na.rm=TRUE))
 }
 base<-v(file.path(root,case,'candidate','Sourcing','static',sprintf('%s_npa_base001_%02d.tif',ch,snapshot)));mask<-v(file.path(run,paste0('candidate_',ch,'_mask.tif')));pressure<-v(file.path(run,paste0('candidate_',ch,'_eligible.tif')))
 stopifnot(same(.rs_eligible(base,mask),pressure))
 accumulator<-v(file.path(run,'annual_accumulator.tif'));stopifnot(is.na(accumulator[4]))
 if(case=='dynamic'&&step%in%c(2,11))stopifnot(is.na(accumulator[3]))
 if(case=='dynamic'&&step%in%c(3,12))stopifnot(accumulator[3]==0)
}
out<-do.call(rbind,rows);write.csv(out,file.path(root,'native_comparisons.csv'),row.names=FALSE)
stopifnot(any(!out$old_matches_recompute[out$case=='dynamic'&out$metric=='normalized']),all(out$candidate_matches_recompute))
cat('PASS: native cold/warm cache across two MCs/two snapshots; fixed-LUC exact parity; dynamic candidate matches annual recomputation; original cache differs; sourcing mask replay exact.\n')
'''
    (scratch / "check.R").write_text(check)
    p = subprocess.run([str(rscript), str(scratch / "check.R"), str(scratch),
                        str(SCRIPTS / "postprocessing_sourcing/2post_runtime_sourcing_v1.R")],
                       cwd=scratch, env=env, capture_output=True, text=True)
    (scratch / "check.log").write_text(p.stdout + p.stderr)
    if p.returncode:
        raise RuntimeError(p.stdout + p.stderr)
    print(p.stdout)


if __name__ == "__main__":
    if "--scratch" in sys.argv:
        import argparse
        parser = argparse.ArgumentParser(description=__doc__)
        parser.add_argument("--scratch", type=Path, required=True)
        parser.add_argument("--engine", type=Path, default=Path("C:/Program Files/Dinamica EGO/DinamicaConsole.exe"))
        parser.add_argument("--rscript", type=Path, default=Path("C:/Program Files/R/R-4.6.0/bin/Rscript.exe"))
        args = parser.parse_args()
        native_probe(args.scratch, args.engine, args.rscript)
    else:
        unittest.main()
