"""Exercise the complete production graph through native scheduling and R startup.

The earlier tiny annual fixtures flatten selected input nodes and therefore cannot
detect dependency cycles between their original Group containers. This test keeps
every production node, connection and container. Only the normal BAU/ICS switches
are configured in fixture copies. A two-cell landscape and harmless R startup stubs
live in a new MoFuSS_Active directory. The stub records the real command arguments
and exits before generating MC tables; the missing MC table then stops Dinamica.
No simulation inputs or outputs from an existing run are opened or modified.

An optional --broken-model checks the reported failure against a preserved graph.
Native expression compilation is enabled by default, matching the launchers.
"""
from __future__ import annotations

import argparse
from concurrent.futures import ThreadPoolExecutor, as_completed
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time
import xml.etree.ElementTree as E

sys.dont_write_bytecode = True
SCRIPTS = Path(__file__).resolve().parents[1]


def digest(data):
    return hashlib.sha256(data).hexdigest()


def expected_mc_stop(log):
    # Independent initialization groups can request these in different orders.
    names = ("Prune_factor_V.csv", "Prune_factor_W.csv", "Harvest_pixels_V.csv",
             "Harvest_pixels_W.csv", "i_st_all.csv", "k_all.csv", "rmax_all.csv")
    return any(re.search(r'Temp/' + re.escape(name) + r'" does not exist[.]', log)
               for name in names)


def validate_isolated_paths(root):
    """Refuse graphs with literal paths that could escape the empty fixture."""
    for item in root.iter("inputport"):
        value = item.text or ""
        if re.search(r"[A-Za-z]:[/\\]|\\\\|(?:^|[/\\])\.\.(?:[/\\]|$)", value):
            raise ValueError("Unsafe absolute or parent path in model input: " + value)
        if item.get("name") == "workdir" and value != ".none":
            raise ValueError("Unexpected working-directory override")


def configure(content, rerun):
    root = E.fromstring(content)
    validate_isolated_paths(root)
    for item in root.iter("functor"):
        outputs = {p.get("id") for p in item.findall("outputport")}
        if "v256" in outputs:
            item.find("inputport[@name='constant']").text = ".yes" if rerun else ".no"
        if "v261" in outputs:
            item.find("inputport[@name='constant']").text = '"BaU"' if rerun else '"ICS"'
    return E.tostring(root, encoding="utf-8", xml_declaration=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--model", type=Path,
                        default=SCRIPTS / "10_dyn_Sc17_webmofuss_ctrees_g_v14.egoml")
    parser.add_argument("--broken-model", type=Path)
    parser.add_argument("--engine", type=Path,
                        default=Path("C:/Program Files/Dinamica EGO/DinamicaConsole.exe"))
    parser.add_argument("--rscript", type=Path,
                        default=Path("C:/Program Files/R/R-4.6.0/bin/Rscript.exe"))
    parser.add_argument("--disable-native-expressions", action="store_true")
    parser.add_argument("--jobs", type=int, choices=(1, 2, 3, 4), default=2)
    args = parser.parse_args()
    work = args.scratch.resolve()
    if "mofuss_active" not in {part.lower() for part in work.parts}:
        raise ValueError("Use a named directory below MoFuSS_Active")
    if work.exists() and any(work.iterdir()):
        raise FileExistsError("Refusing to overwrite test evidence")
    work.mkdir(parents=True, exist_ok=True)
    temp = work / "native_temp"
    temp.mkdir()
    env = os.environ.copy()
    env.update(TEMP=str(temp), TMP=str(temp), TMPDIR=str(temp))
    content = args.model.read_bytes()
    (work / "source_v14.egoml").write_bytes(content)
    cases = [(f"freeze{year}_{scenario}_{mode}", year, scenario == "bau", cap)
             for year in (2026, 2050) for scenario in ("bau", "ics")
             for mode, cap in (("capped", 0), ("uncapped", 1))]
    if args.broken_model:
        cases.insert(0, ("broken_graph", 2026, True, 0))
    rexec = args.rscript.parent / "x64" / "R.exe"
    if not rexec.is_file():
        raise FileNotFoundError(rexec)
    # No package loading, MC generation or production script sourcing occurs.
    stub = 'writeLines(commandArgs(TRUE), "startup_arguments.txt")\nquit(save="no", status=42, runLast=FALSE)\n'
    setup = '''suppressPackageStartupMessages(library(terra))
root<-commandArgs(TRUE)[1]
for(d in list.dirs(root,recursive=FALSE,full.names=TRUE)) {
 p<-file.path(d,"LULCC/TempRaster");if(!dir.exists(p))next
 r<-rast(nrows=1,ncols=2,xmin=0,xmax=2000,ymin=0,ymax=1000,crs="EPSG:32738")
 values(r)<-c(2,1);writeRaster(r,file.path(p,"LULCt3_c.tif"),datatype="INT2S",NAflag=-32768)
 values(r)<-c(0,0);writeRaster(r,file.path(p,"NPA_c.tif"),datatype="INT2S",NAflag=-32768)
 for(name in c("DEM_c.tif","roads_c_d.tif","rivers_c_d.tif","tc2000_c.tif","Gain_00.tif","Loss_00.tif","AnnLoss.tif","k_c.tif","m_c.tif","A_c.tif","agb3_c.tif","TOFvsFOR_mask3.tif")) {
  values(r)<-c(1,1);writeRaster(r,file.path(p,name),datatype="INT2S",NAflag=-32768)
 }
}
'''
    for label, year, rerun, cap in cases:
        case = work / label
        tables = case / "LULCC/TempTables"
        tables.mkdir(parents=True)
        (case / "LULCC/TempRaster").mkdir()
        (case / "Temp").mkdir()
        selected = args.broken_model.read_bytes() if label == "broken_graph" else content
        (case / "model.egoml").write_bytes(configure(selected, rerun))
        (tables / "Rpath.csv").write_text('"Key*","Rpath"\n1,"' + str(rexec) + '"\n')
        (tables / "OStype.csv").write_text('"Key*","OS"\n1,64\n')
        (tables / "TOFvsFOR_Categories3.csv").write_text('"Key","x"\n1,1\n2,0\n')
        weights = "\n\n".join(":" + name + "    0:1000000\n0,1    0"
                                for name in ("NPA/layer_0", "elevation/layer_0",
                                             "rivers/distance_to_1", "roads/distance_to_1",
                                             "slope/layer_0")) + "\n"
        for name in ("weights_loss.dcf", "weights_gain.dcf"):
            (tables / name).write_text(weights)
        (tables / "parameters_dinamica.csv").write_text(
            'Var,ParCHR\nstart_year,2000\nend_year,2050\nmonte_carlo_runs,3\n'
            f'uncapped_regrowth,{cap}\nnpa_ease,10\nwoodman_luc_freeze_year,{year}\n')
        (case / "rnorm_v8.R").write_text(stub)
        (case / "bypassMC_v8.R").write_text(stub)
    (work / "setup.R").write_text(setup)
    setup_result = subprocess.run([str(args.rscript), str(work / "setup.R"), str(work)],
                                 cwd=work, env=env, capture_output=True, text=True,
                                 timeout=60)
    (work / "setup.log").write_text(setup_result.stdout + setup_result.stderr)
    setup_result.check_returncode()
    def execute_case(case_spec):
        label, year, rerun, cap = case_spec
        case = work / label
        case_temp = case / "native_temp"
        case_temp.mkdir()
        case_env = env.copy()
        case_env.update(TEMP=str(case_temp), TMP=str(case_temp), TMPDIR=str(case_temp))
        cmd = [str(args.engine), "-processors", "1", "-predefined-seed", "-log-level", "3"]
        if args.disable_native_expressions:
            cmd.append("-disable-native-expressions")
        cmd.append(str(case / "model.egoml"))
        started = time.monotonic()
        result = subprocess.run(cmd, cwd=case, env=case_env, capture_output=True,
                                text=True, timeout=180)
        log = result.stdout + result.stderr
        (case / "engine.log").write_text(log)
        loop = "Loop detected in the functor list" in log
        sentinel = case / "startup_arguments.txt"
        if label == "broken_graph":
            passed = loop and not sentinel.exists() and result.returncode != 0
            arguments = []
        else:
            arguments = sentinel.read_text().splitlines() if sentinel.exists() else []
            passed = (not loop and result.returncode != 0 and
                      f"WoodmanFreezeYear={year}" in arguments and
                      "LUCmap_v=3" in arguments and
                      "MC=3" in arguments and
                      (f"CTrees={cap}" in arguments if rerun else "RerunMC=0" in arguments) and
                      expected_mc_stop(log) and
                      (case / ("rnorm_v8.Rout" if rerun else "bypassMC_v8.Rout")).is_file())
        created_rasters = [str(p.relative_to(case)) for p in case.rglob("*.tif")
                           if "TempRaster" not in p.parts]
        passed = passed and not created_rasters
        record = dict(case=label, passed=passed, seconds=time.monotonic()-started,
                      exit_code=result.returncode, loop_detected=loop,
                      startup_reached=sentinel.exists(), arguments=arguments,
                      annual_output_rasters=created_rasters,
                      model_sha256=digest((case / "model.egoml").read_bytes()))
        return record

    results = []
    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        futures = [pool.submit(execute_case, case) for case in cases]
        for future in as_completed(futures):
            record = future.result()
            results.append(record)
            report = dict(source=str(args.model.resolve()), source_sha256=digest(content),
                          native_compilation_enabled=not args.disable_native_expressions,
                          full_graph_preserved=True, cases=results,
                          all_passed=len(results)==len(cases) and all(x["passed"] for x in results))
            (work / "summary.json").write_text(json.dumps(report, indent=2) + "\n")
            print(json.dumps(record), flush=True)
    if not all(record["passed"] for record in results):
        raise RuntimeError("Full-graph startup regression failed; see summary.json")
    print("PASS: complete production graph schedules and passes freeze settings to both MC startup branches.")


if __name__ == "__main__":
    main()
