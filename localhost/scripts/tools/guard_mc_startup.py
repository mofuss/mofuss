"""Require a fresh successful R Monte Carlo startup before using any MC tables.

The installed Dinamica RunExternalProcess functor discards process exit codes.
Nested Groups establish a strict reset -> external process -> validation order.
Only the final successful action of rnorm/bypassMC may replace the reset value.
"""
from __future__ import annotations

import argparse
import copy
import sys
from pathlib import Path
import xml.etree.ElementTree as E

sys.dont_write_bytecode = True
from build_dinamica_sourcing_v12 import node, port, prop, serialize_children
from dinamica_v12_transform import _parse_spans, _producers

MARKER_KEY = "mofuss.mc.startup.contract"
CONTRACT = "fresh_r_success_before_mc_tables_v1"
STATUS_FILE = "mc_startup_guard.csv"
FAILURE_MESSAGE = ("MOFUSS_R_STARTUP_FAILED: MoFuSS R Monte Carlo startup did not complete successfully. "
                   "Simulation stopped before consuming MC tables. "
                   "See rnorm_v8.Rout or bypassMC_v8.Rout for the cause.")
NEW_IDS = tuple("v" + str(i) for i in range(95000, 95007))


def _alias(n):
    p = n.find("property[@key='dff.functor.alias']")
    return p.get("value") if p is not None else None


def validate_startup_guard(root):
    marker = root.find(f"property[@key='{MARKER_KEY}']")
    if marker is None or marker.get("value") != CONTRACT:
        raise ValueError("Missing Monte Carlo startup success contract")
    p = _producers(root)
    parents = {child: parent for parent in root.iter() for child in parent}
    reset = parents[p["v95000"]]
    execute = parents[p["v95001"]]
    check = parents[p["v294"]]
    if len({reset, execute, check}) != 3 or any(n.get("name") != "Group" for n in (reset, execute, check)):
        raise ValueError("Startup reset, process and validation must use separate ordered Groups")
    if len({parents[n] for n in (reset, execute, check)}) != 1:
        raise ValueError("Startup guard groups must share the MC startup container")
    runner = execute.find("functor[@name='RunExternalProcess']")
    if (runner is None or runner.find("inputport[@name='secondsToWait']").get("peerid") != "v95000" or
            runner.findtext("inputport[@name='waitProcessCompletion']") != ".yes"):
        raise ValueError("R startup must wait for the native status reset and process completion")
    saver = reset.find("functor[@name='SaveLookupTable']")
    if (saver is None or saver.findtext("inputport[@name='filename']") != f'"{STATUS_FILE}"' or
            '1 -1' not in saver.findtext("inputport[@name='table']", "")):
        raise ValueError("Native startup must invalidate any previous success marker")
    if p["v95002"].find("inputport[@name='filename']").get("peerid") != "v95001":
        raise ValueError("Startup status cannot be read before the R process group finishes")
    if p["v95001"].findtext("inputport[@name='constant']") != f'"{STATUS_FILE}"':
        raise ValueError("Unexpected startup marker filename")
    if (p["v95003"].findtext("inputport[@name='expression']") != "[v1 = 1]" or
            p["v95003"].find("functor/inputport[@name='value']").get("peerid") != "v95004"):
        raise ValueError("Only a newly written success value may permit simulation")
    if check.find("containerfunctor[@name='IfNotThen']/containerfunctor[@name='Print']/functor[@name='Exit']") is None:
        raise ValueError("Unsuccessful R startup must abort the native model")
    if p["v294"].find("inputport[@name='constant']").get("peerid") != "v95005":
        raise ValueError("The downstream MC chain must pass through successful startup validation")
    from add_woodman_freeze_year import validate_root_container_dependencies
    validate_root_container_dependencies(root)
    ids = [x.get("id") for x in root.iter() if x.get("id")]
    if len(ids) != len(set(ids)):
        raise ValueError("Duplicate graph output ID")


def guard_mc_startup(text):
    root, data, spans = _parse_spans(text)
    if root.find(f"property[@key='{MARKER_KEY}']") is not None:
        validate_startup_guard(root)
        return text, {"already_applied": True, "contract": CONTRACT}
    p = _producers(root)
    if set(NEW_IDS) & set(p):
        raise ValueError("Startup guard IDs are already in use")
    group = next(n for n in root if _alias(n) == "group2500")
    old_bool = p["v294"]
    runner = next(n for n in group if _alias(n) == "runExternalProcess2510")
    if old_bool not in list(group) or runner.find("inputport[@name='parameters']").get("peerid") != "v295":
        raise ValueError("Unexpected Monte Carlo startup topology")
    fragment = E.Element("fragment")
    reset = node(fragment, "Group", "Invalidate previous R startup success", True)
    saver = node(reset, "SaveLookupTable", "Reset R startup guard before every attempt")
    port(saver, "table", '[ "Key" "Value" 1 -1 ]')
    port(saver, "filename", f'"{STATUS_FILE}"')
    port(saver, "suffixDigits", "0")
    port(saver, "step", ".none")
    port(saver, "workdir", ".none")
    delay = node(reset, "Int", "R startup guard reset completed")
    port(delay, "constant", "0")
    E.SubElement(delay, "outputport", name="object", id="v95000")

    execute = node(fragment, "Group", "Run R after invalidating previous startup success", True)
    runner = copy.deepcopy(runner)
    wait = runner.find("inputport[@name='secondsToWait']")
    wait.text = None
    wait.set("peerid", "v95000")
    execute.append(runner)
    path = node(execute, "String", "R startup guard filename after external process")
    port(path, "constant", f'"{STATUS_FILE}"')
    E.SubElement(path, "outputport", name="object", id="v95001")
    carried = copy.deepcopy(old_bool)
    carried.find("outputport").set("id", "v95005")
    execute.append(carried)

    check = node(fragment, "Group", "Require successful R startup before MC dynamics", True)
    loader = node(check, "LoadLookupTable", "Read freshly completed R startup status")
    port(loader, "filename", peer="v95001")
    port(loader, "suffixDigits", "0")
    port(loader, "step", ".none")
    port(loader, "workdir", ".none")
    E.SubElement(loader, "outputport", name="table", id="v95002")
    read = node(check, "CalculateValue", "Read R startup success value", True)
    port(read, "expression", "[t1[1]]")
    port(read, "defaultValue", "-1")
    E.SubElement(read, "outputport", name="result", id="v95004")
    tab = node(read, "NumberTable", "R startup status")
    port(tab, "table", peer="v95002")
    port(tab, "tableNumber", "1")
    success = node(check, "CalculateValue", "R startup completed successfully", True)
    port(success, "expression", "[v1 = 1]")
    port(success, "defaultValue", "0")
    E.SubElement(success, "outputport", name="result", id="v95003")
    val = node(success, "NumberValue", "Fresh R success status")
    port(val, "value", peer="v95004")
    port(val, "valueNumber", "1")
    fail = node(check, "IfNotThen", "Abort on incomplete or failed R startup", True)
    port(fail, "condition", peer="v95003")
    message = node(fail, "Print", "Explain Monte Carlo startup failure", True)
    port(message, "initialMessage", '"' + FAILURE_MESSAGE + '"')
    port(message, "logLevel", ".error")
    node(message, "Exit", "Stop before stale MC tables can be consumed")
    gate = copy.deepcopy(old_bool)
    gate.find("inputport[@name='constant']").set("peerid", "v95005")
    check.append(gate)

    old_runner = next(n for n in group if _alias(n) == "runExternalProcess2510")
    bool_start = spans[id(old_bool)].start
    if data[bool_start - 8:bool_start] == b"        ":
        bool_start -= 8
    edits = [(bool_start, spans[id(old_bool)].end, b""),
             (spans[id(old_runner)].start, spans[id(old_runner)].end,
              serialize_children(fragment, 8).lstrip().encode())]
    first_container = next(n for n in root if n.tag != "property")
    marker = E.Element("property", key=MARKER_KEY, value=CONTRACT)
    where = spans[id(first_container)].start
    edits.append((where, where, (E.tostring(marker, encoding="unicode") + "\n    ").encode()))
    for start, end, replacement in sorted(edits, reverse=True):
        data = data[:start] + replacement + data[end:]
    output = data.decode()
    validate_startup_guard(E.fromstring(output))
    return output, {"already_applied": False, "contract": CONTRACT, "status_file": STATUS_FILE}


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("model", type=Path)
    args = parser.parse_args()
    updated, report = guard_mc_startup(args.model.read_text(encoding="utf-8"))
    args.model.write_text(updated, encoding="utf-8")
    print(report)
