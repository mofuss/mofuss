"""Use an explicit annual Step carrier for EGO 8 nested filename scopes.

Only the two Sourcing scalar filenames change wiring. v59 is the existing Step
fed by v39, so the same year index reaches each filename on both Windows engines.
Scientific arithmetic, variables and iteration counts remain untouched.
"""
import xml.etree.ElementTree as ET


def explicit_annual_filename_steps(source: str) -> str:
    root = ET.fromstring(source)
    ids = {p.get("id"): n for n in root.iter() for p in n.findall("outputport")}
    step = ids["v59"]
    if step.get("name") != "Step" or step.find("inputport[@name='step']").get("peerid") != "v39":
        raise ValueError("Existing annual Step carrier differs from reviewed graph")
    for key in ("v4004", "v4024"):
        n = ids[key]
        if n.get("name") != "CreateString":
            raise ValueError("Expected sourcing scalar filename node")
        child = next(c for c in n.findall("functor") if c.get("name") == "NumberValue"
                     and c.findtext("inputport[@name='valueNumber']") == "3")
        port = child.find("inputport[@name='value']")
        if port.get("peerid") not in ("v39", "v59"):
            raise ValueError("Unexpected annual filename input")
        port.set("peerid", "v59")
    ET.indent(root, space="    ")
    return ET.tostring(root, encoding="unicode")
