"""Expose the existing post-simulation rendering switch without changing it."""
import json
import xml.etree.ElementTree as ET

TAG = "Bool_constant_25"
LABEL = "Generate maps, summary tables and report after simulation?"


def expose_report_option(source: str) -> str:
    root = ET.fromstring(source)
    node = next(n for n in root.iter("functor")
                if any(p.get("id") == "v257" for p in n.findall("outputport")))
    if node.get("name") != "Bool" or node.findtext("inputport[@name='constant']") != ".yes":
        raise ValueError("Expected the original enabled report switch")
    bound = node.find("property[@key='wizard.constant.input']")
    if bound is not None and bound.get("value") != TAG:
        raise ValueError("Report switch already bound to a different wizard editor")
    if bound is None:
        node.insert(1, ET.Element("property", key="wizard.constant.input", value=TAG))
    node.find("property[@key='dff.functor.alias']").set("value", LABEL)
    node.find("property[@key='dff.functor.comment']").set("value",
        "Default Yes preserves the standard post-simulation products. No skips maps, animations, fNRB summary tables/vectors and report rendering in maps_animations_v8.R; annual Dinamica rasters, engine Temp tables, MC tables and sourcing captures are still saved.")
    prop = root.find("property[@key='metadata.wizard']")
    wizard = json.loads(prop.get("value"))
    pages = wizard["inputPages"]
    editors = [e for page in pages for e in page.get("editors", []) if e.get("tag") == TAG]
    if editors and (len(editors) != 1 or editors[0].get("name") != LABEL):
        raise ValueError("Reserved report editor tag is already used")
    if not editors:
        page = next(p for p in pages if p["title"] == "Outputs and sensitivity")
        page["editors"].insert(0, {"name": LABEL, "tag": TAG,
            "description": "Yes keeps all standard products. No skips maps, animations, fNRB summary tables/vectors and report rendering. Annual Dinamica rasters, engine Temp tables and sourcing captures remain available. Keep Yes when these summary products are needed."})
    page = next(p for p in pages if p["title"] == "Outputs and sensitivity")
    page["content"] = page["content"].replace(
        "Period-level fNRB tables and vectors, standard maps, animation, and the summary report are fixed on for every supported run.",
        "The option below controls maps, animations, period-level fNRB summary tables/vectors and report rendering. Keep Yes for the complete standard output bundle. Turning it off retains annual Dinamica rasters, engine Temp tables and sourcing captures.")
    page["content"] = page["content"].replace(
        "<p>Maps and animations can be disabled above to defer the final rendering step. Scientific simulation outputs are retained.</p>", "")
    prop.set("value", json.dumps(wizard, ensure_ascii=False, indent=2))
    ET.indent(root, space="    ")
    return ET.tostring(root, encoding="unicode")


def assert_presentation_only(before: str, after: str) -> None:
    """Fail if any functor, port, constant, or numerical text was changed."""
    def scientific_graph(source):
        r = ET.fromstring(source)
        return [(n.tag, sorted(n.attrib.items()), (n.text or "").strip())
                for n in r.iter() if n.tag != "property"]
    if scientific_graph(before) != scientific_graph(after):
        raise AssertionError("Report editor changed the executable graph")
