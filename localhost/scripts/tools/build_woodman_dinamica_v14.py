"""Build the Woodman annual-LULC Dinamica model from the validated v13 model."""

from __future__ import annotations

import re
import importlib.util
import sys
import textwrap
import xml.etree.ElementTree as ET
from pathlib import Path


HERE = Path(__file__).resolve().parents[1]
SOURCE = HERE / "10_dyn_Sc17_webmofuss_ctrees_g_v13.egoml"
TARGET = HERE / "10_dyn_Sc17_webmofuss_ctrees_g_v14.egoml"


def replace_once(source: str, before: str, after: str) -> str:
    # ElementTree preserves quotes literally in element text. The legacy EGO
    # writer escapes them; accept either equivalent serialization for filenames.
    if source.count(before) == 0 and '&quot;' in before:
        literal = before.replace('&quot;', '"')
        if source.count(literal) == 1:
            before = literal
    if source.count(before) != 1:
        raise ValueError(f"Expected one match, found {source.count(before)}: {before[:90]}")
    return source.replace(before, after, 1)


def _fixed_input_branch(block: str) -> str:
    """Select static maps for LUC=1 without opening any annual input file.

    The original initialization already loads the static LUC and TOF maps.
    Conditional branches provide mutually exclusive values to MapJunctions;
    an untaken annual branch cannot request year-labelled files. The fixed
    transition map is zero over the static LUC domain, then the existing
    immutable initial-stock support guard is applied downstream.
    """
    fragment = ET.fromstring("<fragment>" + block + "</fragment>")
    by_id = {p.get("id"): n for n in fragment for p in n.findall("outputport")}
    routing = ET.Element("fragment")
    condition = ET.SubElement(routing, "containerfunctor", name="CalculateValue")
    ET.SubElement(condition, "property", key="dff.functor.alias", value="Use fixed LUC inputs")
    ET.SubElement(condition, "inputport", name="expression").text = "[v1 = 1]"
    ET.SubElement(condition, "inputport", name="defaultValue").text = ".none"
    ET.SubElement(condition, "outputport", name="result", id="v90030")
    selector = ET.SubElement(condition, "functor", name="NumberValue")
    ET.SubElement(selector, "inputport", name="value", peerid="v302")
    ET.SubElement(selector, "inputport", name="valueNumber").text = "1"
    fixed = ET.SubElement(routing, "containerfunctor", name="IfThen")
    ET.SubElement(fixed, "property", key="dff.functor.alias", value="Fixed MODIS inputs without annual files")
    ET.SubElement(fixed, "inputport", name="condition", peerid="v90030")
    dynamic = ET.SubElement(routing, "containerfunctor", name="IfNotThen")
    ET.SubElement(dynamic, "property", key="dff.functor.alias", value="Annual Woodman input files")
    ET.SubElement(dynamic, "inputport", name="condition", peerid="v90030")
    for name, filename_id, output_id, annual_id, fixed_id, static_id, expression in (
        ("LUC", "v90002", "v90020", "v90031", "v90032", "v298", "[i1]"),
        ("TOF", "v90004", "v90021", "v90033", "v90034", "v204", "[i1]"),
        ("transition", "v90006", "v90007", "v90035", "v90036", "v298", "[if isNull(i1) then null else 0]"),
    ):
        for ident in (filename_id, output_id):
            n = by_id[ident]
            fragment.remove(n)
            dynamic.append(n)
        by_id[output_id].find("outputport").set("id", annual_id)
        value = ET.SubElement(fixed, "containerfunctor", name="CalculateMap")
        ET.SubElement(value, "property", key="dff.functor.alias", value="Static " + name + " input")
        for key, text in (("expression", expression), ("cellType", ".int32"),
                          ("nullValue", ".default"), ("resultIsSparse", ".no"), ("resultFormat", ".none")):
            ET.SubElement(value, "inputport", name=key).text = text
        ET.SubElement(value, "outputport", name="result", id=fixed_id)
        number = ET.SubElement(value, "functor", name="NumberMap")
        ET.SubElement(number, "inputport", name="map", peerid=static_id)
        ET.SubElement(number, "inputport", name="mapNumber").text = "1"
        junction = ET.SubElement(routing, "functor", name="MapJunction")
        ET.SubElement(junction, "property", key="dff.functor.alias", value="Selected fixed or annual " + name)
        ET.SubElement(junction, "inputport", name="possibleMap1", peerid=fixed_id)
        ET.SubElement(junction, "inputport", name="possibleMap2", peerid=annual_id)
        ET.SubElement(junction, "outputport", name="map", id=output_id)
    for index, n in enumerate(routing, 1):
        fragment.insert(index, n)
    return "\n".join(ET.tostring(n, encoding="unicode") for n in fragment)


def annual_loader(fixed_inputs: bool = True) -> str:
    block = '''
<containerfunctor name="CalculateValue">
    <property key="dff.functor.alias" value="Woodman annual year" />
    <inputport name="expression">[v1 + v2 - 1]</inputport>
    <inputport name="defaultValue">.none</inputport>
    <outputport name="result" id="v90001" />
    <functor name="NumberValue">
        <property key="dff.functor.alias" value="Start year" />
        <inputport name="value" peerid="v247" />
        <inputport name="valueNumber">1</inputport>
    </functor>
    <functor name="NumberValue">
        <property key="dff.functor.alias" value="Annual step" />
        <inputport name="value" peerid="v39" />
        <inputport name="valueNumber">2</inputport>
    </functor>
</containerfunctor>
<containerfunctor name="CreateString">
    <property key="dff.functor.alias" value="Woodman annual LULC filename" />
    <inputport name="format">&quot;LULCC/TempRaster/LULCt&lt;v1&gt;_c_&lt;v2&gt;.tif&quot;</inputport>
    <outputport name="result" id="v90002" />
    <functor name="NumberValue">
        <property key="dff.functor.alias" value="Selected LUC channel" />
        <inputport name="value" peerid="v302" />
        <inputport name="valueNumber">1</inputport>
    </functor>
    <functor name="NumberValue">
        <property key="dff.functor.alias" value="Annual year" />
        <inputport name="value" peerid="v90001" />
        <inputport name="valueNumber">2</inputport>
    </functor>
</containerfunctor>
<functor name="LoadMap">
    <property key="dff.functor.alias" value="Woodman annual LULC key map" />
    <inputport name="filename" peerid="v90002" />
    <inputport name="nullValue">.none</inputport>
    <inputport name="loadAsSparse">.no</inputport>
    <inputport name="suffixDigits">0</inputport>
    <inputport name="step">.none</inputport>
    <inputport name="workdir">.none</inputport>
    <outputport name="map" id="v90020" />
</functor>
<containerfunctor name="CalculateMap">
    <property key="dff.functor.alias" value="Woodman annual LUC within initial model stock support" />
    <property key="dff.functor.comment" value="Keep cells with valid initial model stock, including numeric zero and the v13 TOF allowance; initial NoData never becomes a fuelwood source after a land-cover transition" />
    <inputport name="expression">[if isNull(i2) then null else i1]</inputport>
    <inputport name="cellType">.int32</inputport>
    <inputport name="nullValue">.default</inputport>
    <inputport name="resultIsSparse">.no</inputport>
    <inputport name="resultFormat">.none</inputport>
    <outputport name="result" id="v90003" />
    <functor name="NumberMap">
        <inputport name="map" peerid="v90020" />
        <inputport name="mapNumber">1</inputport>
    </functor>
    <functor name="NumberMap">
        <inputport name="map" peerid="v200" />
        <inputport name="mapNumber">2</inputport>
    </functor>
</containerfunctor>
<containerfunctor name="CreateString">
    <property key="dff.functor.alias" value="Woodman annual TOF filename" />
    <inputport name="format">&quot;LULCC/TempRaster/TOFvsFOR_mask&lt;v1&gt;_&lt;v2&gt;.tif&quot;</inputport>
    <outputport name="result" id="v90004" />
    <functor name="NumberValue">
        <property key="dff.functor.alias" value="Selected LUC channel" />
        <inputport name="value" peerid="v302" />
        <inputport name="valueNumber">1</inputport>
    </functor>
    <functor name="NumberValue">
        <property key="dff.functor.alias" value="Annual year" />
        <inputport name="value" peerid="v90001" />
        <inputport name="valueNumber">2</inputport>
    </functor>
</containerfunctor>
<functor name="LoadMap">
    <property key="dff.functor.alias" value="Woodman annual TOF map" />
    <inputport name="filename" peerid="v90004" />
    <inputport name="nullValue">.none</inputport>
    <inputport name="loadAsSparse">.no</inputport>
    <inputport name="suffixDigits">0</inputport>
    <inputport name="step">.none</inputport>
    <inputport name="workdir">.none</inputport>
    <outputport name="map" id="v90021" />
</functor>
<containerfunctor name="CalculateMap">
    <property key="dff.functor.alias" value="Woodman annual TOF within initial model stock support" />
    <property key="dff.functor.comment" value="A later TOF category cannot create an allowance where the original v13 model initialization was NoData" />
    <inputport name="expression">[if isNull(i2) then null else i1]</inputport>
    <inputport name="cellType">.int32</inputport>
    <inputport name="nullValue">.default</inputport>
    <inputport name="resultIsSparse">.no</inputport>
    <inputport name="resultFormat">.none</inputport>
    <outputport name="result" id="v90005" />
    <functor name="NumberMap">
        <inputport name="map" peerid="v90021" />
        <inputport name="mapNumber">1</inputport>
    </functor>
    <functor name="NumberMap">
        <inputport name="map" peerid="v200" />
        <inputport name="mapNumber">2</inputport>
    </functor>
</containerfunctor>
<containerfunctor name="CreateString">
    <property key="dff.functor.alias" value="Selected LUC transition filename" />
    <inputport name="format">&quot;LULCC/TempRaster/LULCt&lt;v1&gt;_transition_&lt;v2&gt;.tif&quot;</inputport>
    <outputport name="result" id="v90006" />
    <functor name="NumberValue">
        <property key="dff.functor.alias" value="Selected LUC channel" />
        <inputport name="value" peerid="v302" />
        <inputport name="valueNumber">1</inputport>
    </functor>
    <functor name="NumberValue">
        <property key="dff.functor.alias" value="Annual year" />
        <inputport name="value" peerid="v90001" />
        <inputport name="valueNumber">2</inputport>
    </functor>
</containerfunctor>
<functor name="LoadMap">
    <property key="dff.functor.alias" value="Selected LUC annual transitions" />
    <property key="dff.functor.comment" value="MODIS transitions stay zero; Woodman codes: 1 forest cleared; 2 new forest; 3 TOF gained; 4 TOF lost" />
    <inputport name="filename" peerid="v90006" />
    <inputport name="nullValue">.none</inputport>
    <inputport name="loadAsSparse">.no</inputport>
    <inputport name="suffixDigits">0</inputport>
    <inputport name="step">.none</inputport>
    <inputport name="workdir">.none</inputport>
    <outputport name="map" id="v90007" />
</functor>
<containerfunctor name="CalculateMap">
    <property key="dff.functor.alias" value="Woodman transition in current annual domain" />
    <property key="dff.functor.comment" value="New cells absent from the previous map start with transition code zero" />
    <inputport name="expression">[if isNull(i1) then null else if isNull(i2) then 0 else i2]</inputport>
    <inputport name="cellType">.int32</inputport>
    <inputport name="nullValue">.default</inputport>
    <inputport name="resultIsSparse">.no</inputport>
    <inputport name="resultFormat">.none</inputport>
    <outputport name="result" id="v90018" />
    <functor name="NumberMap">
        <inputport name="map" peerid="v90003" />
        <inputport name="mapNumber">1</inputport>
    </functor>
    <functor name="NumberMap">
        <inputport name="map" peerid="v90007" />
        <inputport name="mapNumber">2</inputport>
    </functor>
</containerfunctor>
<containerfunctor name="CalculateMap">
    <property key="dff.functor.alias" value="Woodman annual category K" />
    <property key="dff.functor.comment" value="Current land-cover category K, including the table-defined TOF allowance" />
    <inputport name="expression">[if isNull(i1) then null else t1[[v1][i1 + 1]]]</inputport>
    <inputport name="cellType">.float32</inputport>
    <inputport name="nullValue">.default</inputport>
    <inputport name="resultIsSparse">.no</inputport>
    <inputport name="resultFormat">.none</inputport>
    <outputport name="result" id="v90010" />
    <functor name="NumberMap">
        <inputport name="map" peerid="v90003" />
        <inputport name="mapNumber">1</inputport>
    </functor>
    <functor name="NumberTable">
        <inputport name="table" peerid="v244" />
        <inputport name="tableNumber">1</inputport>
    </functor>
    <functor name="NumberValue">
        <inputport name="value" peerid="v10" />
        <inputport name="valueNumber">1</inputport>
    </functor>
</containerfunctor>
<containerfunctor name="CalculateMap">
    <property key="dff.functor.alias" value="Woodman annual category rmax" />
    <property key="dff.functor.comment" value="Current category rate; forest clearing and TOF loss provide no conversion-year fuelwood" />
    <inputport name="expression">[if isNull(i1) or isNull(i2) then null else if i2 = 1 or i2 = 2 or i2 = 4 then 0 else t1[[v1][i1 + 1]] / v2 * v3]</inputport>
    <inputport name="cellType">.float32</inputport>
    <inputport name="nullValue">.default</inputport>
    <inputport name="resultIsSparse">.no</inputport>
    <inputport name="resultFormat">.none</inputport>
    <outputport name="result" id="v90011" />
    <functor name="NumberMap">
        <inputport name="map" peerid="v90003" />
        <inputport name="mapNumber">1</inputport>
    </functor>
    <functor name="NumberMap">
        <inputport name="map" peerid="v90018" />
        <inputport name="mapNumber">2</inputport>
    </functor>
    <functor name="NumberTable">
        <inputport name="table" peerid="v243" />
        <inputport name="tableNumber">1</inputport>
    </functor>
    <functor name="NumberValue">
        <inputport name="value" peerid="v10" />
        <inputport name="valueNumber">1</inputport>
    </functor>
    <functor name="NumberValue">
        <inputport name="value" peerid="v6" />
        <inputport name="valueNumber">2</inputport>
    </functor>
    <functor name="NumberValue">
        <inputport name="value" peerid="v5" />
        <inputport name="valueNumber">3</inputport>
    </functor>
</containerfunctor>
<functor name="MuxMap">
    <property key="dff.functor.alias" value="Previous annual LUC domain" />
    <inputport name="initial" peerid="v298" />
    <inputport name="feedback" peerid="v90003" />
    <outputport name="map" id="v90019" />
</functor>
<containerfunctor name="CalculateMap">
    <property key="dff.functor.alias" value="Woodman annual effective forest K" />
    <property key="dff.functor.comment" value="Retain baseline calibrated K, including zero and NoData, when the current class matches the baseline; otherwise use current category K" />
    <inputport name="expression">[if isNull(i1) or isNull(i2) then null else if isNull(i3) then i2 else if i1 = i3 then i4 else i2]</inputport>
    <inputport name="cellType">.float32</inputport>
    <inputport name="nullValue">.default</inputport>
    <inputport name="resultIsSparse">.no</inputport>
    <inputport name="resultFormat">.none</inputport>
    <outputport name="result" id="v90012" />
    <functor name="NumberMap">
        <inputport name="map" peerid="v90003" />
        <inputport name="mapNumber">1</inputport>
    </functor>
    <functor name="NumberMap">
        <inputport name="map" peerid="v90010" />
        <inputport name="mapNumber">2</inputport>
    </functor>
    <functor name="NumberMap">
        <inputport name="map" peerid="v298" />
        <inputport name="mapNumber">3</inputport>
    </functor>
    <functor name="NumberMap">
        <inputport name="map" peerid="v209" />
        <inputport name="mapNumber">4</inputport>
    </functor>
</containerfunctor>
<containerfunctor name="CalculateMap">
    <property key="dff.functor.alias" value="Woodman annual sold-fuelwood forest domain" />
    <property key="dff.functor.comment" value="Annual equivalent of baseline v317: retain current LUC category for forest and exclude TOF" />
    <inputport name="expression">[if isNull(i1) or isNull(i2) then null else if i2 = 1 then null else i1]</inputport>
    <inputport name="cellType">.int32</inputport>
    <inputport name="nullValue">.default</inputport>
    <inputport name="resultIsSparse">.no</inputport>
    <inputport name="resultFormat">.none</inputport>
    <outputport name="result" id="v90013" />
    <functor name="NumberMap">
        <inputport name="map" peerid="v90003" />
        <inputport name="mapNumber">1</inputport>
    </functor>
    <functor name="NumberMap">
        <inputport name="map" peerid="v90005" />
        <inputport name="mapNumber">2</inputport>
    </functor>
</containerfunctor>
<containerfunctor name="CalculateCategoricalMap">
    <property key="dff.functor.alias" value="Woodman annual self-gather Patcher domain" />
    <inputport name="expression">[if i1 &gt; 0 then 0 else null]</inputport>
    <inputport name="cellType">.int32</inputport>
    <inputport name="nullValue">.default</inputport>
    <inputport name="resultIsSparse">.no</inputport>
    <inputport name="resultFormat">.none</inputport>
    <outputport name="result" id="v90014" />
    <functor name="NumberMap">
        <inputport name="map" peerid="v90003" />
        <inputport name="mapNumber">1</inputport>
    </functor>
</containerfunctor>
<containerfunctor name="CalculateCategoricalMap">
    <property key="dff.functor.alias" value="Woodman annual sold-fuelwood Patcher domain" />
    <inputport name="expression">[if i1 &gt; 0 then 0 else null]</inputport>
    <inputport name="cellType">.int32</inputport>
    <inputport name="nullValue">.default</inputport>
    <inputport name="resultIsSparse">.no</inputport>
    <inputport name="resultFormat">.none</inputport>
    <outputport name="result" id="v90015" />
    <functor name="NumberMap">
        <inputport name="map" peerid="v90013" />
        <inputport name="mapNumber">1</inputport>
    </functor>
</containerfunctor>
<containerfunctor name="CalculateMap">
    <property key="dff.functor.alias" value="Woodman annual accumulator zero map" />
    <inputport name="expression">[if isNull(i1) then null else 0]</inputport>
    <inputport name="cellType">.float32</inputport>
    <inputport name="nullValue">.default</inputport>
    <inputport name="resultIsSparse">.no</inputport>
    <inputport name="resultFormat">.none</inputport>
    <outputport name="result" id="v90016" />
    <functor name="NumberMap">
        <inputport name="map" peerid="v90003" />
        <inputport name="mapNumber">1</inputport>
    </functor>
</containerfunctor>
<containerfunctor name="CalculateMap">
    <property key="dff.functor.alias" value="Start-year stock under Woodman land transitions" />
    <property key="dff.functor.comment" value="Within immutable initial model stock support, preserve stock including zero and later NoData; explicit transitions and returning annual LUC start at zero; eligible TOF receives its annual category allowance" />
    <inputport name="expression">[if isNull(i3) or isNull(i4) or isNull(i2) then null else if i2 = 1 or i2 = 2 or i2 = 4 then 0 else if i3 = 1 then i4 else if isNull(i5) then 0 else i1]</inputport>
    <inputport name="cellType">.float32</inputport>
    <inputport name="nullValue">.default</inputport>
    <inputport name="resultIsSparse">.no</inputport>
    <inputport name="resultFormat">.none</inputport>
    <outputport name="result" id="v90008" />
    <functor name="NumberMap">
        <inputport name="map" peerid="v40" />
        <inputport name="mapNumber">1</inputport>
    </functor>
    <functor name="NumberMap">
        <inputport name="map" peerid="v90018" />
        <inputport name="mapNumber">2</inputport>
    </functor>
    <functor name="NumberMap">
        <inputport name="map" peerid="v90005" />
        <inputport name="mapNumber">3</inputport>
    </functor>
    <functor name="NumberMap">
        <inputport name="map" peerid="v90010" />
        <inputport name="mapNumber">4</inputport>
    </functor>
    <functor name="NumberMap">
        <inputport name="map" peerid="v90019" />
        <inputport name="mapNumber">5</inputport>
    </functor>
</containerfunctor>'''
    if fixed_inputs:
        block = _fixed_input_branch(block)
    return textwrap.indent(textwrap.dedent(block).strip(), "                ")


def build_model(source: str, *, annual_cache: bool = True, fixed_inputs: bool = True,
                nrb_attribution: bool = True, freeze_year: bool = True) -> str:
    """Build v14 from v13; opt-outs are only for regression reference graphs."""
    for new_id in (*range(90001, 90022), *range(90030, 90037)):
        if re.search(rf'\bv{new_id}\b', source):
            raise ValueError(f"New Dinamica ID is already in use: v{new_id}")
    marker = '<property key="dff.functor.alias" value="repeat874" />'
    start = source.rfind('<containerfunctor name="Repeat">', 0, source.index(marker))
    if start < 0:
        raise ValueError("Cannot locate annual Repeat")
    tags = re.compile(r'<containerfunctor\b|</containerfunctor>')
    depth = 0
    end = None
    for match in tags.finditer(source, start):
        depth += 1 if match.group() == '<containerfunctor' else -1
        if depth == 0:
            end = match.end()
            break
    if end is None:
        raise ValueError("Annual Repeat is not closed")
    annual = source[start:end]
    annual = annual.replace('peerid="v298"', 'peerid="v90003"')
    annual = annual.replace('peerid="v204"', 'peerid="v90005"')
    annual = annual.replace('peerid="v40"', 'peerid="v90008"')
    annual = annual.replace('peerid="v213"', 'peerid="v90011"')
    annual = annual.replace('peerid="v209"', 'peerid="v90012"')
    annual = annual.replace('peerid="v317"', 'peerid="v90013"')
    annual = annual.replace('peerid="v190"', 'peerid="v90014"')
    annual = annual.replace('peerid="v192"', 'peerid="v90015"')
    annual = annual.replace('peerid="v191"', 'peerid="v90016"')
    # Keep v13's initialization, including raw-AGB-independent TOF allowances.
    # Its resulting model-stock support is immutable: v90003/v90005 exclude
    # initially missing cells from both annual dynamics and supply allocation.
    # Numeric zero is eligible; missing growth parameters after initialization
    # do not change the support. Reentry inside it is handled by v90008.
    # Both growth branches must suppress conversion-year growth before harvest
    # is calculated: setting category rmax to zero only affects logistic growth.
    # Guard the capped branch's final clamp too: a missing calibrated K must
    # not turn the explicit conversion-year zero back into NoData.
    for alias in ("calculateMap1879", "calculateMap1133", "calculateMapForestStockClampV6"):
        marker = f'<property key="dff.functor.alias" value="{alias}" />'
        node_start = annual.rfind('<containerfunctor name="CalculateMap">', 0, annual.index(marker))
        node_end = annual.index('</containerfunctor>', annual.index(marker))
        block = annual[node_start:node_end]
        block = replace_once(
            block,
            '<inputport name="expression">[',
            '<inputport name="expression">[if isNull(i98) then null else if i99 = 1 or i99 = 2 or i99 = 4 then 0 else ',
        )
        block += '''    <functor name="NumberMap">
                                <property key="dff.functor.alias" value="Immutable initial model stock support" />
                                <inputport name="map" peerid="v200" />
                                <inputport name="mapNumber">98</inputport>
                            </functor>
                            <functor name="NumberMap">
                                <property key="dff.functor.alias" value="Annual conversion growth suppression" />
                                <inputport name="map" peerid="v90018" />
                                <inputport name="mapNumber">99</inputport>
                            </functor>
                        '''
        annual = annual[:node_start] + block + annual[node_end:]
    annual = replace_once(
        annual,
        '<internaloutputport name="step" id="v39" />',
        '<internaloutputport name="step" id="v39" />\n' + annual_loader(fixed_inputs),
    )
    annual = replace_once(
        annual,
        'if i2 = 0 and i1 &lt;= 0 then',
        'if isNull(i4) then&#x0A;        null&#x0A;    else if i3 = 1 or i3 = 2 or i3 = 4 then&#x0A;        0&#x0A;    else if i2 = 0 and i1 &lt;= 0 then',
    )
    annual = replace_once(
        annual,
        'value="Seed depleted forest stock with 2 Mg per cell; TOF stock remains at K, including zero"',
        'value="Initial model NoData remains NoData; eligible forest clearing, new forest and TOF loss end at zero stock; ordinary depleted forest retains the 2 Mg seed"',
    )
    old_tof_input = '''<property key="dff.functor.alias" value="numberMap20022" />
                        <inputport name="map" peerid="v90005" />
                        <inputport name="mapNumber">2</inputport>
                    </functor>'''
    annual = replace_once(
        annual,
        old_tof_input,
        old_tof_input + '''
                    <functor name="NumberMap">
                        <property key="dff.functor.alias" value="Woodman transition at end of year" />
                        <inputport name="map" peerid="v90018" />
                        <inputport name="mapNumber">3</inputport>
                    </functor>
                    <functor name="NumberMap">
                        <property key="dff.functor.alias" value="Immutable initial model stock support" />
                        <inputport name="map" peerid="v200" />
                        <inputport name="mapNumber">4</inputport>
                    </functor>''',
    )
    output = source[:start] + annual + source[end:]
    output = replace_once(
        output,
        "&quot;1 = MODIS 2001 (proxy 2000) - 2 = Copernicus 2015&quot;",
        "&quot;LUC data: 1 = MODIS static; 3 = Woodman annual&quot;",
    )
    output = replace_once(
        output,
        '''<property key="dff.functor.alias" value="LUC map version" />
            <property key="wizard.constant.input" value="Int_constant_4" />
            <inputport name="constant">1</inputport>
            <outputport name="object" id="v302" />''',
        '''<property key="dff.functor.alias" value="LUC map version" />
            <property key="wizard.constant.input" value="Int_constant_4" />
            <inputport name="constant">3</inputport>
            <outputport name="object" id="v302" />''',
    )
    output = replace_once(
        output,
        '''<functor name="LoadLookupTable">
            <property key="dff.functor.alias" value="loadLookupTable3952" />
            <inputport name="filename">&quot;LULCC/TempTables/TOFvsFOR_Categories1.csv&quot;</inputport>''',
        '''<containerfunctor name="CreateString">
            <property key="dff.functor.alias" value="Selected LUC TOF categories filename" />
            <inputport name="format">&quot;LULCC/TempTables/TOFvsFOR_Categories&lt;v1&gt;.csv&quot;</inputport>
            <outputport name="result" id="v90009" />
            <functor name="NumberValue">
                <inputport name="value" peerid="v302" />
                <inputport name="valueNumber">1</inputport>
            </functor>
        </containerfunctor>
        <functor name="LoadLookupTable">
            <property key="dff.functor.alias" value="loadLookupTable3952" />
            <inputport name="filename" peerid="v90009" />''',
    )
    if 'value="Exact selected MC row for v242"' in output:
        # Optimized v13 already has the three MC-row helpers. Reuse those for
        # the two annual lookup maps just introduced by this builder.
        path = HERE / "tools" / "optimize_dinamica_windows.py"
        spec = importlib.util.spec_from_file_location("windows_lookup_builder", path)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        output, _ = module.optimize_table_lookups(output)
    if annual_cache:
        # This scientific correction is intentionally distinct from the
        # performance transforms. Source text retains an explicit contract.
        tool_dir = str(HERE / "tools")
        sys.path.insert(0, tool_dir)
        try:
            from fix_woodman_annual_sourcing_cache import correct_annual_sourcing_cache
            output, _ = correct_annual_sourcing_cache(output)
        finally:
            sys.path.remove(tool_dir)
    if nrb_attribution:
        tool_dir = str(HERE / "tools")
        sys.path.insert(0, tool_dir)
        try:
            from fix_woodman_nrb_attribution import correct_nrb_attribution
            output, _ = correct_nrb_attribution(output)
        finally:
            sys.path.remove(tool_dir)
    if freeze_year:
        tool_dir = str(HERE / "tools")
        sys.path.insert(0, tool_dir)
        try:
            from add_woodman_freeze_year import add_freeze_year
            output, _ = add_freeze_year(output)
        finally:
            sys.path.remove(tool_dir)
    ET.fromstring(output)
    return output


def main() -> None:
    output = build_model(SOURCE.read_text(encoding="utf-8"))
    TARGET.write_text(output, encoding="utf-8")
    print(f"Wrote {TARGET}")


if __name__ == "__main__":
    main()
