"""Build the Woodman annual-LULC Dinamica model from the validated v13 model."""

from __future__ import annotations

import re
import textwrap
import xml.etree.ElementTree as ET
from pathlib import Path


HERE = Path(__file__).resolve().parents[1]
SOURCE = HERE / "10_dyn_Sc17_webmofuss_ctrees_g_v13.egoml"
TARGET = HERE / "10_dyn_Sc17_webmofuss_ctrees_g_v14.egoml"


def replace_once(source: str, before: str, after: str) -> str:
    if source.count(before) != 1:
        raise ValueError(f"Expected one match, found {source.count(before)}: {before[:90]}")
    return source.replace(before, after, 1)


def annual_loader() -> str:
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
    <outputport name="map" id="v90003" />
</functor>
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
    <outputport name="map" id="v90005" />
</functor>
<containerfunctor name="CreateString">
    <property key="dff.functor.alias" value="Woodman forest transition filename" />
    <inputport name="format">&quot;LULCC/TempRaster/WoodmanTransition_&lt;v1&gt;.tif&quot;</inputport>
    <outputport name="result" id="v90006" />
    <functor name="NumberValue">
        <inputport name="value" peerid="v90001" />
        <inputport name="valueNumber">1</inputport>
    </functor>
</containerfunctor>
<functor name="LoadMap">
    <property key="dff.functor.alias" value="Woodman annual forest transitions" />
    <property key="dff.functor.comment" value="1 forest cleared; 2 new forest; 3 TOF gained; 4 TOF lost" />
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
<containerfunctor name="CalculateMap">
    <property key="dff.functor.alias" value="Woodman annual effective forest K" />
    <property key="dff.functor.comment" value="Keep baseline calibrated K for an unchanged class; use current category K after a class change" />
    <inputport name="expression">[if isNull(i1) or isNull(i2) then null else if isNull(i3) or isNull(i4) then i2 else if i1 = i3 then i4 else i2]</inputport>
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
    <property key="dff.functor.alias" value="Woodman annual TOF eligibility" />
    <property key="dff.functor.comment" value="Annual equivalent of baseline v317: current TOF mask except category key 1" />
    <inputport name="expression">[if isNull(i1) or isNull(i2) then null else if i1 = 1 then null else i2]</inputport>
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
    <property key="dff.functor.alias" value="Baseline initial stock extended to new Woodman cells" />
    <inputport name="expression">[if isNull(i2) then null else if isNull(i1) then 0 else i1]</inputport>
    <inputport name="cellType">.float32</inputport>
    <inputport name="nullValue">.default</inputport>
    <inputport name="resultIsSparse">.no</inputport>
    <inputport name="resultFormat">.none</inputport>
    <outputport name="result" id="v90017" />
    <functor name="NumberMap">
        <inputport name="map" peerid="v200" />
        <inputport name="mapNumber">1</inputport>
    </functor>
    <functor name="NumberMap">
        <inputport name="map" peerid="v90003" />
        <inputport name="mapNumber">2</inputport>
    </functor>
</containerfunctor>
<containerfunctor name="CalculateMap">
    <property key="dff.functor.alias" value="Start-year stock under Woodman land transitions" />
    <property key="dff.functor.comment" value="Forest clearing, forest gain and TOF loss start at zero; current TOF receives only its annual category allowance" />
    <inputport name="expression">[if isNull(i3) or isNull(i4) or isNull(i2) then null else if i2 = 1 or i2 = 2 or i2 = 4 then 0 else if i3 = 1 then i4 else if isNull(i1) then 0 else i1]</inputport>
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
</containerfunctor>'''
    return textwrap.indent(textwrap.dedent(block).strip(), "                ")


def main() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    for new_id in range(90001, 90019):
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
    annual = replace_once(
        annual,
        '''<property key="dff.functor.alias" value="numberMap5206" />
                                <inputport name="map" peerid="v200" />''',
        '''<property key="dff.functor.alias" value="numberMap5206" />
                                <inputport name="map" peerid="v90017" />''',
    )
    annual = replace_once(
        annual,
        '<internaloutputport name="step" id="v39" />',
        '<internaloutputport name="step" id="v39" />\n' + annual_loader(),
    )
    annual = replace_once(
        annual,
        'if i2 = 0 and i1 &lt;= 0 then',
        'if i3 = 1 or i3 = 2 or i3 = 4 then&#x0A;        0&#x0A;    else if i2 = 0 and i1 &lt;= 0 then',
    )
    annual = replace_once(
        annual,
        'value="Seed depleted forest stock with 2 Mg per cell; TOF stock remains at K, including zero"',
        'value="Forest clearing, new forest and TOF loss end at zero stock; ordinary depleted forest retains the 2 Mg seed"',
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
                    </functor>''',
    )
    output = source[:start] + annual + source[end:]
    output = replace_once(
        output,
        "&quot;1 = MODIS 2001 (proxy 2000) - 2 = Copernicus 2015&quot;",
        "&quot;Woodman annual maps: 1 = legacy LUC1 slot, 3 = LUC3 slot&quot;",
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
    ET.fromstring(output)
    TARGET.write_text(output, encoding="utf-8")
    print(f"Wrote {TARGET}")


if __name__ == "__main__":
    main()
