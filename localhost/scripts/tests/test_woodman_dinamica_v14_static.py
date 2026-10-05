"""Static checks for annual Woodman routing and stock-flow semantics in v14.

The native Dinamica fixture separately checks expression execution. These checks
protect the generated production graph against stale baseline peer references.
"""

from __future__ import annotations

from collections import Counter
from pathlib import Path
import unittest
import xml.etree.ElementTree as ET


SCRIPTS = Path(__file__).resolve().parents[1]


def node_with_output(root: ET.Element, output_id: str) -> ET.Element:
    matches = [
        node
        for node in root.iter()
        if any(port.get("id") == output_id for port in node.findall("outputport"))
    ]
    if len(matches) != 1:
        raise AssertionError(f"Expected one producer of {output_id}, found {len(matches)}")
    return matches[0]


def peers(node: ET.Element) -> list[str]:
    return [port.get("peerid") for port in node.iter("inputport") if port.get("peerid")]


def expression(node: ET.Element) -> str:
    value = node.find("inputport[@name='expression']")
    if value is None or value.text is None:
        raise AssertionError("Expression missing")
    return value.text


class WoodmanDinamicaV14StaticTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.v13 = ET.parse(SCRIPTS / "10_dyn_Sc17_webmofuss_ctrees_g_v13.egoml").getroot()
        cls.v14 = ET.parse(SCRIPTS / "10_dyn_Sc17_webmofuss_ctrees_g_v14.egoml").getroot()
        cls.repeat = next(
            node
            for node in cls.v14.iter("containerfunctor")
            if any(
                item.get("key") == "dff.functor.alias"
                and item.get("value") == "repeat874"
                for item in node.findall("property")
            )
        )

    def test_selected_luc_channel_routes_both_annual_maps_and_tof_table(self) -> None:
        selector = node_with_output(self.v14, "v302")
        self.assertEqual(selector.find("inputport[@name='constant']").text, "3")
        for output_id in ("v90002", "v90004", "v90006", "v90009"):
            self.assertIn("v302", peers(node_with_output(self.v14, output_id)))
        self.assertIn("LULCt<v1>_c_<v2>.tif", "".join(node_with_output(self.v14, "v90002").itertext()))
        self.assertIn("TOFvsFOR_mask<v1>_<v2>.tif", "".join(node_with_output(self.v14, "v90004").itertext()))
        self.assertIn("LULCt<v1>_transition_<v2>.tif", "".join(node_with_output(self.v14, "v90006").itertext()))
        self.assertIn("TOFvsFOR_Categories<v1>.csv", "".join(node_with_output(self.v14, "v90009").itertext()))

    def test_baseline_initial_stock_and_calibration_are_preserved(self) -> None:
        for output_id in ("v200", "v203", "v209", "v213", "v316", "v317"):
            self.assertEqual(
                ET.tostring(node_with_output(self.v13, output_id)),
                ET.tostring(node_with_output(self.v14, output_id)),
                output_id,
            )

    def test_annual_k_rate_and_supply_consumers(self) -> None:
        k = node_with_output(self.repeat, "v90010")
        rate = node_with_output(self.repeat, "v90011")
        effective_k = node_with_output(self.repeat, "v90012")
        self.assertEqual(set(peers(k)), {"v90003", "v244", "v10"})
        self.assertEqual(set(peers(rate)), {"v90003", "v90018", "v243", "v10", "v6", "v5"})
        for transition_code in (1, 2, 4):
            self.assertIn(f"i2 = {transition_code}", expression(rate))
        self.assertEqual(set(peers(effective_k)), {"v90003", "v90010", "v298", "v209"})
        self.assertIn("i1 = i3 then i4 else i2", expression(effective_k))

        counts = Counter(peers(self.repeat))
        self.assertEqual(counts["v90011"], 3)  # W shortfall, TOF accounting, growth
        self.assertEqual(counts["v90012"], 2)  # growth and forest stock clamp
        for stale in ("v213", "v317", "v190", "v191", "v192"):
            self.assertEqual(counts[stale], 0, stale)

    def test_transition_stock_and_annual_domains(self) -> None:
        transition = node_with_output(self.repeat, "v90018")
        start_stock = node_with_output(self.repeat, "v90008")
        end_stock = node_with_output(self.repeat, "v98")
        self.assertEqual(set(peers(transition)), {"v90003", "v90007"})
        self.assertIn("if isNull(i2) then 0", expression(transition))
        self.assertEqual(set(peers(start_stock)), {"v40", "v90018", "v90005", "v90010"})
        self.assertIn("i2 = 1 or i2 = 2 or i2 = 4 then 0", expression(start_stock))
        self.assertIn("i3 = 1 then i4", expression(start_stock))
        self.assertIn("isNull(i1) then 0", expression(start_stock))
        self.assertIn("i3 = 1 or i3 = 2 or i3 = 4 then", expression(end_stock))
        self.assertNotIn("i3 = 3", expression(end_stock))  # TOF gain retains current K

        tof_eligibility = node_with_output(self.repeat, "v90013")
        self.assertEqual(set(peers(tof_eligibility)), {"v90003", "v90005"})
        self.assertIn("i1 = 1 then null else i2", expression(tof_eligibility))
        self.assertEqual(set(peers(node_with_output(self.repeat, "v90014"))), {"v90003"})
        self.assertEqual(set(peers(node_with_output(self.repeat, "v90015"))), {"v90013"})
        self.assertEqual(set(peers(node_with_output(self.repeat, "v90016"))), {"v90003"})
        self.assertEqual(set(peers(node_with_output(self.repeat, "v90017"))), {"v200", "v90003"})


if __name__ == "__main__":
    unittest.main()
