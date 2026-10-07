"""Static checks for annual Woodman routing and stock-flow semantics in v14.

The native Dinamica fixture separately checks expression execution. These checks
protect the generated production graph against stale baseline peer references.
"""

from __future__ import annotations

from collections import Counter
import importlib.util
import os
from pathlib import Path
import re
import sys
import unittest
import xml.etree.ElementTree as ET


SCRIPTS = Path(__file__).resolve().parents[1]
V14_MODEL = Path(os.environ.get("MOFUSS_V14_TEST_MODEL", SCRIPTS / "10_dyn_Sc17_webmofuss_ctrees_g_v14.egoml"))


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


def route_value(node: ET.Element, **values: float | None) -> float | None:
    """Evaluate production routing expressions, without duplicating their rules.

    This deliberately small evaluator accepts only conditionals, comparisons,
    boolean operators, map values and isNull. Actual growth and null arithmetic
    are covered by the separate native Dinamica regression.
    """
    source = expression(node).strip()[1:-1]
    tokens = re.findall(r"isNull|[A-Za-z][A-Za-z0-9]*|\d+(?:\.\d+)?|!=|<=|>=|[()=<>]", source)
    if "".join(tokens) != re.sub(r"\s+", "", source):
        raise AssertionError("Unsupported routing-expression syntax")
    index = 0

    def take(expected: str | None = None) -> str:
        nonlocal index
        token = tokens[index]
        index += 1
        if expected is not None and token != expected:
            raise AssertionError((expected, token))
        return token

    def atom() -> tuple:
        token = take()
        if token == "isNull":
            take("(")
            value = atom()
            take(")")
            return ("isNull", value)
        if token == "null":
            return ("literal", None)
        if token[0].isdigit():
            return ("literal", float(token))
        return ("map", token)

    def comparison() -> tuple:
        left = atom()
        if index < len(tokens) and tokens[index] in ("=", "!=", "<", ">", "<=", ">="):
            return (take(), left, atom())
        return left

    def conjunction() -> tuple:
        left = comparison()
        while index < len(tokens) and tokens[index] == "and":
            take()
            left = ("and", left, comparison())
        return left

    def condition() -> tuple:
        left = conjunction()
        while index < len(tokens) and tokens[index] == "or":
            take()
            left = ("or", left, conjunction())
        return left

    def conditional() -> tuple:
        if tokens[index] != "if":
            return condition()
        take("if")
        predicate = condition()
        take("then")
        yes = conditional()
        take("else")
        return ("if", predicate, yes, conditional())

    def evaluate(tree: tuple) -> float | bool | None:
        operation = tree[0]
        if operation == "literal":
            return tree[1]
        if operation == "map":
            return values[tree[1]]
        left = evaluate(tree[1])
        if operation == "isNull":
            return left is None
        if operation == "if":
            return None if left is None else evaluate(tree[2] if left else tree[3])
        right = evaluate(tree[2])
        if operation == "or":
            return True if left or right else None if left is None or right is None else False
        if operation == "and":
            return False if left is False or right is False else None if left is None or right is None else True
        if left is None or right is None:
            return None
        return {"=": lambda: left == right, "!=": lambda: left != right,
                "<": lambda: left < right, ">": lambda: left > right,
                "<=": lambda: left <= right, ">=": lambda: left >= right}[operation]()

    tree = conditional()
    if index != len(tokens):
        raise AssertionError("Unconsumed routing-expression tokens")
    return evaluate(tree)


class WoodmanDinamicaV14StaticTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.v13 = ET.parse(SCRIPTS / "10_dyn_Sc17_webmofuss_ctrees_g_v13.egoml").getroot()
        cls.v14 = ET.parse(V14_MODEL).getroot()
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

    def test_fixed_luc_never_requires_year_labelled_files(self) -> None:
        selector = node_with_output(self.v14, "v90030")
        self.assertEqual(expression(selector), "[v1 = 1]")
        self.assertEqual(peers(selector), ["v302"])
        parents = {child: parent for parent in self.v14.iter() for child in parent}
        for output, annual, fixed, original in (
            ("v90020", "v90031", "v90032", "v298"),
            ("v90021", "v90033", "v90034", "v204"),
            ("v90007", "v90035", "v90036", "v298"),
        ):
            junction = node_with_output(self.v14, output)
            self.assertEqual(junction.get("name"), "MapJunction")
            self.assertEqual(set(peers(junction)), {annual, fixed})
            annual_node = node_with_output(self.v14, annual)
            self.assertEqual(annual_node.get("name"), "LoadMap")
            self.assertEqual(parents[annual_node].get("name"), "IfNotThen")
            self.assertEqual(parents[annual_node].find("inputport[@name='condition']").get("peerid"), "v90030")
            fixed_node = node_with_output(self.v14, fixed)
            self.assertEqual(peers(fixed_node), [original])
            self.assertEqual(parents[fixed_node].get("name"), "IfThen")
            self.assertEqual(parents[fixed_node].find("inputport[@name='condition']").get("peerid"), "v90030")
        transition = node_with_output(self.v14, "v90036")
        for value in (None, 0, 1, 425):
            self.assertEqual(route_value(transition, i1=value), None if value is None else 0)
        for ident in ("v90032", "v90034"):
            self.assertEqual(expression(node_with_output(self.v14, ident)), "[i1]")

    def test_annual_sourcing_contract_and_graph_references(self) -> None:
        marker = self.v14.find("property[@key='mofuss.sourcing.capture.contract']")
        self.assertEqual(marker.get("value"), "annual_domain_after_static_npa_cache_v1")
        for cold, current, raw, domain in (
            ("v6000", "v362", "v91000", "v90003"),
            ("v6010", "v377", "v91010", "v90013"),
        ):
            self.assertEqual(expression(node_with_output(self.v14, cold)), "[i1]")
            self.assertEqual(set(peers(node_with_output(self.v14, current))), {raw, domain})
        all_ids = [p.get("id") for p in self.v14.iter() if p.get("id")]
        self.assertEqual(len(all_ids), len(set(all_ids)))
        self.assertFalse(set(peers(self.v14)) - set(all_ids))

    def test_annual_domains_use_immutable_model_initial_stock(self) -> None:
        for output, raw in (("v90003", "v90020"), ("v90005", "v90021")):
            mask = node_with_output(self.repeat, output)
            self.assertEqual(set(peers(mask)), {raw, "v200"})
            for annual_value in (None, 0, 1, 3, 425):
                for initial in (None, 0, 2, 100):
                    self.assertEqual(route_value(mask, i1=annual_value, i2=initial),
                                     None if initial is None else annual_value)
        feedback = node_with_output(self.repeat, "v98")
        self.assertIn("v200", peers(feedback))
        self.assertTrue(re.sub(r"\s+", " ", expression(feedback)).startswith("[ if isNull(i4) then null else "))

    def test_annual_k_rate_and_supply_consumers(self) -> None:
        k = node_with_output(self.repeat, "v90010")
        rate = node_with_output(self.repeat, "v90011")
        effective_k = node_with_output(self.repeat, "v90012")
        if "t1[i1 + 1]" in expression(k):
            # Exact MC-row selection now occurs once per draw outside the
            # annual loop; the pixel calculation retains its original guards.
            self.assertEqual(set(peers(k)), {"v90003", "v92007"})
            self.assertEqual(set(peers(rate)), {"v90003", "v90018", "v92011", "v6", "v5"})
            for selected, original in (("v92007", "v244"), ("v92011", "v243")):
                helper = node_with_output(self.v14, selected)
                self.assertIn(original, peers(helper))
                self.assertIn("v10", peers(helper))
                self.assertEqual(expression(helper), "[t2[[v1][line]]]")
        else:
            self.assertEqual(set(peers(k)), {"v90003", "v244", "v10"})
            self.assertEqual(set(peers(rate)), {"v90003", "v90018", "v243", "v10", "v6", "v5"})
        for transition_code in (1, 2, 4):
            self.assertIn(f"i2 = {transition_code}", expression(rate))
        self.assertEqual(set(peers(effective_k)), {"v90003", "v90010", "v298", "v209"})
        self.assertIn("i1 = i3 then i4 else i2", expression(effective_k))

        counts = Counter(peers(self.repeat))
        self.assertEqual(counts["v90011"], 3)  # W shortfall, TOF accounting, growth
        self.assertEqual(counts["v90012"], 2)  # growth and stock clamp
        for stale in ("v213", "v317", "v190", "v191", "v192"):
            self.assertEqual(counts[stale], 0, stale)

    def test_transition_stock_and_annual_domains(self) -> None:
        transition = node_with_output(self.repeat, "v90018")
        start_stock = node_with_output(self.repeat, "v90008")
        end_stock = node_with_output(self.repeat, "v98")
        self.assertEqual(set(peers(transition)), {"v90003", "v90007"})
        self.assertIn("if isNull(i2) then 0", expression(transition))
        self.assertEqual(set(peers(start_stock)), {"v40", "v90018", "v90005", "v90010", "v90019"})
        self.assertIn("i2 = 1 or i2 = 2 or i2 = 4 then 0", expression(start_stock))
        self.assertIn("i3 = 1 then i4", expression(start_stock))
        self.assertNotIn("isNull(i1) then 0", expression(start_stock))
        self.assertIn("i3 = 1 or i3 = 2 or i3 = 4 then", expression(end_stock))
        self.assertNotIn("i3 = 3", expression(end_stock))  # TOF gain retains current K

        tof_eligibility = node_with_output(self.repeat, "v90013")
        self.assertEqual(set(peers(tof_eligibility)), {"v90003", "v90005"})
        self.assertIn("i2 = 1 then null else i1", expression(tof_eligibility))
        self.assertEqual(set(peers(node_with_output(self.repeat, "v90014"))), {"v90003"})
        self.assertEqual(set(peers(node_with_output(self.repeat, "v90015"))), {"v90013"})
        self.assertEqual(set(peers(node_with_output(self.repeat, "v90016"))), {"v90003"})
        self.assertNotIn("v90017", peers(self.repeat))
        self.assertIn("v200", peers(node_with_output(self.repeat, "v180")))

    def test_previous_year_state_is_carried_across_the_loop(self) -> None:
        for output, initial, feedback in (("v90019", "v298", "v90003"),):
            node = node_with_output(self.repeat, output)
            self.assertEqual(node.get("name"), "MuxMap")
            self.assertEqual(node.find("inputport[@name='initial']").get("peerid"), initial)
            self.assertEqual(node.find("inputport[@name='feedback']").get("peerid"), feedback)

    def test_zero_and_missing_stock_remain_distinct_on_unchanged_land(self) -> None:
        stock = node_with_output(self.repeat, "v90008")
        capacity = node_with_output(self.repeat, "v90012")
        for value in (None, 0, 2, 100):
            with self.subTest(previous_stock=value):
                self.assertEqual(route_value(stock, i1=value, i2=0, i3=0, i4=500, i5=2), value)
                self.assertEqual(route_value(capacity, i1=2, i2=500, i3=2, i4=value), value)
        self.assertEqual(route_value(stock, i1=None, i2=0, i3=0, i4=500, i5=None), 0)
        self.assertEqual(route_value(capacity, i1=2, i2=500, i3=None, i4=None), 500)
        for transition in (1, 2, 4):
            self.assertEqual(route_value(stock, i1=100, i2=transition, i3=0, i4=500, i5=2), 0)
        self.assertEqual(route_value(stock, i1=0, i2=0, i3=1, i4=75, i5=7), 75)

    def test_class_return_retains_original_baseline_capacity_rule(self) -> None:
        capacity = node_with_output(self.repeat, "v90012")
        for baseline_k in (None, 0, 100):
            for current_class, category_k in ((2, 500), (3, 700), (3, 700), (2, 500), (2, 500)):
                actual = route_value(capacity, i1=current_class, i2=category_k,
                                     i3=2, i4=baseline_k)
                self.assertEqual(actual, baseline_k if current_class == 2 else category_k)

    def test_sold_fuelwood_domain_matches_v13_for_every_category(self) -> None:
        baseline = node_with_output(self.v13, "v317")
        annual = node_with_output(self.repeat, "v90013")
        patcher = node_with_output(self.repeat, "v90015")
        for category in range(1, 426):
            for tof in (0, 1):
                expected = route_value(baseline, i1=category, i2=tof)
                actual = route_value(annual, i1=category, i2=tof)
                self.assertEqual(actual, expected)
                self.assertEqual(route_value(patcher, i1=actual), None if tof else 0)

    def test_growth_equations_preserved_after_explicit_conversion_guard(self) -> None:
        for output in ("v178", "v180", "v353"):
            before = expression(node_with_output(self.v13, output))
            after = expression(node_with_output(self.repeat, output))
            self.assertEqual(after, before.replace("[", "[if isNull(i98) then null else if i99 = 1 or i99 = 2 or i99 = 4 then 0 else ", 1))
            self.assertIn("v90018", peers(node_with_output(self.repeat, output)))
            self.assertIn("v200", peers(node_with_output(self.repeat, output)))

    def test_builder_reproduces_checked_in_model(self) -> None:
        sys.dont_write_bytecode = True
        path = SCRIPTS / "tools" / "build_woodman_dinamica_v14.py"
        spec = importlib.util.spec_from_file_location("woodman_builder", path)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        self.assertEqual(
            module.build_model((SCRIPTS / "10_dyn_Sc17_webmofuss_ctrees_g_v13.egoml").read_text(encoding="utf-8")),
            V14_MODEL.read_text(encoding="utf-8"),
        )


if __name__ == "__main__":
    unittest.main()
