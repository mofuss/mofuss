"""The native engine must not continue after failed R MC preparation."""
from pathlib import Path
import sys
import unittest
import xml.etree.ElementTree as E

sys.dont_write_bytecode = True
SCRIPTS = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(SCRIPTS / "tools"))
from guard_mc_startup import guard_mc_startup, validate_startup_guard, STATUS_FILE, FAILURE_MESSAGE
from build_woodman_dinamica_v14 import build_model, SOURCE, TARGET


class StartupGuardGraph(unittest.TestCase):
    def setUp(self):
        self.text = TARGET.read_text(encoding="utf-8")
        self.root = E.fromstring(self.text)

    def test_builder_reproduces_and_patch_is_idempotent(self):
        self.assertEqual(build_model(SOURCE.read_text(encoding="utf-8")), self.text)
        same, report = guard_mc_startup(self.text)
        self.assertEqual(same, self.text)
        self.assertTrue(report["already_applied"])

    def test_pending_marker_cannot_be_success(self):
        bad = self.text.replace('[ "Key" "Value" 1 -1 ]', '[ "Key" "Value" 1 1 ]')
        self.assertNotEqual(bad, self.text)
        with self.assertRaisesRegex(ValueError, "invalidate"):
            guard_mc_startup(bad)

    def test_runner_must_wait_for_reset(self):
        runner = next(n for n in self.root.iter("functor") if
                      n.find("property[@value='runExternalProcess2510']") is not None)
        runner.find("inputport[@name='secondsToWait']").attrib.pop("peerid")
        with self.assertRaisesRegex(ValueError, "wait for the native status reset"):
            validate_startup_guard(self.root)

    def test_status_cannot_be_read_before_process_group(self):
        loader = next(n for n in self.root.iter("functor") if
                      n.find("outputport[@id='v95002']") is not None)
        loader.find("inputport[@name='filename']").attrib.pop("peerid")
        with self.assertRaisesRegex(ValueError, "before the R process"):
            validate_startup_guard(self.root)

    def test_failure_aborts_with_machine_readable_message(self):
        self.assertIn("MOFUSS_R_STARTUP_FAILED", FAILURE_MESSAGE)
        self.assertIn(FAILURE_MESSAGE, self.text)
        failed = next(n for n in self.root.iter("containerfunctor") if
                      n.find("property[@value='Explain Monte Carlo startup failure']") is not None)
        failed.remove(failed.find("functor[@name='Exit']"))
        with self.assertRaisesRegex(ValueError, "must abort"):
            validate_startup_guard(self.root)

    def test_marker_does_not_require_temp_directory(self):
        self.assertEqual(STATUS_FILE, "mc_startup_guard.csv")


if __name__ == "__main__":
    unittest.main()
