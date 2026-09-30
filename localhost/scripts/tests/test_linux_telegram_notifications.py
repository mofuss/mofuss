"""Exercise the single-file shell notifier without real credentials or sends."""
import contextlib
import io
import os
from pathlib import Path
import signal
import subprocess
import tempfile
import types
import unittest
from unittest import mock

SHELL = Path(__file__).resolve().parents[1] / 'run_linux.sh'
CODE = SHELL.read_text().split("<<'MOFUSS_TELEGRAM_PY'\n", 1)[1].rsplit('\nMOFUSS_TELEGRAM_PY', 1)[0]
notifier = types.ModuleType('mofuss_notifier_test')
exec(compile(CODE, str(SHELL), 'exec'), notifier.__dict__)


class TestTelegramNotifications(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix='mofuss telegram ')
        self.root = Path(self.temporary.name)
        self.scenario = self.root / 'region with spaces'
        self.scenario.mkdir()
        self.previous_signals = {s: signal.getsignal(s) for s in (signal.SIGINT, signal.SIGTERM)}
        self.env_patch = mock.patch.dict(os.environ, {
            'PATH': os.environ.get('PATH', '/usr/bin:/bin'), 'MOFUSS_TELEGRAM_MSGS': '1'
        }, clear=True)
        self.env_patch.start()

    def tearDown(self):
        self.env_patch.stop()
        for signum, handler in self.previous_signals.items():
            signal.signal(signum, handler)
        self.temporary.cleanup()

    def credentials(self):
        path = self.root / 'mofuss/localhost/scripts/.env'
        path.parent.mkdir(parents=True)
        path.write_text('MOFUSS_TELEGRAM_BOT_TOKEN="fake-token" # comment\n'
                        "export MOFUSS_TELEGRAM_CHAT_ID='123'\n"
                        'UNRELATED=$(touch should_never_exist)\n')
        return path

    def test_auto_discovery_and_environment_precedence(self):
        self.credentials()
        self.assertEqual(notifier.read_credentials(self.scenario),
                         dict(zip(notifier.SECRET_KEYS, ('fake-token', '123'))))
        os.environ[notifier.SECRET_KEYS[0]] = 'process-token'
        self.assertEqual(notifier.read_credentials(self.scenario)[notifier.SECRET_KEYS[0]], 'process-token')
        self.assertFalse((self.root / 'should_never_exist').exists())

    def test_explicit_env_file_on_another_drive(self):
        path = self.root / 'elsewhere.env'
        path.write_text('MOFUSS_TELEGRAM_BOT_TOKEN=other\nMOFUSS_TELEGRAM_CHAT_ID=456\n')
        os.environ['MOFUSS_TELEGRAM_ENV_FILE'] = str(path)
        self.assertEqual(notifier.read_credentials(self.scenario)[notifier.SECRET_KEYS[1]], '456')

    def test_http_error_cannot_expose_token_and_does_not_raise(self):
        sender = notifier.Telegram(dict(zip(notifier.SECRET_KEYS, ('secret-token', '123'))))
        stderr = io.StringIO()
        with mock.patch.object(notifier.urllib.request, 'urlopen',
                               side_effect=RuntimeError('https://api.telegram.org/botsecret-token/sendMessage')), \
                contextlib.redirect_stderr(stderr):
            self.assertFalse(sender.send('hello'))
            self.assertFalse(sender.send('hello'))
        self.assertNotIn('secret-token', stderr.getvalue())
        self.assertEqual(stderr.getvalue().count('notification unavailable'), 1)

    def test_progress_ignores_old_rerun_outputs(self):
        parameters = self.scenario / 'LULCC/TempTables/parameters_dinamica.csv'
        parameters.parent.mkdir(parents=True)
        parameters.write_text('Var,ParCHR\nstart_year,2000\n')
        old = self.scenario / 'debugging_3/Harvest_tot51.tif'
        old.parent.mkdir(); old.touch(); os.utime(old, (1, 1))
        new = self.scenario / 'debugging_1/Harvest_tot21.tif'
        new.parent.mkdir(); new.touch()
        self.assertEqual(notifier.latest_harvest(self.scenario, 2), 'Latest saved harvest: MC1, year 2020.')

    def test_short_success_and_failure_have_only_start_and_final_messages(self):
        self.credentials()
        for exit_code in (0, 7):
            (self.scenario / 'run_linux.py').write_text(f'import sys\nsys.exit({exit_code})\n')
            messages = []
            with mock.patch.object(notifier.Telegram, 'send',
                                   side_effect=lambda text: messages.append(text) or True):
                self.assertEqual(notifier.main(self.scenario, []), exit_code)
            self.assertEqual(len(messages), 2)
            self.assertIn('MoFuSS started', messages[0])
            self.assertIn('MoFuSS completed' if exit_code == 0 else 'MoFuSS failed', messages[1])

    def test_shell_disabled_and_check_modes_forward_arguments_without_notifications(self):
        shell = self.scenario / 'run_linux.sh'; shell.write_bytes(SHELL.read_bytes())
        (self.scenario / 'run_linux.py').write_text(
            'import sys\nprint(repr(sys.argv[1:]))\nsys.exit(7)\n')
        env = dict(os.environ, MOFUSS_EGO='/bin/true', MOFUSS_R='/bin/true')
        for arguments, disabled in ((['--check'], False), (['--processors', '2'], True)):
            env['MOFUSS_TELEGRAM_MSGS'] = '0' if disabled else '1'
            result = subprocess.run(['bash', str(shell), *arguments], env=env,
                                    capture_output=True, text=True)
            self.assertEqual(result.returncode, 7, result.stderr)
            self.assertEqual(result.stderr, '')
            self.assertIn(str(self.scenario), result.stdout)
            self.assertIn(arguments[0], result.stdout)

    def test_test_command_sends_one_message_without_launching(self):
        self.credentials()
        messages = []
        with mock.patch.object(notifier.Telegram, 'send',
                               side_effect=lambda text: messages.append(text) or True):
            self.assertEqual(notifier.main(self.scenario, ['--telegram-test']), 0)
        self.assertEqual(len(messages), 1)
        self.assertIn('No simulation was started', messages[0])

    def test_interrupt_reaches_child_and_returns_130(self):
        import threading
        self.credentials()
        (self.scenario / 'run_linux.py').write_text(
            'import signal, sys, time\n'
            'signal.signal(signal.SIGINT, lambda *args: sys.exit(130))\n'
            'time.sleep(10)\n')
        messages = []
        timer = threading.Timer(0.2, lambda: os.kill(os.getpid(), signal.SIGINT))
        timer.start()
        try:
            with mock.patch.object(notifier.Telegram, 'send',
                                   side_effect=lambda text: messages.append(text) or True):
                self.assertEqual(notifier.main(self.scenario, []), 130)
        finally:
            timer.cancel()
        self.assertIn('MoFuSS stopped', messages[-1])


if __name__ == '__main__':
    unittest.main()
