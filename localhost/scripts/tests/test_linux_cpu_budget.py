"""Ensure automatic workers respect physical topology and CPU allocations."""
from pathlib import Path
import sys
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from run_linux import cpu_capacity


class CpuBudgetTest(unittest.TestCase):
    def capacity(self, affinity=range(8), quota='max 100000', scheduler=None):
        files = {'/proc/self/cgroup': '0::/user.slice/job\n',
                 '/sys/fs/cgroup/cpu.max': 'max 100000',
                 '/sys/fs/cgroup/user.slice/cpu.max': quota,
                 '/sys/fs/cgroup/user.slice/job/cpu.max': 'max 100000'}
        for cpu in affinity:
            base = f'/sys/devices/system/cpu/cpu{cpu}/topology/'
            files[base + 'physical_package_id'] = '0'
            files[base + 'core_id'] = str(cpu % 4)
        def read(path, *args, **kwargs):
            if str(path) not in files: raise FileNotFoundError(str(path))
            return files[str(path)]
        env = {} if scheduler is None else {'SLURM_CPUS_PER_TASK': str(scheduler)}
        with patch('run_linux.os.sched_getaffinity', return_value=set(affinity)), \
             patch('run_linux.Path.read_text', read), \
             patch('run_linux.Path.is_file', lambda p: str(p) in files), \
             patch.dict('run_linux.os.environ', env, clear=True):
            return cpu_capacity()

    def test_hyperthreads_do_not_double_default_workers(self):
        result = self.capacity()
        self.assertEqual((result['physical_cores'], result['logical_cpus'], result['auto_workers']), (4, 8, 4))

    def test_affinity_can_expose_two_threads_of_one_core(self):
        result = self.capacity(affinity=(0, 4))
        self.assertEqual((result['auto_workers'], result['allowed_workers']), (1, 2))

    def test_parent_cgroup_limits_workers(self):
        result = self.capacity(quota='250000 100000')
        self.assertEqual((result['auto_workers'], result['allowed_workers']), (2, 2))

    def test_scheduler_and_quota_take_the_tighter_limit(self):
        self.assertEqual(self.capacity(quota='300000 100000', scheduler=2)['auto_workers'], 2)
        self.assertEqual(self.capacity(quota='150000 100000', scheduler=4)['auto_workers'], 1)


if __name__ == '__main__':
    unittest.main()
