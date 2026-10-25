#!/usr/bin/env python3
"""
Run the SAS regression tests for the SCA parser macros over SASPy.

Usage:  python tests/run_tests.py

Environment:
  SASPY_HELPERS  folder containing connect_saspy.py
                 (default: D:\\Projects\\Python\\saspy_helpers)
  SAS_TEST_DIR   remote scratch folder (default: ~/dotmysasprogram_tests)

Exit code: 0 when every assertion passes, 1 otherwise.
"""
import os
import re
import sys
from pathlib import Path

HELPERS = os.environ.get("SASPY_HELPERS", r"D:\Projects\Python\saspy_helpers")
sys.path.insert(0, HELPERS)
import connect_saspy  # noqa: E402  (auto-connects on import)

ROOT = Path(__file__).resolve().parent.parent
MACROS = Path(os.environ.get("SAS_MACRO_DIR", ROOT / "code" / "macros"))
TESTS = ROOT / "tests" / "sas"

MACRO_FILES = [
    "util_initscaproc.sas",
    "util_termscaproc.sas",
    "util_parsescaproc.sas",
    "util_scatext2graphvizdot.sas",
]
TEST_FILES = ["test_framework.sas", "test_util_parsescaproc.sas"]


def main() -> int:
    sas = connect_saspy.get_connection()._sas_session
    if sas is None:
        print("Could not connect to SAS", file=sys.stderr)
        return 1

    home = re.search(r"^HOME=([^\s;]+)", sas.submit("%put HOME=%sysfunc(sysget(HOME));")["LOG"], re.M).group(1)
    remote = os.environ.get("SAS_TEST_DIR", f"{home}/dotmysasprogram_tests")
    remote_code = f"{remote}/code"

    sas.submit(
        f"options dlcreatedir; libname _t '{remote}'; libname _t clear;"
        f"libname _t '{remote_code}'; libname _t clear;"
    )
    for name in MACRO_FILES:
        sas.upload(str(MACROS / name), f"{remote_code}/{name}", overwrite=True)
    for name in TEST_FILES:
        sas.upload(str(TESTS / name), f"{remote}/{name}", overwrite=True)

    log = sas.submit(
        f"%let g_codeRoot={remote_code}; %let g_testRoot={remote};\n"
        f'%include "{remote}/test_util_parsescaproc.sas";'
    )["LOG"]

    passed = re.findall(r"^TEST_PASS: (.+)$", log, re.M)
    failed = re.findall(r"^TEST_FAIL: (.+)$", log, re.M)
    # T3 deliberately passes an invalid p_rankDir and expects this one warning
    # (T3 passes an invalid p_rankDir, T16 an invalid p_dotTheme)
    expected = re.compile(r"WARNING: Acceptable p_(rankDir|dotTheme) values")
    problems = [
        l for l in log.splitlines()
        if re.match(r"(ERROR|WARNING)\b", l) and not expected.search(l)
    ]
    done = "TEST_DONE" in log

    for name in passed:
        print(f"PASS  {name}")
    for name in failed:
        print(f"FAIL  {name}")
    for line in list(dict.fromkeys(problems))[:15]:
        print(f"LOG   {line}")
    print(f"\n{len(passed)} passed, {len(failed)} failed, {len(problems)} ERROR/WARNING log lines")
    if not done:
        print("Test program did not reach TEST_DONE", file=sys.stderr)

    connect_saspy.close_connection()
    return 0 if (done and not failed and not problems) else 1


if __name__ == "__main__":
    sys.exit(main())
