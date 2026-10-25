# Tests

Regression tests for `util_parseScaproc` and `util_scaText2graphvizDot`, written in SAS and run over SASPy.

    python tests/run_tests.py

- `sas/test_framework.sas` – `%t_assert`, `%t_countRegex`, `%t_cleanDir`
- `sas/test_util_parsescaproc.sas` – fixtures (real SCA recordings made at test time) and cases T1–T8
- `run_tests.py` – uploads the macros and tests to the SAS server, runs them, collects `TEST_PASS:` / `TEST_FAIL:` lines and fails on any unexpected `ERROR`/`WARNING` log line

Environment variables (all optional): `SASPY_HELPERS` (folder with `connect_saspy.py`), `SAS_TEST_DIR` (remote scratch folder), `SAS_MACRO_DIR` (macro folder to test, e.g. an older checkout).
