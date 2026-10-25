/**
  @file test_util_parsescaproc.sas
  @brief Regression tests for %util_parseScaproc and %util_scaText2graphvizDot.
  @details Expects the global macro variables below to be set by the caller
  (tests/run_tests.py does this):
    g_codeRoot  - folder holding the macro files
    g_testRoot  - writable scratch folder for recordings and .dot output
  Each assertion logs one TEST_PASS:/TEST_FAIL: line.
**/

%include "&g_codeRoot/util_initscaproc.sas";
%include "&g_codeRoot/util_termscaproc.sas";
%include "&g_codeRoot/util_parsescaproc.sas";
%include "&g_codeRoot/util_scatext2graphvizdot.sas";
%include "&g_testRoot/test_framework.sas";

%t_cleanDir(p_dir=&g_testRoot)

%global g_enableScaProc g_logsRoot;
%let g_logsRoot = &g_testRoot;

/* ------------------------------------------------------------------
   Fixture 1: summary -> data step -> export -> import
------------------------------------------------------------------- */
%let g_enableScaProc = Y;
%util_initScaproc(p_enableFlagName=g_enableScaProc, p_outFilePath=%str(&g_testRoot), p_outFileName=sca__basic.txt)

PROC SUMMARY DATA=sashelp.prdsale NWAY;
  CLASS year country;
  VAR actual;
  OUTPUT OUT=work.agg(DROP=_:) SUM=;
RUN;
DATA work.usa;
  SET work.agg(WHERE=(country='U.S.A.'));
RUN;
PROC EXPORT DATA=work.usa OUTFILE="&g_testRoot/usa.csv" DBMS=CSV REPLACE; RUN;
PROC IMPORT DATAFILE="&g_testRoot/usa.csv" OUT=work.usa_in DBMS=CSV REPLACE; RUN;

%util_termScaproc(p_enableFlagName=g_enableScaProc)

/* ------------------------------------------------------------------
   Fixture 2: one step reading 40 data sets (edge-list truncation)
------------------------------------------------------------------- */
%macro t_makeMany(p_n=40);
  %local i;
  DATA
  %do i=1 %to &p_n;
    work.t_&i
  %end;
  ;
    x=1;
  %do i=1 %to &p_n;
    output work.t_&i;
  %end;
  RUN;
  DATA work.t_all;
    SET
  %do i=1 %to &p_n;
      work.t_&i
  %end;
    ;
  RUN;
%mend t_makeMany;

%util_initScaproc(p_enableFlagName=g_enableScaProc, p_outFilePath=%str(&g_testRoot), p_outFileName=sca__txt_many.v2.txt)
%t_makeMany(p_n=40)
%util_termScaproc(p_enableFlagName=g_enableScaProc)

%let g_enableScaProc = N;

/* ------------------------------------------------------------------
   T1: defaults (attrs=Y, steps=Y, blank rankDir)
------------------------------------------------------------------- */
%util_parseScaproc(p_inScaprocFileName=%str(&g_testRoot/sca__basic.txt)
, p_outDotFilePath=%str(&g_testRoot), p_outDotFileName=t1.dot, p_rankDir=)

%t_assert(p_name=T1 return code is 0, p_cond=&g_parseScaprocRC = 0)
%t_assert(p_name=T1 dot file created, p_cond=%sysfunc(fileexist(&g_testRoot/t1.dot)) = 1)
%t_assert(p_name=T1 default output data set work.mydigraph exists, p_cond=%sysfunc(exist(work.mydigraph)) = 1)
%t_countRegex(p_file=&g_testRoot/t1.dot, p_regex=^digraph dag, p_outVar=c)
%t_assert(p_name=T1 has digraph header, p_cond=&c = 1)
%t_countRegex(p_file=&g_testRoot/t1.dot, p_regex=^rankdir=LR, p_outVar=c)
%t_assert(p_name=T1 attrs=Y forces rankdir LR, p_cond=&c = 1)
%t_countRegex(p_file=&g_testRoot/t1.dot, p_regex=SASHELP\.PRDSALE, p_outVar=c)
%t_assert(p_name=T1 mentions SASHELP.PRDSALE, p_cond=&c ge 2)
%t_countRegex(p_file=&g_testRoot/t1.dot, p_regex=shape=\x22oval\x22, p_outVar=c)
%t_assert(p_name=T1 has step ovals (summary/data/export/import), p_cond=&c ge 3)
%t_countRegex(p_file=&g_testRoot/t1.dot, p_regex=_EXPORT\x22, p_outVar=c)
%t_assert(p_name=T1 export step present, p_cond=&c ge 1)
%t_countRegex(p_file=&g_testRoot/t1.dot, p_regex=usa\.csv, p_outVar=c)
%t_assert(p_name=T1 external file usa.csv present, p_cond=&c ge 2)
%t_countRegex(p_file=&g_testRoot/t1.dot, p_regex=^\x3C\x2FTABLE>>, p_outVar=c)
%t_assert(p_name=T1 every table node is closed, p_cond=&c ge 3)

/* No leakage of internals */
%t_assert(p_name=T1 no leaked format $unqEntity, p_cond=%sysfunc(cexist(work.formats.unqentity.formatc)) = 0)
%t_assert(p_name=T1 no leaked format stepname, p_cond=%sysfunc(cexist(work.formats.stepname.format)) = 0)
%t_assert(p_name=T1 loop counter l_ei is not global, p_cond=%symexist(l_ei) = 0)
%t_assert(p_name=T1 work tables cleaned, p_cond=%sysfunc(exist(work.step_inputs)) = 0 and %sysfunc(exist(work.in_out_info_flow)) = 0)

/* ------------------------------------------------------------------
   T2: includeSteps=N (previously failed on missing stepname. format)
------------------------------------------------------------------- */
%util_parseScaproc(p_inScaprocFileName=%str(&g_testRoot/sca__basic.txt)
, p_outDotFilePath=%str(&g_testRoot), p_outDotFileName=t2.dot, p_outDsName=work.t2
, p_includeAttrs_yn=N, p_includeSteps_yn=N, p_rankDir=TB)

%t_assert(p_name=T2 return code is 0, p_cond=&g_parseScaprocRC = 0)
%t_assert(p_name=T2 dot file created, p_cond=%sysfunc(fileexist(&g_testRoot/t2.dot)) = 1)
%t_assert(p_name=T2 explicit p_outDsName honoured, p_cond=%sysfunc(exist(work.t2)) = 1)
%t_countRegex(p_file=&g_testRoot/t2.dot, p_regex=shape=\x22oval\x22, p_outVar=c)
%t_assert(p_name=T2 no step ovals, p_cond=&c = 0)
%t_countRegex(p_file=&g_testRoot/t2.dot, p_regex=^rankdir=TB, p_outVar=c)
%t_assert(p_name=T2 rankdir TB honoured when attrs=N, p_cond=&c = 1)
%t_countRegex(p_file=&g_testRoot/t2.dot, p_regex=\} -> \{, p_outVar=c)
%t_assert(p_name=T2 has data-flow edges, p_cond=&c ge 1)

/* ------------------------------------------------------------------
   T3: attrs=N, steps=Y, invalid rankDir falls back to TB
------------------------------------------------------------------- */
%util_parseScaproc(p_inScaprocFileName=%str(&g_testRoot/sca__basic.txt)
, p_outDotFilePath=%str(&g_testRoot), p_outDotFileName=t3.dot
, p_includeAttrs_yn=N, p_includeSteps_yn=Y, p_rankDir=XX)

%t_countRegex(p_file=&g_testRoot/t3.dot, p_regex=^rankdir=TB, p_outVar=c)
%t_assert(p_name=T3 invalid rankDir falls back to TB, p_cond=&c = 1)
%t_countRegex(p_file=&g_testRoot/t3.dot, p_regex=shape=\x22oval\x22, p_outVar=c)
%t_assert(p_name=T3 steps present, p_cond=&c ge 3)

/* ------------------------------------------------------------------
   T4: a step reading 40 data sets keeps all 40 inputs
------------------------------------------------------------------- */
%util_parseScaproc(p_inScaprocFileName=%str(&g_testRoot/sca__txt_many.v2.txt)
, p_outDotFilePath=%str(&g_testRoot), p_outDotFileName=t4.dot
, p_includeAttrs_yn=N, p_includeSteps_yn=Y)

%t_countRegex(p_file=&g_testRoot/t4.dot, p_regex=\x22WORK\.T_\d+_, p_outVar=c)
/* 40 node definitions + 40 references on the edge line, for the set step alone */
%t_assert(p_name=T4 all 40 inputs referenced (no truncation), p_cond=&c ge 80)

/* ------------------------------------------------------------------
   T5: bad output folder / bad input
------------------------------------------------------------------- */
%util_parseScaproc(p_inScaprocFileName=%str(&g_testRoot/sca__basic.txt)
, p_outDotFilePath=%str(&g_testRoot/does_not_exist), p_outDotFileName=t5.dot)
%t_assert(p_name=T5 missing output folder returns rc 3, p_cond=&g_parseScaprocRC = 3)

%util_parseScaproc(p_inScaprocFileName=%str(&g_testRoot/missing.txt)
, p_outDotFilePath=%str(&g_testRoot), p_outDotFileName=t5b.dot)
%t_assert(p_name=T5 missing input file returns rc 2, p_cond=&g_parseScaprocRC = 2)
%t_assert(p_name=T5 no dot file written on error, p_cond=%sysfunc(fileexist(&g_testRoot/t5b.dot)) = 0)

%util_parseScaproc(p_inScaprocFileName=, p_outDotFilePath=%str(&g_testRoot))
%t_assert(p_name=T5 blank input returns rc 1, p_cond=&g_parseScaprocRC = 1)

/* ------------------------------------------------------------------
   T6: mass conversion names the output after the last extension only
   (file name contains the text "txt" and an extra dot)
------------------------------------------------------------------- */
FILENAME tlist "&g_testRoot/list.txt";
DATA _NULL_;
  FILE tlist;
  PUT "&g_testRoot/sca__txt_many.v2.txt";
RUN;
FILENAME tlist CLEAR;

%util_scaText2graphvizDot(p_inTxtFileList=&g_testRoot/list.txt
, p_outDotFilesDir=&g_testRoot, p_includeAttrs_yn=N)

%t_assert(p_name=T6 output named sca__txt_many.v2.dot, p_cond=%sysfunc(fileexist(&g_testRoot/sca__txt_many.v2.dot)) = 1)
%t_assert(p_name=T6 wrong name sca__dot_many.v2.dot not created, p_cond=%sysfunc(fileexist(&g_testRoot/sca__dot_many.v2.dot)) = 0)
%t_assert(p_name=T6 per-file work datasets removed, p_cond=%sysfunc(exist(work.dotfile_0001)) = 0)

/* ------------------------------------------------------------------
   T7: with recording enabled, the parser's own recording is not matched
   by the sca__*.txt glob, and SCAPROC is terminated (file is written)
------------------------------------------------------------------- */
%let g_enableScaProc = Y;
%util_parseScaproc(p_inScaprocFileName=%str(&g_testRoot/sca__basic.txt)
, p_outDotFilePath=%str(&g_testRoot), p_outDotFileName=t7.dot)
%let g_enableScaProc = N;
%t_assert(p_name=T7 self-recording written as scaparse__*, p_cond=%sysfunc(fileexist(&g_testRoot/scaparse__util_parseScaproc.txt)) = 1)
%t_assert(p_name=T7 no sca__util_parseScaproc.txt (glob safe), p_cond=%sysfunc(fileexist(&g_testRoot/sca__util_parseScaproc.txt)) = 0)

/* ------------------------------------------------------------------
   T8: a malformed ATTR line is skipped, not an ERROR (run-level ERROR
   lines fail the suite, so completing cleanly is the assertion)
------------------------------------------------------------------- */
DATA _NULL_;
  INFILE "&g_testRoot/sca__basic.txt" LRECL=1000 TRUNCOVER END=eof;
  FILE "&g_testRoot/sca__malformed.txt" LRECL=1000;
  INPUT;
  RETAIN done 0;
  if (NOT done AND INDEX(_INFILE_,'ATTR') AND INDEX(_INFILE_,'TYPE:')) then
  do;
    _INFILE_ = TRANWRD(_INFILE_,'TYPE:','TYPX:');
    done = 1;
  end;
  PUT _INFILE_;
RUN;

%util_parseScaproc(p_inScaprocFileName=%str(&g_testRoot/sca__malformed.txt)
, p_outDotFilePath=%str(&g_testRoot), p_outDotFileName=t8.dot)
%t_assert(p_name=T8 malformed ATTR line tolerated (rc 0), p_cond=&g_parseScaprocRC = 0)
%t_assert(p_name=T8 dot file still produced, p_cond=%sysfunc(fileexist(&g_testRoot/t8.dot)) = 1)

/* ------------------------------------------------------------------
   T9: JSON graph (attrs=Y, steps=Y) is valid JSON with consistent nodes/edges
------------------------------------------------------------------- */
%util_parseScaproc(p_inScaprocFileName=%str(&g_testRoot/sca__basic.txt)
, p_outDotFilePath=%str(&g_testRoot), p_outDotFileName=t9.dot, p_outJsonFileName=t9.json
, p_includeAttrs_yn=Y, p_includeSteps_yn=Y)

%t_assert(p_name=T9 json file created, p_cond=%sysfunc(fileexist(&g_testRoot/t9.json)) = 1)
%t_assert(p_name=T9 dot file still created, p_cond=%sysfunc(fileexist(&g_testRoot/t9.dot)) = 1)

LIBNAME tj JSON "&g_testRoot/t9.json";   /* the JSON engine fails on invalid JSON */
PROC SQL NOPRINT;
  SELECT COUNT(*) INTO :n_nodes   TRIMMED FROM tj.nodes;
  SELECT COUNT(*) INTO :n_steps   TRIMMED FROM tj.nodes WHERE type = 'step';
  SELECT COUNT(*) INTO :n_edges   TRIMMED FROM tj.edges;
  SELECT COUNT(*) INTO :n_cols    TRIMMED FROM tj.nodes_columns;
  SELECT COUNT(*) INTO :n_dangle  TRIMMED FROM tj.edges
    WHERE source NOT IN (SELECT id FROM tj.nodes) OR target NOT IN (SELECT id FROM tj.nodes);
  SELECT COUNT(*) INTO :n_filecol TRIMMED FROM tj.nodes_columns AS c INNER JOIN tj.nodes AS n
    ON c.ordinal_nodes = n.ordinal_nodes WHERE n.type = 'file';
  SELECT COUNT(*) INTO :n_dirty   TRIMMED FROM tj.edges WHERE type NOT IN ('input','output');
QUIT;
LIBNAME tj CLEAR;

%t_assert(p_name=T9 json has data set/file/step nodes, p_cond=&n_nodes ge 7)
%t_assert(p_name=T9 json has step nodes, p_cond=&n_steps ge 3)
%t_assert(p_name=T9 json has edges, p_cond=&n_edges ge 6)
%t_assert(p_name=T9 json has column lists, p_cond=&n_cols ge 5)
%t_assert(p_name=T9 every edge endpoint is a node, p_cond=&n_dangle = 0)
%t_assert(p_name=T9 file nodes carry no columns, p_cond=&n_filecol = 0)
%t_assert(p_name=T9 edge types are input/output, p_cond=&n_dirty = 0)
%t_countRegex(p_file=&g_testRoot/t9.json, p_regex=\x22directed\x22: true, p_outVar=c)
%t_assert(p_name=T9 json marks graph as directed, p_cond=&c = 1)

/* ------------------------------------------------------------------
   T10: JSON without steps/attributes links inputs straight to outputs
------------------------------------------------------------------- */
%util_parseScaproc(p_inScaprocFileName=%str(&g_testRoot/sca__basic.txt)
, p_outDotFilePath=%str(&g_testRoot), p_outDotFileName=t10.dot, p_outJsonFileName=t10.json
, p_includeAttrs_yn=N, p_includeSteps_yn=N)

LIBNAME tj JSON "&g_testRoot/t10.json";
PROC SQL NOPRINT;
  SELECT COUNT(*) INTO :n_steps TRIMMED FROM tj.nodes WHERE type = 'step';
  SELECT COUNT(*) INTO :n_flow  TRIMMED FROM tj.edges WHERE type = 'flow';
  SELECT COUNT(*) INTO :n_edges TRIMMED FROM tj.edges;
QUIT;
LIBNAME tj CLEAR;
%t_assert(p_name=T10 no step nodes, p_cond=&n_steps = 0)
%t_assert(p_name=T10 all edges are flow edges, p_cond=&n_flow ge 2 and &n_flow = &n_edges)
%t_countRegex(p_file=&g_testRoot/t10.json, p_regex=\x22columns\x22, p_outVar=c)
%t_assert(p_name=T10 no column lists when attrs=N, p_cond=&c = 0)

/* ------------------------------------------------------------------
   T11: mass conversion writes a .json next to each .dot on request
------------------------------------------------------------------- */
%util_scaText2graphvizDot(p_inTxtFileList=&g_testRoot/list.txt
, p_outDotFilesDir=&g_testRoot, p_includeAttrs_yn=N, p_outJson_yn=Y)
%t_assert(p_name=T11 json named after the input file, p_cond=%sysfunc(fileexist(&g_testRoot/sca__txt_many.v2.json)) = 1)

/* ------------------------------------------------------------------
   T12: JSON only (p_outDot_yn=N): no .dot file, no DOT data set, valid JSON
------------------------------------------------------------------- */
%util_parseScaproc(p_inScaprocFileName=%str(&g_testRoot/sca__basic.txt)
, p_outDotFilePath=%str(&g_testRoot), p_outDotFileName=t12.dot, p_outDsName=work.t12
, p_outJsonFileName=t12.json, p_outDot_yn=N)

%t_assert(p_name=T12 return code is 0, p_cond=&g_parseScaprocRC = 0)
%t_assert(p_name=T12 no dot file written, p_cond=%sysfunc(fileexist(&g_testRoot/t12.dot)) = 0)
%t_assert(p_name=T12 no DOT data set created, p_cond=%sysfunc(exist(work.t12)) = 0)
%t_assert(p_name=T12 json file written, p_cond=%sysfunc(fileexist(&g_testRoot/t12.json)) = 1)
LIBNAME tj JSON "&g_testRoot/t12.json";
PROC SQL NOPRINT;
  SELECT COUNT(*) INTO :n_nodes TRIMMED FROM tj.nodes;
  SELECT COUNT(*) INTO :n_edges TRIMMED FROM tj.edges;
QUIT;
LIBNAME tj CLEAR;
%t_assert(p_name=T12 json has nodes and edges, p_cond=&n_nodes ge 7 and &n_edges ge 6)
%t_assert(p_name=T12 work tables cleaned, p_cond=%sysfunc(exist(work.pscp_json_nodes)) = 0 and %sysfunc(exist(work.in_out_info_flow)) = 0)

/* T13: nothing to write is a validation error */
%util_parseScaproc(p_inScaprocFileName=%str(&g_testRoot/sca__basic.txt)
, p_outDotFilePath=%str(&g_testRoot), p_outDotFileName=t13.dot, p_outDot_yn=N)
%t_assert(p_name=T13 p_outDot_yn=N without json returns rc 4, p_cond=&g_parseScaprocRC = 4)

/* T14: mass conversion, JSON only */
%let g_rc = %sysfunc(dcreate(jsononly,&g_testRoot));
%t_cleanDir(p_dir=&g_testRoot/jsononly)
%util_scaText2graphvizDot(p_inTxtFileList=&g_testRoot/list.txt
, p_outDotFilesDir=&g_testRoot/jsononly, p_includeAttrs_yn=N, p_outJson_yn=Y, p_outDot_yn=N)
%t_assert(p_name=T14 json written, p_cond=%sysfunc(fileexist(&g_testRoot/jsononly/sca__txt_many.v2.json)) = 1)
%t_assert(p_name=T14 no dot written, p_cond=%sysfunc(fileexist(&g_testRoot/jsononly/sca__txt_many.v2.dot)) = 0)

%put TEST_DONE;
