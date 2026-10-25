/**
  @file test_framework.sas
  @brief Minimal assertion helpers for the SCA parser tests.
  @details Every assertion prints exactly one line to the log, starting with
  TEST_PASS: or TEST_FAIL:, which tests/run_tests.py collects.
**/

/* Print a PASS/FAIL line. p_cond must be a macro-evaluable expression. */
%macro t_assert(p_name=, p_cond=);
  %if (&p_cond) %then %put TEST_PASS: &p_name;
  %else %put TEST_FAIL: &p_name (condition: &p_cond);
%mend t_assert;

/* Delete the files (not sub-folders) in a scratch folder, so stale output can't make a test pass or fail */
%macro t_cleanDir(p_dir=);
  DATA _NULL_;
    LENGTH fref f2 $8 fn $256;
    rc = FILENAME(fref, "&p_dir");
    did = DOPEN(fref);
    if (did > 0) then
    do;
      do i = 1 to DNUM(did);
        fn = DREAD(did, i);
        /* never delete the test programs themselves */
        if (LOWCASE(SCAN(fn,-1,'.')) = 'sas') then continue;
        rc = FILENAME(f2, CATX('/', "&p_dir", fn));
        if (rc = 0) then
        do;
          rc = FDELETE(f2);   /* folders are non-empty or fail silently here */
          rc = FILENAME(f2);
        end;
      end;
      rc = DCLOSE(did);
    end;
    rc = FILENAME(fref);
  RUN;
%mend t_cleanDir;

/* Count edge endpoints in a SIMPLE-theme .dot file that have no node definition line */
%macro t_dotDangling(p_file=, p_outVar=);
  %global &p_outVar;
  DATA _NULL_;
    INFILE "&p_file" LRECL=32767 END=eof;
    INPUT;
    LENGTH id $210;
    RETAIN rxNode rxEdge;
    if (_n_ = 1) then
    do;
      rxNode = PRXPARSE('/^"([^"]+)" \[/');
      rxEdge = PRXPARSE('/^"([^"]+)" -> "([^"]+)"$/');
      DECLARE HASH nodes();
      nodes.definekey('id');
      nodes.definedone();
      DECLARE HASH ends();
      ends.definekey('id');
      ends.definedone();
      DECLARE HITER ei('ends');
    end;
    if (PRXMATCH(rxNode, _INFILE_)) then
    do;
      id = PRXPOSN(rxNode, 1, _INFILE_);
      rc = nodes.replace();
    end;
    else if (PRXMATCH(rxEdge, _INFILE_)) then
    do;
      id = PRXPOSN(rxEdge, 1, _INFILE_); rc = ends.replace();
      id = PRXPOSN(rxEdge, 2, _INFILE_); rc = ends.replace();
    end;
    if (eof) then
    do;
      dangling = 0;
      rc = ei.first();
      do while (rc = 0);
        if (nodes.check() ne 0) then dangling + 1;
        rc = ei.next();
      end;
      /* -1 = no edge endpoints found at all, so a "0 dangling" result can't pass vacuously */
      CALL SYMPUTX("&p_outVar", IFN(ends.num_items = 0, -1, dangling));
    end;
  RUN;
%mend t_dotDangling;

/* Count regex matches (Perl syntax, no parentheses) across all lines of a file into a macro variable */
%macro t_countRegex(p_file=, p_regex=, p_outVar=);
  %global &p_outVar;
  %let &p_outVar = 0;
  %if (%sysfunc(fileexist(&p_file)) = 0) %then %return;
  DATA _NULL_;
    INFILE "&p_file" LRECL=32767 END=eof;
    INPUT;
    RETAIN n 0 rx;
    if (_n_ = 1) then rx = PRXPARSE("/&p_regex/");
    start = 1;
    stop  = LENGTHN(_INFILE_);
    if (stop > 0) then
    do;
      CALL PRXNEXT(rx, start, stop, _INFILE_, pos, len);
      do while (pos > 0);
        n + 1;
        CALL PRXNEXT(rx, start, stop, _INFILE_, pos, len);
      end;
    end;
    if (eof) then CALL SYMPUTX("&p_outVar", n);
  RUN;
%mend t_countRegex;

