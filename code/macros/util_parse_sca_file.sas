/**
  @file util_parse_sca_file.sas
  @brief SUB-MACRO 1: parse_sca_file
  @details Extracted from util_parsescaproc.sas for modular reuse.
  @version 9.4
  @author Ahmed Al-Attar

**/

/* ============================================================
 * SUB-MACRO 1: parse_sca_file
 * ============================================================
 * Reads SCAPROC output file and creates:
 *   - work.scadata: raw SCAPROC lines with step numbers
 *   - work.sca_libs: concatenated library mappings (if any)
 * ============================================================ */
%MACRO parse_sca_file(
  p_inScaprocFileName=
, p_outScadataDs=work.scadata
, p_outLibsDs=work.sca_libs
) / minoperator;

  %LOCAL l_inScaprocFileName l_concatLibObsCount;

  /* Validate input file */
  %if (%superq(p_inScaprocFileName) EQ ) %then %do;
    %put ERROR: parse_sca_file: p_inScaprocFileName is required;
    %return;
  %end;

  %let l_inScaprocFileName = %sysfunc(translate(&p_inScaprocFileName,%str(/),%str(\)));
  %if (%sysfunc(fileexist(&l_inScaprocFileName)) = 0) %then %do;
    %put ERROR: parse_sca_file: File not found: &l_inScaprocFileName;
    %return;
  %end;

  /* Read the SCA text file */
  DATA &p_outScadataDs(DROP=rc);
    INFILE "&l_inScaprocFileName" LRECL=32767 LENGTH=linelength truncover END=eof;
    LENGTH scaline $32767 file_linenum sca_linenum step 5 libref libname $8;
    LABEL 
      scaline = 'SCA line'
      file_linenum = 'SCA file line number' 
      sca_linenum = 'SCA non-empty line number'
      step = 'Step Counter'
    ;

    if(_n_=1) then
    do;
     dcl hash libHash(ordered:'a');
     libHash.definekey('libref');
     libHash.definekey('libname');
     libHash.definedone();
    end;
    
    INPUT @;
    INPUT scaline $varying32767. linelength;
    file_linenum+1;

    if (length(strip(scaline)) gt 1) then
    do;
      sca_linenum+1;
      if (_n_=1 OR index(scaline,'STEP SOURCE FOLLOWS')) then step+1 ;
      OUTPUT;
      
      /* JOBSPLIT: CONCATMEM #C00003 SASHELP */
      /* Use PRX for robust parsing */
      IF _N_ = 1 THEN prx_concatmem = PRXPARSE("/JOBSPLIT:\s+CONCATMEM\s+#?(\S+)\s+(\S+)\s+\*/");
      IF PRXMATCH(prx_concatmem, scaline) THEN DO;
        libref = PRXPOSN(prx_concatmem, 1, scaline);
        libname = PRXPOSN(prx_concatmem, 2, scaline);
        if (libref ne '' and libname ne '') then rc = libHash.add();
      end; /* End - PRXMATCH CONCATMEM */
    end; /* End - (length(strip(scaline)) gt 1) */

    /* Export/write the hash table */
    if (eof) then
    do;
      libHash.output(dataset: '&p_outLibsDs');
    end;
  RUN;

  /* Find Concatenated Libraries */
  DATA _null_;
    if (0) then SET &p_outLibsDs nobs=nobs;
    call symputx("l_concatLibObsCount",nobs);
  RUN;

  %if (&l_concatLibObsCount GT 0) %then %do;
  DATA &p_outLibsDs;
    SET &p_outLibsDs(RENAME=(libref=start libname=label)) ;
    RETAIN fmtname '$cncatlibs' type 'C';
  RUN;
  PROC FORMAT cntlin=&p_outLibsDs; RUN;
  %end;
  PROC DELETE DATA=&p_outLibsDs; RUN;

%MEND parse_sca_file;