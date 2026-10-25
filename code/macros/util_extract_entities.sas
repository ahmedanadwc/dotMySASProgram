/**
  @file util_extract_entities.sas
  @brief SUB-MACRO 2: extract_entities
  @details Extracted from util_parsescaproc.sas for modular reuse.
  @version 9.4
  @author Ahmed Al-Attar

**/

/* ============================================================
 * SUB-MACRO 2: extract_entities
 * ============================================================
 * Parses work.scadata and creates:
 *   - work.scadata1: filtered SCAPROC lines
 *   - work.step_inputs: input datasets/files per step
 *   - work.step_outputs: output datasets/files per step
 *   - work.step_runtimes: step start/end times (if includeSteps=Y)
 *   - work.step_names: step names (if includeSteps=Y)
 * ============================================================ */
%MACRO extract_entities(
  p_inScadataDs=work.scadata
, p_outScadata1Ds=work.scadata1
, p_outInputsDs=work.step_inputs
, p_outOutputsDs=work.step_outputs
, p_outRuntimesDs=work.step_runtimes
, p_outNamesDs=work.step_names
, p_includeSteps_yn=Y
, p_concatLibObsCount=0
, p_stepFmt=
) / minoperator;

  %LOCAL l_includeSteps_yn l_concatLibObsCount l_stepFmt;

  %let l_includeSteps_yn = %upcase(&p_includeSteps_yn);
  %let l_concatLibObsCount = &p_concatLibObsCount;
  %let l_stepFmt = &p_stepFmt;

  /* Identify Input & Output data sets and their variables */
  DATA &p_outScadata1Ds (KEEP=scaline sca_linenum step )
    &p_outInputsDs (KEEP=step sca_linenum dsname variable type filename entity_name) 
    &p_outOutputsDs (KEEP=step sca_linenum dsname variable type filename entity_name)
  %if (&l_includeSteps_yn = Y) %then %do; 
      &p_outRuntimesDs (KEEP=step step_start_dt ) 
      &p_outNamesDs(KEEP=step step_name type fmtname)
  %end;
    ;
  
  /* Ignoring SASTemp outputs, Global Macro Variables and UTILITY files */
  SET &p_inScadataDs(DROP=file_linenum WHERE=((index(scaline,"JOBSPLIT") > 0) 
  and (indexw(scaline,"SYMBOL")=0)
  and (indexw(scaline,"CATALOG")=0)
  and (index(scaline,".UTILITY")=0)
  and (index(scaline,'DATASET OUTPUT SEQ WORK.SORTTMP')=0)
  ));
  
  LENGTH
    dsname $50 
    variable $32 
    type $15 
    step_start_dt 8 
    filename $200
    step_name $30
    entity_name $210;
    
  FORMAT step_start_dt datetime22.2;
  filename='';
  
  /* PRX Patterns for SCAPROC line parsing - compiled once */
  IF _N_ = 1 THEN DO;
      /* CONCATMEM: JOBSPLIT: CONCATMEM #C00003 SASHELP */ 
      prx_concatmem = PRXPARSE("/JOBSPLIT:\s+CONCATMEM\s+#?(\S+)\s+(\S+)\s+\*/");
      
      /* ATTR: JOBSPLIT: ATTR LIB.DS VARIABLE:var TYPE:type LENGTH:len LABEL:lbl */
      prx_attr = PRXPARSE("/JOBSPLIT:\s+ATTR\s+(\S+)\s+VARIABLE:([^\s]+)\s+TYPE:([^\s]+)\s+LENGTH:([^\s]+)\s+LABEL:/");
      
      /* FILE: JOBSPLIT: FILE INPUT|OUTPUT SEQ|MULTI <filename> */
      prx_file = PRXPARSE("/JOBSPLIT:\s+FILE\s+(INPUT|OUTPUT)\s+(SEQ|MULTI)\s+(.+)/");
      
      /* DATASET: JOBSPLIT: DATASET INPUT|OUTPUT SEQ|MULTI LIB.DS */
      prx_dataset = PRXPARSE("/JOBSPLIT:\s+DATASET\s+(INPUT|OUTPUT)\s+(SEQ|MULTI)\s+(\S+)/");
      
      /* TASKSTARTTIME/JOBENDTIME: JOBSPLIT: TASKSTARTTIME|JOBENDTIME <datetime> */
      prx_time = PRXPARSE("/JOBSPLIT:\s+(TASKSTARTTIME|JOBENDTIME)\s+(\S+\s+\S+)/");
      
      /* PROCNAME: JOBSPLIT: PROCNAME <procname> */
      prx_procname = PRXPARSE("/JOBSPLIT:\s+PROCNAME\s+(\S+)/");
  END;
  
  OUTPUT &p_outScadata1Ds;

  %if (&l_includeSteps_yn = Y) %then %do; 
    /* Parse TASKSTARTTIME / JOBENDTIME for step runtimes */
    IF PRXMATCH(prx_time, scaline) THEN DO;
        step_start_dt = INPUT(PRXPOSN(prx_time, 2, scaline), datetime22.2);
        OUTPUT &p_outRuntimesDs;
    END;
    ELSE IF PRXMATCH(prx_procname, scaline) THEN DO;
        step_name = PRXPOSN(prx_procname, 1, scaline);
        type = 'N';
        fmtname = 'stepName';
        OUTPUT &p_outNamesDs;
    END;
    ELSE 
  %end; /* End - (&l_includeSteps_yn = Y) */ 
   /* Parse ATTR, FILE, DATASET lines */
   IF PRXMATCH(prx_attr, scaline) THEN DO;
      /* ATTR line: extract dsname, variable, type, length */
      dsname = PRXPOSN(prx_attr, 1, scaline);
      dsname = CATX('.', SCAN(dsname, 1, '.'), SCAN(dsname, 2, '.'));
      %if (&l_concatLibObsCount GT 0) %then %do;
          dsname = CATX('.', PUT(SCAN(dsname, 1, '.'), $cncatlibs.), SCAN(dsname, 2, '.'));
      %end;
      
      variable = PRXPOSN(prx_attr, 2, scaline);
      type_raw = PRXPOSN(prx_attr, 3, scaline);
      length_val = PRXPOSN(prx_attr, 4, scaline);
      
      in_flag = INDEXW(scaline, "INPUT");
      
      /* Normalize type: CHARACTER -> char, NUM -> num */
      type = CATX(' ', IFC(type_raw='CHARACTER','char','num'), CATS('(', length_val, ')'));
      
      entity_name = IFC(COALESCEC(dsname, filename)='', '',
                   CATS(IFC(in_flag > 0, 'i', 'o'), '_', PUT(step, &l_stepFmt.), '_', COALESCEC(dsname, filename)));
      
      IF (in_flag > 0) THEN OUTPUT &p_outInputsDs;
      ELSE OUTPUT &p_outOutputsDs;
  END;
  ELSE IF PRXMATCH(prx_file, scaline) THEN DO;
      /* FILE line: extract in/out flag and filename */
      in_out = PRXPOSN(prx_file, 1, scaline);  /* INPUT or OUTPUT */
      filename = STRIP(PRXPOSN(prx_file, 3, scaline));
      in_flag = (in_out = 'INPUT');
      
      entity_name = IFC(COALESCEC(dsname, filename)='', '',
                   CATS(IFC(in_flag, 'i', 'o'), '_', PUT(step, &l_stepFmt.), '_', filename));
      
      IF (in_flag) THEN OUTPUT &p_outInputsDs;
      ELSE OUTPUT &p_outOutputsDs;
  END;
  ELSE IF PRXMATCH(prx_dataset, scaline) THEN DO;
      /* DATASET line: extract in/out flag and dsname */
      in_out = PRXPOSN(prx_dataset, 1, scaline);  /* INPUT or OUTPUT */
      dsname = PRXPOSN(prx_dataset, 3, scaline);
      dsname = CATX('.', SCAN(dsname, 1, '.'), SCAN(dsname, 2, '.'));
      %if (&l_concatLibObsCount GT 0) %then %do;
          dsname = CATX('.', PUT(SCAN(dsname, 1, '.'), $cncatlibs.), SCAN(dsname, 2, '.'));
      %end;
      in_flag = (in_out = 'INPUT');
      
      entity_name = IFC(COALESCEC(dsname, filename)='', '',
                   CATS(IFC(in_flag, 'i', 'o'), '_', PUT(step, &l_stepFmt.), '_', COALESCEC(dsname, filename)));
      
      IF (in_flag) THEN OUTPUT &p_outInputsDs;
      ELSE OUTPUT &p_outOutputsDs;
  END;
RUN;

%MEND extract_entities;