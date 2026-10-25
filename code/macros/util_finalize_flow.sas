/**
  @file util_finalize_flow.sas
  @brief SUB-MACRO 5: finalize_flow
  @details Extracted from util_parsescaproc.sas for modular reuse.
  @version 9.4
  @author Ahmed Al-Attar

**/

/* ============================================================
 * SUB-MACRO 5: finalize_flow
 * ============================================================
 * Finalizes flow data and calculates runtimes
 * ============================================================ */
%MACRO finalize_flow(
  p_inStepsFlowDs=work.steps_in_out_flow
, p_inRuntimesDs=work.step_runtimes
, p_inNamesDs=work.step_names
, p_outSingleLineDs=work.single_line_dag
, p_outNames2Ds=work.step_names2
, p_includeSteps_yn=Y
, p_stepFmt=
, p_stepsCount=
) / minoperator;

  %LOCAL l_includeSteps_yn l_stepFmt l_stepsCount;
  %let l_includeSteps_yn = %upcase(&p_includeSteps_yn);
  %let l_stepFmt = &p_stepFmt;

  /* Clean-up intermediate datasets */
  PROC DATASETS LIB=WORK NOLIST;
   DELETE scadata: step_inputs step_outputs; 
  QUIT; RUN;

  /* Reduce Multi-line step into a single line for each step */
  PROC SORT DATA=&p_inStepsFlowDs(KEEP=step step_name 'in'n 'out'n in_entity_name out_entity_name unq_in_entity_name unq_out_entity_name);
    BY step step_name 'in'n 'out'n in_entity_name out_entity_name unq_in_entity_name unq_out_entity_name;
  RUN;

  DATA &p_outSingleLineDs (KEEP=step step_name ins outs in_entities out_entities unq_in_entities unq_out_entities);
    if (0) then SET &p_inStepsFlowDs;
    LENGTH 
      ins outs $4000 
      in_entities out_entities unq_in_entities unq_out_entities $8000;

    DO UNTIL (last.step);
      SET &p_inStepsFlowDs;
      BY step;
      ins          = CATX(' ',ins,STRIP(in));
      outs         = CATX(' ',outs,STRIP(out));
      in_entities  = CATX(' ',in_entities,STRIP(in_entity_name));
      out_entities = CATX(' ',out_entities,STRIP(out_entity_name));
      unq_in_entities  = CATX(' ',unq_in_entities,STRIP(unq_in_entity_name));
      unq_out_entities = CATX(' ',unq_out_entities,STRIP(unq_out_entity_name));
    END;
    OUTPUT;
  RUN;

  PROC DELETE DATA=&p_inStepsFlowDs; RUN;

  %if (&l_includeSteps_yn = Y) %then %do;
    /* Calculate steps run-time duration */
    DATA work.step_runtimes2;
     if (eof=0) then set &p_inRuntimesDs(firstobs=2 keep=step_start_dt rename=(step_start_dt=step_end_dt)) end=eof;
     else step_end_dt=.;
     
     SET &p_inRuntimesDs;
    LENGTH runTime_duration 8;
    FORMAT runTime_duration time12.2;

    if (step_end_dt > 0) then
      runTime_duration = step_end_dt - step_start_dt;
    if (runTime_duration  ne .);
    RUN;

    /* Join the names with their duration run-times */
    PROC SQL;
     CREATE TABLE &p_outNames2Ds AS
     SELECT 
     a.step
      , a.fmtname
      , CAT('_',PUT(a.step,&l_stepFmt.),'_',STRIP(a.step_name),' (',STRIP(PUT(b.runtime_duration,time12.2)),')') as step_name LENGTH=30
      , a.type
     FROM &p_inNamesDs a
     INNER JOIN work.step_runtimes2 b 
     ON a.step = b.step
     ORDER BY step;
    QUIT;
    %let l_stepsCount = &sqlobs;

    /* Create stepName format */
    PROC FORMAT CNTLIN=&p_outNames2Ds(RENAME=(step=start step_name=label));
    RUN;

    /* Clean-up */
    PROC DATASETS lib=work nolist;
     DELETE step_names: step_runtimes:; 
    QUIT; RUN;
  %end; /* End - (&l_includeSteps_yn = Y) */

  %let &p_stepsCount = &l_stepsCount;

%MEND finalize_flow;