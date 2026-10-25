/**
  @file util_build_flow.sas
  @brief SUB-MACRO 3: build_flow
  @details Extracted from util_parsescaproc.sas for modular reuse.
  @version 9.4
  @author Ahmed Al-Attar

**/

/* ============================================================
 * SUB-MACRO 3: build_flow
 * ============================================================
 * Builds the data flow from step_inputs/step_outputs:
 *   - work.in_out_info_flow: combined input/output flow
 *   - work.steps_in_out_flow: merged flow per step
 *   - work.single_line_dag: single line per step for DAG
 * ============================================================ */
%MACRO build_flow(
  p_inInputsDs=work.step_inputs
, p_inOutputsDs=work.step_outputs
, p_outFlowDs=work.in_out_info_flow
, p_outStepsFlowDs=work.steps_in_out_flow
, p_outSingleLineDs=work.single_line_dag
, p_stepFmt=
) / minoperator;

  %LOCAL l_stepFmt;
  %let l_stepFmt = &p_stepFmt;

  /* Sort inputs and outputs */
  PROC SORT DATA=&p_inInputsDs NODUPKEY;
    BY step dsname variable type filename entity_name;
  RUN;
  PROC SORT DATA=&p_inOutputsDs NODUPKEY;
    BY step dsname variable type filename entity_name;
  RUN;
  PROC SORT DATA=&p_inInputsDs;
    BY step dsname sca_linenum ;
  RUN;
  PROC SORT DATA=&p_inOutputsDs;
    BY step dsname sca_linenum ;
  RUN;

  /* Compose logical steps in_out flow */
  DATA work.in_out_info_v / VIEW=work.in_out_info_v;
    SET &p_inInputsDs(WHERE=(filename NOT IN ('SEQ','MULTI')))
        &p_inOutputsDs(WHERE=(filename NOT IN ('SEQ','MULTI')));
    source=substr(entity_name,1,1);
  RUN;
  PROC SORT DATA=work.in_out_info_v OUT=&p_outFlowDs;
    BY step dsname sca_linenum entity_name;
  RUN;
  PROC DELETE DATA=WORK.IN_OUT_INFO_V (MT=VIEW); RUN;

  /* Find unique Entity reference */
  PROC SQL;
    /* Unique Input/Output Data Sets */
    CREATE TABLE work.unqEntity_fmtds AS
    SELECT DISTINCT 
       entity_name as start  length=210
      ,CAT(STRIP(dsname),'_(',PUT(col_cnt,z3.),')') as label length=210
      ,'$unqEntity' as fmtname
      ,'C' as type
    FROM 
    ( SELECT DISTINCT step,entity_name,dsname, count(*) as col_cnt
      FROM &p_inInputsDs
      WHERE variable ne ''
      AND filename NOT IN ('SEQ','MULTI') 
      GROUP BY entity_name
     UNION
      SELECT DISTINCT step,entity_name,dsname, count(*) as col_cnt
      FROM &p_inOutputsDs
      WHERE variable ne ''
      AND filename NOT IN ('SEQ','MULTI') 
      GROUP BY entity_name
    )
    ORDER BY 1;

    /* Unique Input/Output External Files */
    CREATE TABLE work.unqEntity_fmtds_f AS
    SELECT DISTINCT STRIP(entity_name) as start length=210
      , STRIP(filename) as label length=210
      , '$unqEntity' as fmtname
      , 'C' as type
    FROM &p_outFlowDs
    WHERE strip(filename) ne ''
    AND strip(dsname) = '';
  QUIT;

  /* Combine all into a single table */
  PROC APPEND BASE=WORK.unqentity_fmtds DATA=work.unqentity_fmtds_f; RUN;

  /* Create a Format for lookup */
  PROC FORMAT cntlin=work.unqEntity_fmtds; RUN;

  /* Find unique Entities */
  PROC SQL;
    CREATE TABLE unique_in_out_entities AS
    SELECT DISTINCT label as entity_name
    FROM  
    WORK.unqentity_fmtds;
  QUIT;

  /* Clean-up */
  PROC DATASETS LIB=WORK NOLIST; 
    DELETE unqEntity_fmtds_f columns ds_col_cnts; 
  RUN; QUIT;

  /* NOTE: inner_getFlowDataSet is now a global macro defined separately */

  /* Merge the data into one record for each step */
  PROC SORT DATA=&p_outFlowDs;
    BY step entity_name;
  RUN;
  %inner_getFlowDataSet(p_inDsName=&p_outFlowDs
  , p_outDsName=&p_outStepsFlowDs
  , p_byVarName=ds_entity_name
  , p_byVarNames=%str(ds_step ds_entity_name)
  , p_stepFmt=&l_stepFmt)

%MEND build_flow;