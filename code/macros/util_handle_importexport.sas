/**
  @file util_handle_importexport.sas
  @brief SUB-MACRO 4: handle_importexport
  @details Extracted from util_parsescaproc.sas for modular reuse.
  @version 9.4
  @author Ahmed Al-Attar

**/

/* ============================================================
 * SUB-MACRO 4: handle_importexport
 * ============================================================
 * Handles special processing for PROC IMPORT/EXPORT steps
 * ============================================================ */
%MACRO handle_importexport(
  p_inFlowDs=work.in_out_info_flow
, p_inScadata1Ds=work.scadata1
, p_inStepsFlowDs=work.steps_in_out_flow
, p_inRuntimesDs=work.step_runtimes
, p_outStepsFlowDs=work.steps_in_out_flow
, p_stepFmt=
) / minoperator;

  %LOCAL l_impExpSteps l_impExpStepsCnt l_dataSteps l_impExpStepsExclude l_stepFmt;
  %let l_stepFmt = &p_stepFmt;

  /* Find if there is Proc Export/Import steps */
  PROC SQL NOPRINT;
    create table work.with_known_out as
    select distinct coalesce(dsname,filename) as out, step
    from work.step_outputs
    group by 1
    having count(*) = 1
    order by step;

    select distinct t1.step
    into :l_impExpSteps separated by ' '
    from &p_inScadata1Ds t1
    where t1.step not in
    (select t2.step from work.with_known_out t2)
    and PUT(t1.step,stepname.) in ('IMPORT', 'EXPORT')
    order by t1.step;
  QUIT;
  %let l_impExpStepsCnt = &sqlobs;

  PROC DELETE DATA=work.with_known_out; RUN;

  %if (%superq(l_impExpSteps) NE ) %then %do;
    %let l_dataSteps=;
    %let l_impExpStepsExclude=;

    %do l_ei=1 %to &l_impExpStepsCnt;
      %let l_impExpStep = %scan(&l_impExpSteps,&l_ei,%str( ));
      %let l_dataStep   = %eval(&l_impExpStep + 1);
      %let l_impExpStepsExclude = &l_impExpStepsExclude,&l_impExpStep ;
      %if (%sysfunc(putn(&l_dataStep,stepname.)) EQ DATASTEP ) %then %do;
        %let l_dataSteps = &l_dataSteps,&l_dataStep;
        %let l_impExpStepsExclude = &l_impExpStepsExclude,&l_dataStep ;
      %end;
    %end;
    
    /* Remove leading comma */
    %let l_dataSteps = %substr(%superq(l_dataSteps),2);
    %let l_impExpStepsExclude = %substr(%superq(l_impExpStepsExclude),2);

    PROC SORT DATA=&p_inFlowDs(WHERE=(step in (&l_impExpStepsExclude))) 
      OUT=work.impExp_flow;
      BY step sca_linenum;
    RUN;

    /* Merge the data into one record for each step */
    %inner_getFlowDataSet(p_inDsName=work.impExp_flow
    , p_outDsName=work.impExp_flow2
    , p_byVarName=ds_step
    , p_byVarNames=%str(ds_step sca_linenum)
    , p_stepFmt=&l_stepFmt);

    PROC SQL;
      /* Create consolidated impExp flow data set */
      CREATE TABLE work.impExp_flow3 AS
      SELECT a.step
        , a.step_name
        , CASE
            WHEN INDEX(a.step_name,'IMPORT') THEN coalesce(a.in_filename,b.in_filename)
            ELSE coalesce(b.in_dsname,a.in_dsname)
          END as 'in'n
        , CASE
            WHEN INDEX(a.step_name,'EXPORT') THEN coalesce(b.out_filename,a.out_filename)
            ELSE coalesce(b.out_dsname,a.out_dsname)
          END as 'out'n
        , coalesce(b.in_entity_name,a.in_entity_name) as in_entity_name
        , coalesce(b.out_entity_name,a.out_entity_name) as out_entity_name
        , PUT(coalesce(b.in_entity_name,a.in_entity_name),$unqEntity.) as unq_in_entity_name
        , PUT(coalesce(b.out_entity_name,a.out_entity_name),$unqEntity.) as unq_out_entity_name

      FROM work.impExp_flow2(WHERE=(step IN (&l_impExpSteps))) AS a
      FULL join work.impExp_flow2(WHERE=(step IN (&l_dataSteps))) AS b
      on a.step = (b.step -1);

      /* Exclude/Remove the import/Export steps from other data sets */
      DELETE FROM &p_inRuntimesDs WHERE step IN (&l_dataSteps);
      DELETE FROM &p_inStepsFlowDs WHERE step in (&l_impExpStepsExclude);
    QUIT;

    /* Add reformed Import/Export steps to the flow */
    PROC APPEND BASE=&p_inStepsFlowDs DATA=work.impExp_flow3 FORCE NOWARN; RUN;

    /* Clean-up */
    PROC DATASETS LIB=WORK NOLIST;
      DELETE impExp_flow:; 
    RUN; QUIT;
  %end; /* End - (%superq(l_impExpSteps) NE ) */

%MEND handle_importexport;