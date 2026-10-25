/**
  @file util_generate_json.sas
  @brief SUB-MACRO 7: generate_json
  @details Extracted from util_parsescaproc.sas for modular reuse.
  @version 9.4
  @author Ahmed Al-Attar

**/

/* ============================================================
 * SUB-MACRO 7: generate_json
 * ============================================================
 * Generates JSON output from flow data for modern web integration
 * ============================================================ */
%MACRO generate_json(
  p_inScaprocFileName=
, p_inSingleLineDs=work.single_line_dag
, p_inAttrsDs=work.in_out_info_flow
, p_inEntitiesDs=unique_in_out_entities
, p_inStepsDs=work.step_names2
, p_outJsonFilePath=
, p_outJsonFileName=
, p_includeAttrs_yn=Y
, p_includeSteps_yn=Y
) / minoperator;

  %LOCAL l_includeAttrs_yn l_includeSteps_yn;
  %let l_includeAttrs_yn = %upcase(&p_includeAttrs_yn);
  %let l_includeSteps_yn = %upcase(&p_includeSteps_yn);

  /* Build JSON structure */
  DATA work._json_nodes_ (KEEP=entity_id entity_type entity_label entity_attrs);
    LENGTH entity_id $210 entity_type $10 entity_label $500 entity_attrs $32000;
    SET &p_inAttrsDs;
    BY step entity_name;
    
    /* Only keep first occurrence of each entity */
    IF first.entity_name THEN DO;
      entity_id = STRIP(PUT(entity_name, $unqEntity.));
      entity_type = IFC(STRIP(dsname)='' AND STRIP(filename) ne '', 'file', 'dataset');
      entity_label = STRIP(COALESCEC(dsname, filename));
      
      %if (&l_includeAttrs_yn = Y) %then %do;
        /* Collect attributes for this entity */
        /* We'll aggregate in next step */
      %end;
      
      OUTPUT;
    END;
  RUN;

  /* Aggregate attributes per entity when includeAttrs=Y */
  %if (&l_includeAttrs_yn = Y) %then %do;
    PROC SQL;
      CREATE TABLE work._json_nodes_attrs AS
      SELECT 
        STRIP(PUT(entity_name, $unqEntity.)) as entity_id,
        CATX(',', 
          CATS('"', STRIP(variable), '": {"type": "', 
            IFC(type='CHARACTER','character','numeric'), 
            '", "length": ', length_val, '}')
        ) as entity_attrs
      FROM &p_inAttrsDs
      WHERE STRIP(dsname) ne '' AND STRIP(filename) eq '' AND STRIP(variable) ne ''
      GROUP BY entity_name;
    QUIT;
    
    /* Merge attributes back */
    DATA work._json_nodes_;
      MERGE work._json_nodes_ work._json_nodes_attrs;
      BY entity_id;
    RUN;
  %end;

  /* Build edges from single_line_dag */
  DATA work._json_edges_ (KEEP=from_entity to_entity step_id step_name);
    LENGTH from_entity $210 to_entity $210 step_id 5 step_name $210;
    SET &p_inSingleLineDs;
    
    /* Parse input entities */
    DO i = 1 TO COUNTW(unq_in_entities, ' ');
      from_entity = SCAN(unq_in_entities, i, ' ');
      to_entity = STRIP(PUT(step_name, $unqEntity.)); /* step entity */
      step_id = step;
      step_name = step_name;
      OUTPUT;
    END;
    
    /* Parse output entities */
    DO i = 1 TO COUNTW(unq_out_entities, ' ');
      from_entity = STRIP(PUT(step_name, $unqEntity.)); /* step entity */
      to_entity = SCAN(unq_out_entities, i, ' ');
      step_id = step;
      step_name = step_name;
      OUTPUT;
    END;
  RUN;

  /* Get step metadata */
  %if (&l_includeSteps_yn = Y) %then %do;
    DATA work._json_steps_;
      SET &p_inStepsDs;
      step_id = start;
      step_label = label;
      KEEP step_id step_label;
    RUN;
  %end;

  /* Generate JSON using PROC JSON (SAS 9.4M3+) */
  %macro write_json;
    PROC JSON OUT="&p_outJsonFilePath/&p_outJsonFileName" PRETTY;
      EXPORT work._json_nodes_;
      EXPORT work._json_edges_;
      %if (&l_includeSteps_yn = Y) %then %do;
        EXPORT work._json_steps_;
      %end;
    RUN;
  %mend write_json;
  
  %write_json;

  /* Clean-up */
  PROC DATASETS LIB=WORK NOLIST;
    DELETE _json_:;
  RUN; QUIT;

%MEND generate_json;