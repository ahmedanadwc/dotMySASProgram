/**
  @file util_inner_get_flow_data_set.sas
  @brief GLOBAL MACRO: inner_getFlowDataSet
  @details Extracted from util_parsescaproc.sas for modular reuse.
  @version 9.4
  @author Ahmed Al-Attar

**/

/* ============================================================
 * GLOBAL MACRO: inner_getFlowDataSet
 * ============================================================
 * Merges flow records into single record per step
 * ============================================================ */
%MACRO inner_getFlowDataSet(p_inDsName=
  , p_outDsName=
  , p_byVarName=
  , p_byVarNames=
  , p_stepFmt=
) / minoperator;

  %LOCAL l_stepFmt;
  %let l_stepFmt = &p_stepFmt;

  /* Merge the data into one record for each step */
  DATA &p_outDsName (KEEP=step step_name 'in'n 'out'n in_dsname out_dsname in_filename out_filename 
                          in_entity_name out_entity_name unq_in_entity_name unq_out_entity_name);
    LENGTH 
      step 5
      step_name $30
      in_dsname out_dsname $200
      in_filename out_filename 'in'n 'out'n $200
      in_entity_name out_entity_name unq_in_entity_name unq_out_entity_name $210;

    RETAIN step step_name in_dsname out_dsname 
      in_filename out_filename 
      in_entity_name out_entity_name
      unq_in_entity_name unq_out_entity_name;

    DO UNTIL (last.&p_byVarName);
      SET &p_inDsName(RENAME=(step=ds_step 
        dsname=ds_dsname filename=ds_filename entity_name=ds_entity_name));
      BY &p_byVarNames;

      if (first.&p_byVarName) then
        CALL MISSING (step, step_name, in_dsname, out_dsname
          , in_filename, out_filename, in_entity_name, out_entity_name
          , unq_in_entity_name, unq_out_entity_name);

      if (step = .) then step = ds_step;
      if (step_name = '') then step_name = '_'||CATX("_",PUT(ds_step,&l_stepFmt.),PUT(ds_step,stepname.));
      if (in_dsname = '')  then if (strip(source)='i') then in_dsname = ds_dsname;
      if (in_filename = '')  then if (strip(source)='i') then in_filename = ds_filename;
      if (in_entity_name = '')  then if (strip(source)='i') then in_entity_name = ds_entity_name;
      if (unq_in_entity_name = '')  then if (strip(source)='i') then unq_in_entity_name = PUT(ds_entity_name,$unqEntity.);
      if (out_dsname = '')  then if (strip(source)='o') then out_dsname = ds_dsname;
      if (out_filename = '')  then if (strip(source)='o') then out_filename = ds_filename;
      if (out_entity_name = '')  then if (strip(source)='o') then out_entity_name = ds_entity_name;
      if (unq_out_entity_name = '')  then if (strip(source)='o') then unq_out_entity_name = PUT(ds_entity_name,$unqEntity.);
    END;
    'in'n  = coalescec(in_dsname,in_filename);  
    'out'n = coalescec(out_dsname,out_filename);
    if (CATS(in_dsname,out_dsname,in_filename,out_filename,in_entity_name,out_entity_name) ne '') then
    OUTPUT;
  RUN;
%MEND inner_getFlowDataSet;