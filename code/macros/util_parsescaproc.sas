/**
  @file util_parsescaproc.sas
  @brief Parses out the output generated SAS Code Analyzer.
  @details Parses out the output generated SAS Code Analyzer,
  and generated *.dot output file, that could be rendered by any
  Graphviz (https://www.graphviz.org/) Viewer/Renderer be it
    - Web Viewer: http://webgraphviz.com/
    - VS Code extension: Graphviz (dot) language support for Visual Studio Code
      (https://marketplace.visualstudio.com/items?itemName=joaompinto.vscode-graphviz)

  The inspiration for this code originted from
    - Code presented in
      "Automatically create diagrams showing the structure and performance of your SAS code"
      https://support.sas.com/resources/papers/proceedings17/1104-2017.pdf
      https://github.com/philipmason/Wood-Street-Consultants/tree/main/SCAPROC/SAS
    - Code presented in
      "SCAPROC Impact Analysis - Providing QA and Compliance Insights"
      https://www.lexjansen.com/phuse/2016/tt/TT02.pdf
      Sample 58878: SAS® Life Science Analytics Framework - 
      SCAPROC Impact Analysis: Providing QA and Compliance Insights
      https://support.sas.com/kb/58/878.html
      https://support.sas.com/kb/58/047.html
    - Code presented in
      "QA and Compliance Insights Using the SCAPROC Procedure"
      https://www.pharmasug.org/proceedings/2016/MS/PharmaSUG-2016-MS04.pdf
    - SASjs generated lineage documentation
      https://cli.sasjs.io/doc/
      https://www.youtube.com/watch?v=Sb1DqTLgVyQ&t=182s
      https://www.youtube.com/watch?v=9ea9vn7YmfM
 
  Example usage:
  
      %util_parseScaproc(p_inScaprocFileName=
      , p_outDotFilePath=%sysfunc(pathname(WORK))
      , p_outDotFileName=mydigraph.dot
      , p_outDsName=work.mydigraph
      , p_includeAttrs_yn=Y, p_includeSteps_yn=Y)

  @param [in] p_inScaprocFileName= The name of the SAS Code Analysis recording file. Required.
  @param [in] p_outDotFilePath= The GRAPHVIZ (*.dot) output file path. Required. 
                                Default: %sysfunc(pathname(WORK)).
  @param [in] p_outDotFileName= The GRAPHVIZ (*.dot) output file name. Required.
                                Default: mydigraph.dot
  @param [in] p_outDsName= The output data set name of the parsing results. Optional.
  @param [in] p_includeAttrs_yn= A Y/N flag to include table columns in the output. Default:Y.
  @param [in] p_includeSteps_yn= A Y/N flag to include processing steps and elapse times in the output. 
                                 Default:Y.
  @param [in] p_rankDir= Sets direction of graph layout. Valid Values:TB|BT|LR|RL.
                         Default:TB. Forced to LR when p_includeAttrs_yn=Y.
  @param [in] p_outJsonFileName= Optional. When specified, a JSON graph is also written to
                                 p_outDotFilePath/p_outJsonFileName, so that any graphing tool
                                 (D3, Cytoscape, vis.js, networkx, ...) can render the DAG:

                                     { "generator": "util_parseScaproc", "source": "<sca file>", "directed": true,
                                       "nodes": [ { "id", "label", "type": "dataset|file|step",
                                                    "columns": [ { "name", "type" } ],   (datasets, p_includeAttrs_yn=Y)
                                                    "step": <n> } ],                     (steps)
                                       "edges": [ { "source", "target", "type": "input|output|flow" } ] }

                                 Step nodes and input/output edges are included when p_includeSteps_yn=Y;
                                 otherwise edges link inputs directly to outputs (type "flow").
                                 Column lists follow p_includeAttrs_yn.

  @param [in] p_outDot_yn= A Y/N flag to write the GRAPHVIZ (*.dot) file and the p_outDsName data set.
                           Set to N when only the JSON graph is needed; p_outJsonFileName is then
                           required (rc 4 otherwise). Default:Y.

  @param [in] p_dotTheme= The format of the GRAPHVIZ (*.dot) file. Valid Values:DEFAULT|SIMPLE. Default:DEFAULT.
                          - DEFAULT: global "node [...]" style line, and grouped edges {"a" "b"} -> {"c"}.
                          - SIMPLE: no global node defaults (every node carries its own shape/colour:
                            tables for data sets, notes for files, ovals for steps) and one quoted
                            "source" -> "target" statement per edge, which more tools can parse.

  @return Global macro variable g_parseScaprocRC: 0 on success, non-zero on a validation error.

  @param [in] p_debug= A Y/N flag to print diagnostic messages to the log. Default:N.

  @n[util_parseScaproc Flow Diagram](../diagrams/util_parseScaproc.svg)

  @version 9.4
  @author Ahmed Al-Attar

**/

/* =====================================================================
   Internal stage macros for %util_parseScaproc (declared before it, in run order)
   ===================================================================== */

/**
  @brief Collapses the per-line flow records into one record per grouping key.
  @details Internal helper of %util_parseScaproc (uses l_stepFmt from the caller).
  @param [in] p_inDsName= input flow data set
  @param [out] p_outDsName= output data set
  @param [in] p_byVarName= BY variable whose LAST. ends a group
  @param [in] p_byVarNames= full BY statement variable list
**/
  %macro inner_pscp_getFlowDataSet(p_inDsName=
    , p_outDsName=
    , p_byVarName=
    , p_byVarNames=);

    /* Merge the data into one record for each step */
    DATA &p_outDsName (KEEP=step step_name 'in'n 'out'n in_dsname out_dsname in_filename out_filename 
                            in_entity_name out_entity_name unq_in_entity_name unq_out_entity_name);
      LENGTH 
        step 5
        step_name $60
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
  %mend inner_pscp_getFlowDataSet;

/**
  @brief Stage 1: reads the SCA text file; builds the concatenated-library format.
  @details Internal helper of %util_parseScaproc. Shares the caller's %LOCAL macro variables
  (l_*) and the WORK tables created by the preceding stages.
**/
%macro inner_pscp_readRecording;

  /* Read the SCA text file */
  DATA work.scadata(DROP=rc);
    INFILE "&l_inScaprocFileName" LRECL=1000 LENGTH=linelength truncover END=eof;
    LENGTH scaline $1000 file_linenum sca_linenum step 5 libref libname lineEnd $8;
    LABEL 
		scaline = 'SCA line'
		file_linenum = 'SCA file line number' 
		sca_linenum = 'SCA none empty line number'
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
    INPUT scaline $varying1000. linelength;
    file_linenum+1;

    if (length(strip(scaline)) gt 1) then
    do;
    	sca_linenum+1;
    	if (_n_=1 OR index(scaline,'STEP SOURCE FOLLOWS')) then step+1 ;
    	/* Only JOBSPLIT lines are used downstream, so don't persist the rest */
    	if (index(scaline,'JOBSPLIT') > 0) then OUTPUT;

    	/* JOBSPLIT: CONCATMEM #C00003 SASHELP */
    	if (index(scaline,'CONCATMEM') gt 0) then
    	do;
			scaline = SUBSTR(scaline, index(scaline,'CONCATMEM') + 10);
			libref = SCAN(scaline,1,' ');
			libname = SCAN(scaline,2,' ');
			lineEnd = SCAN(scaline,3,' ');
			if (libref ne '' and libname ne '' and lineEnd eq '*/') then rc = libHash.add();
    	end; /* End - (index(scaline,'CONCATMEM') gt 0) */
    end; /* End - (length(strip(scaline)) gt 1) */

  	/* Export/write the hash table */
  	if (eof) then
  	do;
  		libHash.output(dataset: 'work.sca_libs');
  	end;
  RUN;

  /* Find the Max Step count */
  PROC SQL NOPRINT;
    SELECT CATS('Z',max(2,floor(log10(MAX(step,1)))+1),'.')
    INTO :l_stepFmt
    FROM  work.scadata;
  QUIT;
  %if (&l_debug = Y) %then %put &=l_stepFmt;

  /**
  * Find Concatenated Libraries 
  */
  DATA _null_;
    if (0) then SET work.sca_libs nobs=nobs;
    call symputx("l_concatLibObsCount",nobs);
  RUN;

  %if (&l_debug = Y) %then %put &=l_concatLibObsCount;
  %if (&l_concatLibObsCount GT 0) %then
  %do;
  DATA work.sca_libs;
    SET work.sca_libs(RENAME=(libref=start libname=label)) ;
    RETAIN fmtname '$cncatlibs' type 'C';
  RUN;
  PROC FORMAT cntlin=work.sca_libs; RUN;
  %end;
  PROC DELETE DATA=work.sca_libs; RUN;


%mend inner_pscp_readRecording;

/**
  @brief Stage 2: extracts step inputs/outputs/names/run-times and the stepname. format.
  @details Internal helper of %util_parseScaproc. Shares the caller's %LOCAL macro variables
  (l_*) and the WORK tables created by the preceding stages.
**/
%macro inner_pscp_extractIO;

  /* Identify Input & Output data sets and their variables */
  DATA work.scadata1 (KEEP=scaline sca_linenum step )
  		work.step_inputs (KEEP=step sca_linenum dsname variable type filename entity_name) 
  		work.step_outputs (KEEP=step sca_linenum dsname variable type filename entity_name)
      /* Always created: the stepname. format is needed even when p_includeSteps_yn=N */
      work.step_runtimes (KEEP=step step_start_dt )
      work.step_names(KEEP=step step_name type fmtname)
  		;
  	
  	/* Ignoring SASTemp outputs, Global Macro Variables and UTILITY files */
    SET work.scadata(DROP=file_linenum WHERE=((index(scaline,"JOBSPLIT") > 0) 
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
    	step_name $60
    	entity_name $210;

    FORMAT step_start_dt datetime22.2;
    filename='';

    OUTPUT work.scadata1;

    /* Evaluate each keyword test once per line */
    is_attr = indexw(scaline,"ATTR") > 0;
    is_file = indexw(scaline,"FILE") > 0;
    is_dset = indexw(scaline,"DATASET") > 0;

    IF (   (indexw(scaline,"TASKSTARTTIME") > 0)
    	OR (indexw(scaline,"JOBENDTIME") > 0) ) THEN
    DO;
   		step_start_dt = INPUT(SCAN(scaline,4,' '), datetime22.2);
    	OUTPUT work.step_runtimes;
    END;
    ELSE IF(indexw(scaline,"PROCNAME") > 0) THEN
    DO;
    	step_name = SCAN(scaline,4,' ');
    	type = 'N';
    	fmtname = 'stepName';
    	OUTPUT work.step_names;
    END;
    ELSE IF (is_attr OR is_dset OR is_file) THEN
    DO;
	    IF (is_attr) THEN
	    DO;
	    	scaline  = SUBSTR(scaline,14);
	    	dsname   = SCAN(scaline,2,' ');
        dsname   = CATX('.',SCAN(dsname,1,'.'),SCAN(dsname,2,'.'));
    %if (&l_concatLibObsCount GT 0) %then
    %do;
	    	dsname   = CATX('.',PUT(SCAN(dsname,1,'.'),$cncatlibs.),SCAN(dsname,2,'.'));
    %end;
  			in_flag  = indexw(scaline,"INPUT");

  			/* Guard: skip malformed ATTR lines instead of calling SUBSTR with invalid positions */
	  			if (   index(scaline,'VARIABLE:') = 0
	  			    OR index(scaline,'TYPE:')   <= index(scaline,'VARIABLE:')
	  			    OR index(scaline,'LENGTH:') <= index(scaline,'TYPE:')
	  			    OR index(scaline,'LABEL:')  <= index(scaline,'LENGTH:') ) then
	  			do;
	  				putlog 'NOTE: util_parseScaproc: skipped malformed ATTR line ' sca_linenum=;
	  				DELETE;
	  			end;
	  			scaline  = SUBSTR(scaline,index(scaline,'VARIABLE:'));
  			typ_idx  = index(scaline,'TYPE:');
	     	variable = substr(scaline,(10),(typ_idx - 10));

  			scaline  = SUBSTR(scaline,index(scaline,'TYPE:'));
  			len_idx = index(scaline,'LENGTH:');
  			type    = substr(scaline,(6),(len_idx - 6));

  			scaline = SUBSTR(scaline,index(scaline,'LENGTH:'));
  			lbl_idx = index(scaline,'LABEL:');
  			len     = substr(scaline,(8),(lbl_idx - 8));

  			type    = CATX(' ', IFC(type='CHARACTER','char','num') ,CATS('(',len,')') );
	    END;
	    ELSE IF (is_file) THEN
	    DO;
	    	scaline  = SUBSTR(scaline,14);
	    	filename = SCAN(scaline,3,' ');
        in_flag  = indexw(scaline,"INPUT");
	    END;
	    ELSE IF (is_dset) THEN
	    DO;
	    	scaline  = SUBSTR(scaline,14);
	    	dsname   = SCAN(scaline,4,' ');
        dsname   = CATX('.',SCAN(dsname,1,'.'),SCAN(dsname,2,'.'));
    %if (&l_concatLibObsCount GT 0) %then
    %do;
	    	dsname   = CATX('.',PUT(SCAN(dsname,1,'.'),$cncatlibs.),SCAN(dsname,2,'.'));
    %end;
  			in_flag  = indexw(scaline,"INPUT");
	    END;
	    
	    entity_name = ifc(COALESCEC(dsname,filename)='','',
                    CATS(ifc(in_flag > 0,'i','o'),'_',PUT(step,&l_stepFmt.),'_',COALESCEC(dsname,filename)));
	    
	    /* Write the record to the correct destination based on the in_flag value */
      IF (in_flag > 0) then OUTPUT work.step_inputs;
      ELSE OUTPUT work.step_outputs;
    END;
  RUN;


  PROC SORT DATA=work.step_inputs NODUPKEY;
    BY step dsname variable type filename entity_name;
  RUN;
  PROC SORT DATA=work.step_outputs NODUPKEY;
    BY step dsname variable type filename entity_name;
  RUN;
  PROC SORT DATA=work.step_inputs;
    BY step dsname sca_linenum ;
  RUN;
  PROC SORT DATA=work.step_outputs;
    BY step dsname sca_linenum ;
  RUN;

  /* Create the stepname format (needed in both Y and N modes) */
  PROC FORMAT CNTLIN=WORK.STEP_NAMES(RENAME=(step=start step_name=label)); RUN;


%mend inner_pscp_extractIO;

/**
  @brief Stage 3: builds the entity lookup, the per-step flow and the single-line DAG.
  @details Internal helper of %util_parseScaproc. Shares the caller's %LOCAL macro variables
  (l_*) and the WORK tables created by the preceding stages.
**/
%macro inner_pscp_buildFlow;

  /** -------
  * Compose logical steps in_out flow
   ------- */
  DATA work.in_out_info_v / VIEW=work.in_out_info_v;
    SET work.step_inputs(WHERE=(filename NOT IN ('SEQ','MULTI')))
        work.step_outputs(WHERE=(filename NOT IN ('SEQ','MULTI')));
    source=substr(entity_name,1,1);
  RUN;
  PROC SORT DATA=work.in_out_info_v OUT=work.in_out_info_flow;
    BY step dsname sca_linenum entity_name;
  RUN;
  PROC DELETE DATA=WORK.IN_OUT_INFO_V (MT=VIEW); RUN;

  /** -------
  * Find unique Entity reference 
   ------- */
  PROC SQL;
    /* Unique Input/Output Data Sets */
    CREATE TABLE work.unqEntity_fmtds AS
    SELECT DISTINCT 
       entity_name as start  length=210
      ,CAT(STRIP(dsname),'_(',PUT(col_cnt,z4.),')') as label length=210
      ,'$unqEntity' as fmtname
      ,'C' as type
    FROM 
    ( SELECT DISTINCT step,entity_name,dsname, count(*) as col_cnt
      FROM work.step_inputs
      WHERE variable ne ''
      AND filename NOT IN ('SEQ','MULTI') 
      GROUP BY entity_name
     UNION
      SELECT DISTINCT step,entity_name,dsname, count(*) as col_cnt
      FROM work.step_outputs
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
    FROM work.in_out_info_flow
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
    DELETE unqEntity_fmtds_f;
  RUN; QUIT;


  /** --------
  * Merge the data into one record for each step 
   ------- */
  PROC SORT DATA=work.in_out_info_flow;
    BY step entity_name;
  RUN;
  %inner_pscp_getFlowDataSet(p_inDsName=work.in_out_info_flow
  , p_outDsName=work.steps_in_out_flow
  , p_byVarName=ds_entity_name
  , p_byVarNames=%str(ds_step ds_entity_name))

  /** -------
   * Find if there is Proc Export/Import steps 
   -------- */
  %let l_impExpSteps=;
  %let l_impExpStepsCnt=;

  PROC SQL NOPRINT;
    create table work.with_known_out as
    select distinct coalesce(dsname,filename) as out, step
    from work.step_outputs
    group by 1
    having count(*) = 1
    order by step;

    select distinct t1.step
    into :l_impExpSteps separated by ' '
    from work.scadata1 t1
    where t1.step not in
    (select t2.step from work.with_known_out t2)
    and PUT(t1.step,stepname.) in ('IMPORT', 'EXPORT')
    order by t1.step;
  QUIT;
  %let l_impExpStepsCnt = &sqlobs;
  %if (&l_debug = Y) %then %put &=l_impExpSteps &=l_impExpStepsCnt;

  PROC DELETE DATA=work.with_known_out; RUN;

  /** ------
   * Perform special processing for Export/Import Step(s) 
   ------- */
  %if (%superq(l_impExpSteps) NE ) %then
  %do;
    %let l_dataSteps=;
    %let l_impExpStepsExclude=;

    %do l_ei=1 %to &l_impExpStepsCnt;
      %let l_impExpStep = %scan(&l_impExpSteps,&l_ei,%str( ));
      %let l_dataStep   = %eval(&l_impExpStep + 1);
      %let l_impExpStepsExclude = &l_impExpStepsExclude,&l_impExpStep ;
      %if (%sysfunc(putn(&l_dataStep,stepname.)) EQ DATASTEP ) %then
      %do;
        %let l_dataSteps = &l_dataSteps,&l_dataStep;
        %let l_impExpStepsExclude = &l_impExpStepsExclude,&l_dataStep ;
      %end;
    %end;
    
    /* Remove leading comma */
    %let l_dataSteps = %substr(%superq(l_dataSteps),2);
    %let l_impExpStepsExclude = %substr(%superq(l_impExpStepsExclude),2);

    %if (&l_debug = Y) %then %put &=l_dataSteps &=l_impExpStepsExclude;

    PROC SORT DATA=work.in_out_info_flow(WHERE=(step in (&l_impExpStepsExclude))) 
      OUT=work.impExp_flow;
      BY step sca_linenum;
    RUN;

    /* Merge the data into one record for each step */
    %inner_pscp_getFlowDataSet(p_inDsName=work.impExp_flow
    , p_outDsName=work.impExp_flow2
    , p_byVarName=ds_step
    , p_byVarNames=%str(ds_step sca_linenum));

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
        , PUT(coalesce(b.in_entity_name,a.in_entity_name),$unqEntity.) as unq_in_entity_name length=210
        , PUT(coalesce(b.out_entity_name,a.out_entity_name),$unqEntity.) as unq_out_entity_name length=210

      FROM work.impExp_flow2(WHERE=(step IN (&l_impExpSteps))) AS a
      FULL join work.impExp_flow2(WHERE=(step IN (&l_dataSteps))) AS b
      on a.step = (b.step -1);

      /* Exclude/Remove the import/Export steps from other data sets */
      DELETE FROM work.step_runtimes WHERE step IN (&l_dataSteps);
      DELETE FROM work.steps_in_out_flow WHERE step in (&l_impExpStepsExclude);
    QUIT;

    /* Add reformed Import/Export steps to the flow */
    PROC APPEND BASE=work.steps_in_out_flow DATA=work.impExp_flow3 FORCE NOWARN; RUN;

    /* Clean-up */
    PROC DATASETS LIB=WORK NOLIST;
      DELETE impExp_flow:; 
    RUN; QUIT;
  %end; /* End - (%superq(l_impExpSteps) NE ) */
 
  /* Clean-up */
  PROC DATASETS LIB=WORK NOLIST;
   DELETE scadata: step_inputs step_outputs; 
  QUIT; RUN;

  /** ---------
  * Reduce Multi-line step into a single line for each step 
   --------- */
  PROC SORT DATA=work.steps_in_out_flow(KEEP=step step_name 'in'n 'out'n in_entity_name out_entity_name unq_in_entity_name unq_out_entity_name);
    BY step step_name 'in'n 'out'n in_entity_name out_entity_name unq_in_entity_name unq_out_entity_name;
  RUN;

  DATA work.single_line_dag (KEEP=step step_name ins outs in_entities out_entities unq_in_entities unq_out_entities);
    if (0) then SET work.steps_in_out_flow;
    /* Wide enough that steps with many inputs/outputs are not silently truncated */
    LENGTH
      ins outs
      in_entities out_entities unq_in_entities unq_out_entities $32767;

    DO UNTIL (last.step);
      SET work.steps_in_out_flow;
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

  %if (&l_needEdges = Y) %then
  %do;
    /* Keep the per-step entity links for the JSON / SIMPLE-theme writers (steps_in_out_flow is deleted below) */
    PROC SQL;
      CREATE TABLE work.pscp_stepio AS
      SELECT DISTINCT step, unq_in_entity_name, unq_out_entity_name
      FROM work.steps_in_out_flow;
    QUIT;
  %end;

  PROC DELETE DATA=work.STEPS_IN_OUT_FLOW; RUN;

%mend inner_pscp_buildFlow;

/**
  @brief Stage 4: adds run-time durations to the stepname. format (p_includeSteps_yn=Y).
  @details Internal helper of %util_parseScaproc. Shares the caller's %LOCAL macro variables
  (l_*) and the WORK tables created by the preceding stages.
**/
%macro inner_pscp_addRuntimes;

  %if (&l_includeSteps_yn = Y) %then
  %do;
    /* Calculate steps run-time duration */
    /* Each step ends when the next recorded step starts (one-to-one merge, offset by one row) */
    DATA work.step_runtimes2;
     MERGE work.step_runtimes
           work.step_runtimes(firstobs=2 keep=step_start_dt rename=(step_start_dt=step_end_dt));
    LENGTH runTime_duration 8;
    FORMAT runTime_duration time12.2;

    if (step_end_dt > 0) then
    	runTime_duration = step_end_dt - step_start_dt;
    if (runTime_duration  ne .);
    RUN;

    /* Join the names with their duration run-times */
    PROC SQL;
     CREATE TABLE work.step_names2 AS
     SELECT 
    	a.step
     	, a.fmtname
     	, CAT('_',PUT(a.step,&l_stepFmt.),'_',STRIP(a.step_name),' (',STRIP(PUT(b.runtime_duration,time12.2)),')') as step_name LENGTH=60
     	, a.type
     FROM work.step_names a
     INNER JOIN work.step_runtimes2 b 
     ON a.step = b.step
     ORDER BY step;
    QUIT;
    %let l_stepsCount = &sqlobs;

    /* Create stepName format */
    PROC FORMAT CNTLIN=work.step_names2(RENAME=(step=start step_name=label));
    RUN;

  %end; /* End - (&l_includeSteps_yn = Y) */

  /* Clean-up */
  PROC DATASETS lib=work nolist;
   DELETE step_names: step_runtimes:;
  QUIT; RUN;

%mend inner_pscp_addRuntimes;

/**
  @brief Expression (for use in a DATA step) that JSON-escapes a character value.
  @param [in] p_val= character expression
**/
%macro inner_pscp_jsonEsc(p_val);
  tranwrd(tranwrd(strip(&p_val),'\','\\'),'"','\"')
%mend inner_pscp_jsonEsc;

/**
  @brief Builds one edge per row (source, target, kind) from the per-step entity links.
  @details Internal helper of %util_parseScaproc, shared by the JSON writer and the SIMPLE DOT theme.
  p_stepIdDs maps step -> the id of its node, so each writer can use its own step node ids.
  @param [in] p_stepIdDs= data set with columns step, id (used when p_includeSteps_yn=Y)
  @param [out] p_outDsName= edges data set (source, target, kind), sorted by those columns
**/
%macro inner_pscp_buildEdges(p_stepIdDs=, p_outDsName=);

  PROC SQL;
  %if (&l_includeSteps_yn EQ Y) %then
  %do;
    /* Edges input -> step -> output */
    CREATE TABLE &p_outDsName AS
    SELECT DISTINCT io.unq_in_entity_name AS source LENGTH=210, s.id AS target LENGTH=210
      , 'input' AS kind LENGTH=10
    FROM work.pscp_stepio AS io INNER JOIN &p_stepIdDs AS s ON io.step = s.step
    WHERE io.unq_in_entity_name NE ''
    UNION
    SELECT DISTINCT s.id, io.unq_out_entity_name, 'output'
    FROM work.pscp_stepio AS io INNER JOIN &p_stepIdDs AS s ON io.step = s.step
    WHERE io.unq_out_entity_name NE '';
  %end;
  %else
  %do;
    /* No step nodes: link inputs directly to outputs */
    CREATE TABLE &p_outDsName AS
    SELECT DISTINCT a.unq_in_entity_name AS source LENGTH=210, b.unq_out_entity_name AS target LENGTH=210
      , 'flow' AS kind LENGTH=10
    FROM work.pscp_stepio AS a INNER JOIN work.pscp_stepio AS b ON a.step = b.step
    WHERE a.unq_in_entity_name NE '' AND b.unq_out_entity_name NE '';
  %end;
  QUIT;

  PROC SORT DATA=&p_outDsName; BY source target kind; RUN;

%mend inner_pscp_buildEdges;

/**
  @brief Stage 4b: writes the data flow as a node/edge JSON graph (p_outJsonFileName=).
  @details Internal helper of %util_parseScaproc. Runs after pscp_addRuntimes and before pscp_writeDot,
  while in_out_info_flow, unique_in_out_entities, pscp_stepio and the $unqEntity./stepname. formats exist.
**/
%macro inner_pscp_writeJson;

  %LOCAL l_jsonPath;
  %let l_jsonPath = &l_outDotFilePath/&l_outJsonFileName;

  PROC SQL;
    /* Data set and external-file nodes (one per unique entity) */
    CREATE TABLE work.pscp_json_nodes AS
    SELECT DISTINCT ent.entity_name AS id LENGTH=210
      , CASE WHEN STRIP(flow.dsname) = '' AND STRIP(flow.filename) NE ''
             THEN STRIP(flow.filename) ELSE STRIP(flow.dsname) END AS label LENGTH=210
      , CASE WHEN STRIP(flow.dsname) = '' AND STRIP(flow.filename) NE ''
             THEN 'file' ELSE 'dataset' END AS type LENGTH=10
      , . AS step
    FROM work.in_out_info_flow AS flow
    INNER JOIN work.unique_in_out_entities AS ent
    ON PUT(flow.entity_name,$unqEntity.) = ent.entity_name AND ent.entity_name IS NOT NULL;

  %if (&l_includeAttrs_yn EQ Y) %then
  %do;
    /* Column lists, in order of first appearance */
    CREATE TABLE work.pscp_json_cols AS
    SELECT ent.entity_name AS id LENGTH=210
      , STRIP(flow.variable) AS name LENGTH=32
      , STRIP(flow.type) AS coltype LENGTH=15
      , MIN(flow.sca_linenum) AS seq
    FROM work.in_out_info_flow AS flow
    INNER JOIN work.unique_in_out_entities AS ent
    ON PUT(flow.entity_name,$unqEntity.) = ent.entity_name AND ent.entity_name IS NOT NULL
    WHERE STRIP(flow.dsname) NE '' AND STRIP(flow.variable) NE ''
    GROUP BY 1, 2, 3
    ORDER BY 1, 4;
  %end;
  %else
  %do;
    CREATE TABLE work.pscp_json_cols
      (id CHAR(210), name CHAR(32), coltype CHAR(15), seq NUM);
  %end;

  %if (&l_includeSteps_yn EQ Y) %then
  %do;
    /* Step nodes, and edges input -> step -> output */
    INSERT INTO work.pscp_json_nodes (id, label, type, step)
    SELECT DISTINCT PUT(step,stepname.), STRIP(SCAN(PUT(step,stepname.),2,'_')), 'step', step
    FROM work.pscp_stepio;

    /* JSON step node ids include the run-time, e.g. "_01_SUMMARY (0:00:00.02)" */
    CREATE TABLE work.pscp_json_stepid AS
    SELECT DISTINCT step, PUT(step,stepname.) AS id LENGTH=210
    FROM work.pscp_stepio;
  %end;
  QUIT;

  %inner_pscp_buildEdges(p_stepIdDs=work.pscp_json_stepid, p_outDsName=work.pscp_json_edges)

  PROC SORT DATA=work.pscp_json_nodes; BY id; RUN;
  DATA work.pscp_json_nodes;   /* one node per id */
    SET work.pscp_json_nodes;
    BY id;
    IF (first.id);
  RUN;
  PROC SORT DATA=work.pscp_json_cols;  BY id seq; RUN;

  /* Header */
  DATA _NULL_;
    FILE "&l_jsonPath" LRECL=32767;
    LENGTH line $32767;
    PUT '{';
    line = CATS('"generator": "util_parseScaproc", "source": "', %inner_pscp_jsonEsc("&l_inScaprocFileName"), '", "directed": true,');
    PUT line;
    PUT '"nodes": [';
  RUN;

  /* Nodes, each with its optional column list */
  DATA _NULL_;
    FILE "&l_jsonPath" LRECL=32767 MOD;
    MERGE work.pscp_json_nodes(IN=isNode) work.pscp_json_cols;
    BY id;
    IF (NOT isNode) THEN DELETE;
    LENGTH line $32767;
    RETAIN nodeCnt 0 colCnt 0 hasCols 0;

    IF (first.id) THEN
    DO;
      line = CATS(IFC(nodeCnt > 0, ',', ' '), '{"id": "', %inner_pscp_jsonEsc(id), '", "label": "', %inner_pscp_jsonEsc(label)
                 , '", "type": "', type, '"');
      IF (step NE .) THEN line = CATS(line, ', "step": ', step);
      hasCols = (name NE '');
      colCnt  = 0;
      IF (hasCols) THEN line = CATS(line, ', "columns": [');
      nodeCnt + 1;
      PUT line;
    END;

    IF (name NE '') THEN
    DO;
      line = CATS(IFC(colCnt > 0, ',', ' '), '{"name": "', %inner_pscp_jsonEsc(name), '", "type": "', %inner_pscp_jsonEsc(coltype), '"}');
      PUT '  ' line;
      colCnt + 1;
    END;

    IF (last.id) THEN
    DO;
      IF (hasCols) THEN PUT ']';
      PUT '}';
    END;
  RUN;

  DATA _NULL_;
    FILE "&l_jsonPath" LRECL=32767 MOD;
    PUT '],';
    PUT '"edges": [';
  RUN;

  /* Edges */
  DATA _NULL_;
    FILE "&l_jsonPath" LRECL=32767 MOD;
    SET work.pscp_json_edges;
    LENGTH line $32767;
    line = CATS(IFC(_n_ > 1, ',', ' '), '{"source": "', %inner_pscp_jsonEsc(source), '", "target": "', %inner_pscp_jsonEsc(target)
               , '", "type": "', kind, '"}');
    PUT line;
  RUN;

  DATA _NULL_;
    FILE "&l_jsonPath" LRECL=32767 MOD;
    PUT ']';
    PUT '}';
  RUN;

%mend inner_pscp_writeJson;

/**
  @brief Stage 5: composes the DOT directives and writes the .dot file and output data set.
  @details Internal helper of %util_parseScaproc. Shares the caller's %LOCAL macro variables
  (l_*) and the WORK tables created by the preceding stages.
**/
%macro inner_pscp_writeDot;

  %if (&l_outDot_yn = Y) %then
  %do;

  /* SIMPLE theme: no global node defaults, so each data set node carries its own shape/colour */
  %LOCAL l_dsNodeAttrs;
  %let l_dsNodeAttrs = ;
  %if (&l_dotTheme = SIMPLE) %then
  %do;
    %if (&l_includeAttrs_yn EQ Y) %then %let l_dsNodeAttrs = %str(shape=table color=lightblue style=filled );
    %else %let l_dsNodeAttrs = %str(shape=cylinder color=lightblue style=filled );
  %end;

  * ---------------;
  * Create .DOT directives to make a diagram ;
  * ---------------;
  * First: Produce header lines;
  DATA work._flow_start_ ;
    LENGTH line $32767;

    line="// Generated by SAS for &l_inScaprocFileName" ;
    OUTPUT ;
    line='digraph dag {' ;
    OUTPUT ;
    line="rankdir=&l_rankDir";
    OUTPUT;
    line="graph [label=""\n&l_inScaprocFileName""]" ;
    OUTPUT ;
  %if (&l_includeAttrs_yn EQ Y) %then
  %do;
    line='node [shape=table color=lightblue style=filled]' ;
  %end;
  %else
  %do;
    line='node [shape=cylinder color=lightblue style=filled]' ;
  %end;
  %if (&l_dotTheme NE SIMPLE) %then
  %do;
    /* DEFAULT theme: global node defaults. SIMPLE theme styles every node explicitly instead. */
    OUTPUT ;
  %end;
  RUN;

  PROC SQL;
    CREATE TABLE work._flow_attrs_ AS
    SELECT DISTINCT
       flow.step 
      ,flow.entity_name
      ,IFC((STRIP(flow.dsname) eq '' AND STRIP(flow.filename) ne ''),'F','D') AS entity_type 
      ,CASE
  /* Write out Entities definitions */
  %if (&l_includeAttrs_yn EQ Y) %then
  %do;
        /* Variable Attribute */
        WHEN (STRIP(flow.dsname) ne '' AND STRIP(flow.filename) eq '' AND STRIP(variable) ne '') THEN
          CATS('<TR><TD BORDER="0" ALIGN="LEFT"><FONT POINT-SIZE="10">'
        	  ,flow.variable
        	  ,'</FONT></TD><TD BORDER="0" ALIGN="RIGHT"><FONT POINT-SIZE="10">'
        	  ,flow.type
        	  ,'</FONT></TD></TR>')
  %end; /* End - (&l_includeAttrs_yn EQ Y) */
        /* External File */
        WHEN (STRIP(flow.dsname) eq '' AND STRIP(flow.filename) ne '') THEN
          '"'||STRIP(PUT(flow.entity_name,$unqEntity.))||'" [ shape="note" label="'|| STRIP(TRANWRD(flow.filename,'\','\\'))||'" color=lightgreen style=filled ]'
        /* Data set */
        /*WHEN (STRIP(flow.dsname) ne '' AND STRIP(flow.filename) eq '') THEN*/
        ELSE
          '"'||STRIP(PUT(flow.entity_name,$unqEntity.))||'" ['||"&l_dsNodeAttrs"||'label=<<TABLE BORDER="0" CELLSPACING="0"><TR><TD BORDER="0" COLSPAN="2" CELLPADDING="0"><FONT POINT-SIZE="12">'
             || STRIP(PUT(flow.entity_name,$unqEntity.))||'</FONT></TD></TR>'
      END as line LENGTH=6000 FORMAT=$6000.

    FROM work.in_out_info_flow as flow
    INNER JOIN work.unique_in_out_entities as ent
    ON PUT(flow.entity_name,$unqEntity.) = ent.entity_name 
    AND ent.entity_name IS NOT NULL
    ORDER BY 1,2;
  QUIT;

  /* Add Table closing tag */
  DATA work._flow_attrs_ ;
    SET work._flow_attrs_;
    BY step entity_name;
    OUTPUT;
    if (last.entity_name) then
    do; 
      if (entity_type='D') then
      do;
        line='</TABLE>>];';
        OUTPUT;
      end;
    end;
  RUN;
 
  %if (&l_dotTheme = SIMPLE) %then
  %do;
    /* SIMPLE theme: one quoted "source" -> "target" statement per edge (no {a b} -> {c} set syntax) */
  %if (&l_includeSteps_yn = Y) %then
  %do;
    PROC SQL;
      CREATE TABLE work.pscp_dot_stepid AS
      SELECT DISTINCT step, step_name AS id LENGTH=210
      FROM work.single_line_dag;
    QUIT;
  %end;

    %inner_pscp_buildEdges(p_stepIdDs=work.pscp_dot_stepid, p_outDsName=work.pscp_dot_edges)

    DATA work._flow_dag_(KEEP=line);
      LENGTH line $32767;
      SET
  %if (&l_includeSteps_yn = Y) %then
  %do;
        work.single_line_dag(KEEP=step step_name IN=isStep)
  %end;
        work.pscp_dot_edges(IN=isEdge);

      IF (isEdge) THEN
      DO;
        line = CATS('"', source, '" -> "', target, '"');
        OUTPUT;
      END;
  %if (&l_includeSteps_yn = Y) %then
  %do;
      ELSE
      DO;
        line = CATS('"', step_name, '"')||' [shape="oval" label="'||STRIP(SCAN(PUT(step,stepname.),2,'_'))||'" color=yellow style=filled ]';
        OUTPUT;
      END;
  %end;
    RUN;
  %end; /* End - SIMPLE theme dag */
  %else
  %do; /* DEFAULT theme dag */

  /* Write out the Dag */
	DATA work._flow_dag_(KEEP=line) ;
		LENGTH line $32767 step_name $60;
    SET work.single_line_dag;
    BY step;

  %if (&l_includeSteps_yn = Y) %then 
  %do; 
    /* Step Definition */
		line = CATS('"',step_name,'"')||' [shape="oval" label="'||STRIP(SCAN(PUT(step,stepname.),2,'_'))||'" color=yellow style=filled ]';
    line = strip(line);
		OUTPUT;
    /* Inputs */
    if (STRIP(unq_in_entities) ne '') then
    do;
      line = CATX(' -> ',CATS('{"', TRANWRD(STRIP(unq_in_entities),' ','" "') ,'"}'),CATS('{"',step_name,'"}'));
      line = strip(line);
      OUTPUT;
    end;
    /* Outputs */
    if (STRIP(unq_out_entities) ne '') then
    do;
      line = CATX(' -> ',CATS('{"',step_name,'"}'),CATS('{"', TRANWRD(STRIP(unq_out_entities),' ','" "') ,'"}'));
      line = strip(line);
      OUTPUT;
    end;
  %end;
  %else
  %do;
    /* Inputs */
    if (STRIP(unq_in_entities) ne '') then
    do;
      line = CAT(CATS('{"', TRANWRD(STRIP(unq_in_entities),' ','" "') ,'"}'),' -> ');
      line = strip(line);
    end;
    /* Outputs */
    if (STRIP(unq_out_entities) ne '') then
    do;
      line = CATX(' ',line,CATS('{"', TRANWRD(STRIP(unq_out_entities),' ','" "') ,'"}'));
      OUTPUT;
    end;
  %end;
  RUN;
  
  %end; /* End - DEFAULT theme dag */

  * Last: Produce closing lines;
  DATA work._flow_end_ ;
    LENGTH line $32767;
    line= '}' ;
    OUTPUT ;
  RUN;

  DATA &l_outDsName(COMPRESS=YES);
    FILE "&l_outDotFilePath/&l_outDotFileName" lRECL=32767;
    LENGTH line $32767;
    SET work._flow_start_
      work._flow_attrs_
      work._flow_dag_
    	work._flow_end_;

    line=STRIP(line);
    PUT line;
  RUN;

  %end; /* End - (&l_outDot_yn = Y) */

  /* Clean-up */
  PROC DATASETS LIB=WORK NOLIST NOWARN;
    DELETE _flow_: in_out_info_flow single_line_dag unique_in_out_entities unqEntity_fmtds pscp_:;
  RUN; QUIT;

%mend inner_pscp_writeDot;



%MACRO util_parseScaproc(p_inScaprocFileName=
, p_outDotFilePath=%sysfunc(pathname(WORK))
, p_outDotFileName=mydigraph.dot
, p_outDsName=
, p_includeAttrs_yn=Y
, p_includeSteps_yn=Y
, p_rankDir=TB
, p_debug=N
, p_outJsonFileName=
, p_outDot_yn=Y
, p_dotTheme=DEFAULT) / minoperator;

  %LOCAL
    l_eTime
    l_MSG
    l_RC
    l_rTime
    l_sTime
    ;

  %LOCAL
    l_inScaprocFileName
    l_scaprocFileName
    l_debug
    l_ei
    l_impExpStep
    l_dataStep
    l_logsRoot
    l_outJsonFileName
    l_outDot_yn
    l_dotTheme
    l_needEdges
    l_outDotFilePath
    l_outDotFileName
    l_outDsName
    l_stepFmt
    l_includeAttrs_yn
    l_includeSteps_yn
    l_rankDir
    l_impExpSteps
    l_dataSteps
    l_impExpStepsCnt
    l_impExpStepsExclude
    l_stepsCount
    l_concatLibObsCount
    ;

  %let l_sTime=%sysfunc(time());

  /* Return code for callers/tests: 0 = success, non-zero = validation failure */
  %if (NOT %symexist(g_parseScaprocRC)) %then %global g_parseScaprocRC;
  %let l_rc = 0;
  %let g_parseScaprocRC = 0;

  /* Initialize the collection and open the output file */
  /* Named scaparse__ (not sca__) so the usage program's sca__*.txt glob never picks it up */
  %let l_logsRoot = %sysfunc(pathname(WORK));
  %if (%symexist(g_logsRoot)) %then %let l_logsRoot = &g_logsRoot;
  %util_initScaproc(p_enableFlagName=g_enableScaProc
  , p_outFilePath=%str(&l_logsRoot)
  , p_outFileName=scaparse__util_parseScaproc.txt);

  %let l_debug = %upcase(%superq(p_debug));

  /********** BEGIN -- Macro Parameter Validation **********/
  /* Check if the p_inScaprocFileName is specified or not */
  %if (%superq(p_inScaprocFileName) EQ ) %then 
  %do;
    %let l_rc  = 1;
    %let l_msg = ERROR: util_parseScaproc: p_inScaprocFileName is invalid. Please specify non-missing value;
    %goto exit;
  %end;
  %else 
  %do;
    /* Convert \ to / for supplied parameter paths */
    %let l_inScaprocFileName = %sysfunc(translate(&p_inScaprocFileName,%str(/),%str(\)));

    %if (%sysfunc(fileexist(&l_inScaprocFileName)) EQ 1) %then 
    %do;
      %let l_scaprocFileName = %scan(&l_inScaprocFileName,-1,%str(/));
      %if (&l_debug = Y) %then %put &=l_scaprocFileName;
    %end;
    %else 
    %do;
      %let l_rc  = 2;
      %let l_msg = ERROR: util_parseScaproc: p_inScaprocFileName is invalid. Please specify an existing file;
      %goto exit;
    %end;
  %end;

  /* Set Defaults */
  %let l_outDotFilePath = &p_outDotFilePath;
  %if (%superq(l_outDotFilePath) EQ ) %then
    %let l_outDotFilePath = %sysfunc(pathname(WORK));
  %else
    %let l_outDotFilePath = %sysfunc(translate(&l_outDotFilePath,%str(/),%str(\)));

  /* Fail fast on a bad output folder, before doing any work */
  %if (%sysfunc(fileexist(&l_outDotFilePath)) EQ 0) %then
  %do;
    %let l_rc  = 3;
    %let l_msg = ERROR: util_parseScaproc: p_outDotFilePath does not exist: &l_outDotFilePath;
    %goto exit;
  %end;

  %let l_outJsonFileName = %superq(p_outJsonFileName);

  %let l_outDot_yn = Y;
  %if (%superq(p_outDot_yn) NE ) %then
    %let l_outDot_yn = %upcase(&p_outDot_yn);
  %if (&l_outDot_yn NE Y AND %superq(l_outJsonFileName) EQ ) %then
  %do;
    %let l_rc  = 4;
    %let l_msg = ERROR: util_parseScaproc: p_outDot_yn=N requires p_outJsonFileName (nothing would be written);
    %goto exit;
  %end;

  %let l_outDotFileName = &p_outDotFileName;
  %if (%superq(l_outDotFileName) EQ ) %then
    %let l_outDotFileName = mydigraph.dot;

  %let l_dotTheme = DEFAULT;
  %if (%superq(p_dotTheme) NE ) %then
    %let l_dotTheme = %upcase(&p_dotTheme);
  %if NOT (&l_dotTheme IN (DEFAULT SIMPLE)) %then
  %do;
    %PUT WARNING: Acceptable p_dotTheme values: DEFAULT|SIMPLE. Defaulting to: DEFAULT;
    %let l_dotTheme = DEFAULT;
  %end;

  /* The per-step entity links are needed by the JSON writer and the SIMPLE theme */
  %let l_needEdges = N;
  %if (%length(%superq(l_outJsonFileName)) GT 0) %then %let l_needEdges = Y;
  %if (&l_dotTheme = SIMPLE AND &l_outDot_yn = Y) %then %let l_needEdges = Y;

  %let l_outDsName = &p_outDsName;
  %if (%superq(l_outDsName) EQ ) %then
    %let l_outDsName=work.mydigraph;

  %let l_includeAttrs_yn = Y;
  %if (%superq(p_includeAttrs_yn) NE ) %then
    %let l_includeAttrs_yn = %upcase(&p_includeAttrs_yn);

  %let l_includeSteps_yn = Y;
  %if (%superq(p_includeSteps_yn) NE ) %then
    %let l_includeSteps_yn = %upcase(&p_includeSteps_yn);

  %let l_rankDir = TB;
  %if (%superq(p_rankDir) NE ) %then
    %let l_rankDir = %upcase(&p_rankDir);
  %if NOT (&l_rankDir IN (TB BT LR RL)) %then
  %do;
    %PUT WARNING: Acceptable p_rankDir values: TB|BT|LR|RL. Defaulting to: TB;
    %let l_rankDir = TB;
  %end;

  /* Set the direction to LR when the columns attributes in included */
  %if (&l_includeAttrs_yn = Y) %then
    %let l_rankDir = LR;

  /********** END -- Macro Parameter Validation **********/
  %let l_concatLibObsCount = 0;

  /* Stage 1: read the recording */
  %inner_pscp_readRecording

  /* Stage 2: extract inputs, outputs, step names and run-times */
  %inner_pscp_extractIO

  /* Stage 3: build the entity lookup and per-step data flow */
  %inner_pscp_buildFlow

  /* Stage 4: add run-time durations to the step names */
  %inner_pscp_addRuntimes

  /* Stage 5: write the optional JSON graph, then the .dot file (which cleans up the shared tables) */
  %if (%superq(l_outJsonFileName) NE ) %then
  %do;
    %inner_pscp_writeJson
  %end;
  %inner_pscp_writeDot

  /* Remove the work formats created by this run so they can't leak into the next call */
  PROC CATALOG CAT=work.formats;
    DELETE unqEntity / ENTRYTYPE=FORMATC;
  %if (&l_concatLibObsCount GT 0) %then
  %do;
    DELETE cncatlibs / ENTRYTYPE=FORMATC;
  %end;
    DELETE stepname  / ENTRYTYPE=FORMAT;
  RUN; QUIT;

  %goto finished;

  %exit:
    %PUT *** ERROR: util_parseScaproc  ***;
    %PUT *** l_RC must be zero (0).   ***;
    %PUT *** l_RC= &l_RC    .         ***;
    %PUT *** &l_MSG ***;
    %PUT *** ERROR: util_parseScaproc  ***;

  %finished:
    /* Always terminate the collection (success or error), so PROC SCAPROC is never left recording */
    %util_termScaproc(p_enableFlagName=g_enableScaProc)

    %let g_parseScaprocRC = &l_rc;
    %let l_eTime=%sysfunc(time());
    %let l_rTime=%sysfunc(putn(%sysevalf(&l_eTime - &l_sTime),time12.2));
    %PUT >>> util_parseScaproc :>>> Total RunTime = &l_rTime;

%MEND util_parseScaproc;
