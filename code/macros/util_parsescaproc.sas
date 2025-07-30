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
                         Default:TB.

  @n[util_parseScaproc Flow Diagram](../diagrams/util_parseScaproc.svg)

  @version 9.4
  @author Ahmed Al-Attar

**/

%MACRO util_parseScaproc(p_inScaprocFileName=
, p_outDotFilePath=%sysfunc(pathname(WORK))
, p_outDotFileName=mydigraph.dot
, p_outDsName=
, p_includeAttrs_yn=Y
, p_includeSteps_yn=Y
, p_rankDir=TB) / minoperator;

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
    l_scaprocFolderName
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

  /* Initialize the collection and open the output file */
  %util_initScaproc(p_enableFlagName=g_enableScaProc
  , p_outFilePath=%str(&g_logsRoot)
  , p_outFileName=sca__util_parseScaproc.txt);

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
      %let l_scaprocFolderName =%sysfunc(prxchange(s/&l_scaprocFileName//,-1,&l_inScaprocFileName));
      %put &=l_scaprocFileName &=l_scaprocFolderName;
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

  %let l_outDotFileName = &p_outDotFileName;
  %if (%superq(l_outDotFileName) EQ ) %then
    %let l_outDotFileName = mydigraph.dot;

  %let l_outDsName = &p_outDsName;
  %if (%superq(l_outDsName) EQ ) %then
    %let l_outDsName=work.mydigraph;

  %let l_includeAttrs_yn = Y;
  %if (%superq(p_includeAttrs_yn) NE ) %then
    %let l_includeAttrs_yn = %upcase(&p_includeAttrs_yn);

  %let l_includeSteps_yn = Y;
  %if (%superq(p_includeSteps_yn) NE ) %then
    %let l_includeSteps_yn = %upcase(&p_includeSteps_yn);

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

  /* Read the SCA text file */
  DATA work.scadata(DROP=rc);
    INFILE "&l_inScaprocFileName" LRECL=1000 LENGTH=linelength truncover END=eof;
    LENGTH scaline $1000 file_linenum sca_linenum step 5 libref libname $8;
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
    	OUTPUT;
    	
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
    SELECT CATS('Z',max(2,ceil(log10(MAX(step)))),'.')
    INTO :l_stepFmt
    FROM  work.scadata;
  QUIT;
  %put &=l_stepFmt;

  /**
  * Find Concatenated Libraries 
  */
  DATA _null_;
    if (0) then SET work.sca_libs nobs=nobs;
    call symputx("l_concatLibObsCount",nobs);
  RUN;

  %put &=l_concatLibObsCount;
  %if (&l_concatLibObsCount GT 0) %then
  %do;
  DATA work.sca_libs;
    SET work.sca_libs(RENAME=(libref=start libname=label)) ;
    RETAIN fmtname '$cncatlibs' type 'C';
  RUN;
  PROC FORMAT cntlin=work.sca_libs; RUN;
  %end;
  PROC DELETE DATA=work.sca_libs; RUN;

  /* Identify Input & Output data sets and their variables */
  DATA work.scadata1 (KEEP=scaline sca_linenum step )
  		work.step_inputs (KEEP=step sca_linenum dsname variable type filename entity_name) 
  		work.step_outputs (KEEP=step sca_linenum dsname variable type filename entity_name)
  %if (&l_includeSteps_yn = Y) %then 
  %do; 
      work.step_runtimes (KEEP=step step_start_dt ) 
      work.step_names(KEEP=step step_name type fmtname)
  %end;
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
    	step_name $30
    	entity_name $210;
    	
    FORMAT step_start_dt datetime22.2;
    filename='';
    
    OUTPUT work.scadata1;

  %if (&l_includeSteps_yn = Y) %then 
  %do; 
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
    ELSE 
  %end; /* End - (&l_includeSteps_yn = Y) */ 
     IF ( (indexw(scaline,"ATTR") > 0) 
              OR (indexw(scaline,"DATASET") > 0)
              OR (indexw(scaline,"FILE") > 0) ) THEN 
    DO;
	    IF (indexw(scaline,"ATTR") > 0) THEN 
	    DO;
	    	scaline  = SUBSTR(scaline,14);
	    	dsname   = SCAN(scaline,2,' ');
        dsname   = CATX('.',SCAN(dsname,1,'.'),SCAN(dsname,2,'.'));
    %if (&l_concatLibObsCount GT 0) %then
    %do;
	    	dsname   = CATX('.',PUT(SCAN(dsname,1,'.'),$cncatlibs.),SCAN(dsname,2,'.'));
    %end;
  			in_flag  = indexw(scaline,"INPUT");

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
	    ELSE IF (indexw(scaline,"FILE") > 0) THEN 
	    DO;
	    	scaline  = SUBSTR(scaline,14);
	    	filename = SCAN(scaline,3,' ');
        in_flag  = indexw(scaline,"INPUT");
	    END;
	    ELSE IF (indexw(scaline,"DATASET") > 0) THEN 
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

  %if (&l_includeSteps_yn = Y) %then 
  %do; 
  /* Create the stepname format */
  PROC FORMAT CNTLIN=WORK.STEP_NAMES(RENAME=(step=start step_name=label)); RUN;
  %end;

  /** -------
  * Compose logical steps in_out flow
   ------- */
  DATA work.in_out_info_v / VIEW=work.in_out_info_v;
    SET work.step_inputs(WHERE=(filename NOT IN ('SEQ','MULTI')))
        work.step_outputs(WHERE=(filename NOT IN ('SEQ','MULTI'))) INDSNAME=src_ds ;
    source=src_ds;
  RUN;
  PROC SORT DATA=work.in_out_info_v OUT=work.in_out_info_flow;
    BY step dsname;
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
      ,CAT(STRIP(dsname),'_(',PUT(col_cnt,z3.),')') as label length=210
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
    order by 1;

    /* Unique Input/Output External Files */
    CREATE TABLE work.unqEntity_fmtds_f AS
    SELECT DISTINCT STRIP(entity_name) as start length=210
      , STRIP(entity_name) as label length=210
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

  /* Clean-up */
  PROC DATASETS LIB=WORK NOLIST; 
    DELETE unqEntity_fmtds: columns ds_col_cnts; 
  RUN; QUIT;

  /** ------
  * Declare internal macro to process flow records
   ------- */
  %macro inner_getFlowDataSet(p_inDsName=
    , p_outDsName=
    , p_byVarName=
    , p_byVarNames=
    , p_stpsInputsDsName=
    , p_stpsOutputsDsName=);

    /* Merge the data into one record for each step */
    DATA &p_outDsName (KEEP=step step_name 'in'n 'out'n in_dsname out_dsname in_filename out_filename in_entity_name out_entity_name);
      LENGTH 
        step 5
        step_name $30
        in_dsname out_dsname $200
        in_filename out_filename 'in'n 'out'n $200
        in_entity_name out_entity_name $210;

      RETAIN step step_name in_dsname out_dsname in_filename out_filename in_entity_name out_entity_name;
 
      DO UNTIL (last.&p_byVarName);
        SET &p_inDsName(RENAME=(step=ds_step 
          dsname=ds_dsname filename=ds_filename entity_name=ds_entity_name));
        BY &p_byVarNames;

        if (first.&p_byVarName) then
          call missing (step, step_name, in_dsname, out_dsname, in_filename, out_filename, in_entity_name, out_entity_name);

        if (step = .) then step = ds_step;
        if (step_name = '') then step_name = '_'||CATX("_",PUT(ds_step,&l_stepFmt.),PUT(ds_step,stepname.));
        if (in_dsname = '')  then if (strip(source)="&p_stpsInputsDsName") then in_dsname = ds_dsname;
        if (in_filename = '')  then if (strip(source)="&p_stpsInputsDsName") then in_filename = ds_filename;
        if (in_entity_name = '')  then if (strip(source)="&p_stpsInputsDsName") then in_entity_name = PUT(ds_entity_name,$unqEntity.);
        if (out_dsname = '')  then if (strip(source)="&p_stpsOutputsDsName") then out_dsname = ds_dsname;
        if (out_filename = '')  then if (strip(source)="&p_stpsOutputsDsName") then out_filename = ds_filename;
        if (out_entity_name = '')  then if (strip(source)="&p_stpsOutputsDsName") then out_entity_name = PUT(ds_entity_name,$unqEntity.);
      END;
      'in'n  = coalescec(in_dsname,in_filename);  
      'out'n = coalescec(out_dsname,out_filename);
      if (CATS(in_dsname,out_dsname,in_filename,out_filename,in_entity_name,out_entity_name) ne '') then
      OUTPUT;
    RUN;
  %mend inner_getFlowDataSet;

  /** --------
  * Merge the data into one record for each step 
   ------- */
  PROC SORT DATA=work.in_out_info_flow;
    BY step entity_name;
  RUN;
  %inner_getFlowDataSet(p_inDsName=work.in_out_info_flow
  , p_outDsName=work.steps_in_out_flow
  , p_byVarName=ds_entity_name
  , p_byVarNames=%str(ds_step ds_entity_name)
  , p_stpsInputsDsName=WORK.STEP_INPUTS
  , p_stpsOutputsDsName=WORK.STEP_OUTPUTS)

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
  %put &=l_impExpSteps &=l_impExpStepsCnt;

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

    %put &=l_dataSteps &=l_impExpStepsExclude;

    PROC SORT DATA=work.in_out_info_flow(WHERE=(step in (&l_impExpStepsExclude))) 
      OUT=work.impExp_flow;
      BY step sca_linenum;
    RUN;

    /* Merge the data into one record for each step */
    %inner_getFlowDataSet(p_inDsName=work.impExp_flow
    , p_outDsName=work.impExp_flow2
    , p_byVarName=ds_step
    , p_byVarNames=%str(ds_step sca_linenum)
    , p_stpsInputsDsName=WORK.STEP_INPUTS
    , p_stpsOutputsDsName=WORK.STEP_OUTPUTS);

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
  PROC SORT DATA=work.steps_in_out_flow(KEEP=step step_name 'in'n 'out'n in_entity_name out_entity_name);
    BY step step_name 'in'n 'out'n in_entity_name out_entity_name;
  RUN;

  DATA work.single_line_dag (KEEP=step step_name ins outs in_entities out_entities);
    if (0) then SET work.steps_in_out_flow;
    LENGTH 
      ins outs $4000 
      in_entities out_entities $8000;

    DO UNTIL (last.step);
      SET work.steps_in_out_flow;
      BY step;
      ins          = CATX(' ',ins,in);
      outs         = CATX(' ',outs,out);
      in_entities  = CATX(' ',in_entities,in_entity_name);
      out_entities = CATX(' ',out_entities,out_entity_name);
    END;
    OUTPUT;
  RUN;

  /* Find unique Entities */
  PROC SQL;
    CREATE TABLE unique_in_out_entities AS
    SELECT DISTINCT entity_name
    FROM
    (SELECT in_entity_name as entity_name
     FROM work.steps_in_out_flow
     UNION
     SELECT out_entity_name as entity_name
     FROM work.steps_in_out_flow);
  QUIT;

  PROC DELETE DATA=work.STEPS_IN_OUT_FLOW; RUN;

  %if (&l_includeSteps_yn = Y) %then
  %do;
    /* Calculate steps run-time duration */
    DATA work.step_runtimes2;
     if (eof=0) then set work.step_runtimes(firstobs=2 keep=step_start_dt rename=(step_start_dt=step_end_dt)) end=eof;
     else step_end_dt=.;
     
     SET work.step_runtimes;
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
     	, CAT('_',PUT(a.step,&l_stepFmt.),'_',STRIP(a.step_name),' (',STRIP(PUT(b.runtime_duration,time12.2)),')') as step_name LENGTH=30
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

    /* Clean-up */
    PROC DATASETS lib=work nolist;
     DELETE step_names: step_runtimes:; 
    QUIT; RUN;
  %end; /* End - (&l_includeSteps_yn = Y) */

  * ---------------;
  * Create .DOT directives to make a diagram ;
  * ---------------;
  * First: Produce header lines;
  DATA work._flow_start_ ;
    LENGTH line $6000;

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
    OUTPUT ;    
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
          '"'||STRIP(PUT(flow.entity_name,$unqEntity.))||'" [ shape="note" label="'|| STRIP(flow.filename)||'" color=lightgreen style=filled ]'
        /* Data set */
        /*WHEN (STRIP(flow.dsname) ne '' AND STRIP(flow.filename) eq '') THEN*/
        ELSE
          '"'||STRIP(PUT(flow.entity_name,$unqEntity.))||'" [label=<<TABLE BORDER="0" CELLSPACING="0"><TR><TD BORDER="0" COLSPAN="2" CELLPADDING="0"><FONT POINT-SIZE="12">'
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
 
  /* Write out the Dag */
	DATA work._flow_dag_(KEEP=line) ;
		LENGTH line $6000 step_name $30;
    SET work.single_line_dag;
    BY step;

  %if (&l_includeSteps_yn = Y) %then 
  %do; 
    /* Step Definition */
		line = CATS('"',step_name,'"')||' [shape="oval" label="'||STRIP(SCAN(PUT(step,stepname.),2,'_'))||'" color=yellow style=filled ]';
    line = strip(line);
		OUTPUT;
    /* Inputs */
    line = CATX(' -> ',CATS('{"', TRANWRD(STRIP(in_entities),' ','" "') ,'"}'),CATS('{"',step_name,'"}'));
    line = strip(line);
    OUTPUT;
    /* Outputs */
    line = CATX(' -> ',CATS('{"',step_name,'"}'),CATS('{"', TRANWRD(STRIP(out_entities),' ','" "') ,'"}'));
    line = strip(line);
    OUTPUT;
  %end;
  %else
  %do;
    /* Inputs */
    line = CAT(CATS('{"', TRANWRD(STRIP(in_entities),' ','" "') ,'"}'),' -> ');
    line = strip(line);
    /* Outputs */
    line = CATX(' ',line,CATS('{"', TRANWRD(STRIP(out_entities),' ','" "') ,'"}'));
    OUTPUT;
  %end;
  RUN;
  
  * Last: Produce closing lines;
  DATA work._flow_end_ ;
    LENGTH line $6000;
    line= '}' ;
    OUTPUT ;
  RUN;

  DATA _NULL_;
    FILE "&l_outDotFilePath/&l_outDotFileName" lRECL=6000;
    SET work._flow_start_ 
      work._flow_attrs_
      work._flow_dag_
    	work._flow_end_;

    line=STRIP(line);
    PUT line;
  RUN;

  /* Clean-up */
  PROC DATASETS LIB=WORK NOLIST;
    DELETE _flow_: in_out_info_flow single_line_dag unique_in_out_entities;
  RUN; QUIT;

  /* Terminate the collection and close the output file */
  %util_termScaproc(p_enableFlagName=g_enableScaProc)

  %goto finished;

  %exit:
    %PUT *** ERROR: util_parseScaproc  ***;
    %PUT *** l_RC must be zero (0).   ***;
    %PUT *** l_RC= &l_RC    .         ***;
    %PUT *** &l_MSG ***;
    %PUT *** ERROR: util_parseScaproc  ***;

  %finished:
    %let l_eTime=%sysfunc(time());
    %let l_rTime=%sysfunc(putn(%sysevalf(&l_eTime - &l_sTime),time12.2));
    %PUT >>> util_parseScaproc :>>> Total RunTime = &l_rTime;

%MEND util_parseScaproc;
