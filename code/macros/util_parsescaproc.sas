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
 
      Usage:
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
    ;

  %let l_sTime=%sysfunc(time());

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

  /* Read the */
  DATA work.scadata;
    INFILE "&l_inScaprocFileName" LRECL=1000 LENGTH=linelength truncover END=eof;
    LENGTH scaline $1000 linenum step 5;
    INPUT @;
    INPUT scaline $varying1000. linelength;
    linenum+1;
    if (_n_=1 OR index(scaline,'STEP SOURCE FOLLOWS')) then step+1 ;
  RUN;

  /* Ignoring SASTemp outputs and Global Macro Variables */
  DATA work.scadata1;
    SET work.scadata(WHERE=((index(scaline,"JOBSPLIT") > 0) 
    and (index(scaline,"SYMBOL")=0)
    and (index(scaline,"CATALOG")=0)));
  RUN;

  /* Find Concatenated Libraries */
  DATA work.concatmem;
    set work.scadata1;
    LENGTH libname concatmem $8;
    where index(scaline, "/* JOBSPLIT: CONCATMEM ")=1;
    linenum = max(1, linenum -2);
    concatmem = put(scan(scaline, 4, " "), $20.);
    libname = put(scan(scaline, 5, " "), $20.);
  RUN;

  /* Process the lines and extract data set columns/attributes */
  DATA
    work.scadata2(DROP=word1-word2 word4-word7)
    work.ext_files(KEEP=word4)
  %if (&l_includeAttrs_yn EQ Y) %then
  %do;
    work.attrs(KEEP=word4-word7 lib_dsname linenum step RENAME=(word4=in_out))
  %end;
    ;

    LENGTH 
      word1-word7 $100 
      concatmem libname $8 
      dsname $32
      lib_dsname $41
      in out $200
      ;

    if (_n_=1) then
    do;
      dcl hash h_ds();
      h_ds.defineKey('word5');
      h_ds.defineData('lib_dsname');
      h_ds.defineDone(); 
    end;

    MERGE 
      scadata1(in=a)
      concatmem(in=b KEEP=linenum concatmem libname 
        RENAME=(concatmem=_concatmem libname=_libname))
    ;
    BY linenum;
    if a;

    if (b) then 
    do;
      concatmem=_concatmem;
      libname=_libname;
    end;

    if scaline=:'/* JOBSPLIT: ' ;
    word1=scan(scaline,2,' ') ;
    word2=scan(scaline,3,' ') ;
    word3=scan(scaline,4,' ') ;
    word4=scan(scaline,5,' ') ;
    word5=scan(scaline,6,' ') ;
    word6=scan(scaline,7,' ') ;
    word7=scan(scaline,8,' ') ;

    if (word2 in ('DATASET','ATTR')) then
    do;
      * exclude utility files;
      if (word2 = 'DATASET') then
      do;
        if (index(word5,'UTILITY')=0) then
        do;
          dsname = scan(word5,2,'.');
          if (libname ='') then libname = scan(word5,1,'.');

          lib_dsname = catx('.',libname,dsname);

          /* track unique lib_dsname values */
          h_ds.ref();
        end; /* (index(word5,'UTILITY')=0) */
        else delete;
      end; /* (word2 = 'DATASET') */
      else
      do; /* (word2 = 'ATTR') */
        %if (&l_includeAttrs_yn EQ Y) %then
        %do;
        if (h_ds.find(key:word3) = 0) then OUTPUT work.attrs;
        %end;
      end; /* (word2 = 'ATTR') */
    end;

    if ( (concatmem ^= " ") 
      AND (index(scaline, strip(concatmem))) ) then 
    do;
      concatmemfl="Y";
      word5=tranwrd(word5, strip(concatmem), strip(libname));
    end;

    if (word2='DATASET' & word3='INPUT')  then in=strip(word4)||'~'||scan(word5,1,'.')||'.'||scan(word5,2,'.') ;
    if (word2='DATASET' & word3='OUTPUT') then out=strip(word4)||'~'||scan(word5,1,'.')||'.'||scan(word5,2,'.') ;
    if (word2='DATASET' & word3='UPDATE') then out=strip(word4)||'~'||scan(word5,1,'.')||'.'||scan(word5,2,'.') ;
    if (word2='PROCNAME')                 then procname=word3 ;
    if (word2='ELAPSED')                  then elapsed=input(word3,8.3) ;
    if (word2='FILE' & word3='INPUT')     then do; in=strip(word4); word4=strip(word4); OUTPUT work.ext_files; end;
    if (word2='FILE' & word3='OUTPUT')    then do; out=strip(word4); word4=strip(word4); OUTPUT work.ext_files; end;

    OUTPUT work.scadata2;
    DROP _concatmem _libname;
  RUN;

  /* Clean-up */
  PROC DELETE DATA=work.concatmem work.scadata1 work.scadata; RUN;

  PROC SQL NOPRINT ;
    /* Find the Max Step count */
    SELECT CATS('Z',ceil(log10(MAX(step))),'.')
    INTO :l_stepFmt
    FROM  work.scadata2
    ;
  QUIT;

  /**
   * Find if there is Proc Export/Import steps 
  */
  %let l_impExpSteps=;
  %let l_dataSteps=;
  %let l_impExpStepsCnt=;
  %let l_impExpStepsExclude=;

  PROC SQL NOPRINT;
  
    create table work.with_known_out as
    select distinct out, step
    from work.scadata2
    group by out
    having count(*) = 1
    order by step;
    
    select distinct t1.step
    into :l_impExpSteps separated by ' '
    from work.scadata2 t1
    where t1.step not in
    (select t2.step from work.with_known_out t2)
    and t1.word3 in ('IMPORT', 'EXPORT')
    order by t1.step
    ;
  QUIT;
  PROC DELETE DATA=work.with_known_out; RUN;

  %let l_impExpStepsCnt = &sqlobs; 

  /** 
   * Perform special processing for Export/Import Step(s) 
  */
  %if (%superq(l_impExpSteps) NE ) %then
  %do;
    %do l_ei=1 %to &l_impExpStepsCnt;
      %let l_impExpStep = %scan(&l_impExpSteps,&l_ei,%str( ));
      %let l_dataStep   = %eval(&l_impExpStep + 1);
      %let l_dataSteps = &l_dataSteps,&l_dataStep;
      %let l_impExpStepsExclude = &l_impExpStepsExclude,&l_impExpStep,&l_dataStep ;
    %end;
    
    /* Remove leading comma */
    %let l_dataSteps = %substr(%superq(l_dataSteps),2);
    %let l_impExpStepsExclude = %substr(%superq(l_impExpStepsExclude),2);

    /* %put &=l_dataSteps &=l_impExpStepsExclude; */

    PROC SORT DATA=work.scadata2(WHERE=(step in (&l_impExpStepsExclude))) OUT=work.impExp_flow;
      BY step linenum;
    RUN;

    /* Merge the data into one record for each step */
    DATA work.impExp_flow2 (KEEP=step procname in_access out_access 'in'n 'out'n elapsed);
      LENGTH 
        step 5
        procname $104 
        in_access out_access $200 
        'in'n 'out'n $200 
        elapsed 8;
  
      RETAIN step procname in_access out_access 'in'n 'out'n elapsed;
      
      DO UNTIL (last.step_ds);
        SET work.impExp_flow(RENAME=(step=step_ds elapsed=elapsed_ds 'in'n=in_ds 'out'n=out_ds procname=procname_ds));
        BY step_ds;
        
        if (first.step_ds) then
          call missing (step , procname , in_access , out_access , 'in'n , 'out'n , elapsed);
  
        if (step = .)        then step=step_ds;
        if (scan(procname,3,'_') = '') then procname = "_"||put(step_ds,&l_stepFmt.)||"_"||strip(procname_ds);
        if (in_access = '')  then in_access = scan(in_ds,1,'~');
        if (out_access = '') then out_access = scan(out_ds,1,'~');
        if ('in'n = '')      then 'in'n = coalescec(scan(in_ds,2,'~'),in_access);
        if ('out'n = '')     then 'out'n = coalescec(scan(out_ds,2,'~'), out_access);
        if (elapsed = .)     then elapsed = elapsed_ds;
      END;
      if (elapsed) then OUTPUT;
    RUN;

    PROC SQL;
      CREATE TABLE work.impExp_flow AS
      SELECT a.step
        , a.procname
        , b.in_access
        , b.out_access
        , b.in
        , b.out
        , (a.elapsed + b.elapsed) as elapsed
      FROM work.impExp_flow2(WHERE=(step IN (&l_impExpSteps))) AS a
      FULL join work.impExp_flow2(WHERE=(step IN (&l_dataSteps))) AS b
      on a.step = (b.step -1);
    QUIT;
    
    PROC DELETE DATA=work.impExp_flow2; RUN;
  %end;
  
  PROC SQL NOPRINT;
    /* Merge the data into one record for each step */
    CREATE TABLE work.flow AS
    SELECT DISTINCT COALESCE(a.step,b.step,c.step) AS step
      ,"_"||put(coalesce(a.step,b.step,c.step),&l_stepFmt.)||"_"||strip(a.procname) as procname
      ,COALESCE(scan(b.in,1,'~'),scan(c.in,1,'~')) as in_access
      ,coalesce(scan(b.out,1,'~'),scan(c.out,1,'~')) as out_access
      ,coalesce(scan(b.in,2,'~'),scan(c.in,2,'~')) as in
      ,coalesce(scan(b.out,2,'~'),scan(c.out,2,'~')) as out
      ,d.elapsed
    FROM work.scadata2(where=(procname ^= '' %if(&l_impExpStepsCnt > 0) %then %do; AND NOT (step in (&l_impExpStepsExclude)) %end;)) as a
    FULL JOIN
      work.scadata2(where=(in ^= '' %if(&l_impExpStepsCnt > 0) %then %do; AND NOT (step in (&l_impExpStepsExclude)) %end;)) as b ON a.step=b.step
    FULL JOIN  
      work.scadata2(where=(out ^= '' %if(&l_impExpStepsCnt > 0) %then %do; AND NOT (step in (&l_impExpStepsExclude)) %end;)) as c ON a.step=c.step 
    LEFT JOIN
      work.scadata2(where=(elapsed>0 %if(&l_impExpStepsCnt > 0) %then %do; AND NOT (step in (&l_impExpStepsExclude)) %end;)) as d ON a.step=d.step 
    ORDER BY CALCULATED step
    ;
  QUIT;

  %if ((&l_impExpStepsCnt > 0 ) AND (%sysfunc(exist(work.impExp_flow)))) %then 
  %do;
    PROC APPEND BASE=work.flow DAta=work.impExp_flow FORCE; RUN;
    PROC DELETE DATA=work.impExp_flow; RUN;
  %end;

  /* Ensure unique flow steps */
  PROC SORT DATA=WORK.flow /*(WHERE=(STRIP(in) NE '' AND STRIP(out) NE ''))*/ NODUPKEY;
    BY step procname in_access in out;
  RUN;

  /* Clean-up */
  PROC DELETE DATA=work.scadata2; RUN;

  * ---------------;
  * Create .DOT directives to make a diagram ;
  * ---------------;
  * First: Produce header lines;
  DATA &l_outDsName ;
    LENGTH line $6000;

    line="// Generated by SAS for &l_inScaprocFileName" ;
    OUTPUT ;
    line='digraph dag {' ;
    OUTPUT ;
    line="rankdir=&l_rankDir";
    OUTPUT;
    /*line="graph [label=""\n\n&l_inScaprocFileName\n%sysfunc(datetime(),datetime.)""]" ;*/
    line="graph [label=""\n&l_inScaprocFileName""]" ;
    OUTPUT ;
    line='node [shape=cylinder color=lightblue style=filled]' ;
    OUTPUT ;
  RUN;

  %if (&l_includeAttrs_yn EQ Y) %then
  %do;

    /* ------------------------------ */
    /* Find unique entity definitions */
    /* ------------------------------ */
    
    /* Sort for proper processing */
    PROC SORT DATA=WORK.attrs;
      BY step in_out lib_dsname;
    RUN;
    
    /* Use Hash of Hash to ensure only unique entity attributes are stored */
    DATA attrs_unique(drop=find_rc eq_rc eq in_out);

      length eq 3;

      if (0) then set work.attrs;

      declare hash HoH(ordered:'a');
      HoH.definekey ('lib_dsname');
      HoH.definedata('h','iter','lib_dsname');
      HoH.definedone();

      declare hash h;
      declare hiter iter;
      declare hiter HoHiter("HoH");
      
      declare hash h2(ordered:'a', multidata:'yes');
      h2.definekey('lib_dsname','word5');
      h2.definedata('word5','word6','word7','lib_dsname','linenum','step');
      h2.definedone();
      declare hiter hi2('h2');
      
      /* Create a Hash object for every table (lib_dsname) */
      do until (eof);
        do until (last.lib_dsname);
        
          SET work.attrs End=eof;
          BY step in_out lib_dsname;
          
          /* Strip trailing spaces */
          lib_dsname=STRIP(lib_dsname);
          word5=STRIP(word5);
          word6=STRIP(word6);
          word7=STRIP(WORD7);
  
          /* Create a hash object for every table/entity and Populate it */
          if (first.lib_dsname) then
          do;
            h = _new_ hash(ordered:'a', multidata:'yes');
            h.definekey('lib_dsname','word5');
            h.definedata('word5','word6','word7','lib_dsname','linenum','step');
            h.definedone();
            iter = _new_ hiter ('h');
          end;
          h.add();
          
          /* 
           * Create a backup copy in case we needed to compare 
           * against a pre-processed entity definition 
          */
          h2.add(); 
          
        end; /* End - do until (last.lib_dsname) */

        /* Try to add the definition */

        find_rc = HoH.find();
        /* put find_rc=; */
        if  (find_rc ne 0) then 
        do;
          HoH.add();
        end;
        else
        do;
          /* Check if the entities attributes are matching */
          eq_rc = h.equals(hash: 'h2', result: eq);
          /* put eq_rc= eq=; */
        end;
        h2.clear();
      end; /* End - do until (eof) */
      
      /* By Now - we should have unique list of data sets */
      do while (HoHiter.next() = 0);
        do while (iter.next() = 0);
          OUTPUT attrs_unique;
        end;
      end; /* End - do while ... */
    RUN;

    /* Ensure the entities are ordered based on their appearance in the steps */
    PROC SORT DATA=attrs_unique;
      BY step linenum lib_dsname;
    RUN;

    /* Next: Generate Entity/Table definitions lines */
    DATA work.attrs_lines(KEEP=line);
      LENGTH line $6000; 

      DO UNTIL (last.lib_dsname);
        SET attrs_unique;
        BY step lib_dsname;

        /* Output Table name */
        if (first.lib_dsname) then 
        do;
          line =QUOTE(STRIP(lib_dsname))|| ' [label=<<TABLE BORDER="0" CELLSPACING="0"><TR><TD BORDER="0" COLSPAN="2" CELLPADDING="0">' 
                ||'<FONT POINT-SIZE="12">'||STRIP(lib_dsname)|| '</FONT></TD></TR>' ;
          OUTPUT;
        end;
        line ='<TR><TD BORDER="0" ALIGN="LEFT"><FONT POINT-SIZE="10">'||STRIP(SCAN(word5,2,':'))
              ||'</FONT></TD><TD BORDER="0" ALIGN="RIGHT"><FONT POINT-SIZE="10">'||IFC(scan(word6,2,':')='CHARACTER','char','num')
              ||' ('||STRIP(scan(word7,2,':'))||')</FONT></TD></TR>' ;
        OUTPUT;
      END;
      /* Close out the node definition */
      line = '</TABLE>>];';
      OUTPUT;
    RUN;
    PROC APPEND BASE=&l_outDsName DATA=work.attrs_lines FORCE; RUN;

    /* Clean-up */
    PROC DELETE DATA=work.attrs work.attrs_unique work.attrs_lines; RUN;
  %end;

  /* Next: Generate flow lines */
  DATA work.flow_lines (keep=line) ;
    LENGTH line $6000 style $12;
    
    if (0) then set work.ext_files;
    
    declare hash h(dataset:'work.ext_files');
    h.defineKey('word4');
    h.defineData('word4');
    h.defineDone();
    call missing (word4);

    /* Write out the steps/process definition */

    DO UNTIL (end);
      SET work.flow end=end ;
      BY step;
  
      in  = quote(coalescec(strip(in),strip(in_access))) ;
      out = quote(coalescec(strip(out),strip(out_access))) ;
      procname=lowcase(strip(procname)) ;
      style='style=solid ' ;

      %if (&l_includeSteps_yn = Y) %then
      %do;
        LENGTH in_prefix in_suffix out_prefix out_suffix prev_in prev_out $3000;
        RETAIN in_prefix in_suffix out_prefix out_suffix prev_in prev_out ;
        
        /* Step Definition */
        if (first.step) then
        do;
          line='"'||lowcase(strip(procname))||'"'||
          '[ shape="oval" label="'||lowcase(strip(procname))||' ('||strip(put(elapsed,8.3))||')" color=yellow style=filled ]';
          OUTPUT;

          /* Cater for File nodes */
          if (h.check(key:dequote(strip(out))) = 0) then
          do;
            line=strip(out)||
            '[ shape="note" label='||strip(out)||' color=lightgreen style=filled ]';
            OUTPUT;
          end;
          
          if (h.check(key:dequote(strip(in))) = 0) then
          do;
            line=strip(in)||
            '[ shape="note" label='||strip(in)||' color=lightgreen style=filled ]';
            OUTPUT;
          end;
          
          CALL MISSING(in_prefix,in_suffix,out_prefix,out_suffix,prev_in,prev_out,line);
        end;
        
        /* Single input - Single Output */
        if ((first.step = 1) AND (last.step = 1)) then
        do;
          if (compress(in,'"')>'') then line='{'||strip(in)||'} -> ';
          line=strip(line)||'{"'||lowcase(strip(procname))||'"}';
          if (compress(out,'"')>'') then line=strip(line)||' -> {'||strip(out)||'}';

          OUTPUT;
        end;
        else if ((first.step = 1) AND (last.step = 0)) then
        do;
          if (compress(in,'"')>'') then
          do;
            in_prefix =CATX(' ','{',strip(in));  
            in_suffix = ' -> {"'||lowcase(strip(procname))||'"}';
            prev_in = strip(in);
          end;
          if (compress(out,'"')>'') then
          do;
            out_prefix = '{"'||lowcase(strip(procname))||'"}';  
            out_suffix = ' -> {'||strip(out);
            prev_out = strip(out);
          end;
        end;
        else if ((first.step = 0) AND (last.step = 1)) then
        do;

          if ((compress(in,'"')>'') AND (prev_in ne strip(in))) then
            in_prefix =strip(in_prefix)||' '||strip(in)||'}';
          else if (strip(in_prefix) ne '') then
            in_prefix =CATS(in_prefix,'}');
          
          line=CATS(in_prefix,in_suffix);
          OUTPUT;

          if ((compress(out,'"')>'') AND (prev_out ne strip(out))) then
            out_suffix = strip(out_suffix)||' '||strip(out)||'}';
          else if (strip(out_suffix) ne '') then
            out_suffix = CATS(out_suffix,'}');
            
          line=cats(out_prefix,out_suffix);
          OUTPUT;
        end;
      %end; /* End - (&l_includeSteps_yn = Y) */
      %else
      %do;
        /* File Definition */
        if (first.step) then
        do;
          /* Cater for File nodes */
          if (h.check(key:dequote(strip(out))) = 0) then
          do;
            line=strip(out)||
            '[ shape="note" label='||strip(out)||' color=lightgreen style=filled ]';
            OUTPUT;
          end;
          
          if (h.check(key:dequote(strip(in))) = 0) then
          do;
            line=strip(in)||
            '[ shape="note" label='||strip(in)||' color=lightgreen style=filled ]';
            OUTPUT;
          end;
        end;
        /* Single input - Single Output */
        if ((first.step = 1) AND (last.step = 1)) then
        do;
          if (compress(in,'"')>'') then line='{'||strip(in)||'}';
          if (compress(out,'"')>'') then line=strip(line)||' -> {'||strip(out)||'}';
          OUTPUT;
        end;
        else if ((first.step = 1) AND (last.step = 0)) then
        do;
          if (compress(in,'"')>'') then
          do;
            in_prefix =CATX(' ','{',strip(in));
            prev_in = strip(in);
          end;
          if (compress(out,'"')>'') then
          do;
            out_suffix = ' -> {'||strip(out);
            prev_out = strip(out);
          end;
        end;
        else if ((first.step = 0) AND (last.step = 1)) then
        do;
          if ((compress(in,'"')>'') AND (prev_in ne strip(in))) then
            in_prefix =CATX(' ',in_prefix,strip(in),'}');
          else
            in_prefix =CATS(in_prefix,'}');

          if ((compress(out,'"')>'') AND (prev_out ne strip(out))) then
            out_suffix = CATX(' ',out_suffix,strip(out),'}');
          else
            out_suffix = CATS(out_suffix,'}');
            
          line=CATS(in_prefix,out_suffix);
          OUTPUT;
        end;
      %end; /* End - (&l_includeSteps_yn = N) */
  
      if (end) then 
      do ;
        line='}' ;
        OUTPUT ;
      end ;
    end;
    STOP;
  RUN ;
  
  DATA _NULL_;
    FILE "&l_outDotFilePath/&l_outDotFileName" lRECL=6000;
    SET &l_outDsName work.flow_lines;
    line=STRIP(line);
    PUT line;
  RUN;

  %if (%superq(p_outDsName) NE ) %then
  %do;
    PROC APPEND BASE=&l_outDsName DATA=work.flow_lines FORCE; RUN;
  %end;

  /* Clean-up */
  PROC DELETE DATA=work.flow work.flow_lines work.ext_files; RUN;

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
