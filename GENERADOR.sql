SET DEFINE OFF;
SET SERVEROUTPUT ON SIZE 1000000;
DECLARE




  ------------------------------------------------------------------------------
  -- TERMINA SECCION DE FUNCIONES PARA   L L A M A D O
  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
  FUNCTION OBTENER_VARDECLA(pNombrePkg        VARCHAR2, 
                            pNombreProc       VARCHAR2, 
                            pTipoDecla        VARCHAR2 DEFAULT 'P', 
                            pSeccion          VARCHAR2 DEFAULT 'D') RETURN VARCHAR2 IS
    
    cCadena    VARCHAR2(32767);   
    nOffSet    NUMBER := 4;
    cNombRet   CONSTANT VARCHAR2(10) := 'Regreso';
    
    CURSOR CurParams IS
      SELECT DECODE(Position, 0, Prefijo||cNombRet,  DECODE(pTipoDecla, 'P', ArgNamePar1, Prefijo)||ArgNamePar2) NomVar, 
             Position, REPLACE(Data_Type, 'REF CURSOR', 'SYS_REFCURSOR') Data_Type, In_Out, Defaulted, Default_Value, 
             Data_Length, Data_Precision, TamaDato
      FROM   (
              SELECT Argument_Name , Position, Sequence, Data_Type, In_Out, Defaulted, Default_Value, Data_Length, Data_Precision,
                     DECODE(Data_Type, 'VARCHAR2', 'c', 'NUMBER', 'n', 'DATE', 'd', 'REF CURSOR', 'rf') Prefijo,
                     LOWER(SUBSTR(Argument_Name,1,1))ArgNamePar1, OHBCASE(SUBSTR(Argument_Name,2)) ArgNamePar2,
                     DECODE(Data_Type, 'VARCHAR2', '('||NVL(Data_Length, 200)||')', 'NUMBER', '', 'DATE', '', 'REF CURSOR', '') TamaDato
              FROM   ALL_ARGUMENTS 
              WHERE  Package_Name = pNombrePkg
              AND    Object_Name  = pNombreProc
              )
      ORDER  BY Position;
  
  BEGIN
    --
    FOR i IN CurParams LOOP
      IF(pSeccion = 'D')THEN
        cCadena := cCadena ||LPAD(' ', nOffSet, ' ')||RPAD(i.NomVar, 20, ' ') || i.Data_Type||i.TamaDato||';'||CHR(10);
      ELSE
        cCadena := cCadena ||LPAD(' ', nOffSet, ' ')||RPAD(i.NomVar, 20, ' ') ||':= NULL;'||CHR(10);
      END IF;
    END LOOP;
    
    RETURN cCadena;
    --
  END OBTENER_VARDECLA; 
  

  ------------------------------------------------------------------------------
  FUNCTION OBTENER_REGRESO(pNombrePkg        VARCHAR2, 
                           pNombreProc       VARCHAR2) RETURN VARCHAR2 IS
    
    cCadena    VARCHAR2(32767);   
    cNombRet   CONSTANT VARCHAR2(10) := 'Regreso';
    
    CURSOR CurParams IS
      SELECT DECODE(Data_Type, 'VARCHAR2', 'c', 'NUMBER', 'n', 'DATE', 'd', 'REF CURSOR', 'rf') Prefijo
      FROM   ALL_ARGUMENTS 
      WHERE  Package_Name = pNombrePkg
      AND    Object_Name  = pNombreProc
      AND    Position = 0
      ORDER  BY Position;
  
  BEGIN
    --
    FOR i IN CurParams LOOP
      cCadena :=  i.Prefijo || cNombRet || ' := ';
    END LOOP;
    
    RETURN cCadena;
    --
  END OBTENER_REGRESO; 

  ------------------------------------------------------------------------------
  FUNCTION OBTENER_PARAMSCALL(pNombrePkg        VARCHAR2, 
                              pNombreProc       VARCHAR2, 
                              pOffSet           NUMBER,
                              pTipoCall         VARCHAR2 DEFAULT 'R', 
                              pTipoDecla        VARCHAR2 DEFAULT 'P') RETURN VARCHAR2 IS
    
    cCadena    VARCHAR2(32767);   
    nOffSet    NUMBER := 0;
    
    CURSOR CurParams IS
      SELECT ArgNamePar1||ArgNamePar2 ArgName, DECODE(pTipoDecla, 'P', ArgNamePar1, Prefijo)||ArgNamePar2 ArgName2
      FROM   (
              SELECT DECODE(Data_Type, 'VARCHAR2', 'c', 'NUMBER', 'n', 'DATE', 'd', 'REF CURSOR', 'rf') Prefijo,
                     LOWER(SUBSTR(Argument_Name,1,1))ArgNamePar1, OHBCASE(SUBSTR(Argument_Name,2)) ArgNamePar2, Position
              FROM   ALL_ARGUMENTS 
              WHERE  Package_Name = pNombrePkg
              AND    Object_Name  = pNombreProc
              AND    Position > 0
              )
      ORDER  BY Position;      
  
  BEGIN
    --
    nOffSet := LENGTH(pNombrePkg) + LENGTH(pNombreProc) + pOffSet;
    
    FOR i IN CurParams LOOP
      IF(pTipoCall = 'R')THEN
        cCadena := cCadena ||LPAD(' ', nOffSet, ' ')||RPAD(i.ArgName, 20, ' ')||' => '||i.ArgName2||','||CHR(10);
      ELSE
        cCadena := cCadena || i.ArgName2||', ';
      END IF;
    END LOOP;
    
    cCadena := TRIM(SUBSTR(cCadena, 1, LENGTH(cCadena) - 2));
    
    RETURN cCadena;
    --
  END OBTENER_PARAMSCALL; 

  FUNCTION OBTENER_LLAMADO(pNombrePkg        VARCHAR2, 
                           pNombreProc       VARCHAR2, 
                           pTipoCall         VARCHAR2 DEFAULT 'R', 
                           pTipoDecla        VARCHAR2 DEFAULT 'P') RETURN VARCHAR2 IS
    cCadena    VARCHAR2(32767);  
    nOffSet    NUMBER := 4;
  BEGIN
    cCadena := LPAD(' ', nOffSet, ' ') || OBTENER_REGRESO(pNombrePkg, pNombreProc);
    nOffSet := LENGTH(cCadena) + 2;
    cCadena := cCadena || pNombrePkg ||'.'|| pNombreProc || '(';
    cCadena := cCadena || OBTENER_PARAMSCALL(pNombrePkg, pNombreProc, nOffSet, pTipoCall, pTipoDecla) || ');';
    
    RETURN cCadena;
  END OBTENER_LLAMADO;  

  ------------------------------------------------------------------------------
  FUNCTION REEMPLAZAR_VARANOM(pLinea           CODIGO_BASE.Linea%TYPE, 
                              pNombrePkg       VARCHAR2, 
                              pNombreProc      VARCHAR2, 
                              pTipoCall        VARCHAR2 DEFAULT 'R',
                              pTipoDecla       VARCHAR2 DEFAULT 'P') RETURN VARCHAR2 IS
    cLinea           VARCHAR2(32767);
    nPosVarDecla     NUMBER := 0;
    nPosVarAsigna    NUMBER := 0;
    nPosVarCall      NUMBER := 0;
    nPosBase         NUMBER := 0;
  BEGIN
    
    nPosVarDecla   := INSTR(pLinea, '&VARDECLA');
    nPosVarAsigna  := INSTR(pLinea, '&VARASIGNA');
    nPosVarCall    := INSTR(pLinea, '&VARLLAMADO');

    nPosBase      := INSTR(pLinea, '&BASE');
     
    IF(nPosVarDecla > 0 )THEN
      cLinea := OBTENER_VARDECLA(pNombrePkg, pNombreProc, pTipoDecla, 'D');
    ELSIF(nPosVarAsigna > 0 )THEN
      cLinea := OBTENER_VARDECLA(pNombrePkg, pNombreProc, pTipoDecla, 'A');
    ELSIF(nPosVarCall > 0 )THEN
      cLinea := OBTENER_LLAMADO(pNombrePkg, pNombreProc, pTipoCall, pTipoDecla);
    ELSIF(nPosBase > 0) THEN 
      cLinea := NULL;
    ELSE
      cLinea := REPLACE(REPLACE(pLinea, '&NAME', pNombrePkg), '&DATE', SYSDATE);
    END IF;  
    RETURN cLinea;
  END REEMPLAZAR_VARANOM;  
  
  
  ------------------------------------------------------------------------------
  -- TERMINA SECCION DE FUNCIONES PARA   L L A M A D O
  ------------------------------------------------------------------------------
  
  

  ------------------------------------------------------------------------------
  FUNCTION ARMAR_PARAM(pCadena             VARCHAR2,
                       pTable_Name         CODIGO_BASE.Nombre%TYPE, 
                       pColumn_Name        CODIGO_BASE.Nombre%TYPE, 
                       pOffSet        NUMBER) RETURN VARCHAR2 IS
  BEGIN
    RETURN pCadena||LPAD(' ', pOffSet, ' ')||RPAD('p'||OHBCASE(pColumn_Name), 30, ' ')||' '||pTable_Name||'.'||OHBCASE(pColumn_Name)||'%TYPE,'||CHR(10);
  END;

  ------------------------------------------------------------------------------
  FUNCTION OBTENER_PARAMS(pNombre        CODIGO_BASE.Nombre%TYPE, 
                          pOffSet        NUMBER,
                          pTipParam      VARCHAR2 DEFAULT 'T') RETURN VARCHAR2 IS
    
    cCadena    VARCHAR2(32767);   
    
    CURSOR CurParams IS
      SELECT Table_Name, Column_Name
      FROM   ALL_TAB_COLUMNS TC
      WHERE  Table_Name = pNombre
      ORDER BY Column_Id;

    CURSOR CurParamsPk IS
      SELECT CC.Table_Name, CC.Column_Name
      FROM   ALL_CONS_COLUMNS CC, ALL_CONSTRAINTS AC
      WHERE  CC.Table_Name  = pNombre
      AND    CC.Table_Name  = AC.Table_Name
      AND    CC.Constraint_Name = AC.Constraint_Name
      AND    AC.Constraint_Type = pTipParam
      ORDER BY CC.Position;      
  
  BEGIN
    --
    IF(pTipParam = 'T')THEN
      FOR i IN CurParams LOOP
        cCadena := ARMAR_PARAM(cCadena, i.Table_Name, i.Column_Name, pOffSet);
      END LOOP;
    ELSIF(pTipParam = 'P')THEN
      FOR i IN CurParamsPk LOOP
        cCadena := ARMAR_PARAM(cCadena, i.Table_Name, i.Column_Name, pOffSet);
      END LOOP;
    END IF;
    
    cCadena := TRIM(SUBSTR(cCadena, 1, LENGTH(cCadena)-2));
    
    RETURN cCadena;
    --
  END OBTENER_PARAMS;   

  ------------------------------------------------------------------------------
  FUNCTION OBTENER_CAMPINS(pNombre        CODIGO_BASE.Nombre%TYPE,        pPrefijo  VARCHAR2 DEFAULT NULL) RETURN VARCHAR2 IS
    cCadena       VARCHAR2(32767);   
    nCnt          NUMBER := 0;
    nNumRegsLin   CONSTANT NUMBER := 6;
    cSalto        VARCHAR2(40) := CHR(10)||RPAD(' ', 12, ' ');
    
    CURSOR CurCols IS
      SELECT RPAD(pPrefijo||OHBCASE(Column_Name)||',', 25, ' ') columna
      FROM   ALL_TAB_COLUMNS
      WHERE  Table_Name = pNombre
      ORDER BY Column_Id;    
  BEGIN
    --
    FOR i IN CurCols LOOP
      nCnt := nCnt + 1;
      cCadena := cCadena || i.columna;
      
      IF(MOD(nCnt, nNumRegsLin) = 0)THEN
        cCadena := cCadena || cSalto;
      END IF;  
    END LOOP;
    
    cCadena := SUBSTR(TRIM(cCadena), 1, LENGTH(TRIM(cCadena)) - 1);
    
    RETURN TRIM(cCadena);  
    --
  END OBTENER_CAMPINS;   

  ------------------------------------------------------------------------------
  FUNCTION OBTENER_DEFWHERE(pNombre        CODIGO_BASE.Nombre%TYPE,        
                            pTipParam      VARCHAR2 DEFAULT 'P',
                            pPrefijo       VARCHAR2 DEFAULT NULL ) RETURN VARCHAR2 IS
                            
    cCadena       VARCHAR2(32767);   
    
    CURSOR CurCols IS
      SELECT DECODE(CC.Position, 1, '', '    AND    ') || OHBCASE(CC.Column_Name)||' = '|| pPrefijo ||OHBCASE(CC.Column_Name)||CHR(10) columna
      FROM   ALL_CONS_COLUMNS CC, ALL_CONSTRAINTS AC
      WHERE  CC.Table_Name  = pNombre
      AND    CC.Table_Name  = AC.Table_Name
      AND    CC.Constraint_Name = AC.Constraint_Name
      AND    AC.Constraint_Type = pTipParam;    
  BEGIN
    --
    FOR i IN CurCols LOOP
      cCadena := cCadena || i.columna;
    END LOOP;
    
    cCadena := SUBSTR(TRIM(cCadena), 1, LENGTH(TRIM(cCadena)) - 1);
    
    RETURN TRIM(cCadena);  
    --
  END OBTENER_DEFWHERE;    


  ------------------------------------------------------------------------------
  FUNCTION OBTENER_DEFUPDATE(pNombre        CODIGO_BASE.Nombre%TYPE,        
                             pTipParam      VARCHAR2 DEFAULT 'P',
                             pPrefijo       VARCHAR2 DEFAULT NULL ) RETURN VARCHAR2 IS
                            
    cCadena       VARCHAR2(32767);   
    
    CURSOR CurCols IS
      SELECT LPAD(' ', 11, ' ') || RPAD(OHBCASE(TC.Column_Name), 20, ' ')||' = '|| 'NVL(' ||pPrefijo ||OHBCASE(TC.Column_Name)|| ', ' || OHBCASE(TC.Column_Name) || '),'||CHR(10) columna
      FROM   ALL_TAB_COLUMNS TC
      WHERE  TC.Table_Name = pNombre
     /* AND    TC.Column_Name NOT IN (SELECT CC.Column_Name
                                    FROM   ALL_CONS_COLUMNS CC, ALL_CONSTRAINTS AC
                                    WHERE  CC.Table_Name      = pNombre
                                    AND    CC.Table_Name      = AC.Table_Name
                                    AND    CC.Constraint_Name = AC.Constraint_Name
                                    AND    AC.Constraint_Type = pTipParam)*/
      ORDER BY TC.Column_Id;   
      
    CURSOR CurCols2 IS
      SELECT LPAD(' ', 11, ' ') || RPAD(OHBCASE(Column_Name), 20, ' ')||' = '|| 'NVL(' ||pPrefijo ||OHBCASE(Column_Name)|| ', ' || OHBCASE(Column_Name) || '),'||CHR(10) columna
      FROM   (
              SELECT TC.Column_Name, TC.Column_Id
              FROM   ALL_TAB_COLUMNS TC
              WHERE  TC.Table_Name = pNombre
              MINUS
              SELECT CC.Column_Name, TC.Column_Id
              FROM   ALL_CONS_COLUMNS CC, ALL_CONSTRAINTS AC, ALL_TAB_COLUMNS TC
              WHERE  CC.Table_Name      = pNombre
              AND    CC.Table_Name      = TC.Table_Name
              AND    CC.Column_Name     = TC.Column_Name
              AND    CC.Table_Name      = AC.Table_Name
              AND    CC.Constraint_Name = AC.Constraint_Name
              AND    AC.Constraint_Type = pTipParam
              )                              
      ORDER BY Column_Id;   
      
  BEGIN
    --
    FOR i IN CurCols2 LOOP
      cCadena := cCadena || i.Columna;
    END LOOP;
    
    cCadena := SUBSTR(TRIM(cCadena), 1, LENGTH(TRIM(cCadena)) - 2);
    
    RETURN TRIM(cCadena);  
    --
  END OBTENER_DEFUPDATE;    
  
  ------------------------------------------------------------------------------
  FUNCTION CONVERTER_FEC(pFecGen        DATE) RETURN VARCHAR2 IS
  BEGIN
    RETURN TO_CHAR(pFecGen, 'DD/MM/YYYY');
  END CONVERTER_FEC;
  
  ------------------------------------------------------------------------------
  FUNCTION REEMPLAZAR_VAR(pLinea         CODIGO_BASE.Linea%TYPE, 
                          pNombre        CODIGO_BASE.Nombre%TYPE, 
                          pFecGen        DATE DEFAULT SYSDATE, 
                          pOffSet        NUMBER DEFAULT 20, 
                          pTipParam      VARCHAR2 DEFAULT 'T') RETURN VARCHAR2 IS
    cLinea           VARCHAR2(32767);
    nPosParams       NUMBER := 0;
    nPosCampIns      NUMBER := 0;
    nPosValsIns      NUMBER := 0;
    nPosBase         NUMBER := 0;
    nPosDefWhere     NUMBER := 0;
    nPosDefUpdate    NUMBER := 0;
  BEGIN
    
    nPosParams    := INSTR(pLinea, '&PARAMS');
    nPosCampIns   := INSTR(pLinea, '&CAMPINS');
    nPosValsIns   := INSTR(pLinea, '&VALSINS');
    nPosDefWhere  := INSTR(pLinea, '&DEFWHERE');
    nPosDefUpdate := INSTR(pLinea, '&DEFUPDATE');
    nPosBase      := INSTR(pLinea, '&BASE');
     
    IF(nPosParams > 0 )THEN
      cLinea := SUBSTR(pLinea, 1, nPosParams - 1);
      cLinea := cLinea || OBTENER_PARAMS(pNombre, pOffSet, pTipParam);
      cLinea := cLinea || SUBSTR(pLinea, nPosParams + 7);
    ELSIF(nPosCampIns > 0 )THEN
      cLinea := SUBSTR(pLinea, 1, nPosCampIns - 1);
      cLinea := cLinea || OBTENER_CAMPINS(pNombre);
      cLinea := cLinea || SUBSTR(pLinea, nPosCampIns + 8);
    ELSIF(nPosValsIns > 0 )THEN
      cLinea := SUBSTR(pLinea, 1, nPosValsIns - 1);
      cLinea := cLinea || OBTENER_CAMPINS(pNombre, 'p');
      cLinea := cLinea || SUBSTR(pLinea, nPosValsIns + 8);
    ELSIF(nPosDefWhere > 0 )THEN
      cLinea := SUBSTR(pLinea, 1, nPosDefWhere - 1);
      cLinea := cLinea || OBTENER_DEFWHERE(pNombre, 'P', 'p');
      cLinea := cLinea || SUBSTR(pLinea, nPosDefWhere + 9);
    ELSIF(nPosDefUpdate > 0 )THEN
      cLinea := SUBSTR(pLinea, 1, nPosDefUpdate - 1);
      cLinea := cLinea || OBTENER_DEFUPDATE(pNombre, 'P', 'p');
    ELSIF(nPosBase > 0) THEN 
      cLinea := NULL;
    ELSE
      cLinea := REPLACE(REPLACE(pLinea, '&NAME', pNombre), '&DATE', pFecGen);
    END IF;  
    RETURN cLinea;
  END REEMPLAZAR_VAR;

  ------------------------------------------------------------------------------
  PROCEDURE GENERAR_SUBPROC(pNombreBase    CODIGO_BASE.Nombre%TYPE,
                            pNombre        CODIGO_BASE.Nombre%TYPE, 
                            pFecGen        DATE DEFAULT SYSDATE, 
                            pIndSpec       CODIGO_BASE.IndSpec%TYPE DEFAULT 'C', 
                            pOffSet        NUMBER DEFAULT 20, 
                            pTipParam      VARCHAR2 DEFAULT 'T') IS
    
    nNivel        CODIGO_BASE.Nivel%TYPE    := 2;
    cDescFecha    VARCHAR2(10);
    
    CURSOR CurBase IS
    SELECT Nombre, Tipo, Numlinea, Linea, Nivel
    FROM   CODIGO_BASE
    WHERE  Nombre  = pNombreBase
    AND    Nivel   = nNivel
    AND    IndSpec = DECODE(pIndSpec, 'C', IndSpec, pIndSpec)
    ORDER BY Tipo, Numlinea;
    
  BEGIN
    cDescFecha := CONVERTER_FEC(pFecGen);  
    
    FOR i IN CurBase LOOP
      DBMS_OUTPUT.PUT_LINE(REEMPLAZAR_VAR(i.Linea, pNombre, pFecGen, pOffSet, pTipParam));
    END LOOP;
  END GENERAR_SUBPROC; 
  
  ------------------------------------------------------------------------------
  PROCEDURE GENERAR_PROCCREAR(pNombre        CODIGO_BASE.Nombre%TYPE, 
                              pFecGen        DATE DEFAULT SYSDATE, 
                              pIndSpec       CODIGO_BASE.IndSpec%TYPE DEFAULT 'C') IS
    
    cNombreBase    CODIGO_BASE.Nombre%TYPE  := 'BASECREAR';
    nOffSet        CONSTANT NUMBER := 18;
    cTipParam      CONSTANT VARCHAR2(1) := 'T';
  BEGIN
    GENERAR_SUBPROC(cNombreBase, pNombre, pFecGen, pIndSpec, nOffSet, cTipParam);
  END GENERAR_PROCCREAR;

  ------------------------------------------------------------------------------
  PROCEDURE GENERAR_PROCELIMINAR(pNombre        CODIGO_BASE.Nombre%TYPE, 
                              pFecGen        DATE DEFAULT SYSDATE, 
                              pIndSpec       CODIGO_BASE.IndSpec%TYPE DEFAULT 'C') IS
    
    cNombreBase    CODIGO_BASE.Nombre%TYPE  := 'BASEELIMINAR';
    nOffSet        CONSTANT NUMBER := 21;
    cTipParam      CONSTANT VARCHAR2(1) := 'P';
  BEGIN
    GENERAR_SUBPROC(cNombreBase, pNombre, pFecGen, pIndSpec, nOffSet, cTipParam);
  END GENERAR_PROCELIMINAR;  

  ------------------------------------------------------------------------------
  PROCEDURE GENERAR_PROCACTUALIZAR(pNombre        CODIGO_BASE.Nombre%TYPE, 
                                   pFecGen        DATE DEFAULT SYSDATE, 
                                   pIndSpec       CODIGO_BASE.IndSpec%TYPE DEFAULT 'C') IS
    
    cNombreBase    CODIGO_BASE.Nombre%TYPE  := 'BASEACTUALIZAR';
    nOffSet        CONSTANT NUMBER := 21;
    cTipParam      CONSTANT VARCHAR2(1) := 'P';
  BEGIN
    GENERAR_SUBPROC(cNombreBase, pNombre, pFecGen, pIndSpec, nOffSet, cTipParam);
  END GENERAR_PROCACTUALIZAR;    

  ------------------------------------------------------------------------------
  PROCEDURE GENERAR_SUBRUTINA(pLinea         CODIGO_BASE.Linea%TYPE, 
                              pNombre        CODIGO_BASE.Nombre%TYPE, 
                              pFecGen        DATE DEFAULT SYSDATE, 
                              pIndSpec       CODIGO_BASE.IndSpec%TYPE) IS
    cCadena    VARCHAR2(1000);   
  BEGIN
    --
    IF(TRIM(pLinea) = '&BASECREAR')THEN
      GENERAR_PROCCREAR(pNombre, pFecGen, pIndSpec);
    ELSIF(TRIM(pLinea) = '&BASEELIMINAR')THEN
      GENERAR_PROCELIMINAR(pNombre, pFecGen, pIndSpec);
    ELSIF(TRIM(pLinea) = '&BASEACTUALIZAR')THEN
      GENERAR_PROCACTUALIZAR(pNombre, pFecGen, pIndSpec);
    END IF;
    --
  END GENERAR_SUBRUTINA;    
  
  ------------------------------------------------------------------------------
  PROCEDURE REEMPLAZAR_SUBPROC(pLinea         CODIGO_BASE.Linea%TYPE, 
                               pNombre        CODIGO_BASE.Nombre%TYPE, 
                               pFecGen        DATE DEFAULT SYSDATE, 
                               pIndSpec       CODIGO_BASE.IndSpec%TYPE) IS
    cLinea           CODIGO_BASE.Linea%TYPE;
    nPosBase         NUMBER := 0;
  BEGIN
    
    nPosBase    := INSTR(pLinea, '&BASE');
    
    IF(nPosBase > 0) THEN
      GENERAR_SUBRUTINA(pLinea, pNombre, pFecGen, pIndSpec);
    END IF;  
  END REEMPLAZAR_SUBPROC;  

  ------------------------------------------------------------------------------
  PROCEDURE GENERAR_PAQUETE(pNombre        CODIGO_BASE.Nombre%TYPE, 
                            pFecGen        DATE DEFAULT SYSDATE) IS
    
    cNombreBase   CODIGO_BASE.Nombre%TYPE := 'PAQBASE';
    nNivel        CODIGO_BASE.Nivel%TYPE    := 1;
    cDescFecha    VARCHAR2(10);
    
    CURSOR CurBase IS
    SELECT Nombre, Tipo, Numlinea, Linea, Nivel, IndSpec
    FROM   CODIGO_BASE
    WHERE  Nombre = cNombreBase
    AND    Nivel  = nNivel
    ORDER BY Tipo, Numlinea;
    
  BEGIN
    cDescFecha := CONVERTER_FEC(pFecGen);  
    
    FOR i IN CurBase LOOP
      DBMS_OUTPUT.PUT_LINE(REEMPLAZAR_VAR(i.Linea, pNombre, pFecGen));
      REEMPLAZAR_SUBPROC(i.Linea, pNombre, pFecGen, i.IndSpec);
    END LOOP;
  END GENERAR_PAQUETE;

  ------------------------------------------------------------------------------
  PROCEDURE GENERAR_ANONIMO(pNombrePkg        VARCHAR2, 
                            pNombreProc       VARCHAR2, 
                            pTipoCall         VARCHAR2 DEFAULT 'R', -- R PARÁMETROS REFERENCIADOS, O  DIRECTOS
                            pTipoDecla        VARCHAR2 DEFAULT 'P') IS -- TIPO DECLARACIÓN DE VARIABLES P COMO PARAMETRO O  T DE ACUERDO A TIPO
    
    cNombreBase   CODIGO_BASE.Nombre%TYPE := 'CALLBASE';
    nNivel        CODIGO_BASE.Nivel%TYPE    := 1;
    cDescFecha    VARCHAR2(10);
    
    CURSOR CurBase IS
    SELECT Nombre, Tipo, Numlinea, Linea, Nivel, IndSpec
    FROM   CODIGO_BASE
    WHERE  Nombre = cNombreBase
    AND    Nivel  = nNivel
    ORDER BY Tipo, Numlinea;
    
  BEGIN
    
    FOR i IN CurBase LOOP
      DBMS_OUTPUT.PUT_LINE(REEMPLAZAR_VARANOM(i.Linea, pNombrePkg, pNombreProc, pTipoCall, pTipoDecla));
    END LOOP;
  END GENERAR_ANONIMO;  
  
BEGIN
  --GENERAR_PAQUETE('CDC_USUARIO', SYSDATE);
  --GENERAR_ANONIMO('PR_COT_DATOS_PARTICULARES', 'BUSCAR_DATOS', 'R', 'P');
  GENERAR_ANONIMO('PR_COTIZA_ALIANZAS', 'COTIZAR_AUTO', 'R', 'P');
END;


