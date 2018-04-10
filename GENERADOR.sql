SET DEFINE OFF;
SET SERVEROUTPUT ON SIZE 1000000;
DECLARE

  ------------------------------------------------------------------------------
  FUNCTION OBTENER_PARAMS(pNombre        CODIGO_BASE.Nombre%TYPE) RETURN VARCHAR2 IS
    
    cCadena    VARCHAR2(4000);   
    
    CURSOR CurParams IS
      SELECT Table_Name, Column_Name, Data_Type
      FROM   ALL_TAB_COLUMNS
      WHERE  Table_Name = pNombre
      ORDER BY Column_Id;
  
  BEGIN
    --
    FOR i IN CurParams LOOP
      cCadena := cCadena||LPAD(' ', 19, ' ')||RPAD('p'||OHBCASE(i.Column_Name), 30, ' ')||' '||i.Table_Name||'.'||OHBCASE(i.Column_Name)||'%TYPE,'||CHR(10);
    END LOOP;
    
    cCadena := TRIM(SUBSTR(cCadena, 1, LENGTH(cCadena)-2));
    
    RETURN cCadena;
    --
  END OBTENER_PARAMS;   

  ------------------------------------------------------------------------------
  FUNCTION OBTENER_CAMPINS(pNombre        CODIGO_BASE.Nombre%TYPE,        pPrefijo  VARCHAR2 DEFAULT NULL) RETURN VARCHAR2 IS
    cCadena       VARCHAR2(1000);   
    nCnt          NUMBER := 0;
    nNumRegsLin   CONSTANT NUMBER := 6;
    cSalto        VARCHAR2(40) := CHR(10)||RPAD(' ', 12, ' ');
    
    CURSOR CurCols IS
      SELECT RPAD(pPrefijo||OHBCASE(Column_Name)||',', 25, ' ') columna
      FROM   ALL_TAB_COLUMNS
      WHERE  Table_Name = pNombre;    
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
  FUNCTION CONVERTER_FEC(pFecGen        DATE) RETURN VARCHAR2 IS
  BEGIN
    RETURN TO_CHAR(pFecGen, 'DD/MM/YYYY');
  END CONVERTER_FEC;
  
  ------------------------------------------------------------------------------
  FUNCTION REEMPLAZAR_VAR(pLinea         CODIGO_BASE.Linea%TYPE, 
                          pNombre        CODIGO_BASE.Nombre%TYPE, 
                          pFecGen        DATE DEFAULT SYSDATE) RETURN VARCHAR2 IS
    cLinea           CODIGO_BASE.Linea%TYPE;
    nPosParams       NUMBER := 0;
    nPosCampIns      NUMBER := 0;
    nPosValsIns      NUMBER := 0;
    nPosBase         NUMBER := 0;
  BEGIN
    
    nPosParams  := INSTR(pLinea, '&PARAMS');
    nPosCampIns := INSTR(pLinea, '&CAMPINS');
    nPosValsIns := INSTR(pLinea, '&VALSINS');
    nPosBase    := INSTR(pLinea, '&BASE');
    
    IF(nPosParams > 0 )THEN
      cLinea := SUBSTR(pLinea, 1, nPosParams - 1);
      cLinea := cLinea || OBTENER_PARAMS(pNombre);
      cLinea := cLinea || SUBSTR(pLinea, nPosParams + 7);
    ELSIF(nPosCampIns > 0 )THEN
      cLinea := SUBSTR(pLinea, 1, nPosCampIns - 1);
      cLinea := cLinea || OBTENER_CAMPINS(pNombre);
      cLinea := cLinea || SUBSTR(pLinea, nPosCampIns + 8);
    ELSIF(nPosValsIns > 0 )THEN
      cLinea := SUBSTR(pLinea, 1, nPosValsIns - 1);
      cLinea := cLinea || OBTENER_CAMPINS(pNombre, 'p');
      cLinea := cLinea || SUBSTR(pLinea, nPosValsIns + 8);
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
                            pIndSpec       CODIGO_BASE.IndSpec%TYPE DEFAULT 'C') IS
    
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
      DBMS_OUTPUT.PUT_LINE(REEMPLAZAR_VAR(i.Linea, pNombre, pFecGen));
    END LOOP;
  END GENERAR_SUBPROC; 
  
  ------------------------------------------------------------------------------
  PROCEDURE GENERAR_PROCCREAR(pNombre        CODIGO_BASE.Nombre%TYPE, 
                              pFecGen        DATE DEFAULT SYSDATE, 
                              pIndSpec       CODIGO_BASE.IndSpec%TYPE DEFAULT 'C') IS
    
    cNombreBase    CODIGO_BASE.Nombre%TYPE  := 'BASECREAR';
  BEGIN
    GENERAR_SUBPROC(cNombreBase, pNombre, pFecGen, pIndSpec);
  END GENERAR_PROCCREAR;

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
  
BEGIN
  GENERAR_PAQUETE('CDC_USUARIO', SYSDATE);
  --GENERAR_PROCCREAR('CDC_USUARIO', SYSDATE);
END;


