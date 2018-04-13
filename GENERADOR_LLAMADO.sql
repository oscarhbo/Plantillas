SET DEFINE OFF;
SET SERVEROUTPUT ON SIZE 1000000;
DECLARE

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
             Position, Data_Type, In_Out, Defaulted, Default_Value, Data_Length, Data_Precision, TamaDato
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
        cCadena := cCadena ||LPAD(' ', nOffSet, ' ')||RPAD(i.NomVar, 25, ' ') || i.Data_Type||i.TamaDato||';'||CHR(10);
      ELSE
        cCadena := cCadena ||LPAD(' ', nOffSet, ' ')||RPAD(i.NomVar, 25, ' ') ||':= NULL;'||CHR(10);
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
                              pTipoCall         VARCHAR2 DEFAULT 'R') RETURN VARCHAR2 IS
    
    cCadena    VARCHAR2(32767);   
    nOffSet    NUMBER := 0;
    
    CURSOR CurParams IS
      SELECT Argument_Name , Position, Sequence, Data_Type, In_Out, Defaulted, Default_Value, Data_Length, Data_Precision,
             LOWER(SUBSTR(Argument_Name,1,1)) || OHBCASE(SUBSTR(Argument_Name,2)) ArgName
      FROM   ALL_ARGUMENTS 
      WHERE  Package_Name = pNombrePkg
      AND    Object_Name  = pNombreProc
      AND    Position > 0
      ORDER  BY Position;
  
  BEGIN
    --
    nOffSet := LENGTH(pNombrePkg) + LENGTH(pNombreProc) + pOffSet;
    
    FOR i IN CurParams LOOP
      IF(pTipoCall = 'R')THEN
        cCadena := cCadena ||LPAD(' ', nOffSet, ' ')||RPAD(i.ArgName, 25, ' ')||' => '||i.ArgName||','||CHR(10);
      ELSE
        cCadena := cCadena || i.ArgName||', ';
      END IF;
    END LOOP;
    
    cCadena := TRIM(SUBSTR(cCadena, 1, LENGTH(cCadena) - 2));
    
    RETURN cCadena;
    --
  END OBTENER_PARAMSCALL; 
  
  ------------------------------------------------------------------------------
  PROCEDURE GENERAR_LLAMADO(pNombrePkg        VARCHAR2, 
                            pNombreProc       VARCHAR2, 
                            pTipoCall         VARCHAR2 DEFAULT 'R') IS
    cCadena    VARCHAR2(32767);  
    nOffSet    NUMBER := 4;
  BEGIN
    cCadena := LPAD(' ', nOffSet, ' ') || OBTENER_REGRESO(pNombrePkg, pNombreProc);
    nOffSet := LENGTH(cCadena) + 2;
    cCadena := cCadena || pNombrePkg ||'.'|| pNombreProc || '(';
    cCadena := cCadena || OBTENER_PARAMSCALL(pNombrePkg, pNombreProc, nOffSet, pTipoCall) || ');';
    DBMS_OUTPUT.PUT_LINE(cCadena); 
  END GENERAR_LLAMADO;
  
BEGIN
  --GENERAR_LLAMADO('PR_COT_DATOS_PARTICULARES', 'BUSCAR_DATOS', 'R');
  dbms_output.put_line(OBTENER_VARDECLA('PR_COT_DATOS_PARTICULARES', 'BUSCAR_DATOS', 'V', 'A'));
END;