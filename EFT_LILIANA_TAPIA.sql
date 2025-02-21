-- ******************************************************************************************
--                            EFT: LILIANA TAPIA
-- ******************************************************************************************
SET SERVEROUTPUT ON;

-----------------------------------------------------
-- ESPECIFICACIÓN DEL PACKAGE: PKG_PUNTAJE_EXTRA
-- Contiene la variable global para el puntaje extra y
-- la función para calcularlo según las reglas:
-- Se calcula si las horas totales > 30 y la cantidad de establecimientos > 1.
-----------------------------------------------------
CREATE OR REPLACE PACKAGE PKG_PUNTAJE_EXTRA AS
    V_PUNTAJE_EXTRA NUMBER(10); -- Variable global para almacenar el puntaje extra calculado
    FUNCTION FN_CALC_PUNTAJE_EXTRA(
        P_SUMA_PUNTAJES IN NUMBER,
        P_HORAS_TOTALES IN NUMBER,
        P_NUM_ESTAB     IN NUMBER,
        P_PORCENTAJE    IN NUMBER
    ) RETURN NUMBER;
END PKG_PUNTAJE_EXTRA;
/
-----------------------------------------------------
-- CUERPO DEL PACKAGE: PKG_PUNTAJE_EXTRA
-----------------------------------------------------
CREATE OR REPLACE PACKAGE BODY PKG_PUNTAJE_EXTRA AS
    FUNCTION FN_CALC_PUNTAJE_EXTRA(
        P_SUMA_PUNTAJES IN NUMBER,
        P_HORAS_TOTALES IN NUMBER,
        P_NUM_ESTAB     IN NUMBER,
        P_PORCENTAJE    IN NUMBER
    ) RETURN NUMBER IS
    BEGIN
    -- Se verifica la condición para asignar el puntaje extra:
        -- Más de 1 establecimiento y más de 30 horas totales.
        IF (P_NUM_ESTAB > 1 AND P_HORAS_TOTALES > 30) THEN
        -- Se aplica el porcentaje (dividido por 100) a la suma de puntajes base.
            RETURN ROUND(P_SUMA_PUNTAJES * (P_PORCENTAJE / 100));
        ELSE
            RETURN 0;
        END IF;
    END FN_CALC_PUNTAJE_EXTRA;
END PKG_PUNTAJE_EXTRA;
/
-----------------------------------------------------
-- FUNCION: FN_FORMAT_RUT
-- Formatea el RUT recibiendo el número y el dígito verificador
-- Devuelve el RUT en formato: XX.XXX.XXX-X
-----------------------------------------------------
CREATE OR REPLACE FUNCTION FN_FORMAT_RUT(
    p_numrun IN VARCHAR2,
    p_dvrun  IN VARCHAR2
) RETURN VARCHAR2 IS
    -- Se rellena el RUN con ceros a la izquierda hasta tener 8 dígitos
    v_run_limpio     VARCHAR2(20);
     -- Se construye el formato concatenando las partes y agregando puntos y guion
    v_run_formateado VARCHAR2(20);
BEGIN
    v_run_limpio := LPAD(p_numrun, 8, '0');
    v_run_formateado := SUBSTR(v_run_limpio, 1, 2) || '.' ||
                        SUBSTR(v_run_limpio, 3, 3) || '.' ||
                        SUBSTR(v_run_limpio, 6, 3) || '-' || p_dvrun;
    RETURN v_run_formateado;
END;
/
-----------------------------------------------------
-- FUNCIÓN: FN_PUNTAJE_EXPERIENCIA
-- Calcula el puntaje de experiencia a partir de la fecha de contrato más antigua
-----------------------------------------------------
CREATE OR REPLACE FUNCTION FN_PUNTAJE_EXPERIENCIA(
    P_NUMRUN IN VARCHAR2
) RETURN NUMBER IS
    V_EARLIEST_DATE DATE;
    V_ANIOS         NUMBER(2);
    V_PUNTAJE       NUMBER(10) := 0;
BEGIN
    -- Obtener la fecha de contrato más antigua
    SELECT MIN(FECHA_CONTRATO)
      INTO V_EARLIEST_DATE
      FROM ANTECEDENTES_LABORALES
     WHERE NUMRUN = P_NUMRUN;
      
    -- Calcular años de experiencia
    V_ANIOS := FLOOR(MONTHS_BETWEEN(SYSDATE, V_EARLIEST_DATE) / 12);
        
    -- Obtener el puntaje según los años de experiencia
    SELECT A.PTJE_EXPERIENCIA
      INTO V_PUNTAJE
      FROM PTJE_ANNOS_EXPERIENCIA A
     WHERE V_ANIOS BETWEEN A.RANGO_ANNOS_INI AND A.RANGO_ANNOS_TER;
     
    RETURN V_PUNTAJE;
    
  -- Se captura la excepción si no se encuentra información y se registra el error   
EXCEPTION
    WHEN NO_DATA_FOUND THEN
         INSERT INTO ERROR_PROCESO (NUMRUN, RUTINA_ERROR, MENSAJE_ERROR)
         VALUES (P_NUMRUN, 'FN_PUNTAJE_EXPERIENCIA', 'ORA-01403: No se ha encontrado ningún dato');
         RETURN 0;
    WHEN OTHERS THEN
         DECLARE
             v_error_msg VARCHAR2(4000);
         BEGIN
             v_error_msg := SQLERRM;
             INSERT INTO ERROR_PROCESO (NUMRUN, RUTINA_ERROR, MENSAJE_ERROR)
             VALUES (P_NUMRUN, 'FN_PUNTAJE_EXPERIENCIA', v_error_msg);
             RETURN 0;
         END;
END;
/
-----------------------------------------------------
-- FUNCIÓN: FN_PUNTAJE_PAIS
-- Obtiene el puntaje asociado al país del postulante
-----------------------------------------------------
CREATE OR REPLACE FUNCTION FN_PUNTAJE_PAIS(
    P_NUMRUN IN VARCHAR2
) RETURN NUMBER IS
   V_PUNTAJE  NUMBER(10);
BEGIN
   BEGIN
      -- Obtener el puntaje del país al que postula el candidato mediante el join de las tablas correspondientes
      SELECT P.PTJE_PAIS
        INTO V_PUNTAJE
        FROM POSTULACION_PASANTIA_PERFEC PP
             INNER JOIN PASANTIA_PERFECCIONAMIENTO PF ON PP.COD_PROGRAMA = PF.COD_PROGRAMA
             INNER JOIN INSTITUCION I ON PF.COD_INST = I.COD_INST
             INNER JOIN PTJE_PAIS_POSTULA P ON I.COD_PAIS = P.COD_PAIS
       WHERE PP.NUMRUN = P_NUMRUN;
   EXCEPTION
    -- Si no se encuentran datos, se registra el error y se asigna 0 como puntaje
      WHEN NO_DATA_FOUND THEN
         INSERT INTO ERROR_PROCESO(NUMRUN, RUTINA_ERROR, MENSAJE_ERROR)
         VALUES (P_NUMRUN, 'FN_PUNTAJE_PAIS', 'No existe puntaje definido para el NUMRUN='||P_NUMRUN);
         V_PUNTAJE := 0;
      WHEN OTHERS THEN
         DECLARE
            V_ERROR_MSG VARCHAR2(4000);
         BEGIN
            V_ERROR_MSG := SQLERRM;
            INSERT INTO ERROR_PROCESO(NUMRUN, RUTINA_ERROR, MENSAJE_ERROR)
            VALUES (P_NUMRUN, 'FN_PUNTAJE_PAIS', V_ERROR_MSG);
            V_PUNTAJE := 0;
         END;
   END;
   RETURN V_PUNTAJE;
END;
/

-----------------------------------------------------
-- PROCEDIMIENTO ALMACENADO: PRC_CALCULA_PUNTAJES
-- Recorre los postulantes, calcula puntajes de experiencia, país y extra,
-- formatea el RUT y guarda la información en las tablas de detalle.
-----------------------------------------------------
CREATE OR REPLACE PROCEDURE PRC_CALCULA_PUNTAJES (
    P_PORCENTAJE IN NUMBER 
) IS
-- Cursor para recorrer todos los postulantes ordenados por RUN
    CURSOR C_CANDIDATOS IS
        SELECT NUMRUN,
               DVRUN,
               INITCAP(PNOMBRE) || ' ' || INITCAP(NVL(SNOMBRE, '')) || ' ' ||
               INITCAP(APATERNO) || ' ' || INITCAP(AMATERNO) AS NOMBRE_COMPLETO
          FROM ANTECEDENTES_PERSONALES
         ORDER BY NUMRUN;
         
    V_NUMRUN       VARCHAR2(20);
    V_NOMBRE       VARCHAR2(200);
    V_PTJE_EXP     NUMBER(10);
    V_PTJE_PAIS    NUMBER(10);
    V_SUM_HORAS    NUMBER(10);
    V_CANT_ESTAB   NUMBER(10);
    V_PTJE_EXTRA   NUMBER(10);
BEGIN
    -- Limpiar tablas de salida
    EXECUTE IMMEDIATE 'TRUNCATE TABLE DETALLE_PUNTAJE_POSTULACION';
    EXECUTE IMMEDIATE 'TRUNCATE TABLE RESULTADO_POSTULACION';
    EXECUTE IMMEDIATE 'TRUNCATE TABLE ERROR_PROCESO';

    FOR rec IN C_CANDIDATOS LOOP
        -- Formatear el RUN utilizando la función dedicada
        V_NUMRUN := FN_FORMAT_RUT(rec.NUMRUN, rec.DVRUN);
        V_NOMBRE := rec.NOMBRE_COMPLETO;
      
        -- Obtener puntajes de experiencia y país
        V_PTJE_EXP  := FN_PUNTAJE_EXPERIENCIA(rec.NUMRUN);
        V_PTJE_PAIS := FN_PUNTAJE_PAIS(rec.NUMRUN);
    
        BEGIN
            -- Obtener suma de horas y cantidad de establecimientos
            SELECT NVL(SUM(HORAS_SEMANALES), 0), NVL(COUNT(*), 0)
              INTO V_SUM_HORAS, V_CANT_ESTAB
              FROM ANTECEDENTES_LABORALES
             WHERE NUMRUN = rec.NUMRUN;
        EXCEPTION
            WHEN NO_DATA_FOUND THEN
                V_SUM_HORAS  := 0;
                V_CANT_ESTAB := 0;
        END;
        
        -- Calcular puntaje extra usando el package, sumando los puntajes base
        V_PTJE_EXTRA := PKG_PUNTAJE_EXTRA.FN_CALC_PUNTAJE_EXTRA(
                            P_SUMA_PUNTAJES => V_PTJE_EXP + V_PTJE_PAIS,
                            P_HORAS_TOTALES => V_SUM_HORAS,
                            P_NUM_ESTAB     => V_CANT_ESTAB,
                            P_PORCENTAJE    => P_PORCENTAJE
                        );
        -- Se guarda el valor calculado en la variable pública del package
        PKG_PUNTAJE_EXTRA.V_PUNTAJE_EXTRA := V_PTJE_EXTRA;
    
     -- Se inserta la información en la tabla DETALLE_PUNTAJE_POSTULACIÓN
        INSERT INTO DETALLE_PUNTAJE_POSTULACION (
            RUN_POSTULANTE,
            NOMBRE_POSTULANTE,
            PTJE_ANNOS_EXP,
            PTJE_PAIS_POSTULA,
            PTJE_EXTRA
        ) VALUES (
            V_NUMRUN,
            V_NOMBRE,
            V_PTJE_EXP,
            V_PTJE_PAIS,
            V_PTJE_EXTRA
        );
    END LOOP;
END;
/
-----------------------------------------------------
-- TRIGGER: TRG_DETALLE_PTJE_POST
-- Luego de insertar en DETALLE_PUNTAJE_POSTULACION, calcula el puntaje final
-- y actualiza o inserta en RESULTADO_POSTULACION.
-----------------------------------------------------
CREATE OR REPLACE TRIGGER TRG_DETALLE_PTJE_POST
AFTER INSERT ON DETALLE_PUNTAJE_POSTULACION
FOR EACH ROW
DECLARE
    V_PUNTAJE_FINAL NUMBER(10);
    V_RESULTADO     VARCHAR2(20);
BEGIN
    -- Calcular el puntaje final sumando los puntajes de experiencia, país y extra
    V_PUNTAJE_FINAL := :NEW.PTJE_ANNOS_EXP + :NEW.PTJE_PAIS_POSTULA + :NEW.PTJE_EXTRA;
    
    -- Determinar si el postulante es seleccionado según el puntaje obtenido
    IF V_PUNTAJE_FINAL >= 2500 THEN
        V_RESULTADO := 'SELECCIONADO';
    ELSE
        V_RESULTADO := 'NO SELECCIONADO';
    END IF;
    
    -- Intentar actualizar el resultado de postulación si ya existe
    UPDATE RESULTADO_POSTULACION
       SET PTJE_FINAL_POST = V_PUNTAJE_FINAL,
           RESULTADO_POST  = V_RESULTADO
     WHERE RUN_POSTULANTE = :NEW.RUN_POSTULANTE;
     
    --  Si no se actualizó ningún registro (no existe), se inserta uno nuevo
    IF SQL%ROWCOUNT = 0 THEN
        INSERT INTO RESULTADO_POSTULACION (
            RUN_POSTULANTE,
            PTJE_FINAL_POST,
            RESULTADO_POST
        ) VALUES (
            :NEW.RUN_POSTULANTE,
            V_PUNTAJE_FINAL,
            V_RESULTADO
        );
    END IF;
END;
/
-----------------------------------------------------
-- EJECUCIÓN DE PRUEBA, Se invoca el procedimiento principal con un porcentaje de 35%
-----------------------------------------------------
BEGIN
    PRC_CALCULA_PUNTAJES(35);  
END;
/
-----------------------------------------------------
-- CONSULTAS PARA VERIFICAR RESULTADOS
-----------------------------------------------------
SELECT 
    RUN_POSTULANTE AS "RUN_POSTULANTE",
    NOMBRE_POSTULANTE AS "NOMBRE_POSTULANTE",
    PTJE_ANNOS_EXP AS "PTJE_ANNOS_EXP",
    PTJE_PAIS_POSTULA AS "PTJE_PAIS_POSTULA",
    PTJE_EXTRA AS "PTJE_EXTRA"
FROM 
    DETALLE_PUNTAJE_POSTULACION
ORDER BY 
    TO_NUMBER(REPLACE(SUBSTR(RUN_POSTULANTE, 1, INSTR(RUN_POSTULANTE, '-') - 1), '.', ''));

    
SELECT * FROM RESULTADO_POSTULACION ORDER BY run_postulante;
SELECT * FROM ERROR_PROCESO;

