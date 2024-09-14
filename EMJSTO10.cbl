      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "EMJSTO10".

      ******************************************************************
      *                      ENVIRONMENT DIVISION                      *
      ******************************************************************

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      ******************************************************************
      *                   archivos de entrada                          *
      ******************************************************************
           SELECT OPTIONAL ENTRADA1 ASSIGN TO "../entrada1.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-ENTRADA1.

           SELECT OPTIONAL ENTRADA2 ASSIGN TO "../entrada2.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-ENTRADA2.

      ******************************************************************
      *                   archivos de saliada                          *
      ******************************************************************

           SELECT OPTIONAL SALIDA1 ASSIGN TO "../salida1.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS FS-SALIDA1.



       DATA DIVISION.
       FILE SECTION.

       FD  ENTRADA1
            RECORDING MODE IS F
            LABEL RECORDS ARE STANDARD
            BLOCK CONTAINS 0 RECORDS
            DATA RECORD IS REG-ENTRADA1.
        01  REG-ENTRADA1 PIC X(178).

       FD  ENTRADA2
            RECORDING MODE IS F
            LABEL RECORDS ARE STANDARD
            BLOCK CONTAINS 0 RECORDS
            DATA RECORD IS REG-ENTRADA2.
        01  REG-ENTRADA2 PIC X(81).

       FD  SALIDA1
            RECORDING MODE IS F
            LABEL RECORDS ARE STANDARD
            BLOCK CONTAINS 0 RECORDS
            DATA RECORD IS REG-SALIDA1.
        01  REG-SALIDA1 PIC X(123).


       WORKING-STORAGE SECTION.

       01  WSS-REG-ENTRADA1.
            05 WSS-NUM-EMP1            PIC X(08).
            05 WSS-NOMBRE-EMP1         PIC X(20).
            05 WSS-APE-PATERNO-EMP1    PIC X(20).
            05 WSS-APE-MATERNO-EMP1    PIC X(20).
            05 WSS-DIRECCION-EMP1      PIC X(30).
            05 WSS-CODIGO-POSTAL-EMP1  PIC X(05).
            05 WSS-TELEFONO-EMP1       PIC X(10).
            05 WSS-PUESTO-EMP1         PIC X(20).
            05 WSS-DEPARTAMENTO-EMP1   PIC X(15).
            05 WSS-RFC-EMP1            PIC X(13).
            05 WSS-CURP-EMP1           PIC X(17).

       01  WSS-REG-ENTRADA2.
            05 WSS-NUM-EMP2            PIC X(08).
            05 WSS-RFC-EMP2            PIC X(13).
            05 WSS-CODIGO-POSTAL-EMP2  PIC X(05).
            05 WSS-DIRECCION-EMP2      PIC X(30).
            05 WSS-DEPARTAMENTO-EMP2   PIC X(15).
            05 WSS-TELEFONO-EMP2       PIC X(10).

       01  WSS-REG-SALIDA1.
            05 WSS-NUM-SAL1            PIC X(08).
            05 WSS-NOMBRE-SAL1         PIC X(20).
            05 WSS-APE-PATERNO-SAL1    PIC X(20).
            05 WSS-APE-MATERNO-SAL1    PIC X(20).
            05 WSS-TELEFONO-SAL1       PIC X(10).
            05 WSS-DIRECCION-SAL1      PIC X(30).
            05 WSS-DEPARTAMENTO-SAL1   PIC X(15).

       01  LLAVE-ENTRADA1.
            05 LLAVE-NUM-ENTRADA1      PIC X(08).
      *      05 LLAVE-RFC-ENTRADA1      PIC X(13).

       01  LLAVE-ENTRADA2.
            05 LLAVE-NUM-ENTRADA2      PIC X(08).
      *      05 LLAVE-RFC-ENTRADA2      PIC X(13).

       01  FILE-STATUS.
            05 FS-ENTRADA1             PIC X(02).
            05 FS-ENTRADA2             PIC X(02).
            05 FS-SALIDA1              PIC X(02).

       01  W-ACUMULADOR.
            05 WA-LEIDO-ENTRADA1       PIC 9(06) VALUE ZEROES.
            05 WA-LEIDO-ENTRADA2       PIC 9(06) VALUE ZEROES.
            05 WA-LEIDO-SALIDA1        PIC 9(06) VALUE ZEROES.

       01  W-SWITCHES.
            05 WS-FIN-ENTRADA1         PIC X(01) VALUE SPACE.
            05 WS-FIN-ENTRADA2         PIC X(01) VALUE SPACE.
            05 WS-FIN-SALIDA1          PIC X(01) VALUE SPACE.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            PERFORM 1000-INICIO.
            PERFORM 2000-PROCESO UNTIL LLAVE-ENTRADA1
                                 EQUAL LLAVE-ENTRADA2.
            PERFORM 5000-FIN-PROG
       .

       1000-INICIO.
            PERFORM 1100-ABRIR-ARCHIVO-ENT1.
            PERFORM 1200-ABRIR-ARCHIVO-ENT2.
            PERFORM 1300-ABRIR-ARCHIVO-SAL1

            PERFORM 1400-LEER-ENTRADA1
            PERFORM 1500-LEER-ENTRADA2
       .

       1100-ABRIR-ARCHIVO-ENT1.
            OPEN INPUT ENTRADA1

            IF FS-ENTRADA1 = '00'
                DISPLAY "1: ABRIÓ CORECTO ENTRADA1" FS-ENTRADA1
                CONTINUE
            ELSE
                DISPLAY "ERROR AL ABRIR EL ARCHIVO ENT1: " FS-ENTRADA1
                PERFORM 5000-FIN-PROG
            END-IF.

       1200-ABRIR-ARCHIVO-ENT2.
            OPEN INPUT ENTRADA2

            IF FS-ENTRADA2 = '00'
                DISPLAY "2: ABRIÓ CORECTO ENTRADA2" FS-ENTRADA2
                CONTINUE
            ELSE
                DISPLAY "ERROR AL ABRIR EL ARCHIVO EMP2: " FS-ENTRADA2
                PERFORM 2210-CERRAR-ENTRADA1
                PERFORM 5000-FIN-PROG
            END-IF.

       1300-ABRIR-ARCHIVO-SAL1.
            OPEN OUTPUT SALIDA1
            IF FS-SALIDA1 = '00'
                DISPLAY "3: ABRIO CORECTO SALIDA1" FS-SALIDA1
                CONTINUE
            ELSE
                DISPLAY "ERROR AL ABRIR EL ARCHIVO SAL1: " FS-SALIDA1
                PERFORM 2210-CERRAR-ENTRADA1
                PERFORM 2220-CERRAR-ENTRADA2
                PERFORM 5000-FIN-PROG
            END-IF
       .

       1400-LEER-ENTRADA1.
            READ ENTRADA1 INTO WSS-REG-ENTRADA1
            IF FS-ENTRADA1 = '00'
                MOVE WSS-NUM-EMP1 TO LLAVE-NUM-ENTRADA1
                ADD 1 TO WA-LEIDO-ENTRADA1
                DISPLAY "LEER ENTRADA1 ------------------------"
                DISPLAY "REGISTRO: " WA-LEIDO-ENTRADA1
                DISPLAY WSS-REG-ENTRADA1
                CONTINUE
            ELSE
                IF FS-ENTRADA1 = '10'
                    MOVE 'S' TO WS-FIN-ENTRADA1
                    DISPLAY "ARCHIVO VACIO ENTRADA1" FS-ENTRADA1
                    PERFORM 2000-PROCESO
                ELSE
                    DISPLAY "ERROR AL LEER ARCHIVO ENTRADA1" FS-ENTRADA1
                    PERFORM 2200-CERRAR-ARCHIVOS THRU
                                                2240-FIN-CERRAR-ACHIVOS
                    PERFORM 5000-FIN-PROG
                END-IF
            END-IF
       .

       1500-LEER-ENTRADA2.
            READ ENTRADA2 INTO WSS-REG-ENTRADA2

            IF FS-ENTRADA2 = '00'
                MOVE WSS-NUM-EMP2 TO LLAVE-NUM-ENTRADA2
                ADD 1 TO WA-LEIDO-ENTRADA2
                DISPLAY "LEER ENTRADA2------------------------"
                DISPLAY "REGISTRO: " WA-LEIDO-ENTRADA2
                DISPLAY WSS-REG-ENTRADA2

            ELSE
                IF FS-ENTRADA2 = '10'
                    MOVE 'S' TO WS-FIN-ENTRADA2
                    DISPLAY "ARCHIVO VACIO ENTRADA2" FS-ENTRADA2
                    PERFORM 2000-PROCESO
                ELSE
                        PERFORM 2200-CERRAR-ARCHIVOS THRU
                                                2240-FIN-CERRAR-ACHIVOS
                        PERFORM 5000-FIN-PROG
                END-IF
            END-IF
       .

       2000-PROCESO.

           PERFORM 2100-VALIDA-REGISTRO UNTIL WS-FIN-ENTRADA1 = 'S'
                                        OR WS-FIN-ENTRADA2 = 'S'
           PERFORM 2200-CERRAR-ARCHIVOS THRU 2240-FIN-CERRAR-ACHIVOS
           PERFORM 2300-CIFRAS-CONTROL
       .

       2100-VALIDA-REGISTRO.
           DISPLAY "-----------------VALIDAR REGISTRO-----------------"

           EVALUATE TRUE

               WHEN LLAVE-ENTRADA1 = LLAVE-ENTRADA2
                   DISPLAY "LAS LLAVES SON IGUALES"
      *             PERFORM 2110-MOVER-CAMPOS
      *             PERFORM 2120-GRAVAR-CAMPOS
                   PERFORM 1500-LEER-ENTRADA2
                   PERFORM 1400-LEER-ENTRADA1
                   PERFORM 2100-VALIDA-REGISTRO

               WHEN LLAVE-ENTRADA1 > LLAVE-ENTRADA2
                   DISPLAY "LLAVE-ENTRADA1 ES MAYOR LLAVE-ENTRADA2"
                   DISPLAY WSS-REG-ENTRADA1
                   DISPLAY WSS-REG-ENTRADA2
                   DISPLAY "-------------------------------------------"
                   PERFORM 2210-MOVER-CAMPOS
                   PERFORM 2220-GRAVAR-CAMPOS
                   PERFORM 1500-LEER-ENTRADA2
                   PERFORM 2100-VALIDA-REGISTRO

               WHEN LLAVE-ENTRADA1 < LLAVE-ENTRADA2
                   DISPLAY "LLAVE-ENTRADA1 ES ESMENOR LLAVE-ENTRADA2"
                   DISPLAY WSS-REG-ENTRADA1
                   DISPLAY WSS-REG-ENTRADA2
                   DISPLAY "-------------------------------------------"
                   PERFORM 2310-MOVER-CAMPOS
                   PERFORM 2320-GRAVAR-CAMPOS
                   PERFORM 1400-LEER-ENTRADA1
                   PERFORM 2100-VALIDA-REGISTRO
           END-EVALUATE.

      *---------------------- WHEN 1 =---------------------------------*
       2110-MOVER-CAMPOS.
            MOVE WSS-NUM-EMP1          TO WSS-NUM-SAL1
            MOVE WSS-NOMBRE-EMP1       TO WSS-NOMBRE-SAL1
            MOVE WSS-APE-PATERNO-EMP1  TO WSS-APE-PATERNO-SAL1
            MOVE WSS-APE-MATERNO-EMP1  TO WSS-APE-MATERNO-SAL1
            MOVE WSS-TELEFONO-EMP1     TO WSS-TELEFONO-SAL1
            MOVE WSS-DIRECCION-EMP1    TO WSS-DIRECCION-SAL1
            MOVE WSS-DEPARTAMENTO-EMP1 TO WSS-DEPARTAMENTO-SAL1
            DISPLAY "REGISTRO MOVIENDO SALIDA1: " WSS-REG-SALIDA1
       .

       2120-GRAVAR-CAMPOS.
            WRITE REG-SALIDA1 FROM WSS-REG-SALIDA1
            IF FS-SALIDA1 = '00'
                DISPLAY "REGISTRO GRAVADO CORECTO "
                ADD 1 TO WA-LEIDO-SALIDA1
                CONTINUE
            ELSE
                DISPLAY "ERROR AL REGISTARA EMPLEADO IGUL" FS-SALIDA1
                PERFORM 5000-FIN-PROG
            END-IF
       .


      *---------------------------WHEN 2 > ----------------------------*
       2210-MOVER-CAMPOS.

            MOVE WSS-NUM-EMP1          TO WSS-NUM-SAL1
            MOVE WSS-NOMBRE-EMP1       TO WSS-NOMBRE-SAL1
            MOVE WSS-APE-PATERNO-EMP1  TO WSS-APE-PATERNO-SAL1
            MOVE WSS-APE-MATERNO-EMP1  TO WSS-APE-MATERNO-SAL1
            MOVE WSS-TELEFONO-EMP1     TO WSS-TELEFONO-SAL1
            MOVE WSS-DIRECCION-EMP1    TO WSS-DIRECCION-SAL1
            MOVE WSS-DEPARTAMENTO-EMP1 TO WSS-DEPARTAMENTO-SAL1
            DISPLAY "REGISTRO MOVIENDO: " WSS-REG-SALIDA1
       .

       2220-GRAVAR-CAMPOS.

           WRITE REG-SALIDA1 FROM WSS-REG-SALIDA1
           IF FS-SALIDA1 = '00'
               ADD 1 TO WA-LEIDO-SALIDA1
               DISPLAY "REGISTRO GRAVADO CORECTO "
               CONTINUE
            ELSE
                DISPLAY "ERROR AL REGISTARA EMPLEADO MAYOR"
                PERFORM 5000-FIN-PROG
            END-IF
           .

      *-----------------------------WHEN 3-----------------------------*

       2310-MOVER-CAMPOS.

            MOVE WSS-NUM-EMP2            TO WSS-NUM-SAL1
            MOVE "WSS-NOMBRE-EMP2"       TO WSS-NOMBRE-SAL1
            MOVE "WSS-APE-PATERNO-EMP2"  TO WSS-APE-PATERNO-SAL1
            MOVE "WSS-APE-MATERNO-EMP2"  TO WSS-APE-MATERNO-SAL1
            MOVE WSS-TELEFONO-EMP2       TO WSS-TELEFONO-SAL1
            MOVE WSS-DIRECCION-EMP2      TO WSS-DIRECCION-SAL1
            MOVE WSS-DEPARTAMENTO-EMP2   TO WSS-DEPARTAMENTO-SAL1
            DISPLAY "REGISTRO MOVIENDO: " WSS-REG-SALIDA1
       .

       2320-GRAVAR-CAMPOS.
            WRITE REG-SALIDA1 FROM WSS-REG-SALIDA1

            IF FS-SALIDA1 = '00'
                DISPLAY "REGISTRO GRAVADO CORECTO "
                ADD 1 TO WA-LEIDO-SALIDA1
                CONTINUE
            ELSE
                DISPLAY "ERROR AL REGISTARA ENTRADA2 MENOR" FS-SALIDA1
                PERFORM 5000-FIN-PROG
            END-IF.


       2200-CERRAR-ARCHIVOS.
            DISPLAY "CERRANDO ARCHIVOS".

       2210-CERRAR-ENTRADA1.
            CLOSE ENTRADA1
             IF FS-ENTRADA1 = '00'
                 DISPLAY "ENTRADA1 CERRADO"
                 CONTINUE
             ELSE
                 DISPLAY "ERRO AL CERRAR ENTRADA1" FS-ENTRADA1
                 PERFORM 5000-FIN-PROG
             END-IF.
       2220-CERRAR-ENTRADA2.
            CLOSE ENTRADA2
            IF FS-ENTRADA2 = '00'
                DISPLAY "ENTRADA2 CERRADO"
                CONTINUE
             ELSE
                 DISPLAY "ERRO AL CERRAR ARCHIVOS" FS-ENTRADA2
                 PERFORM 5000-FIN-PROG
             END-IF.
       2230-CERRAR-SALIDA1.
            CLOSE SALIDA1
             IF FS-SALIDA1    = '00'
                 DISPLAY "SALIDA1 CERRADO"
                 CONTINUE
             ELSE
                 DISPLAY "ERRO AL CERRAR ARCHIVOS" FS-SALIDA1
                 PERFORM 5000-FIN-PROG
             END-IF.

       2240-FIN-CERRAR-ACHIVOS.
            EXIT.

       2300-CIFRAS-CONTROL.
            DISPLAY "CIFRAS"
            DISPLAY "ENTRADA1: " WA-LEIDO-ENTRADA1
            DISPLAY "ENTRADA2: " WA-LEIDO-ENTRADA2
            DISPLAY "ENTRADA1: " WA-LEIDO-SALIDA1
       .

       5000-FIN-PROG.
            STOP RUN
       .

       END PROGRAM "EMJSTO10".
