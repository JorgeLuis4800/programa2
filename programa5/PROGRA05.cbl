       IDENTIFICATION DIVISION.
       PROGRAM-ID. "PROG05".

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT OPTIONAL ENTRADA1 ASSIGN TO "../entrada1.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-ENTRADA1.

           SELECT OPTIONAL SALIDA1 ASSIGN TO "../salida2.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-SALIDA1.


       DATA DIVISION.
       FILE SECTION.

       FD  ENTRADA1
           RECORDING MODE IS F
            LABEL RECORDS ARE STANDARD
            BLOCK CONTAINS 0 RECORDS
            DATA RECORD IS REG-ENTRADA1.
        01  REG-ENTRADA1 PIC X(80).

       FD  SALIDA1
           RECORDING MODE IS F
            LABEL RECORDS ARE STANDARD
            BLOCK CONTAINS 0 RECORDS
            DATA RECORD IS REG-SALIDA1.
        01  REG-SALIDA1 PIC X(103).

       WORKING-STORAGE SECTION.

      ************************* REG-ENTRADA1 ***************************
       01  W-REG-ENTRADA1.
            05 WRM-TIPMOV              PIC X(01).
            05 WRM-CVEMOV              PIC 9(02).
            05 WRM-NUMCTE.
               10 WRMNC-CONS           PIC 9(05).
               10 WRMNC-DIGVER         PIC 9(01).
            05 WRM-NOMCOM.
               10 WRMNC-NOMBRE         PIC X(10).
               10 WRMNC-APEPAT         PIC X(10).
               10 WRMNC-APEMAT         PIC X(10).
            05 WRM-RFC.
               10 WRMR-RFC             PIC X(10).
               10 WRMR-HCVE            PIC X(03).
            05 WRM-IMPORTE             PIC 9(05)V99.
            05 WRM-FECMOV              PIC 9(08).
            05 WRM-CVEREG              PIC 9(02).
            05 WRM-CVEPZA              PIC 9(02).
            05 WRM-NUMMOV              PIC 9(04).
            05 FILLER                  PIC X(05) VALUE SPACES.

      ******************************************************************
      *                        LINEA DE ENCAVEZADO 1                   *
      ******************************************************************


       01  WSS-LINEA-ENC1.
            05 FILLER                  PIC X(10) VALUE SPACES.
            05 FILLER                  PIC X(57) VALUE
                  'REPORTE  DE INSTITUTO  PROFESIONAL  EN  INFORMATICA'.
            05 FILLER                  PIC X(17) VALUES SPACES.
            05 FILLER                  PIC X(08) VALUE 'FECHA : '.
            05 WLE1-DD                 PIC 9(02) VALUE ZEROES.
            05 FILLER                  PIC X(01) VALUE '-'.
            05 WLE1-MM                 PIC X(03) VALUE ZEROES.
            05 FILLER                  PIC X(01) VALUE '-'.
            05 WLE1-AAAA               PIC 9(04) VALUE ZEROES.

      ******************************************************************
      *                        LINEA DE ENCABEZADO 2                   *
      ******************************************************************

       01  WSS-LINEA-ENC2.
            05 FILLER                  PIC X(103) VALUE ALL ' '.

      ******************************************************************
      *                        LINEA DE ENCAVEZADO 3                   *
      ******************************************************************

       01  WSS-LINEA-ENC3.
            05 FILLER                  PIC X(06) VALUE 'TIPMOV'.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 FILLER                  PIC X(06) VALUE 'NUMCTE'.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 FILLER                  PIC X(10) VALUE 'NOMBRE    '.
            05 FILLER                  PIC X(01) VALUE SPACES.
            05 FILLER                  PIC X(10) VALUE 'COMPLETO  '.
            05 FILLER                  PIC X(01) VALUE SPACES.
            05 FILLER                  PIC X(10) VALUE '          '.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 FILLER                  PIC X(13) VALUE 'RFC          '.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 FILLER                  PIC X(09) VALUE 'IMPORTE  '.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 FILLER                  PIC X(06) VALUE 'CVEREG'.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 FILLER                  PIC X(06) VALUE 'CVEPZA'.
            05 FILLER                  PIC X(02) VALUE '¦'.

      ******************************************************************
      *                        LINEA DE ENCAVEZADO 4                   *
      ******************************************************************

       01  WSS-LINEA-ENC4.
            05 FILLER                  PIC X(06) VALUE '------'.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 FILLER                  PIC X(06) VALUE '------'.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 FILLER                  PIC X(10) VALUE '----------'.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 FILLER                  PIC X(10) VALUE '----------'.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 FILLER                  PIC X(10) VALUE '----------'.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 FILLER                  PIC X(13) VALUE '-------------'.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 FILLER                  PIC X(09) VALUE '---------'.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 FILLER                  PIC X(06) VALUE '------'.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 FILLER                  PIC X(06) VALUE '------'.
            05 FILLER                  PIC X(02) VALUE '¦'.

      ******************************************************************
      *                        LINEA DE ENCAVEZADO 5                   *
      ******************************************************************

      *************************** REG-SALIDA1 **************************
       01  WSS-REG-SALIDA1.
            05 WRR-TIPMOV              PIC X(01) VALUE SPACES.
            05 FILLER                  PIC X(05) VALUE SPACES.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 WRR-NUMCTE              PIC 9(05) VALUE ZEROES.
            05 FILLER                  PIC X(01) VALUE SPACES.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 WRR-NOMBRE-COM          PIC X(30) VALUE SPACE.
            05 FILLER                  PIC X(02) VALUE SPACES.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 WRR-RFC                 PIC X(13) VALUE SPACES.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 WRR-IMPORTE             PIC ZZ,ZZ9.99.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 FILLER                  PIC X(04) VALUE SPACES.
            05 WRR-CVEREG              PIC 9(02) VALUE ZEROES.
            05 FILLER                  PIC X(02) VALUE '¦'.
            05 FILLER                  PIC X(04) VALUE SPACES.
            05 WRR-CVEPZA              PIC 9(02) VALUE ZEROES.
            05 FILLER                  PIC X(02) VALUE '¦'.

      ******************************************************************
      *           ACEPTA FECHA DEL SISTEMA FORMATO : AAMMDD            *
      ******************************************************************
       01  W-FEC-SIS.
            05 WFS-AA                  PIC 9(02) VALUE ZEROES.
            05 WFS-MM                  PIC 9(02) VALUE ZEROES.
            05 WFS-DD                  PIC 9(02) VALUE ZEROES.

      ******************************************************************

       01  CONCATENAR-NOMBRES.
            05 AUX-NOMBRE              PIC X(10) VALUE SPACE.
            05 AUX-AP-P                PIC X(10) VALUE SPACE.
            05 AUX-AP-M                PIC X(10) VALUE SPACE.

            05 AUX-NOM-COM             PIC X(30) VALUE SPACE.

      *----------------------------------------------------------------*
      *                 ARREGLO DE TABLA DE MESES                      *
      *----------------------------------------------------------------*

       01 W-TABLAS.
            05 W-MESES-LETRA.
               10 FILLER               PIC X(03) VALUE 'ENE'.
               10 FILLER               PIC X(03) VALUE 'FER'.
               10 FILLER               PIC X(03) VALUE 'MAR'.
               10 FILLER               PIC X(03) VALUE 'ABR'.
               10 FILLER               PIC X(03) VALUE 'MAY'.
               10 FILLER               PIC X(03) VALUE 'JUN'.
               10 FILLER               PIC X(03) VALUE 'JUL'.
               10 FILLER               PIC X(03) VALUE 'AGO'.
               10 FILLER               PIC X(03) VALUE 'SEP'.
               10 FILLER               PIC X(03) VALUE 'OCT'.
               10 FILLER               PIC X(03) VALUE 'NOV'.
               10 FILLER               PIC X(03) VALUE 'DIC'.
            05 W-TAB-MESES             REDEFINES W-MESES-LETRA.
               10 W-TM-MES             PIC X(03) OCCURS 12 TIMES.


       01  FILE-STATUS.
            05 FS-ENTRADA1             PIC X(02).
            05 FS-SALIDA1              PIC X(02).

       01  CONTADORES.
            05 CONTADOR-ENTRE1         PIC 9(08) VALUE ZEROES.
            05 CONTADOR-SALID1         PIC 9(08) VALUE ZEROES.


       01  WSS-SWITCHES.
            05 WS-FIN-ENTRADA1         PIC X(01) VALUE SPACE.
            05 WS-FIN-SALIDA1          PIC X(01) VALUE SPACE.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            PERFORM 1000-INICIO
            PERFORM 2000-PROCESO
            PERFORM 3000-FIN
           .

       1000-INICIO.
            MOVE 'N' TO WS-FIN-ENTRADA1
            PERFORM 1100-ABRIR-ARCHIVOS
            PERFORM 1200-LEER-ENTRADA1

            .

       1100-ABRIR-ARCHIVOS.
            OPEN INPUT ENTRADA1
                 OUTPUT SALIDA1

            IF FS-ENTRADA1 > '07'
                DISPLAY "Hubo un error al abrir entrada1 codigo: "
                                                            FS-ENTRADA1
                PERFORM 3000-FIN
            ELSE
                DISPLAY "Abrio correcto entrada1 " FS-ENTRADA1

                IF FS-SALIDA1 > '07'
                    DISPLAY "Hubo un error al abrir salida1 codigo: "
                                                             FS-SALIDA1
                    PERFORM 2500-CERRAR-ENTRADA1
                    PERFORM 3000-FIN
                ELSE
                    DISPLAY "Abrio correcto salida1 " FS-SALIDA1
                END-IF
            END-IF
           .

       1200-LEER-ENTRADA1.
            INITIALIZE W-REG-ENTRADA1
            READ ENTRADA1 INTO W-REG-ENTRADA1

            IF FS-ENTRADA1 = '00'
                ADD 1 TO  CONTADOR-ENTRE1
                CONTINUE
            ELSE
                IF FS-ENTRADA1 = '10'
                    MOVE 'S' TO WS-FIN-ENTRADA1
                    DISPLAY "FIN DEL ARCHIVO "
                ELSE
                    DISPLAY "Erro al leer el archivo " FS-ENTRADA1
                    PERFORM 2500-CERRAR-ENTRADA1
                           THRU 2600-CERRAR-SALIDA1
                    PERFORM 3000-FIN
                END-IF
            END-IF
           .


       2000-PROCESO.
            PERFORM FECHA
            PERFORM 2100-GRABANDO-ENCABEZADO
            PERFORM 2200-MOVER-SALIDA1 UNTIL WS-FIN-ENTRADA1 EQUAL 'S'
            PERFORM 2500-CERRAR-ENTRADA1 THRU 2600-CERRAR-SALIDA1.

       FECHA.
            ACCEPT W-FEC-SIS FROM DATE
            MOVE WFS-AA TO WLE1-AAAA
            MOVE WFS-MM TO WLE1-MM
            MOVE WFS-DD TO WLE1-DD
            ADD 2000    TO WLE1-AAAA
            .

       2100-GRABANDO-ENCABEZADO.
            DISPLAY "grabando ENCAVEZAD".
            WRITE REG-SALIDA1 FROM WSS-LINEA-ENC1
            WRITE REG-SALIDA1 FROM WSS-LINEA-ENC2
            WRITE REG-SALIDA1 FROM WSS-LINEA-ENC3
            WRITE REG-SALIDA1 FROM WSS-LINEA-ENC4
            .


       2200-MOVER-SALIDA1.
            INITIALIZE WSS-REG-SALIDA1
            MOVE WRM-TIPMOV TO WRR-TIPMOV
            MOVE WRM-NUMCTE TO WRR-NUMCTE
            MOVE WRM-NOMCOM TO WRR-NOMBRE-COM
            MOVE WRM-RFC TO WRR-RFC
            MOVE WRM-IMPORTE TO WRR-IMPORTE
            MOVE WRM-CVEREG TO WRR-CVEREG
            MOVE WRM-CVEPZA TO WRR-CVEPZA
            PERFORM 2220-GRABAR-SALIDA
            PERFORM 1200-LEER-ENTRADA1
            .

       2220-GRABAR-SALIDA.
            WRITE REG-SALIDA1 FROM WSS-REG-SALIDA1
            ADD 1 TO CONTADOR-SALID1
            .

       2500-CERRAR-ENTRADA1.
            CLOSE ENTRADA1
            .

       2600-CERRAR-SALIDA1.
            CLOSE SALIDA1
            .

       3000-FIN.
            STOP RUN.

       END PROGRAM "PROG05".
