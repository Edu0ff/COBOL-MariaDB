       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. BANCO.
       AUTHOR. MARÍA EDUARDA ALMEIDA LOIOLA.

      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
      *----------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
      *---------------------
       FILE-CONTROL.
           SELECT FICHCLI ASSIGN TO 'clientes-banco-ok.dat'
                          ORGANIZATION IS LINE SEQUENTIAL
                          ACCESS MODE  IS SEQUENTIAL
                          FILE STATUS  IS FS-FICHCLI.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
      *-------------
      * Cada registro del fichero contiene 77 caracteres.
       FD FICHCLI RECORD CONTAINS 77 CHARACTERS
                  LABEL RECORD IS STANDARD.
       01  REG-FICHCLI                   PIC X(77).

       WORKING-STORAGE SECTION.
      *------------------------
      * Valor de retorno al sistema operativo (status).
      * Cada uno de los errores que queremos contener.
       01  SW-STAT                       PIC 9(02) VALUE 00.
           88  STAT-OK                   VALUE 00.
           88  STAT-ERR-TIP-REG          VALUE 01.
           88  STAT-ERR-NOMBRE           VALUE 02.
           88  STAT-ERR-FEC-NAC          VALUE 03.
           88  STAT-ERR-NIF              VALUE 04.
           88  STAT-ERR-CALLE            VALUE 05.
           88  STAT-ERR-COD-POST         VALUE 06.
           88  STAT-ERR-POBL             VALUE 07.
           88  STAT-ERR-PROV             VALUE 08.
           88  STAT-ERR-CTA-DUPL         VALUE 09.
           88  STAT-ERR-CTA-MAX          VALUE 10.
           88  STAT-ERR-NUM-CUENTA       VALUE 11. 
           88  STAT-ERR-SALDO            VALUE 12.
           88  STAT-ERR-CUENTA-AUT       VALUE 13.
           88  STAT-ERR-NUM-TARJETA      VALUE 14.
           88  STAT-ERR-TAR-DUPL         VALUE 15. 
           88  STAT-ERR-CREDITO          VALUE 16. 
           88  STAT-ERR-TARJETA-FEC      VALUE 17.
           88  STAT-ERR-MOV-CPT          VALUE 18.
           88  STAT-ERR-MOV-MEDIO        VALUE 19.
           88  STAT-ERR-MOV-NO-MEDIO     VALUE 20.
           88  STAT-ERR-MOV-IMPORTE      VALUE 21.
           88  STAT-ERR-MOV-FEC          VALUE 22.
           88  STAT-ERR-MOV-MAX          VALUE 23.
           88  STAT-ERR-MOV-NUM-MEDIO    VALUE 24.
           88  STAT-ERR-IO               VALUE 25.
           88  STAT-ERR-AFD              VALUE 26.
      
      * Mensajes para los errores.
       01  WK-DESC-STAT.
           05 FILLER PIC X(50) VALUE 'STATUS OK'.
           05 FILLER PIC X(50) VALUE 'TIPO DE REGISTRO DESCONOCIDO'.
           05 FILLER PIC X(50) VALUE 'NOMBRE DE CLIENTE VACIO/ERRONEO'.
           05 FILLER PIC X(50) VALUE 'FECHA DE NAC. VACIA/ERRONEA'.
           05 FILLER PIC X(50) VALUE 'NIF DE CLIENTE VACIO'.
           05 FILLER PIC X(50) VALUE 'CALLE DE CLIENTE VACIA'.
           05 FILLER PIC X(50) VALUE 'CODIGO POSTAL NO VALIDO'.
           05 FILLER PIC X(50) VALUE 'POBLACION VACIA'.
           05 FILLER PIC X(50) VALUE 'PROVINCIA VACIA'.
           05 FILLER PIC X(50) VALUE 'CUENTA DUPLICADA'.
           05 FILLER PIC X(50) VALUE 'MAXIMO DE CUENTAS ALCANZADO'.
           05 FILLER PIC X(50) VALUE 'NUMERO DE CUENTA VACIO/ERRONEO'.
           05 FILLER PIC X(50) VALUE 'SALDO DE CUENTA NO VALIDO'.
           05 FILLER PIC X(50) VALUE 'RELACION ENTRE CUENTA Y CLIENTE'.
           05 FILLER PIC X(50) VALUE 'NUMERO DE TARJETA NO VALIDO'.
           05 FILLER PIC X(50) VALUE 'TARJETA DUPLICADA'.
           05 FILLER PIC X(50) VALUE 'CREDITO DE TARJETA NO VALIDO'.
           05 FILLER PIC X(50) VALUE 'ERROR EN LA FECHA DE LA TARJETA'.
           05 FILLER PIC X(50) VALUE 'CONCEPTO DE MOVIMIENTO VACIO'.
           05 FILLER PIC X(50) VALUE 'ORIGEN CARGO DE MOVIMIENTO ERRONEO
      -                              'O VACIO'.
           05 FILLER PIC X(50) VALUE 'MEDIO DE CARGO DE MOVIMIENTO ERRON
      -                              'O O VACIO'.
           05 FILLER PIC X(50) VALUE 'IMPORTE DE MOVIMIENTO ERRONEO O VA
      -                              'CIO'.
           05 FILLER PIC X(50) VALUE 'FECHA DE MOVIMIENTO ERRONEA O VACI
      -                              'A'.
           05 FILLER PIC X(50) VALUE 'MAXIMO DE MOVIMIENTOS ALCANZADO'.
           05 FILLER PIC X(50) VALUE 'NUMERO DE MEDIO DEL MOVIMIENTO ERR
      -                              'ONEO O VACIO'.
           05 FILLER PIC X(50) VALUE 'ERROR DE ENTRADA/SALIDA'.
           05 FILLER PIC X(50) VALUE 'ESTADO DEL AFD INALCANZABLE'.

      * Atar los 26 errores a su correspondiente STAT-ERR con una tabla.
       01  WK-DESC-STAT-R                   REDEFINES WK-DESC-STAT.
           05  DESC-ERR-TABLA               PIC X(50) OCCURS 27 TIMES.
       
       01  DESC-ERR-INDICE                  PIC 9(02).
       
      * Switch para determinar en qué estado del automata finito
      * determinista nos encontramos. Nos sirve para identificar que
      * los registros están correctamente ordenados, y determinar cuando
      * hemos terminado de leer los datos correspondientes a un cliente
      * y pasado al siguiente (de un estado final a uno inicial).
       01  WK-AFD.
           05  SW-AFD-STAT                  PIC 9(01) VALUE 0.
               88  AFD-STAT-Q0              VALUE 0.
               88  AFD-STAT-Q1              VALUE 1.
               88  AFD-STAT-Q2              VALUE 2.
               88  AFD-STAT-Q3              VALUE 3.
               88  AFD-STAT-Q4              VALUE 4.
           05  SW-AFD-STAT-FINAL            PIC 9(01).
               88  AFD-STAT-FINAL-SI        VALUES 3, 4.

      * El siguiente símbolo de la secuencia de entrada. Cada registro
      * identifica un conjunto de datos distinto. Los distinguimos por
      * su registro inicial (dos dígitos entre 01 y 04).
       01  TIP-REG.
           05  REG-TIPO                      PIC 9(02).
               88  REG-TIPO-CLIENTE          VALUE 01.
               88  REG-TIPO-DOMICILIO        VALUE 02.
               88  REG-TIPO-CUENTA-TARJETA   VALUE 03.
               88  REG-TIPO-MOVIMIENTO       VALUE 04.
           05  FILLER                        PIC X(75).

      * En algunos registros redefinimos el dato a PIC X para el manejo 
      * de nuestros errores y hacer las comprobaciones de tipo numérico.
      * Tipo registro cliente.
       01  TIP-REG-CLIENTE                   REDEFINES TIP-REG.
           05  REG-CLIENTE-TIPO              PIC X(02).
           05  REG-CLIENTE-NOMBRE            PIC X(57).
           05  REG-CLIENTE-FEC-NAC           PIC 9(08).
           05  REG-CLIENTE-FEC-NAC-X         REDEFINES 
               REG-CLIENTE-FEC-NAC           PIC X(08).
           05  REG-CLIENTE-NIF               PIC X(10).

      * Tipo registro domicilio.
       01  TIP-REG-DOMICILIO                 REDEFINES TIP-REG.
           05  REG-DOMICILIO-TIP             PIC X(02).
           05  REG-DOMICILIO-CALLE           PIC X(35).
           05  REG-DOMICILIO-NUMERO          PIC X(03).
           05  REG-DOMICILIO-CODPOST         PIC 9(05).
           05  REG-DOMICILIO-CODPOST-X       REDEFINES 
               REG-DOMICILIO-CODPOST         PIC X(05).  
           05  REG-DOMICILIO-PROV            PIC X(16).
           05  REG-DOMICILIO-POBL            PIC X(16).

      * Tipo registro cuenta.
       01  TIP-REG-CUENTA-TARJETA            REDEFINES TIP-REG.
           05  REG-CUENTA-TIPO               PIC X(02).
           05  REG-CUENTA-NUMERO             PIC 9(20).
           05  REG-CUENTA-NUMERO-X           REDEFINES 
               REG-CUENTA-NUMERO             PIC X(20).
           05  REG-CUENTA-SALDO              PIC S9(08)V99 
                                             LEADING SEPARATE.
           05  REG-CUENTA-SALDO-R            REDEFINES 
               REG-CUENTA-SALDO.
               10 REG-CUENTA-SALDO-S         PIC X(01).
               10 REG-CUENTA-SALDO-VAL       PIC X(10).
           05  REG-CUENTA-AUT                PIC X(01).
           05  FILLER                        PIC X(12).
           05  REG-TARJETA-NUMERO            PIC 9(16).
           05  REG-TARJETA-NUMERO-X          REDEFINES 
               REG-TARJETA-NUMERO            PIC X(16).
           05  REG-TARJETA-CREDITO           PIC S9(08)V99 
                                             LEADING SEPARATE.
           05  REG-TARJETA-CREDITO-R         REDEFINES 
               REG-TARJETA-CREDITO.
               10 REG-TARJETA-CREDITO-S      PIC X(01).
               10 REG-TARJETA-CREDITO-VAL    PIC X(10).
           05  REG-TARJETA-FEC               PIC 9(04).
           05  REG-TARJETA-FEC-X             REDEFINES 
               REG-TARJETA-FEC               PIC X(04).
      * Tipo registro movimiento.
       01  TIP-REG-MOVIMIENTO                REDEFINES TIP-REG.
           05  REG-MOVIMIENTO-TIP            PIC X(02).
           05  REG-MOVIMIENTO-CPT            PIC X(23).
           05  REG-MOVIMIENTO-NUM-MEDIO      PIC 9(20).
           05  REG-MOVIMIENTO-NUM-MEDIO-X    REDEFINES
               REG-MOVIMIENTO-NUM-MEDIO      PIC X(20).
           05  REG-MOVIMIENTO-NUM-TAR-R      REDEFINES 
               REG-MOVIMIENTO-NUM-MEDIO.
               10  REG-MOVIMIENTO-NUM-TAR    PIC X(16).
               10  FILLER                    PIC X(04). 
           05  REG-MOVIMIENTO-TIPO-MEDIO     PIC X(01).
           05  REG-MOV-IMPORTE               PIC S9(08)V99 
                                             LEADING SEPARATE.
           05  REG-MOV-IMPORTE-R             REDEFINES 
               REG-MOV-IMPORTE.
               10 REG-MOV-IMPORTE-S          PIC X(01).
               10 REG-MOV-IMPORTE-VAL        PIC X(10).
           05  REG-MOVIMIENTO-FEC            PIC 9(20).
           05  REG-MOVIMIENTO-FEC-X          REDEFINES 
               REG-MOVIMIENTO-FEC            PIC X(20).   
      * Variables para calcular un número 'aleatorio' para el CCV de la
      * tarjeta.
       01  WK-CCV.
           05  WK-FECHA-ACTUAL               PIC 9(06).
           05  WK-HORA-ACTUAL                PIC 9(06).
           05  WK-SEMILLA                    PIC 9(12).
           05  WK-SEMILLA-2                  PIC 9(12).
           05  WK-CCV-ALEATORIO              PIC 9(03).
           05  WK-CCV-CONTADOR               PIC 9(02) VALUE 1.
       
      * Objeto cliente. Usamos esta estructura para guardar en ella los
      * datos que vamos leyendo en los registros para así no perderlos
      * y poder imprimir el cliente completo una vez comprobemos que
      * hemos transitado al siguiente.
       01  WK-CTE-CUENTAS-MAX                PIC 9(01) VALUE 5.
       01  WK-MOV-MAX                        PIC 9(01) VALUE 5.

       01  WK-OBJ-CLIENTE.
           05  WK-CLIENTE.
               10  WK-CLIENTE-NOMBRE         PIC X(57).
               10  WK-CLIENTE-FEC-NAC        PIC 9(08).
               10  WK-CLIENTE-NIF            PIC X(10).

           05  WK-DOMICILIO.
               10  WK-DOMICILIO-CALLE        PIC X(35).
               10  WK-DOMICILIO-NUMERO       PIC X(03).
               10  WK-DOMICILIO-CODPOST      PIC 9(05). 
               10  WK-DOMICILIO-PROV         PIC X(16).
               10  WK-DOMICILIO-POBL         PIC X(16).
               10  WK-DOMICILIO-COMPL        PIC X(100).
      * Máximo de cuentas, tarjetas y movimientos por cliente (5).
      * Creamos variables para controlar el tamaño máximo de las tablas
      * de cuentas, tarjetas y movimientos. 
           05  WK-CUE-TAR-TABLA.
               10  WK-CUE-TAR-CONTADOR       PIC 9(01) VALUE 0.
               10  WK-CUE-TAR                OCCURS 5 TIMES.
                   15  WK-CLIENTE-NUM-CTA    PIC 9(20).
                   15  WK-CLIENTE-SALD-CTA   PIC S9(08)V99 
                                             LEADING SEPARATE.
      * La relación entre cliente y cuenta la expresamos como TITULAR,
      * COTITULAR o AUTORIZADO.                                       
                   15  SW-CLIENTE-RLN-CTA    PIC X(01).
                       88  CLI-CUE-TIT       VALUE 'T'.
                       88  CLI-CUE-CO        VALUE 'C'.
                       88  CLI-CUE-AU        VALUE 'A'.

                   15  WK-CLIENTE-NUM-TAR    PIC 9(16).
                   15  WK-CLIENTE-CRE-TAR    PIC S9(08)V99 
                                             LEADING SEPARATE.
                   15  WK-TARJETA-FEC        PIC 9(04).
                   15  WK-TARJETA-CCV        PIC 9(03).

           05  WK-MOVIMIENTO-TABLA.
               10  WK-MOV-CONTADOR           PIC 9(01) VALUE 0.
               10  WK-MOVIMIENTO             OCCURS 5 TIMES.
                   15  WK-MOV-CPT            PIC X(23).
                   15  WK-MOV-IMPORTE        PIC S9(08)V99 
                                             LEADING SEPARATE.
                   15  WK-MOV-NUM-MEDIO-CTA  PIC 9(20).
                   15  WK-MOV-NUM-MEDIO-TAR  PIC 9(16).    
                   15  WK-MOV-TIPO-MEDIO     PIC X(01).
                       88  MOV-TIPO-MEDIO-C  VALUE 'C'.
                       88  MOV-TIPO-MEDIO-T  VALUE 'T'.
                   15  WK-MOV-FEC            PIC 9(20).

      * Variables auxiliares para buscar números de cuentas y contador.
       01  SW-ENCONTRADO                     PIC 9(01).
           88  ENCONTRADO-SI                 VALUE 1.
           88  ENCONTRADO-NO                 VALUE 0.

       01  WK-I                              PIC 9(01).

      * Formateo de fecha nacimiento cliente.
       01  WK-FECHA-AUX                      PIC 9(08).
       01  WK-FECHA-AUX-R                    REDEFINES WK-FECHA-AUX.
           05  WK-FECHA-AUX-AAAA             PIC 9(04).
           05  WK-FECHA-AUX-MM               PIC 9(02).
           05  WK-FECHA-AUX-DD               PIC 9(02).

      * Formateo de fecha nacimiento cliente dividida por barras 
      * para mejor DISPLAY.
       01  WK-FECHA-DMA.
           05  WK-FECHA-DMA-DIA              PIC 9(02).
           05  FILLER                        PIC X(01) VALUE '/'.
           05  WK-FECHA-DMA-MES              PIC 9(02).
           05  FILLER                        PIC X(01) VALUE '/'.
           05  WK-FECHA-DMA-ANO              PIC 9(04).
      
      * Formateo de número de cuenta cliente.
       01  WK-NUM-CUENTA-AUX                 PIC 9(20).
       01  WK-NUM-CUENTA-AUX-R               REDEFINES 
           WK-NUM-CUENTA-AUX.
           05  WK-NUM-CUENTA-AUX-EEEE        PIC 9(04).
           05  WK-NUM-CUENTA-AUX-OOOO        PIC 9(04).
           05  WK-NUM-CUENTA-AUX-DD          PIC 9(02).
           05  WK-NUM-CUENTA-AUX-PPPP        PIC 9(04).
           05  WK-NUM-CUENTA-AUX-CCCCCC      PIC 9(06).

      * Formateo de número de cuenta cliente con espacios para DISPLAY.
       01  WK-NUM-CUENTA-SEPARADA.
           05  WK-NUM-CUENTA-AUX-ENT         PIC 9(04).
           05  FILLER                        PIC X(01) VALUE ' '.
           05  WK-NUM-CUENTA-AUX-OFC         PIC 9(04).
           05  FILLER                        PIC X(01) VALUE ' '.
           05  WK-NUM-CUENTA-AUX-DC          PIC 9(02).
           05  FILLER                        PIC X(01) VALUE ' '.
           05  WK-NUM-CUENTA-AUX-PROD        PIC 9(04).
           05  FILLER                        PIC X(01) VALUE ' '.
           05  WK-NUM-CUENTA-AUX-CNT         PIC 9(06).

      * Formateo de número de tarjeta.
       01  WK-NUM-TARJETA-AUX                PIC 9(16).
       01  WK-NUM-TARJETA-AUX-R              REDEFINES 
           WK-NUM-TARJETA-AUX.
           05  WK-NUM-TARJETA-AUX-G1         PIC 9(04).
           05  WK-NUM-TARJETA-AUX-G2         PIC 9(04).
           05  WK-NUM-TARJETA-AUX-G3         PIC 9(04).
           05  WK-NUM-TARJETA-AUX-G4         PIC 9(04).

      * Formateo de número de tarjeta con espacios para DISPLAY.
       01  WK-NUM-TARJETA-SEPARADA.
           05  WK-NUM-TARJETA-SEP-G1         PIC 9(04).
           05  FILLER                        PIC X(01) VALUE ' '.
           05  WK-NUM-TARJETA-SEP-G2         PIC 9(04).
           05  FILLER                        PIC X(01) VALUE ' '.
           05  WK-NUM-TARJETA-SEP-G3         PIC 9(04).
           05  FILLER                        PIC X(01) VALUE ' '.
           05  WK-NUM-TARJETA-SEP-G4         PIC 9(04).
      
      * Formateo de fecha de caducidad de la tarjeta.
       01  WK-TARJETA-FEC-AUX                PIC 9(04).
       01  WK-TARJETA-FEC-AUX-R              REDEFINES 
           WK-TARJETA-FEC-AUX.
           05  WK-TARJETA-FEC-AUX-DD         PIC 9(02).
           05  WK-TARJETA-FEC-AUX-MM         PIC 9(02).

      * Formateo de fecha de caducidad de la tarjeta dividida por barras 
      * para mejor DISPLAY.
       01  WK-TARJETA-FEC-MA.
           05  WK-FECHA-MA-DIA               PIC 9(02).
           05  FILLER                        PIC X(01) VALUE '/'.
           05  WK-FECHA-MA-MES               PIC 9(02).

      * Formateo de fecha de movimiento.
       01  WK-MOV-FEC-AUX                    PIC 9(20).
       01  WK-MOV-FEC-AUX-R                  REDEFINES WK-MOV-FEC-AUX.
           05  WK-MOV-FEC-AAAA               PIC 9(04).
           05  WK-MOV-FEC-MM                 PIC 9(02).
           05  WK-MOV-FEC-DD                 PIC 9(02).
           05  WK-MOV-FEC-HH                 PIC 9(02).
           05  WK-MOV-FEC-MN                 PIC 9(02).
           05  WK-MOV-FEC-SS                 PIC 9(02).
           05  WK-MOV-FEC-FFFFFF             PIC 9(06).

      * Formateo de fecha de movimiento para mejor DISPLAY.
       01  WK-MOV-FEC-AUX-D.
           05  WK-MOV-FEC-D-AAAA             PIC 9(04).
           05  FILLER                        VALUE '-'.
           05  WK-MOV-FEC-D-MM               PIC 9(02).
           05  FILLER                        VALUE '-'.
           05  WK-MOV-FEC-D-DD               PIC 9(02).
           05  FILLER                        VALUE ' '.
           05  WK-MOV-FEC-D-HH               PIC 9(02).
           05  FILLER                        VALUE ':'.
           05  WK-MOV-FEC-D-MN               PIC 9(02).
           05  FILLER                        VALUE ':'.
           05  WK-MOV-FEC-D-SS               PIC 9(02).
           05  FILLER                        VALUE '.'.
           05  WK-MOV-FEC-D-FFFFFF           PIC 9(06).

      * Formateo de la relación cuenta-cliente.
       01  SW-RLN-CLI-CUE                    PIC X(10).
           88 RLN-TIT                        VALUE 'TITULAR'.
           88 RLN-COT                        VALUE 'COTITULAR'.
           88 RLN-AUT                        VALUE 'AUTORIZADO'.

      * Formateo de la relación movimiento-cuenta-tarjeta.
       01  SW-MOV-CTA-TAR                    PIC X(07).
           88  MOV-CTA                       VALUE 'CUENTA'.
           88  MOV-TAR                       VALUE 'TARJETA'.

      * Formateo de los importes. Importante indicar el formato (líneas
      * 11 y 12).
       01  WK-CTA-SALDO-FMT                  PIC +ZZ.ZZZ.ZZZ,99.
       01  WK-TAR-CRED-FMT                   PIC +ZZ.ZZZ.ZZZ,99.
       01  WK-MOV-IMP-FMT                    PIC +ZZ.ZZZ.ZZZ,99.

       01  FS-FICHCLI                        PIC X(02).

       01  SW-STAT-FICHERO                   PIC 9(01).
           88  STAT-FICHERO-ABRT             VALUE 1.
           88  STAT-FICHERO-CERR             VALUE 0.

       01  SW-FIN-FICHERO                    PIC 9(01).
           88  FIN-FICHERO-SI                VALUE 1.
           88  FIN-FICHERO-NO                VALUE 0.

           COPY 'altacliente.cpy'.
       
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       DECLARATIVES.
      *-------------
      * Esta sección es una especie de "catch" de C++ o Java. El flujo
      * del programa se desviaría a esta sección automáticamente si se
      * produjera algún error de E/S.
       FS-FICHCLI-ERROR SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON FICHCLI.
       CONTROL-FS-FICHCLI.
      *    Si se ha producido algún error de E/S, entonces mostrar el
      *    código de error y devolverlo al sistema operativo.
           IF   FS-FICHCLI NOT = '00'
           THEN DISPLAY 'ERROR E/S FILE STATUS [' FS-FICHCLI ']'
                SET STAT-ERR-IO               TO TRUE
      *         No se traga la sentencia GO TO.
      *         GO TO FIN-PRG
                IF   STAT-FICHERO-ABRT
                THEN CLOSE FICHCLI
                END-IF

                STOP RUN RETURNING SW-STAT
           END-IF.
       END DECLARATIVES.

       INICIO-PROGRAMA.
      *----------------
           PERFORM INICIALIZAR                THRU FIN-INICIALIZAR.
           PERFORM ABRIR-FICHERO              THRU FIN-ABRIR-FICHERO.
           PERFORM LEER-FICHERO               THRU FIN-LEER-FICHERO.
           
      *    Repetir hasta el final del fichero o error. 
      *    Leemos el registro, y vamos transitando por los distintos 
      *    estados(leyendo los tipos de registro). Dentro de cada estado
      *    transitaremos al siguiente y volveremos a este bucle.
           PERFORM UNTIL FIN-FICHERO-SI OR NOT STAT-OK
                
                EVALUATE TRUE
                WHEN AFD-STAT-Q0
                     PERFORM PROCESAR-STAT-Q0 THRU FIN-PROCESAR-STAT-Q0
                WHEN AFD-STAT-Q1
                     PERFORM PROCESAR-STAT-Q1 THRU FIN-PROCESAR-STAT-Q1
                WHEN AFD-STAT-Q2
                     PERFORM PROCESAR-STAT-Q2 THRU FIN-PROCESAR-STAT-Q2
                WHEN AFD-STAT-Q3
                     PERFORM PROCESAR-STAT-Q3 THRU FIN-PROCESAR-STAT-Q3
                WHEN AFD-STAT-Q4
                     PERFORM PROCESAR-STAT-Q4 THRU FIN-PROCESAR-STAT-Q4
                WHEN OTHER
                     SET STAT-ERR-AFD         TO TRUE
                END-EVALUATE

                PERFORM LEER-FICHERO          THRU FIN-LEER-FICHERO

           END-PERFORM.

      *    Comprobar si el AFD ha llegado a un estado final.
      *    En caso afirmativo, imprimir el cliente, e insertarlo en la 
      *    BBDD.

           MOVE SW-AFD-STAT                   TO SW-AFD-STAT-FINAL.
           IF AFD-STAT-FINAL-SI AND STAT-OK
           THEN PERFORM IMPR-CLIENTE          THRU FIN-IMPR-CLIENTE
                PERFORM ALTA-CLIENTE          THRU FIN-ALTA-CLIENTE
           END-IF.

       FIN-PROGRAMA.
           PERFORM CERRAR-FICHERO             THRU FIN-CERRAR-FICHERO.
           PERFORM MOSTRAR-ESTADO             THRU FIN-MOSTRAR-ESTADO.
           STOP RUN RETURNING SW-STAT.

       INICIALIZAR.
      *------------
      * Lo que es alfabético lo inicializa a espacios y lo que es numé-
      * rico lo inicializa a ceros.
           INITIALIZE                         TIP-REG
                                              WK-OBJ-CLIENTE.
                                         
           SET AFD-STAT-Q0                    TO TRUE.                              
           SET STAT-FICHERO-CERR              TO TRUE.
           SET FIN-FICHERO-NO                 TO TRUE.
           SET STAT-OK                        TO TRUE.

       FIN-INICIALIZAR.
           EXIT.

       ABRIR-FICHERO.
      *--------------
           IF   STAT-FICHERO-CERR
           THEN OPEN INPUT FICHCLI
                SET STAT-FICHERO-ABRT         TO TRUE
                SET FIN-FICHERO-NO            TO TRUE
                DISPLAY 'FICHERO ABIERTO'
           END-IF.

       FIN-ABRIR-FICHERO.
           EXIT.

       CERRAR-FICHERO.
      *---------------
           IF   STAT-FICHERO-ABRT
           THEN SET STAT-FICHERO-CERR         TO TRUE
                CLOSE FICHCLI
                DISPLAY 'FICHERO CERRADO'
           END-IF.

       FIN-CERRAR-FICHERO.
           EXIT.

       LEER-FICHERO.
      *-------------
           IF   STAT-FICHERO-ABRT
           THEN
      *         Inicializar nuestras estructuras de trabajo para evitar
      *         que contentan datos de la lectura anterior.
                INITIALIZE               TIP-REG

      *         Leer la siguiente línea del fichero.
      *         Si no es fin de fichero, entonces tenemos que identifi-
      *         car qué tipo de registro es. Al mover el contenido del
      *         búfer de lectura, REG-FICHCLI, en la estructura temporal
      *         TIP-REG, el switch REG-TIPO se inicializa.
                READ FICHCLI             INTO TIP-REG
                AT END
      *            Activar el switch de fin de fichero cuando leamos pa-
      *            sado el último registro del fichero.
                   SET FIN-FICHERO-SI    TO TRUE
                END-READ
           END-IF.

       FIN-LEER-FICHERO.
           EXIT.

       PROCESAR-STAT-Q0.
      *-----------------
           EVALUATE TRUE
           WHEN REG-TIPO-CLIENTE
                SET AFD-STAT-Q1           TO TRUE
                PERFORM VALIDAR-CLIENTE   THRU FIN-VALIDAR-CLIENTE
                IF STAT-OK
                THEN 
                   PERFORM CARGAR-CLIENTE THRU FIN-CARGAR-CLIENTE
                END-IF
                
           WHEN REG-TIPO-DOMICILIO
                SET AFD-STAT-Q0           TO TRUE

           WHEN REG-TIPO-CUENTA-TARJETA
                SET AFD-STAT-Q0           TO TRUE

           WHEN REG-TIPO-MOVIMIENTO
                SET AFD-STAT-Q0           TO TRUE

           WHEN OTHER
                SET STAT-ERR-TIP-REG      TO TRUE

           END-EVALUATE.

       FIN-PROCESAR-STAT-Q0.
           EXIT.

       PROCESAR-STAT-Q1.
      *-----------------
           EVALUATE TRUE
           WHEN REG-TIPO-CLIENTE
                SET AFD-STAT-Q0             TO TRUE
                PERFORM INIZ-CLIENTE        THRU FIN-INIZ-CLIENTE

           WHEN REG-TIPO-DOMICILIO
                SET AFD-STAT-Q2             TO TRUE
                PERFORM VALIDAR-DOMICILIO   THRU FIN-VALIDAR-DOMICILIO
                IF STAT-OK
                THEN
                   PERFORM CARGAR-DOMICILIO THRU FIN-CARGAR-DOMICILIO
                END-IF

           WHEN REG-TIPO-CUENTA-TARJETA
                SET AFD-STAT-Q3             TO TRUE
                PERFORM VALIDAR-CUENTA-TARJETA 
                THRU FIN-VALIDAR-CUENTA-TARJETA
                IF STAT-OK
                THEN
                   PERFORM CARGAR-CUENTA    THRU FIN-CARGAR-CUENTA
                END-IF

           WHEN REG-TIPO-MOVIMIENTO
                SET AFD-STAT-Q0             TO TRUE
                PERFORM INIZ-CLIENTE        THRU FIN-INIZ-CLIENTE

           WHEN OTHER
                SET STAT-ERR-TIP-REG        TO TRUE

           END-EVALUATE.

       FIN-PROCESAR-STAT-Q1.
           EXIT.

       PROCESAR-STAT-Q2.
      *-----------------
           EVALUATE TRUE
           WHEN REG-TIPO-CLIENTE
                SET AFD-STAT-Q0             TO TRUE
                PERFORM INIZ-CLIENTE        THRU FIN-INIZ-CLIENTE

           WHEN REG-TIPO-DOMICILIO
                SET AFD-STAT-Q0             TO TRUE
                PERFORM INIZ-CLIENTE        THRU FIN-INIZ-CLIENTE

           WHEN REG-TIPO-CUENTA-TARJETA
                SET AFD-STAT-Q3             TO TRUE
                PERFORM VALIDAR-CUENTA-TARJETA 
                THRU FIN-VALIDAR-CUENTA-TARJETA
                IF STAT-OK
                THEN
                   PERFORM CARGAR-CUENTA    THRU FIN-CARGAR-CUENTA
                END-IF

           WHEN REG-TIPO-MOVIMIENTO
                SET AFD-STAT-Q0             TO TRUE
                PERFORM INIZ-CLIENTE        THRU FIN-INIZ-CLIENTE

           WHEN OTHER
                SET STAT-ERR-TIP-REG        TO TRUE

           END-EVALUATE.

       FIN-PROCESAR-STAT-Q2.
           EXIT.

       PROCESAR-STAT-Q3.
      *-----------------
           EVALUATE TRUE
           WHEN REG-TIPO-CLIENTE
                SET AFD-STAT-Q1             TO TRUE
                PERFORM IMPR-CLIENTE        THRU FIN-IMPR-CLIENTE
                PERFORM ALTA-CLIENTE        THRU FIN-ALTA-CLIENTE
                PERFORM IMPR-LINEA          THRU FIN-IMPR-LINEA
                PERFORM INIZ-CLIENTE        THRU FIN-INIZ-CLIENTE
                PERFORM VALIDAR-CLIENTE     THRU FIN-VALIDAR-CLIENTE
                IF STAT-OK
                THEN 
                   PERFORM CARGAR-CLIENTE   THRU FIN-CARGAR-CLIENTE
                END-IF

           WHEN REG-TIPO-DOMICILIO
                SET AFD-STAT-Q0             TO TRUE
                PERFORM INIZ-CLIENTE        THRU FIN-INIZ-CLIENTE

           WHEN REG-TIPO-CUENTA-TARJETA
                SET AFD-STAT-Q3             TO TRUE
                PERFORM VALIDAR-CUENTA-TARJETA 
                THRU FIN-VALIDAR-CUENTA-TARJETA
                IF STAT-OK
                THEN
                   PERFORM CARGAR-CUENTA    THRU FIN-CARGAR-CUENTA
                END-IF

           WHEN REG-TIPO-MOVIMIENTO
                SET AFD-STAT-Q4             TO TRUE
                PERFORM VALIDAR-MOVIMIENTO  
                THRU FIN-VALIDAR-MOVIMIENTO
                IF STAT-OK
                THEN
                   PERFORM ANADIR-MOVIMIENTO THRU FIN-ANADIR-MOVIMIENTO
                END-IF

           WHEN OTHER
                SET STAT-ERR-TIP-REG         TO TRUE

           END-EVALUATE.

       FIN-PROCESAR-STAT-Q3.
           EXIT.

       PROCESAR-STAT-Q4.
      *-----------------
           EVALUATE TRUE
           WHEN REG-TIPO-CLIENTE
                SET AFD-STAT-Q1             TO TRUE
                PERFORM IMPR-CLIENTE        THRU FIN-IMPR-CLIENTE
                PERFORM ALTA-CLIENTE        THRU FIN-ALTA-CLIENTE
                PERFORM IMPR-LINEA          THRU FIN-IMPR-LINEA
                PERFORM INIZ-CLIENTE        THRU FIN-INIZ-CLIENTE
                PERFORM VALIDAR-CLIENTE     THRU FIN-VALIDAR-CLIENTE
                IF STAT-OK
                THEN 
                   PERFORM CARGAR-CLIENTE   THRU FIN-CARGAR-CLIENTE
                END-IF

           WHEN REG-TIPO-DOMICILIO
                SET AFD-STAT-Q0             TO TRUE
                PERFORM INIZ-CLIENTE        THRU FIN-INIZ-CLIENTE

           WHEN REG-TIPO-CUENTA-TARJETA
                SET AFD-STAT-Q0             TO TRUE
                PERFORM INIZ-CLIENTE        THRU FIN-INIZ-CLIENTE

           WHEN REG-TIPO-MOVIMIENTO
                SET AFD-STAT-Q4             TO TRUE
                PERFORM VALIDAR-MOVIMIENTO  THRU FIN-VALIDAR-MOVIMIENTO
                IF STAT-OK
                THEN 
                   PERFORM ANADIR-MOVIMIENTO 
                   THRU FIN-ANADIR-MOVIMIENTO
                END-IF

           WHEN OTHER
                SET STAT-ERR-TIP-REG        TO TRUE

           END-EVALUATE.

       FIN-PROCESAR-STAT-Q4.
           EXIT.

      * Párrafos para validar que los tipos de datos de entrada que 
      * figuran en el fichero sean los adecuados y se puedan procesar
      * correctamente. Si no, esos errores se manejan en las variables
      * designadas para ello "STAT-ERR-...".
       VALIDAR-CLIENTE.
      *----------------     
           EVALUATE TRUE
           WHEN REG-CLIENTE-NOMBRE = ALL SPACES
                SET STAT-ERR-NOMBRE TO TRUE
           WHEN REG-CLIENTE-FEC-NAC-X IS NOT NUMERIC
                OR REG-CLIENTE-FEC-NAC = ZEROES
                SET STAT-ERR-FEC-NAC TO TRUE
           WHEN REG-CLIENTE-NIF = ALL SPACES
                SET STAT-ERR-NIF TO TRUE
           WHEN OTHER
                CONTINUE
           END-EVALUATE.

       FIN-VALIDAR-CLIENTE.
           EXIT.

       VALIDAR-DOMICILIO.
      *------------------
           EVALUATE TRUE
           WHEN REG-DOMICILIO-CALLE = ALL SPACES
                SET STAT-ERR-CALLE TO TRUE
           WHEN REG-DOMICILIO-CODPOST-X IS NOT NUMERIC
                DISPLAY REG-DOMICILIO-CODPOST REG-DOMICILIO-CODPOST-X
                SET STAT-ERR-COD-POST TO TRUE
           WHEN REG-DOMICILIO-POBL = ALL SPACES
                SET STAT-ERR-POBL TO TRUE
           WHEN REG-DOMICILIO-PROV = ALL SPACES
                SET STAT-ERR-PROV TO TRUE
           WHEN OTHER
                CONTINUE
           END-EVALUATE.

       FIN-VALIDAR-DOMICILIO.
           EXIT.

       VALIDAR-CUENTA-TARJETA.
      *-----------------------
           EVALUATE TRUE
           WHEN REG-CUENTA-NUMERO-X = ALL SPACES
                OR REG-CUENTA-NUMERO-X IS NOT NUMERIC
                OR REG-CUENTA-NUMERO = ZEROES
                SET STAT-ERR-NUM-CUENTA TO TRUE

           WHEN (REG-CUENTA-SALDO-S IS NOT = ("-" AND "+" AND " "))
                OR REG-CUENTA-SALDO-VAL IS NOT NUMERIC
                SET STAT-ERR-SALDO TO TRUE

           WHEN (REG-CUENTA-AUT IS NOT = ("T" AND "C" AND "A"))
                OR REG-CUENTA-AUT = ALL SPACES
                SET STAT-ERR-CUENTA-AUT TO TRUE

      *    Si la tarjeta viene informada   
           WHEN (REG-TARJETA-NUMERO-X IS NOT = ALL SPACES AND 
                 REG-TARJETA-NUMERO-X IS NOT NUMERIC)
                 OR (REG-TARJETA-NUMERO-X IS NUMERIC AND
                     REG-TARJETA-NUMERO = ZEROES)
                 SET STAT-ERR-NUM-TARJETA TO TRUE

           WHEN REG-TARJETA-NUMERO-X IS NUMERIC AND
                ((REG-TARJETA-CREDITO-S IS NOT = ("-" AND "+" AND " ")) 
                OR REG-TARJETA-CREDITO-VAL IS NOT NUMERIC 
                OR REG-TARJETA-CREDITO-VAL = ALL SPACES)
                SET STAT-ERR-CREDITO TO TRUE

           WHEN REG-TARJETA-NUMERO-X IS NUMERIC AND
                (REG-TARJETA-FEC-X IS NOT NUMERIC
                OR REG-TARJETA-FEC = ZEROES)
                SET STAT-ERR-TARJETA-FEC TO TRUE

           WHEN OTHER
                CONTINUE
           END-EVALUATE.

       FIN-VALIDAR-CUENTA-TARJETA.
           EXIT.

       VALIDAR-MOVIMIENTO.
      *-------------------
           EVALUATE TRUE
           WHEN REG-MOVIMIENTO-CPT = ALL SPACES
                SET STAT-ERR-MOV-CPT TO TRUE
           WHEN REG-MOVIMIENTO-TIPO-MEDIO IS NOT = ('T' AND 'C')
                SET STAT-ERR-MOV-MEDIO TO TRUE
           WHEN REG-MOV-IMPORTE-S IS NOT = ('-' AND '+' AND ' ')
                OR REG-MOV-IMPORTE-VAL IS NOT NUMERIC
                SET STAT-ERR-MOV-IMPORTE TO TRUE
           WHEN REG-MOVIMIENTO-FEC-X IS NOT NUMERIC
                OR REG-MOVIMIENTO-FEC = ZEROES
                SET STAT-ERR-MOV-FEC  TO TRUE
           WHEN OTHER
                CONTINUE
           END-EVALUATE.

      * Para validar el número de cuenta o tarjeta se tiene que 
      * comprobar primero el tipo de medio.
           IF REG-MOVIMIENTO-TIPO-MEDIO = 'T'
      *    Si es una tarjeta movemos el número de medio a una variable
      *    alfabética y sin 4 espacios al final correspondiente a
      *    caracteres vacíos.
              THEN IF   REG-MOVIMIENTO-NUM-TAR IS NOT NUMERIC
                   THEN SET STAT-ERR-MOV-NUM-MEDIO TO TRUE
                   END-IF
              ELSE IF   REG-MOVIMIENTO-NUM-MEDIO-X IS NOT NUMERIC
                   THEN SET STAT-ERR-MOV-NUM-MEDIO TO TRUE
                   END-IF
           END-IF.

       FIN-VALIDAR-MOVIMIENTO.
           EXIT.

       GENERAR-CCV-ALEATORIO.
      *----------------------
      * Se cogen datos numéricos de la fecha y hora del sistema. Se 
      * pasan y combinan esos números a una variable y se divide por 999
      * quedándose el resto como nuestro número pseudo aleatorio.  
           ACCEPT WK-FECHA-ACTUAL      FROM DATE.
           ACCEPT WK-HORA-ACTUAL       FROM TIME.

           MOVE WK-FECHA-ACTUAL        TO WK-SEMILLA(1:6).
           MOVE WK-HORA-ACTUAL         TO WK-SEMILLA(7:6).

           DIVIDE WK-SEMILLA           BY 999 GIVING WK-SEMILLA 
                                       REMAINDER WK-CCV-ALEATORIO.
           ADD 1                       TO WK-CCV-CONTADOR.

       FIN-GENERAR-CCV-ALEATORIO.
           EXIT.

       CARGAR-CLIENTE.
      *---------------
           MOVE REG-CLIENTE-NOMBRE      TO WK-CLIENTE-NOMBRE.
           MOVE REG-CLIENTE-FEC-NAC     TO WK-CLIENTE-FEC-NAC.
           MOVE REG-CLIENTE-NIF         TO WK-CLIENTE-NIF.

       FIN-CARGAR-CLIENTE.
           EXIT.

       CARGAR-DOMICILIO.
      *-----------------
           MOVE  REG-DOMICILIO-CALLE    TO WK-DOMICILIO-CALLE.
           MOVE  REG-DOMICILIO-NUMERO   TO WK-DOMICILIO-NUMERO.
           MOVE  REG-DOMICILIO-CODPOST  TO WK-DOMICILIO-CODPOST.
           MOVE  REG-DOMICILIO-PROV     TO WK-DOMICILIO-PROV.
           MOVE  REG-DOMICILIO-POBL     TO WK-DOMICILIO-POBL.
           
           STRING
                    WK-DOMICILIO-CALLE   DELIMITED BY SIZE
                    ', '                  DELIMITED BY SIZE
                    WK-DOMICILIO-NUMERO  DELIMITED BY SIZE
                    ', '                  DELIMITED BY SIZE
                    WK-DOMICILIO-CODPOST DELIMITED BY SIZE
                    ', '                  DELIMITED BY SIZE
                    WK-DOMICILIO-PROV    DELIMITED BY SIZE
                    ', '                  DELIMITED BY SIZE
                    WK-DOMICILIO-POBL    DELIMITED BY SIZE
                INTO WK-DOMICILIO-COMPL
           END-STRING.

       FIN-CARGAR-DOMICILIO.
           EXIT.

       ANADIR-CUENTA.
      *--------------
      *    Nos aseguramos que la tabla de cuentas/tarjetas no está 
      *    completa antes de añadir una nueva cuenta/tarjeta.
           IF   WK-CUE-TAR-CONTADOR < WK-CTE-CUENTAS-MAX
           THEN ADD 1                    TO WK-CUE-TAR-CONTADOR
                MOVE REG-CUENTA-NUMERO 
                TO WK-CLIENTE-NUM-CTA(WK-CUE-TAR-CONTADOR)
                MOVE REG-CUENTA-SALDO
                TO WK-CLIENTE-SALD-CTA(WK-CUE-TAR-CONTADOR)
                MOVE REG-TARJETA-NUMERO
                TO WK-CLIENTE-NUM-TAR(WK-CUE-TAR-CONTADOR)
                MOVE REG-TARJETA-CREDITO
                TO WK-CLIENTE-CRE-TAR(WK-CUE-TAR-CONTADOR)
                MOVE REG-CUENTA-AUT
                TO SW-CLIENTE-RLN-CTA(WK-CUE-TAR-CONTADOR)
                MOVE REG-TARJETA-FEC
                TO WK-TARJETA-FEC(WK-CUE-TAR-CONTADOR)

                PERFORM GENERAR-CCV-ALEATORIO 
                THRU FIN-GENERAR-CCV-ALEATORIO
                
                ADD WK-CCV-CONTADOR   TO WK-CCV-ALEATORIO
                
                MOVE WK-CCV-ALEATORIO
                TO WK-TARJETA-CCV(WK-CUE-TAR-CONTADOR)

                MOVE REG-CUENTA-AUT
                TO SW-CLIENTE-RLN-CTA(WK-CUE-TAR-CONTADOR)
            ELSE SET STAT-ERR-CTA-MAX TO TRUE
           END-IF.

       FIN-ANADIR-CUENTA.
           EXIT.

       BUSCAR-CUENTA.
      *--------------
      *    Buscar cuentas repetidas por cliente.
           MOVE 1                        TO WK-I.
           SET ENCONTRADO-NO             TO TRUE.
           
           PERFORM UNTIL ENCONTRADO-SI OR WK-I > WK-CUE-TAR-CONTADOR
                IF   WK-NUM-CUENTA-AUX = WK-CLIENTE-NUM-CTA(WK-I)
                THEN SET ENCONTRADO-SI   TO TRUE
                ELSE ADD 1               TO WK-I
                END-IF
           END-PERFORM.

       FIN-BUSCAR-CUENTA.
           EXIT.

       BUSCAR-TARJETA.
      *---------------
      *    Buscar tarjetas repetidas por cliente.
           MOVE 1                        TO WK-I.
           SET ENCONTRADO-NO             TO TRUE.
           
           PERFORM UNTIL ENCONTRADO-SI OR WK-I > WK-CUE-TAR-CONTADOR
                IF WK-NUM-TARJETA-AUX = WK-CLIENTE-NUM-TAR(WK-I) AND
                   WK-CLIENTE-NUM-TAR(WK-I) > ZERO
                THEN SET ENCONTRADO-SI   TO TRUE
                ELSE ADD 1               TO WK-I
                END-IF
           END-PERFORM.

       FIN-BUSCAR-TARJETA.
           EXIT.

       BUSCAR-MEDIO-MOVIMIENTO.
      *------------------------
      * Comprueba si el medio que venga indicado en el movimiento en el
      * que se hizo el cargo existe. Ya sea una tarjeta, o una cuenta.
           MOVE 1                        TO WK-I.
           SET ENCONTRADO-NO             TO TRUE.
           
           PERFORM UNTIL ENCONTRADO-SI OR WK-I > WK-CUE-TAR-CONTADOR
                IF WK-NUM-CUENTA-AUX = WK-CLIENTE-NUM-CTA(WK-I)
                THEN SET ENCONTRADO-SI   TO TRUE
                ELSE ADD 1               TO WK-I
                END-IF
           END-PERFORM.

           IF ENCONTRADO-NO
              PERFORM UNTIL ENCONTRADO-SI OR WK-I > WK-CUE-TAR-CONTADOR
                IF WK-NUM-TARJETA-AUX = WK-CLIENTE-NUM-TAR(WK-I) AND
                   WK-CLIENTE-NUM-TAR(WK-I) > ZERO
                THEN SET ENCONTRADO-SI   TO TRUE
                ELSE ADD 1               TO WK-I
                END-IF
           END-PERFORM
           END-IF.

       FIN-BUSCAR-MEDIO-MOVIMIENTO.
           EXIT.

       CARGAR-CUENTA.
      *--------------
      *    Cuando el registro sea de tipo cuenta tenemos que
      *    asegurarnos de que la cuenta no esté repetida para un
      *    mismo cliente. Si no lo estuviera, la guardamos, si lo
      *    está, pasamos un error de que la cuenta estaría duplicada.   
           MOVE REG-CUENTA-NUMERO        TO WK-NUM-CUENTA-AUX.  
           PERFORM BUSCAR-CUENTA         THRU FIN-BUSCAR-CUENTA.

           IF   ENCONTRADO-NO
           THEN
                MOVE REG-TARJETA-NUMERO  TO WK-NUM-TARJETA-AUX
                PERFORM BUSCAR-TARJETA   THRU FIN-BUSCAR-TARJETA
                IF ENCONTRADO-NO
                THEN PERFORM ANADIR-CUENTA THRU FIN-ANADIR-CUENTA
                ELSE SET STAT-ERR-TAR-DUPL TO TRUE
                END-IF
           ELSE 
                SET STAT-ERR-CTA-DUPL TO TRUE
           END-IF.

       FIN-CARGAR-CUENTA.
           EXIT.

       CARGAR-MOVIMIENTO.
      *------------------
      *    Cuando el registro sea de tipo cuenta tenemos que
      *    asegurarnos de que la cuenta no esté repetida para un
      *    mismo cliente. Si no lo estuviera, la guardamos, si lo
      *    está, pasamos un error de que la cuenta estaría duplicada.   
           MOVE REG-MOVIMIENTO-NUM-MEDIO        TO WK-NUM-CUENTA-AUX.
           MOVE REG-MOVIMIENTO-NUM-TAR          TO WK-NUM-TARJETA-AUX.

           PERFORM BUSCAR-MEDIO-MOVIMIENTO
           THRU FIN-BUSCAR-MEDIO-MOVIMIENTO.

           IF   ENCONTRADO-SI
           THEN PERFORM ANADIR-MOVIMIENTO  THRU FIN-ANADIR-MOVIMIENTO
           ELSE SET STAT-ERR-MOV-NO-MEDIO  TO TRUE
           END-IF.
           
       FIN-CARGAR-MOVIMIENTO.
           EXIT.

       ANADIR-MOVIMIENTO.
      *------------------
      *    Nos aseguramos que la tabla de movimientos no está completa 
      *    antes de añadir un nuevo movimiento.
           IF   WK-MOV-CONTADOR < WK-MOV-MAX
           THEN ADD 1                    TO WK-MOV-CONTADOR
                MOVE REG-MOVIMIENTO-CPT 
                TO WK-MOV-CPT(WK-MOV-CONTADOR)

                IF   REG-MOVIMIENTO-TIPO-MEDIO = 'T'
                THEN MOVE REG-MOVIMIENTO-NUM-TAR
                     TO WK-MOV-NUM-MEDIO-TAR(WK-MOV-CONTADOR)
                ELSE MOVE REG-MOVIMIENTO-NUM-MEDIO
                     TO WK-MOV-NUM-MEDIO-CTA(WK-MOV-CONTADOR)
                END-IF

                MOVE REG-MOVIMIENTO-TIPO-MEDIO
                TO WK-MOV-TIPO-MEDIO(WK-MOV-CONTADOR)
                MOVE REG-MOV-IMPORTE
                TO WK-MOV-IMPORTE(WK-MOV-CONTADOR)
                MOVE REG-MOVIMIENTO-FEC
                TO WK-MOV-FEC(WK-MOV-CONTADOR)
            ELSE SET STAT-ERR-MOV-MAX TO TRUE
           END-IF.
           
       FIN-ANADIR-MOVIMIENTO.
           EXIT.

       IMPR-LINEA.
      *-----------
      * Línea separadora entre clientes.
           DISPLAY '--------------------------------------------------'.
       FIN-IMPR-LINEA.
           EXIT.

       INIZ-CLIENTE.
      *-------------
           INITIALIZE                    WK-OBJ-CLIENTE.

       FIN-INIZ-CLIENTE.
           EXIT.

       FORMAT-FEC-NAC.
      *---------------
           MOVE WK-CLIENTE-FEC-NAC       TO WK-FECHA-AUX.
           MOVE WK-FECHA-AUX-DD          TO WK-FECHA-DMA-DIA.
           MOVE WK-FECHA-AUX-MM          TO WK-FECHA-DMA-MES.
           MOVE WK-FECHA-AUX-AAAA        TO WK-FECHA-DMA-ANO.

       FIN-FORMAT-FEC-NAC.
           EXIT.

       FORMAT-NUM-CUENTA.
      *------------------
           MOVE WK-NUM-CUENTA-AUX-EEEE   TO WK-NUM-CUENTA-AUX-ENT.
           MOVE WK-NUM-CUENTA-AUX-OOOO   TO WK-NUM-CUENTA-AUX-OFC.
           MOVE WK-NUM-CUENTA-AUX-DD     TO WK-NUM-CUENTA-AUX-DC.
           MOVE WK-NUM-CUENTA-AUX-PPPP   TO WK-NUM-CUENTA-AUX-PROD.
           MOVE WK-NUM-CUENTA-AUX-CCCCCC TO WK-NUM-CUENTA-AUX-CNT.

       FIN-FORMAT-NUM-CUENTA.
           EXIT.

       FORMAT-NUM-TARJETA.
      *-------------------
           MOVE WK-NUM-TARJETA-AUX-G1    TO WK-NUM-TARJETA-SEP-G1.
           MOVE WK-NUM-TARJETA-AUX-G2    TO WK-NUM-TARJETA-SEP-G2.
           MOVE WK-NUM-TARJETA-AUX-G3    TO WK-NUM-TARJETA-SEP-G3.
           MOVE WK-NUM-TARJETA-AUX-G4    TO WK-NUM-TARJETA-SEP-G4.

       FIN-FORMAT-NUM-TARJETA.
           EXIT.

       FORMAT-FEC-TARJETA.
      *-------------------
           MOVE WK-TARJETA-FEC-AUX-DD    TO WK-FECHA-MA-DIA.
           MOVE WK-TARJETA-FEC-AUX-MM    TO WK-FECHA-MA-MES.

       FIN-FORMAT-FEC-TARJETA.
           EXIT.

       FORMAT-FEC-MOVIMIENTO.
      *----------------------
           MOVE WK-MOV-FEC-AAAA          TO WK-MOV-FEC-D-AAAA. 
           MOVE WK-MOV-FEC-MM            TO WK-MOV-FEC-D-MM.
           MOVE WK-MOV-FEC-DD            TO WK-MOV-FEC-D-DD.
           MOVE WK-MOV-FEC-HH            TO WK-MOV-FEC-D-HH.
           MOVE WK-MOV-FEC-MN            TO WK-MOV-FEC-D-MN.
           MOVE WK-MOV-FEC-SS            TO WK-MOV-FEC-D-SS.
           MOVE WK-MOV-FEC-FFFFFF        TO WK-MOV-FEC-D-FFFFFF.


       FIN-FORMAT-FEC-MOVIMIENTO.
           EXIT.

       RELACION-CLIENTE-CUENTA.
      *------------------------
      * Párrafo que nos ayuda a formatear la letra que nos indica la 
      * relación entre cliente y cuenta a la palabra completa para 
      * imprimirla posteriormente.
           EVALUATE TRUE
           WHEN CLI-CUE-TIT(WK-I)
               SET RLN-TIT               TO TRUE
           WHEN CLI-CUE-CO(WK-I)
               SET RLN-COT               TO TRUE
           WHEN CLI-CUE-AU(WK-I)
               SET RLN-AUT               TO TRUE
           WHEN OTHER
               CONTINUE
           END-EVALUATE.

       FIN-RELACION-CLIENTE-CUENTA.
           EXIT.

       RELACION-MOVIMIENTO-MEDIO.
      *--------------------------
      * Párrafo para determinar el medio de pago relacionado con un
      * movimiento, cuenta o tarjeta.
           EVALUATE TRUE
           WHEN MOV-TIPO-MEDIO-C(WK-I)
                SET MOV-CTA              TO TRUE
           WHEN MOV-TIPO-MEDIO-T(WK-I)
                SET MOV-TAR              TO TRUE
           WHEN OTHER
                CONTINUE
           END-EVALUATE.

       FIN-RELACION-MOVIMIENTO-MEDIO.
           EXIT.

       IMPR-CLIENTE.
      *-------------
      * Cuando hayamos llegado a un estado final imprimimos todos los
      * registros del cliente que se han ido cargando en nuestra
      * estructura de "objeto".
           IF   WK-CLIENTE-NOMBRE NOT = ALL SPACES
           THEN DISPLAY 'NOMBRE           [' WK-CLIENTE-NOMBRE ']'         
           END-IF.

           IF   WK-CLIENTE-NIF NOT = ALL SPACES
           THEN DISPLAY 'NIF              [' WK-CLIENTE-NIF ']'
           END-IF.

           IF   WK-CLIENTE-FEC-NAC > ZERO
           THEN PERFORM FORMAT-FEC-NAC THRU FIN-FORMAT-FEC-NAC
                DISPLAY 'FECHA NACIMIENTO [' WK-FECHA-DMA ']'
           END-IF.

           IF   WK-DOMICILIO-COMPL NOT = ALL SPACES
           THEN DISPLAY 'DOMILICIO        [' WK-DOMICILIO-COMPL ']'
           END-IF.

      *    Para imprimir el contenido de las tablas de cuentas y 
      *    tarjetas tenemos que recorrerlas asegurándonos de que el 
      *    orden sea el correcto. Que cada tarjeta se imprimirá 
      *    seguidamente de la cuenta a la que está asociada. Para ello 
      *    compartimos el índice WK-I en para ambas tablas.
           PERFORM VARYING WK-I FROM 1 BY 1 
           UNTIL WK-I > WK-CUE-TAR-CONTADOR
      *         Para imprimir las cuentas y las tarjetas llamaremos a 
      *         nuestros párrafos de formateo del contenido para una 
      *         mejor lectura. Y al párrafo que nos formatea la relación 
      *         entre cuenta y cliente (titular, cotitular, autorizado). 
                PERFORM RELACION-CLIENTE-CUENTA
                THRU FIN-RELACION-CLIENTE-CUENTA

                MOVE WK-CLIENTE-NUM-CTA(WK-I) TO WK-NUM-CUENTA-AUX
                PERFORM FORMAT-NUM-CUENTA    THRU FIN-FORMAT-NUM-CUENTA
                
                DISPLAY 'NUMERO CUENTA    [' WK-NUM-CUENTA-SEPARADA 
                                          ' - ' SW-RLN-CLI-CUE ']'

                MOVE    WK-CLIENTE-SALD-CTA(WK-I) TO WK-CTA-SALDO-FMT
                DISPLAY 'SALDO            [' WK-CTA-SALDO-FMT ']'

                IF WK-CLIENTE-NUM-TAR(WK-I) > ZERO
                THEN
                MOVE WK-CLIENTE-NUM-TAR(WK-I) TO WK-NUM-TARJETA-AUX
                PERFORM FORMAT-NUM-TARJETA   THRU FIN-FORMAT-NUM-TARJETA
                
                MOVE WK-TARJETA-FEC(WK-I)     TO WK-TARJETA-FEC-AUX
                PERFORM FORMAT-FEC-TARJETA   THRU FIN-FORMAT-FEC-TARJETA

                DISPLAY 'NUMERO TARJETA   [' WK-NUM-TARJETA-SEPARADA ']'

                MOVE    WK-CLIENTE-CRE-TAR(WK-I) TO WK-TAR-CRED-FMT
                DISPLAY 'CRÉDITO          [' WK-TAR-CRED-FMT ']'
                DISPLAY 'FECHA TARJETA    [' WK-TARJETA-FEC-MA ']'
                DISPLAY 'CCV              [' WK-TARJETA-CCV(WK-I) ']'
                END-IF

           END-PERFORM.

           PERFORM VARYING WK-I FROM 1 BY 1
           UNTIL WK-I > WK-MOV-CONTADOR
      *    Los movimientos van independientes y se imprimen indexando 
      *    con su propio índice.
                 IF   WK-MOV-CPT(WK-I) NOT = ALL SPACES
                 THEN DISPLAY 
                 'MOVIMIENTO       [' WK-MOV-CPT(WK-I) ']'
                 END-IF

                 IF   WK-MOV-NUM-MEDIO-CTA(WK-I) NOT = ALL SPACES
                      AND 
                      WK-MOV-NUM-MEDIO-TAR(WK-I) NOT = ALL SPACES
                      AND 
                      WK-MOV-TIPO-MEDIO(WK-I) NOT = ALL SPACES
                 THEN PERFORM RELACION-MOVIMIENTO-MEDIO
                      THRU FIN-RELACION-MOVIMIENTO-MEDIO
      *         Si el movimiento se realiza desde una tarjeta o cuenta
      *         se realiza un formateo de número de cuenta o tarjeta
      *         reutilizando los párrafos de formateo de num de cuenta o
      *         num de tarjeta. También para eliminar los ceros que se
      *         agregan por defecto al final del num de tarjeta.
                      IF   MOV-CTA
                      THEN MOVE WK-MOV-NUM-MEDIO-CTA(WK-I)
                           TO WK-NUM-CUENTA-AUX
                           PERFORM FORMAT-NUM-CUENTA    
                           THRU FIN-FORMAT-NUM-CUENTA
                           DISPLAY 
                           'CARGADO EN       [' SW-MOV-CTA-TAR ': '   
                           WK-NUM-CUENTA-SEPARADA ']'
                      ELSE MOVE WK-MOV-NUM-MEDIO-TAR(WK-I) 
                           TO WK-NUM-TARJETA-AUX
                           PERFORM FORMAT-NUM-TARJETA    
                           THRU FIN-FORMAT-NUM-TARJETA
                           DISPLAY 
                           'CARGADO EN       [' SW-MOV-CTA-TAR ': '   
                           WK-NUM-TARJETA-SEPARADA ']'
                      END-IF
                 END-IF
           
                 MOVE WK-MOV-IMPORTE(WK-I)  TO WK-MOV-IMP-FMT
                 DISPLAY 
                 'IMPORTE          [' WK-MOV-IMP-FMT ']'

                 IF   WK-MOV-FEC(WK-I) > ZERO
                 THEN MOVE WK-MOV-FEC(WK-I) TO WK-MOV-FEC-AUX
                      PERFORM FORMAT-FEC-MOVIMIENTO 
                      THRU FIN-FORMAT-FEC-MOVIMIENTO
                      DISPLAY 
                      'FECHA            [' WK-MOV-FEC-AUX-D ']'
                 END-IF
           END-PERFORM.
           
       FIN-IMPR-CLIENTE.
           EXIT.

       ALTA-CLIENTE.
      *-------------
      *    Se inicializan las areas de entrada de datos de las copys. 
      *    Se mueven los datos obtenidos y procesados por este pograma a 
      *    su respectiva copy de altacliente y se realizan las búsquedas 
      *    e inserciones.
           INITIALIZE                      ALTACLIENTE-ENTRADA.

      *    Datos de domicilio.
           MOVE WK-DOMICILIO-NUMERO        TO ALTACLI-DOM-NUMERO.
           MOVE WK-DOMICILIO-CALLE         TO ALTACLI-DOM-CALLE.
           MOVE WK-DOMICILIO-CODPOST       TO ALTACLI-DOM-CODPOS.
           MOVE WK-DOMICILIO-PROV          TO ALTACLI-DOM-PROV.    
           MOVE WK-DOMICILIO-POBL          TO ALTACLI-DOM-POBL.

           DISPLAY '*********BANCO-ALTA-CLIENTE************'.
           DISPLAY 'NUM' WK-DOMICILIO-NUMERO.
           DISPLAY 'CALLE' WK-DOMICILIO-CALLE.
           DISPLAY 'CODPOS' WK-DOMICILIO-CODPOST.
           DISPLAY 'PROV' WK-DOMICILIO-PROV.
           DISPLAY 'POBL' WK-DOMICILIO-POBL.

      *    Datos de cliente.
           MOVE WK-CLIENTE-NIF              TO ALTACLI-CLI-NIF.  
           MOVE WK-CLIENTE-NOMBRE           TO ALTACLI-CLI-NOMBRE. 
           MOVE WK-CLIENTE-FEC-NAC          TO ALTACLI-CLI-FEC-NAC.
      
      *    Datos de cuenta y tarjetas.
           MOVE WK-CUE-TAR-CONTADOR         TO ALTACLI-CUE-TAR-CONTADOR.
           MOVE 1                           TO WK-I.
           PERFORM VARYING WK-I FROM 1 BY 1 
           UNTIL WK-I > WK-CUE-TAR-CONTADOR
              MOVE WK-CLIENTE-NUM-CTA(WK-I)  TO ALTACLI-NUM-CTA(WK-I)
              MOVE WK-CLIENTE-SALD-CTA(WK-I) TO ALTACLI-SALDO-CTA(WK-I)
              MOVE SW-CLIENTE-RLN-CTA(WK-I)  TO ALTACLI-RLN-CTA(WK-I)
              MOVE WK-CLIENTE-NUM-TAR(WK-I)  TO ALTACLI-NUM-TAR(WK-I)
              MOVE WK-CLIENTE-CRE-TAR(WK-I)  TO ALTACLI-CRE-TAR(WK-I)
              MOVE WK-TARJETA-FEC(WK-I)      TO ALTACLI-FEC-TAR(WK-I)
              MOVE WK-TARJETA-CCV(WK-I)      TO ALTACLI-CCV-TAR(WK-I)
           END-PERFORM.

      *    Datos de movimiento.
           MOVE WK-MOV-CONTADOR              TO ALTACLI-MOV-CONTADOR.
           MOVE 1                            TO WK-I.
           PERFORM VARYING WK-I FROM 1 BY 1 
           UNTIL WK-I > WK-MOV-CONTADOR
              MOVE WK-MOV-CPT(WK-I)          TO ALTACLI-MOV-CPT(WK-I)
              MOVE WK-MOV-IMPORTE(WK-I)    
              TO ALTACLI-MOV-IMPORTE(WK-I)
              MOVE WK-MOV-NUM-MEDIO-CTA(WK-I) 
              TO ALTACLI-MOV-NUM-MEDIO-CTA(WK-I)
              MOVE WK-MOV-NUM-MEDIO-TAR(WK-I) 
              TO ALTACLI-MOV-NUM-MEDIO-TAR(WK-I)
              MOVE WK-MOV-TIPO-MEDIO(WK-I) 
              TO ALTACLI-MOV-TIPO-MEDIO(WK-I)
              MOVE WK-MOV-FEC(WK-I)          TO ALTACLI-MOV-FEC(WK-I)
           END-PERFORM.

           CALL "ALTACLIENTE"                USING AREA-ALTACLIENTE.

       FIN-ALTA-CLIENTE.
           EXIT.

       MOSTRAR-ESTADO.
      *---------------
      *    Para mostrar el mensaje asociado al error que se haya 
      *    producido añadimos 1 al error y usamos el resultado como 
      *    índice en nuestra tabla de mensajes. Ya que el 0 es el OK, y
      *    y seguidamente todos los mensajes están ordenados a la par 
      *    con su respectivo error.
           ADD 1 TO SW-STAT      GIVING DESC-ERR-INDICE.
           DISPLAY DESC-ERR-TABLA(DESC-ERR-INDICE).
       
       FIN-MOSTRAR-ESTADO.
           EXIT.

