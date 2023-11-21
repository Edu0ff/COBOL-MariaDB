       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. PRBBANCO.
       AUTHOR. MARÍA EDUARDA ALMEIDA LOIOLA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *------------------------
           COPY 'conexmdb.cpy'.
           COPY 'busqcli.cpy'.
           COPY 'inscli.cpy'.
           COPY 'busqcta.cpy'.
           COPY 'inscta.cpy'.
           COPY 'busqdom.cpy'.
           COPY 'insdom.cpy'.
           COPY 'busqtar.cpy'.
           COPY 'instar.cpy'.
           COPY 'busqmov.cpy'.
           COPY 'insmov.cpy'.
           COPY 'busqrln.cpy'.
           COPY 'insrln.cpy'.
          
       01  WK-DATASOURCE                   PIC X(50) VALUE 
           'edu/banco@bancoDS'.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
           
           PERFORM CONECTAR-BD             THRU FIN-CONECTAR-BD.
           
           IF CONEXMDB-STAT-OK
           THEN 
                PERFORM PRUEBA-DOMICILIO   THRU FIN-PRUEBA-DOMICILIO
                PERFORM PRUEBA-CLIENTE     THRU FIN-PRUEBA-CLIENTE
                PERFORM PRUEBA-CUENTA      THRU FIN-PRUEBA-CUENTA
                PERFORM PRUEBA-RELACION    THRU FIN-PRUEBA-RELACION
                PERFORM PRUEBA-TARJETA     THRU FIN-PRUEBA-TARJETA
                PERFORM PRUEBA-MOVIMIENTO  THRU FIN-PRUEBA-MOVIMIENTO
                PERFORM DESCONECTAR-BD     THRU FIN-DESCONECTAR-BD
           END-IF.

           STOP RUN.

       CONECTAR-BD.
      *------------
           INITIALIZE AREA-CONEXMDB-ENTRADA.
           SET CONEXMDB-E-ACC-ABRIR        TO TRUE.
           MOVE WK-DATASOURCE              TO CONEXMDB-E-DSNAME.
           CALL "CONEXMDB"                 USING AREA-CONEXMDB. 
       FIN-CONECTAR-BD.
           EXIT.

       DESCONECTAR-BD.
      *---------------
           INITIALIZE AREA-CONEXMDB-ENTRADA.
           SET CONEXMDB-E-ACC-CERRAR       TO TRUE.
           CALL "CONEXMDB"                 USING AREA-CONEXMDB.
       FIN-DESCONECTAR-BD.
           EXIT.

       PRUEBA-DOMICILIO.
      *-----------------
      *    Creación de domicilio.
           PERFORM INSDOM-ERR-COD-POS     THRU FIN-INSDOM-ERR-COD-POS.
           PERFORM INSDOM-ERR-CALLE       THRU FIN-INSDOM-ERR-CALLE.
           PERFORM INSDOM-ERR-PROV        THRU FIN-INSDOM-ERR-PROV.
           PERFORM INSDOM-ERR-POBL        THRU FIN-INSDOM-ERR-POBL.
           PERFORM INS-DOM-NUM            THRU FIN-INS-DOM-NUM.
           PERFORM INS-DOM-NO-NUM         THRU FIN-INS-DOM-NO-NUM.
           
      *    Búsqueda de domicilio.
           PERFORM BUSQDOM-ERR-CRIT       THRU FIN-BUSQDOM-ERR-CRIT.
           PERFORM BUSQDOM-ERR-ID         THRU FIN-BUSQDOM-ERR-ID.
           PERFORM BUSQDOM-ERR-COD-POS    THRU FIN-BUSQDOM-ERR-COD-POS.
           PERFORM BUSQDOM-ERR-CALLE      THRU FIN-BUSQDOM-ERR-CALLE.
           PERFORM BUSQDOM-ERR-ID-ASNT    THRU FIN-BUSQDOM-ERR-ID-ASNT.
           PERFORM BUSQDOM-ERR-COM-ASNT   THRU FIN-BUSQDOM-ERR-COM-ASNT.
           PERFORM BUSQDOM-ID             THRU FIN-BUSQDOM-ID.
           PERFORM BUSQDOM-COM            THRU FIN-BUSQDOM-COM.
           PERFORM BUSQDOM-COM-NO-NUM     THRU FIN-BUSQDOM-COM-NO-NUM.
 
       FIN-PRUEBA-DOMICILIO.
           EXIT.

       INSDOM-ERR-COD-POS.
      *-------------------
      * Insertar domicilio con código postal erróneo.

           INITIALIZE                     AREA-INSDOM-ENTRADA.

           MOVE '89'                      TO INSDOM-E-NUM.
           MOVE 'EGIPTO'                  TO INSDOM-E-CALLE.
           MOVE 'AAAAA'                   TO INSDOM-E-COD-POS.
           MOVE 'PARIS'                   TO INSDOM-E-PROV.
           MOVE 'MORIOH'                  TO INSDOM-E-POBL.

           DISPLAY '*** DEBUG ***       PRBBANCO INSDOM-ERR-COD-POS'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSDOM-E-CALLE      [' INSDOM-E-CALLE ']'.
           DISPLAY 'INSDOM-E-NUM        [' INSDOM-E-NUM   ']'.
           DISPLAY 'INSDOM-E-COD-POS    [' INSDOM-E-COD-POS ']'.
           DISPLAY 'INSDOM-E-POBL       [' INSDOM-E-POBL ']'.
           DISPLAY 'INSDOM-E-PROV       [' INSDOM-E-PROV ']'.

           CALL "INSDOM"                  USING AREA-INSDOM.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSDOM-STAT         [' INSDOM-STAT ']'.
           DISPLAY 'INSDOM-S-SQLCODE    [' INSDOM-S-SQLCODE ']'.
           DISPLAY 'INSDOM-S-DOM-ID     [' INSDOM-S-DOM-ID ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INSDOM-ERR-COD-POS.
           EXIT.

       INSDOM-ERR-CALLE.
      *-----------------
      * Insertar un domicilio con la calle errónea.

           INITIALIZE                     AREA-INSDOM-ENTRADA.

           MOVE '89'                      TO INSDOM-E-NUM.
           MOVE SPACES                    TO INSDOM-E-CALLE.
           MOVE '25487'                   TO INSDOM-E-COD-POS.
           MOVE 'PARIS'                   TO INSDOM-E-PROV.
           MOVE 'MORIOH'                  TO INSDOM-E-POBL.

           DISPLAY '*** DEBUG ***       PRBBANCO INSDOM-ERR-CALLE'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSDOM-E-CALLE      [' INSDOM-E-CALLE ']'.
           DISPLAY 'INSDOM-E-NUM        [' INSDOM-E-NUM   ']'.
           DISPLAY 'INSDOM-E-COD-POS    [' INSDOM-E-COD-POS ']'.
           DISPLAY 'INSDOM-E-POBL       [' INSDOM-E-POBL ']'.
           DISPLAY 'INSDOM-E-PROV       [' INSDOM-E-PROV '] '.

           CALL "INSDOM"                  USING AREA-INSDOM.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSDOM-STAT         [' INSDOM-STAT ']'.
           DISPLAY 'INSDOM-S-SQLCODE    [' INSDOM-S-SQLCODE ']'.
           DISPLAY 'INSDOM-S-DOM-ID     [' INSDOM-S-DOM-ID '] '.
           DISPLAY '------------------------------------------------'.

       FIN-INSDOM-ERR-CALLE.
           EXIT.
       
       INSDOM-ERR-PROV.
      *----------------
      * Insertar un domicilio con la provincia errónea.

           INITIALIZE                     AREA-INSDOM-ENTRADA.

           MOVE '89'                      TO INSDOM-E-NUM.
           MOVE 'EGIPTO'                  TO INSDOM-E-CALLE.
           MOVE '25487'                   TO INSDOM-E-COD-POS.
           MOVE SPACES                    TO INSDOM-E-PROV.
           MOVE 'MORIOH'                  TO INSDOM-E-POBL.

           DISPLAY '*** DEBUG ***       PRBBANCO INSDOM-ERR-PROV'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSDOM-E-CALLE      [' INSDOM-E-CALLE ']'.
           DISPLAY 'INSDOM-E-NUM        [' INSDOM-E-NUM   ']'.
           DISPLAY 'INSDOM-E-COD-POS    [' INSDOM-E-COD-POS ']'.
           DISPLAY 'INSDOM-E-POBL       [' INSDOM-E-POBL ']'.
           DISPLAY 'INSDOM-E-PROV       [' INSDOM-E-PROV ']'.

           CALL "INSDOM"                  USING AREA-INSDOM.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSDOM-STAT         [' INSDOM-STAT ']'.
           DISPLAY 'INSDOM-S-SQLCODE    [' INSDOM-S-SQLCODE ']'.
           DISPLAY 'INSDOM-S-DOM-ID     [' INSDOM-S-DOM-ID ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INSDOM-ERR-PROV.
           EXIT.
       
       INSDOM-ERR-POBL.
      *----------------
      * Insertar un domicilio con la población errónea.

           INITIALIZE                     AREA-INSDOM-ENTRADA.

           MOVE '89'                      TO INSDOM-E-NUM.
           MOVE 'EGIPTO'                  TO INSDOM-E-CALLE.
           MOVE '25487'                   TO INSDOM-E-COD-POS.
           MOVE 'PARIS'                   TO INSDOM-E-PROV.
           MOVE SPACES                    TO INSDOM-E-POBL.

           DISPLAY '*** DEBUG ***       PRBBANCO INSDOM-ERR-POBL'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSDOM-E-CALLE      [' INSDOM-E-CALLE ']'.
           DISPLAY 'INSDOM-E-NUM        [' INSDOM-E-NUM   ']'.
           DISPLAY 'INSDOM-E-COD-POS    [' INSDOM-E-COD-POS ']'.
           DISPLAY 'INSDOM-E-POBL       [' INSDOM-E-POBL ']'.
           DISPLAY 'INSDOM-E-PROV       [' INSDOM-E-PROV ']'.

           CALL "INSDOM"                  USING AREA-INSDOM.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSDOM-STAT         [' INSDOM-STAT ']'.
           DISPLAY 'INSDOM-S-SQLCODE    [' INSDOM-S-SQLCODE ']'.
           DISPLAY 'INSDOM-S-DOM-ID     [' INSDOM-S-DOM-ID '] '.
           DISPLAY '------------------------------------------------'.

       FIN-INSDOM-ERR-POBL.
           EXIT.
       
       INS-DOM-NUM.         
      *------------
      * Insertar un domicilio correcto con número.

           INITIALIZE                     AREA-INSDOM-ENTRADA.

           MOVE '89'                      TO INSDOM-E-NUM.
           MOVE 'EGIPTO'                  TO INSDOM-E-CALLE.
           MOVE '25487'                   TO INSDOM-E-COD-POS.
           MOVE 'PARIS'                   TO INSDOM-E-PROV.
           MOVE 'MORIOH'                  TO INSDOM-E-POBL.

           DISPLAY '*** DEBUG ***       PRBBANCO INS-DOM-NUM'.
           DISPLAY '*** DEBUG ***       [INICIO]'
           DISPLAY 'INSDOM-E-CALLE      [' INSDOM-E-CALLE ']'.
           DISPLAY 'INSDOM-E-NUM        [' INSDOM-E-NUM   ']'.
           DISPLAY 'INSDOM-E-COD-POS    [' INSDOM-E-COD-POS ']'.
           DISPLAY 'INSDOM-E-POBL       [' INSDOM-E-POBL ']'.
           DISPLAY 'INSDOM-E-PROV       [' INSDOM-E-PROV ']'.

           CALL "INSDOM"                  USING AREA-INSDOM.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSDOM-STAT         [' INSDOM-STAT ']'.
           DISPLAY 'INSDOM-S-SQLCODE    [' INSDOM-S-SQLCODE ']'.
           DISPLAY 'INSDOM-S-DOM-ID     [' INSDOM-S-DOM-ID ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INS-DOM-NUM.
           EXIT.

       INS-DOM-NO-NUM.         
      *---------------
      * Insertar un domicilio correcto sin número.

           INITIALIZE                     AREA-INSDOM-ENTRADA.

           MOVE 'EGIPTO'                  TO INSDOM-E-CALLE.
           MOVE '25487'                   TO INSDOM-E-COD-POS.
           MOVE 'PARIS'                   TO INSDOM-E-PROV.
           MOVE 'MORIOH'                  TO INSDOM-E-POBL.

           DISPLAY '*** DEBUG ***       PRBBANCO INS-DOM-NO-NUM'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSDOM-E-CALLE      [' INSDOM-E-CALLE ']'.
           DISPLAY 'INSDOM-E-NUM        [' INSDOM-E-NUM   ']'.
           DISPLAY 'INSDOM-E-COD-POS    [' INSDOM-E-COD-POS ']'.
           DISPLAY 'INSDOM-E-POBL       [' INSDOM-E-POBL ']'.
           DISPLAY 'INSDOM-E-PROV       [' INSDOM-E-PROV ']'.

           CALL "INSDOM"                  USING AREA-INSDOM.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSDOM-STAT         [' INSDOM-STAT ']'.
           DISPLAY 'INSDOM-S-SQLCODE    [' INSDOM-S-SQLCODE ']'.
           DISPLAY 'INSDOM-S-DOM-ID     [' INSDOM-S-DOM-ID ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INS-DOM-NO-NUM.
           EXIT.

       BUSQDOM-ERR-CRIT.  
      *-----------------
      * Criterio de búsqueda de domicilio erróneo.

           INITIALIZE                     AREA-BUSQDOM-ENTRADA.

           MOVE 3                         TO BUSQDOM-E-CRITERIO.

           DISPLAY '*** DEBUG ***       PRBBANCO BUSQDOM-ERR-CRIT'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQDOM-E-CRITERIO  [' BUSQDOM-E-CRITERIO ']'.

           CALL "BUSQDOM"                 USING AREA-BUSQDOM.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQDOM-STAT        [' BUSQDOM-STAT ']'.
           DISPLAY 'BUSQDOM-SQLCODE     [' BUSQDOM-SQLCODE ']'.
           DISPLAY 'BUSQDOM-S-ID        [' BUSQDOM-S-ID ']'.
           DISPLAY 'BUSQDOM-S-CALLE     [' BUSQDOM-S-CALLE ']'.
           DISPLAY 'BUSQDOM-S-NUM       [' BUSQDOM-S-NUM ']'.
           DISPLAY 'BUSQDOM-S-COD-POS   [' BUSQDOM-S-COD-POS ']'.
           DISPLAY 'BUSQDOM-S-PROV      [' BUSQDOM-S-PROV ']'.
           DISPLAY 'BUSQDOM-S-POBL      [' BUSQDOM-S-POBL ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQDOM-ERR-CRIT.
           EXIT.

       BUSQDOM-ERR-ID.
      *---------------
      * Criterio de búsqueda por ID erróneo.

           INITIALIZE                     AREA-BUSQDOM-ENTRADA.

           SET BUSQDOM-CRIT-ID            TO TRUE.

           MOVE 'AAAAAAAAAA'              TO BUSQDOM-E-ID.

           DISPLAY '*** DEBUG ***       PRBBANCO BUSQDOM-ERR-ID'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQDOM-E-ID        [' BUSQDOM-E-ID '],'.
           DISPLAY 'BUSQDOM-E-CRITERIO  [' BUSQDOM-E-CRITERIO ']'.

           CALL "BUSQDOM"                 USING AREA-BUSQDOM.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQDOM-STAT        [' BUSQDOM-STAT ']'.
           DISPLAY 'BUSQDOM-SQLCODE     [' BUSQDOM-SQLCODE ']'.
           DISPLAY 'BUSQDOM-S-ID        [' BUSQDOM-S-ID ']'.
           DISPLAY 'BUSQDOM-S-CALLE     [' BUSQDOM-S-CALLE ']'.
           DISPLAY 'BUSQDOM-S-NUM       [' BUSQDOM-S-NUM ']'.
           DISPLAY 'BUSQDOM-S-COD-POS   [' BUSQDOM-S-COD-POS ']'.
           DISPLAY 'BUSQDOM-S-PROV      [' BUSQDOM-S-PROV ']'.
           DISPLAY 'BUSQDOM-S-POBL      [' BUSQDOM-S-POBL ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQDOM-ERR-ID.
           EXIT.

       BUSQDOM-ERR-COD-POS.
      *--------------------
      * Criterio de búsqueda por código postal erróneo.

           INITIALIZE                     AREA-BUSQDOM-ENTRADA.

           SET BUSQDOM-CRIT-COMBI         TO TRUE.

           MOVE 89                        TO BUSQDOM-E-NUM.
           MOVE 'EGIPTO'                  TO BUSQDOM-E-CALLE.
           MOVE ZEROES                    TO BUSQDOM-E-COD-POS.

           DISPLAY '*** DEBUG ***        PRBBANCO BUSQDOM-ERR-COD-POS'.
           DISPLAY '*** DEBUG ***        [INICIO]'.
           DISPLAY  'BUSQDOM-E-CRITERIO  [' BUSQDOM-E-CRITERIO ']'.
           DISPLAY  'BUSQDOM-E-NUM       [' BUSQDOM-E-NUM ']'.
           DISPLAY  'BUSQDOM-E-CALLE     [' BUSQDOM-E-CALLE ']'.
           DISPLAY  'BUSQDOM-E-COD-POS   [' BUSQDOM-E-COD-POS ']'.

           CALL "BUSQDOM"                 USING AREA-BUSQDOM.

           DISPLAY '*** DEBUG ***        [FIN]'.
           DISPLAY 'BUSQDOM-STAT         [' BUSQDOM-STAT ']'.
           DISPLAY 'BUSQDOM-SQLCODE      [' BUSQDOM-SQLCODE ']'.
           DISPLAY 'BUSQDOM-S-ID         [' BUSQDOM-S-ID ']'.
           DISPLAY 'BUSQDOM-S-CALLE      [' BUSQDOM-S-CALLE ']'.
           DISPLAY 'BUSQDOM-S-NUM        [' BUSQDOM-S-NUM ']'.
           DISPLAY 'BUSQDOM-S-COD-POS    [' BUSQDOM-S-COD-POS ']'.
           DISPLAY 'BUSQDOM-S-PROV       [' BUSQDOM-S-PROV ']'.
           DISPLAY 'BUSQDOM-S-POBL       [' BUSQDOM-S-POBL ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQDOM-ERR-COD-POS.
           EXIT.

       BUSQDOM-ERR-CALLE.  
      *------------------
      * Criterio de búsqueda por calle erróneo.

           INITIALIZE                     AREA-BUSQDOM-ENTRADA.

           SET BUSQDOM-CRIT-COMBI         TO TRUE.

           MOVE 89                        TO BUSQDOM-E-NUM.
           MOVE SPACES                    TO BUSQDOM-E-CALLE.
           MOVE 25487                     TO BUSQDOM-E-COD-POS.

           DISPLAY '*** DEBUG ***       PRBBANCO BUSQDOM-ERR-CALLE'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQDOM-E-CRITERIO  [' BUSQDOM-E-CRITERIO ']'.
           DISPLAY 'BUSQDOM-E-NUM       [' BUSQDOM-E-NUM ']'.
           DISPLAY 'BUSQDOM-E-CALLE     [' BUSQDOM-E-CALLE ']'.
           DISPLAY 'BUSQDOM-E-COD-POS   [' BUSQDOM-E-COD-POS ']'.

           CALL "BUSQDOM"                 USING AREA-BUSQDOM.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQDOM-STAT        [' BUSQDOM-STAT ']'.
           DISPLAY 'BUSQDOM-SQLCODE     [' BUSQDOM-SQLCODE ']'.
           DISPLAY 'BUSQDOM-S-ID        [' BUSQDOM-S-ID ']'.
           DISPLAY 'BUSQDOM-S-CALLE     [' BUSQDOM-S-CALLE ']'.
           DISPLAY 'BUSQDOM-S-NUM       [' BUSQDOM-S-NUM ']'.
           DISPLAY 'BUSQDOM-S-COD-POS   [' BUSQDOM-S-COD-POS ']'.
           DISPLAY 'BUSQDOM-S-PROV      [' BUSQDOM-S-PROV ']'.
           DISPLAY 'BUSQDOM-S-POBL      [' BUSQDOM-S-POBL ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQDOM-ERR-CALLE.
           EXIT.

       BUSQDOM-ERR-ID-ASNT. 
      *--------------------
      * Domicilio buscado por ID no existente.

           INITIALIZE                     AREA-BUSQDOM-ENTRADA.

           SET BUSQDOM-CRIT-ID            TO TRUE.

           MOVE 9999999991                TO BUSQDOM-E-ID.

           DISPLAY '*** DEBUG ***       PRBBANCO BUSQDOM-ERR-ID-ASNT'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQDOM-E-CRITERIO  [' BUSQDOM-E-CRITERIO ']'.
           DISPLAY 'BUSQDOM-E-ID        [' BUSQDOM-E-ID ']'.

           CALL "BUSQDOM"                 USING AREA-BUSQDOM.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQDOM-STAT        [' BUSQDOM-STAT ']'.
           DISPLAY 'BUSQDOM-SQLCODE     [' BUSQDOM-SQLCODE ']'.
           DISPLAY 'BUSQDOM-S-ID        [' BUSQDOM-S-ID ']'.
           DISPLAY 'BUSQDOM-S-CALLE     [' BUSQDOM-S-CALLE ']'.
           DISPLAY 'BUSQDOM-S-NUM       [' BUSQDOM-S-NUM ']'.
           DISPLAY 'BUSQDOM-S-COD-POS   [' BUSQDOM-S-COD-POS ']'.
           DISPLAY 'BUSQDOM-S-PROV      [' BUSQDOM-S-PROV ']'.
           DISPLAY 'BUSQDOM-S-POBL      [' BUSQDOM-S-POBL ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQDOM-ERR-ID-ASNT.
           EXIT.

       BUSQDOM-ERR-COM-ASNT.
      *---------------------
      * Domicilio buscado por combinación CALLE+NUM+COD-POS no existe.

           INITIALIZE                     AREA-BUSQDOM-ENTRADA.
           
           SET BUSQDOM-CRIT-COMBI         TO TRUE.

           MOVE 999                       TO BUSQDOM-E-NUM.
           MOVE 'LOLA FLORES'             TO BUSQDOM-E-CALLE.
           MOVE 99999                     TO BUSQDOM-E-COD-POS.

           DISPLAY '*** DEBUG ***       PRBBANCO BUSQDOM-ERR-COM-ASNT'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQDOM-E-CRITERIO  [' BUSQDOM-E-CRITERIO ']'.
           DISPLAY 'BUSQDOM-E-NUM       [' BUSQDOM-E-NUM ']'.
           DISPLAY 'BUSQDOM-E-CALLE     [' BUSQDOM-E-CALLE ']'.
           DISPLAY 'BUSQDOM-E-COD-POS   [' BUSQDOM-E-COD-POS ']'.

           CALL "BUSQDOM"                 USING AREA-BUSQDOM.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQDOM-STAT        [' BUSQDOM-STAT ']'.
           DISPLAY 'BUSQDOM-SQLCODE     [' BUSQDOM-SQLCODE ']'.
           DISPLAY 'BUSQDOM-S-ID        [' BUSQDOM-S-ID ']'.
           DISPLAY 'BUSQDOM-S-CALLE     [' BUSQDOM-S-CALLE ']'.
           DISPLAY 'BUSQDOM-S-NUM       [' BUSQDOM-S-NUM ']'.
           DISPLAY 'BUSQDOM-S-COD-POS   [' BUSQDOM-S-COD-POS ']'.
           DISPLAY 'BUSQDOM-S-PROV      [' BUSQDOM-S-PROV ']'.
           DISPLAY 'BUSQDOM-S-POBL      [' BUSQDOM-S-POBL ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQDOM-ERR-COM-ASNT.
           EXIT.

       BUSQDOM-ID. 
      *-----------
      * Domicilio buscado por ID existe.

           INITIALIZE                     AREA-BUSQDOM-ENTRADA.

           SET BUSQDOM-CRIT-ID            TO TRUE.

           MOVE 0000000001                TO BUSQDOM-E-ID.

           DISPLAY '*** DEBUG ***       PRBBANCO BUSQDOM-ID'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQDOM-E-CRITERIO  [' BUSQDOM-E-CRITERIO ']'.
           DISPLAY 'BUSQDOM-E-ID        [' BUSQDOM-E-ID ']'.

           CALL "BUSQDOM"                 USING AREA-BUSQDOM.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQDOM-STAT        [' BUSQDOM-STAT ']'.
           DISPLAY 'BUSQDOM-SQLCODE     [' BUSQDOM-SQLCODE ']'.
           DISPLAY 'BUSQDOM-S-ID        [' BUSQDOM-S-ID ']'.
           DISPLAY 'BUSQDOM-S-CALLE     [' BUSQDOM-S-CALLE ']'.
           DISPLAY 'BUSQDOM-S-NUM       [' BUSQDOM-S-NUM ']'.
           DISPLAY 'BUSQDOM-S-COD-POS   [' BUSQDOM-S-COD-POS ']'.
           DISPLAY 'BUSQDOM-S-PROV      [' BUSQDOM-S-PROV ']'.
           DISPLAY 'BUSQDOM-S-POBL      [' BUSQDOM-S-POBL ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQDOM-ID.
           EXIT.

       BUSQDOM-COM.
      *------------
      * Domicilio buscado por combinación CALLE+NUM+COD-POS existe.

           INITIALIZE                     AREA-BUSQDOM-ENTRADA.
           
           SET BUSQDOM-CRIT-COMBI         TO TRUE.

           MOVE 89                        TO BUSQDOM-E-NUM.
           MOVE 'EGIPTO'                  TO BUSQDOM-E-CALLE.
           MOVE 25487                     TO BUSQDOM-E-COD-POS.

           DISPLAY '*** DEBUG ***       PRBBANCO BUSQDOM-COM'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQDOM-E-CRITERIO  [' BUSQDOM-E-CRITERIO ']'.
           DISPLAY 'BUSQDOM-E-NUM       [' BUSQDOM-E-NUM ']'.
           DISPLAY 'BUSQDOM-E-CALLE     [' BUSQDOM-E-CALLE ']'.
           DISPLAY 'BUSQDOM-E-COD-POS   [' BUSQDOM-E-COD-POS ']'.

           CALL "BUSQDOM"                 USING AREA-BUSQDOM.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQDOM-STAT        [' BUSQDOM-STAT ']'.
           DISPLAY 'BUSQDOM-SQLCODE     [' BUSQDOM-SQLCODE ']'.
           DISPLAY 'BUSQDOM-S-ID        [' BUSQDOM-S-ID ']'.
           DISPLAY 'BUSQDOM-S-CALLE     [' BUSQDOM-S-CALLE ']'.
           DISPLAY 'BUSQDOM-S-NUM       [' BUSQDOM-S-NUM ']'.
           DISPLAY 'BUSQDOM-S-COD-POS   [' BUSQDOM-S-COD-POS ']'.
           DISPLAY 'BUSQDOM-S-PROV      [' BUSQDOM-S-PROV ']'.
           DISPLAY 'BUSQDOM-S-POBL      [' BUSQDOM-S-POBL ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQDOM-COM.
           EXIT.

       BUSQDOM-COM-NO-NUM.
      *-------------------
      * Domicilio buscado por combinación CALLE+COD-POS.

           INITIALIZE                     AREA-BUSQDOM-ENTRADA.
           
           SET BUSQDOM-CRIT-COMBI         TO TRUE.

           MOVE SPACES                    TO BUSQDOM-E-NUM.
           MOVE 'EGIPTO'                  TO BUSQDOM-E-CALLE.
           MOVE 25487                     TO BUSQDOM-E-COD-POS.

           DISPLAY '*** DEBUG ***       PRBBANCO BUSQDOM-COM-NO-NUM'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQDOM-E-CRITERIO  [' BUSQDOM-E-CRITERIO ']'.
           DISPLAY 'BUSQDOM-E-NUM       [' BUSQDOM-E-NUM ']'.
           DISPLAY 'BUSQDOM-E-CALLE     [' BUSQDOM-E-CALLE ']'.
           DISPLAY 'BUSQDOM-E-COD-POS   [' BUSQDOM-E-COD-POS ']'.

           CALL "BUSQDOM"                 USING AREA-BUSQDOM.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQDOM-STAT        [' BUSQDOM-STAT ']'.
           DISPLAY 'BUSQDOM-SQLCODE     [' BUSQDOM-SQLCODE ']'.
           DISPLAY 'BUSQDOM-S-ID        [' BUSQDOM-S-ID ']'.
           DISPLAY 'BUSQDOM-S-CALLE     [' BUSQDOM-S-CALLE ']'.
           DISPLAY 'BUSQDOM-S-NUM       [' BUSQDOM-S-NUM ']'.
           DISPLAY 'BUSQDOM-S-COD-POS   [' BUSQDOM-S-COD-POS ']'.
           DISPLAY 'BUSQDOM-S-PROV      [' BUSQDOM-S-PROV ']'.
           DISPLAY 'BUSQDOM-S-POBL      [' BUSQDOM-S-POBL ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQDOM-COM-NO-NUM.
           EXIT.

       PRUEBA-CLIENTE.
      *---------------
      *    Creación de clientes.
           PERFORM INSCLI-ERR-NIF          THRU FIN-INSCLI-ERR-NIF.
           PERFORM INSCLI-ERR-NOM          THRU FIN-INSCLI-ERR-NOM.
           PERFORM INSCLI-ERR-FECNAC       THRU FIN-INSCLI-ERR-FECNAC.
           PERFORM INSCLI-ERR-DOM          THRU FIN-INSCLI-ERR-DOM.
           PERFORM INS-CLI-NO-DOM          THRU FIN-INS-CLI-NO-DOM.
           PERFORM INS-CLI-DOM             THRU INS-CLI-DOM.
       
      *    Búsqueda de clientes.
           PERFORM BUSQCLI-ERR-CRIT        THRU FIN-BUSQCLI-ERR-CRIT.
           PERFORM BUSQCLI-ERR-ID          THRU FIN-BUSQCLI-ERR-ID.
           PERFORM BUSQCLI-ERR-NIF         THRU FIN-BUSQCLI-ERR-NIF.
           PERFORM BUSQCLI-ID              THRU FIN-BUSQCLI-ID.
           PERFORM BUSQCLI-NIF             THRU FIN-BUSQCLI-NIF.
           PERFORM BUSQCLI-ID-ASNT         THRU FIN-BUSQCLI-ID-ASNT.
           PERFORM BUSQCLI-NIF-ASNT        THRU FIN-BUSQCLI-NIF-ASNT.
       
       FIN-PRUEBA-CLIENTE.
           EXIT.
       
       INSCLI-ERR-NIF.
      *---------------
      *    Inserta un cliente con el NIF erróneo.
       
           INITIALIZE                      AREA-INSCLI-ENTRADA.
       
           MOVE SPACES                     TO INSCLI-E-NIF.
           MOVE 'POL NAREFF'               TO INSCLI-E-NOM.
           MOVE 19651207                   TO INSCLI-E-FEC-NAC.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INSCLI-ERR-NIF'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSCLI-E-NIF        [' INSCLI-E-NIF ']'.
           DISPLAY 'INSCLI-E-NOM        [' INSCLI-E-NOM ']'.
           DISPLAY 'INSCLI-E-FEC-NAC    [' INSCLI-E-FEC-NAC ']'.
       
           CALL "INSCLI"                   USING AREA-INSCLI.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSCLI-STAT         [' INSCLI-STAT ']'.
           DISPLAY 'INSCLI-S-CLI-ID     [' INSCLI-S-CLI-ID ']'.
           DISPLAY 'INSCLI-S-SQLCODE    [' INSCLI-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.
                   
       FIN-INSCLI-ERR-NIF.
           EXIT.
       
       INSCLI-ERR-NOM.
      *---------------
      *    Inserta un cliente con el nombre erróneo.
       
           INITIALIZE                      AREA-INSCLI-ENTRADA.
       
           MOVE '784216985S'               TO INSCLI-E-NIF.
           MOVE SPACES                     TO INSCLI-E-NOM.
           MOVE '19651207'                 TO INSCLI-E-FEC-NAC.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INSCLI-ERR-NOM'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSCLI--E-NIF       [' INSCLI-E-NIF ']'.
           DISPLAY 'INSSCLI-E-NOM       [' INSCLI-E-NOM ']'.
           DISPLAY 'INSSCLI-E-FEC-NAC   [' INSCLI-E-FEC-NAC ']'.
       
           CALL "INSCLI"                   USING AREA-INSCLI.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSCLI-STAT         [' INSCLI-STAT ']'.
           DISPLAY 'INSCLI-S-CLI-ID     [' INSCLI-S-CLI-ID ']'.
           DISPLAY 'INSCLI-S-SQLCODE    [' INSCLI-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-INSCLI-ERR-NOM.
           EXIT.
       
       INSCLI-ERR-FECNAC.
      *------------------
      *    Inserta un cliente con la fecha de nacimiento errónea.
       
           INITIALIZE                      AREA-INSCLI-ENTRADA.
       
           MOVE '784216985S'               TO INSCLI-E-NIF.
           MOVE 'POL NAREFF'               TO INSCLI-E-NOM.
           MOVE ZEROES                     TO INSCLI-E-FEC-NAC.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INSCLI-ERR-FECNAC'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSCLI--E-NIF       [' INSCLI-E-NIF ']'.
           DISPLAY 'INSSCLI-E-NOM       [' INSCLI-E-NOM ']'.
           DISPLAY 'INSSCLI-E-FEC-NAC   [' INSCLI-E-FEC-NAC ']'.
       
           CALL "INSCLI"                   USING AREA-INSCLI.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSCLI-STAT         [' INSCLI-STAT ']'.
           DISPLAY 'INSCLI-S-CLI-ID     [' INSCLI-S-CLI-ID ']'.
           DISPLAY 'INSCLI-S-SQLCODE    [' INSCLI-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.  
       
       FIN-INSCLI-ERR-FECNAC.
           EXIT.
       
       INSCLI-ERR-DOM.
      *---------------
      *    Inserta un cliente con el domicilio erróneo.
       
           INITIALIZE                      AREA-INSCLI-ENTRADA.
       
           MOVE '784216985S'               TO INSCLI-E-NIF.
           MOVE 'POL NAREFF'               TO INSCLI-E-NOM.
           MOVE '19651207'                 TO INSCLI-E-FEC-NAC.
           MOVE 'AAAAAAAAAA'               TO INSCLI-E-ID-DOM.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INSCLI-ERR-DOM'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSCLI--E-NIF       [' INSCLI-E-NIF ']'.
           DISPLAY 'INSSCLI-E-NOM       [' INSCLI-E-NOM ']'.
           DISPLAY 'INSSCLI-E-FEC-NAC   [' INSCLI-E-FEC-NAC ']'.
           DISPLAY 'INSSCLI-E-ID-DOM    [' INSCLI-E-ID-DOM ']'.
       
           CALL "INSCLI"                   USING AREA-INSCLI.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSCLI-STAT         [' INSCLI-STAT ']'.
           DISPLAY 'INSCLI-S-CLI-ID     [' INSCLI-S-CLI-ID ']'.
           DISPLAY 'INSCLI-S-SQLCODE    [' INSCLI-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-INSCLI-ERR-DOM.
           EXIT.
       
       INS-CLI-NO-DOM.
      *---------------
      *    Inserta un cliente correcto sin domicilio.
       
           INITIALIZE                      AREA-INSCLI-ENTRADA.
       
           MOVE '784216985S'               TO INSCLI-E-NIF.
           MOVE 'POL NAREFF'               TO INSCLI-E-NOM.
           MOVE 19651207                   TO INSCLI-E-FEC-NAC.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INS-CLI-NO-DOM'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSCLI--E-NIF       [' INSCLI-E-NIF ']'.
           DISPLAY 'INSSCLI-E-NOM       [' INSCLI-E-NOM ']'.
           DISPLAY 'INSSCLI-E-FEC-NAC   [' INSCLI-E-FEC-NAC ']'.
           DISPLAY 'INSSCLI-E-ID-DOM    [' INSCLI-E-ID-DOM ']'.
       
           CALL "INSCLI"                   USING AREA-INSCLI.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSCLI-STAT         [' INSCLI-STAT ']'.
           DISPLAY 'INSCLI-S-CLI-ID     [' INSCLI-S-CLI-ID ']'.
           DISPLAY 'INSCLI-S-SQLCODE    [' INSCLI-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-INS-CLI-NO-DOM.
           EXIT.

       INS-CLI-DOM.
      *------------
      *    Inserta un cliente correcto con domicilio.
       
           INITIALIZE                      AREA-INSCLI-ENTRADA.
       
           MOVE '874596584X'               TO INSCLI-E-NIF.
           MOVE 'IGGY THE FOOL'            TO INSCLI-E-NOM.
           MOVE 19890116                   TO INSCLI-E-FEC-NAC.
           MOVE 0000000001                 TO INSCLI-E-ID-DOM.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INS-CLI-NO-DOM'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSCLI--E-NIF       [' INSCLI-E-NIF ']'.
           DISPLAY 'INSSCLI-E-NOM       [' INSCLI-E-NOM ']'.
           DISPLAY 'INSSCLI-E-FEC-NAC   [' INSCLI-E-FEC-NAC ']'.
           DISPLAY 'INSSCLI-E-ID-DOM    [' INSCLI-E-ID-DOM ']'.
       
           CALL "INSCLI"                   USING AREA-INSCLI.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSCLI-STAT         [' INSCLI-STAT ']'.
           DISPLAY 'INSCLI-S-CLI-ID     [' INSCLI-S-CLI-ID ']'.
           DISPLAY 'INSCLI-S-SQLCODE    [' INSCLI-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-INS-CLI-DOM.
           EXIT.
       
       BUSQCLI-ERR-CRIT.
      *-----------------
      * Criterio de búsqueda de cliente erróneo.
       
           INITIALIZE                      AREA-BUSQCLI-ENTRADA.
       
           MOVE 3                          TO BUSQCLI-E-CRITERIO.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQCLI-ERR-CRIT'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQCLI-E-CRITERIO  [' BUSQCLI-E-CRITERIO ']'.
       
           CALL "BUSQCLI"                  USING AREA-BUSQCLI.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQCLI-STAT        [' BUSQCLI-STAT ']'.
           DISPLAY 'BUSQCLI-SQLCODE     [' BUSQCLI-SQLCODE ']'.
           DISPLAY 'BUSQCLI-S-ID        [' BUSQCLI-S-ID ']'.
           DISPLAY 'BUSQCLI-S-NIF       [' BUSQCLI-S-NIF ']'.
           DISPLAY 'BUSQCLI-S-FEC-NAC   [' BUSQCLI-S-FEC-NAC ']'.
           DISPLAY 'BUSQCLI-S-ID-DOM    [' BUSQCLI-S-ID-DOM ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-BUSQCLI-ERR-CRIT.
           EXIT.
       
       BUSQCLI-ERR-ID.
      *---------------
      * Criterio de búsqueda por ID erróneo.
       
           INITIALIZE                      AREA-BUSQCLI-ENTRADA.
       
           SET BUSQCLI-CRIT-ID             TO TRUE.
       
           MOVE 'AAAAAAAAAA'               TO BUSQCLI-E-ID.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQCLI-ERR-ID'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQCLI-E-CRITERIO  [' BUSQCLI-E-CRITERIO ']'.
           DISPLAY 'BUSQCLI-E-ID        [' BUSQCLI-E-ID ']'.
       
           CALL "BUSQCLI"                  USING AREA-BUSQCLI.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQCLI-STAT        [' BUSQCLI-STAT ']'.
           DISPLAY 'BUSQCLI-SQLCODE     [' BUSQCLI-SQLCODE ']'.
           DISPLAY 'BUSQCLI-S-ID        [' BUSQCLI-S-ID ']'.
           DISPLAY 'BUSQCLI-S-NIF       [' BUSQCLI-S-NIF ']'.
           DISPLAY 'BUSQCLI-S-FEC-NAC   [' BUSQCLI-S-FEC-NAC ']'.
           DISPLAY 'BUSQCLI-S-ID-DOM    [' BUSQCLI-S-ID-DOM ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-BUSQCLI-ERR-ID.
           EXIT.
       
       BUSQCLI-ERR-NIF.
      *----------------
      * Criterio de búsqueda por NIF erróneo.
       
           INITIALIZE                      AREA-BUSQCLI-ENTRADA.

           SET BUSQCLI-CRIT-NIF            TO TRUE.
       
           MOVE SPACES                     TO BUSQCLI-E-NIF.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQCLI-ERR-NIF'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQCLI-E-CRITERIO  [' BUSQCLI-E-CRITERIO ']'.
           DISPLAY 'BUSQCLI-E-NIF       [' BUSQCLI-E-NIF ']'.
       
           CALL "BUSQCLI"                  USING AREA-BUSQCLI.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQCLI-STAT        [' BUSQCLI-STAT ']'.
           DISPLAY 'BUSQCLI-SQLCODE     [' BUSQCLI-SQLCODE ']'.
           DISPLAY 'BUSQCLI-S-ID        [' BUSQCLI-S-ID ']'.
           DISPLAY 'BUSQCLI-S-NIF       [' BUSQCLI-S-NIF ']'.
           DISPLAY 'BUSQCLI-S-FEC-NAC   [' BUSQCLI-S-FEC-NAC ']'.
           DISPLAY 'BUSQCLI-S-ID-DOM    [' BUSQCLI-S-ID-DOM ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-BUSQCLI-ERR-NIF.
           EXIT.
       
       BUSQCLI-ID.
      *-----------
      * Cliente con ID indicado existe en la BBDD.
       
           INITIALIZE                      AREA-BUSQCLI-ENTRADA.

           SET BUSQCLI-CRIT-ID             TO TRUE.
       
           MOVE 0000000001                 TO BUSQCLI-E-ID.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQCLI-ID'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQCLI-E-CRITERIO  [' BUSQCLI-E-CRITERIO ']'.
           DISPLAY 'BUSQCLI-E-ID        [' BUSQCLI-E-ID ']'.
       
           CALL "BUSQCLI"                  USING AREA-BUSQCLI.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQCLI-STAT        [' BUSQCLI-STAT ']'.
           DISPLAY 'BUSQCLI-SQLCODE     [' BUSQCLI-SQLCODE ']'.
           DISPLAY 'BUSQCLI-S-ID        [' BUSQCLI-S-ID ']'.
           DISPLAY 'BUSQCLI-S-NIF       [' BUSQCLI-S-NIF ']'.
           DISPLAY 'BUSQCLI-S-FEC-NAC   [' BUSQCLI-S-FEC-NAC ']'.
           DISPLAY 'BUSQCLI-S-ID-DOM    [' BUSQCLI-S-ID-DOM ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-BUSQCLI-ID.
           EXIT.
       
       BUSQCLI-NIF.
      *------------
      * Cliente con NIF indicado existe en la BBDD.
       
           INITIALIZE                      AREA-BUSQCLI-ENTRADA.

           SET BUSQCLI-CRIT-NIF            TO TRUE.
           
           MOVE '784216985S'               TO BUSQCLI-E-NIF.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQCLI-NIF'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQCLI-E-CRITERIO  [' BUSQCLI-E-CRITERIO ']'.
           DISPLAY 'BUSQCLI-E-NIF       [' BUSQCLI-E-NIF ']'.
       
           CALL "BUSQCLI"                  USING AREA-BUSQCLI.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQCLI-STAT        [' BUSQCLI-STAT ']'.
           DISPLAY 'BUSQCLI-SQLCODE     [' BUSQCLI-SQLCODE ']'.
           DISPLAY 'BUSQCLI-S-ID        [' BUSQCLI-S-ID ']'.
           DISPLAY 'BUSQCLI-S-NIF       [' BUSQCLI-S-NIF ']'.
           DISPLAY 'BUSQCLI-S-FEC-NAC   [' BUSQCLI-S-FEC-NAC ']'.
           DISPLAY 'BUSQCLI-S-ID-DOM    [' BUSQCLI-S-ID-DOM ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-BUSQCLI-NIF.
           EXIT.

       BUSQCLI-ID-ASNT.
      *----------------
      * Cliente con ID indicado NO existe en la BBDD.
       
           INITIALIZE                      AREA-BUSQCLI-ENTRADA.

           SET BUSQCLI-CRIT-ID             TO TRUE.
       
           MOVE 9999999991                 TO BUSQCLI-E-ID.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQCLI-ID-ASNT'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQCLI-E-CRITERIO  [' BUSQCLI-E-CRITERIO ']'.
           DISPLAY 'BUSQCLI-E-ID        [' BUSQCLI-E-ID ']'.
       
           CALL "BUSQCLI"                  USING AREA-BUSQCLI.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQCLI-STAT        [' BUSQCLI-STAT ']'.
           DISPLAY 'BUSQCLI-SQLCODE     [' BUSQCLI-SQLCODE ']'.
           DISPLAY 'BUSQCLI-S-ID        [' BUSQCLI-S-ID ']'.
           DISPLAY 'BUSQCLI-S-NIF       [' BUSQCLI-S-NIF ']'.
           DISPLAY 'BUSQCLI-S-FEC-NAC   [' BUSQCLI-S-FEC-NAC ']'.
           DISPLAY 'BUSQCLI-S-ID-DOM    [' BUSQCLI-S-ID-DOM ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-BUSQCLI-ID-ASNT.
           EXIT.
       
       BUSQCLI-NIF-ASNT.
      *-----------------
      * Cliente con NIF indicado NO existe en la BBDD.
       
           INITIALIZE                      AREA-BUSQCLI-ENTRADA.

           SET BUSQCLI-CRIT-NIF            TO TRUE.
           
           MOVE '999999999Z'               TO BUSQCLI-E-NIF.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQCLI-NIF-ASNT'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQCLI-E-CRITERIO  [' BUSQCLI-E-CRITERIO ']'.
           DISPLAY 'BUSQCLI-E-NIF       [' BUSQCLI-E-NIF ']'.
       
           CALL "BUSQCLI"                  USING AREA-BUSQCLI.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQCLI-STAT        [' BUSQCLI-STAT ']'.
           DISPLAY 'BUSQCLI-SQLCODE     [' BUSQCLI-SQLCODE ']'.
           DISPLAY 'BUSQCLI-S-ID        [' BUSQCLI-S-ID ']'.
           DISPLAY 'BUSQCLI-S-NIF       [' BUSQCLI-S-NIF ']'.
           DISPLAY 'BUSQCLI-S-FEC-NAC   [' BUSQCLI-S-FEC-NAC ']'.
           DISPLAY 'BUSQCLI-S-ID-DOM    [' BUSQCLI-S-ID-DOM ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-BUSQCLI-NIF-ASNT.
           EXIT. 

       PRUEBA-CUENTA.
      *--------------
      *    Creación de cuenta.
           PERFORM INSCTA-ERR-NUM         THRU FIN-INSCTA-ERR-NUM.
           PERFORM INS-CTA                THRU FIN-INS-CTA.
       
      *    Búsqueda de cuenta.
           PERFORM BUSQCTA-ERR-CRIT       THRU FIN-BUSQCTA-ERR-CRIT.
           PERFORM BUSQCTA-ERR-ID         THRU FIN-BUSQCTA-ERR-ID.
           PERFORM BUSQCTA-ERR-NUM        THRU FIN-BUSQCTA-ERR-NUM.
           PERFORM BUSQCTA-ID             THRU FIN-BUSQCTA-ID.
           PERFORM BUSQCTA-NUM            THRU FIN-BUSQCTA-NUM.
           PERFORM BUSQCTA-ID-ASNT        THRU FIN-BUSQCTA-ID-ASNT.
           PERFORM BUSQCTA-NUM-ASNT       THRU FIN-BUSQCTA-NUM-ASNT.

       FIN-PRUEBA-CUENTA.
           EXIT.
       
       INSCTA-ERR-NUM. 
      *---------------
      * Insertar una cuenta con el número erróneo.
       
           INITIALIZE                     AREA-INSCTA-ENTRADA.
       
           MOVE SPACES                    TO INSCTA-E-NUM.
           MOVE -00052487.45              TO INSCTA-E-SALDO.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INSCTA-ERR-NUM'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSCTA-E-NUM        [' INSCTA-E-NUM ']'.
           DISPLAY 'INSCTA-E-SALDO      [' INSCTA-E-SALDO ']'.
       
           CALL "INSCTA"                   USING AREA-INSCTA.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSCTA-STAT         [' INSCTA-STAT ']'.
           DISPLAY 'INSCTA-S-CTA-ID     [' INSCTA-S-CTA-ID ']'.
           DISPLAY 'INSCTA-S-SQLCODE    [' INSCTA-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-INSCTA-ERR-NUM.
           EXIT.
       
       INS-CTA.
      *--------
      * Insertar cuenta correcta en BBDD.
       
           INITIALIZE                     AREA-INSCTA-ENTRADA.
       
           MOVE 85478965487541254896      TO INSCTA-E-NUM.
           MOVE -00052487.45              TO INSCTA-E-SALDO.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INS-CTA'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSCTA-E-NUM        [' INSCTA-E-NUM ']'.
           DISPLAY 'INSCTA-E-SALDO      [' INSCTA-E-SALDO ']'.
       
           CALL "INSCTA"                  USING AREA-INSCTA.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSCTA-STAT         [' INSCTA-STAT ']'.
           DISPLAY 'INSCTA-S-CTA-ID     [' INSCTA-S-CTA-ID ']'.
           DISPLAY 'INSCTA-S-SQLCODE    [' INSCTA-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-INS-CTA.
           EXIT.
       
       BUSQCTA-ERR-CRIT.    
      *-----------------
      * Criterio de búsqueda para cuenta erróneo.
       
           INITIALIZE                     AREA-BUSQCTA-ENTRADA.
       
           MOVE 3                         TO BUSQCTA-E-CRITERIO.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQCTA-ERR-CRIT'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQCTA-E-CRITERIO  [' BUSQCTA-E-CRITERIO ']'.
       
           CALL "BUSQCTA"                 USING AREA-BUSQCTA.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQCTA-STAT        [' BUSQCTA-STAT ']'.
           DISPLAY 'BUSQCTA-SQLCODE     [' BUSQCTA-SQLCODE ']'.
           DISPLAY 'BUSQCTA-S-ID        [' BUSQCTA-S-ID ']'.
           DISPLAY 'BUSQCTA-S-NUM       [' BUSQCTA-S-NUM ']'.
           DISPLAY 'BUSQCTA-S-SALDO     [' BUSQCTA-S-SALDO ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-BUSQCTA-ERR-CRIT.
           EXIT.
       
       BUSQCTA-ERR-ID.     
      *---------------
      * Criterio de búsqueda por ID erróneo.
       
           INITIALIZE                     AREA-BUSQCTA-ENTRADA.
       
           SET BUSQCTA-CRIT-ID            TO TRUE.
       
           MOVE 'AAAAAAAAAA'              TO BUSQCTA-E-ID.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQCTA-ERR-ID'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQCTA-E-CRITERIO  [' BUSQCTA-E-CRITERIO ']'.
           DISPLAY 'BUSQCTA-E-ID        [' BUSQCTA-E-ID ']'.
       
           CALL "BUSQCTA"                 USING AREA-BUSQCTA.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQCTA-STAT        [' BUSQCTA-STAT ']'.
           DISPLAY 'BUSQCTA-SQLCODE     [' BUSQCTA-SQLCODE ']'.
           DISPLAY 'BUSQCTA-S-ID        [' BUSQCTA-S-ID ']'.
           DISPLAY 'BUSQCTA-S-NUM       [' BUSQCTA-S-NUM ']'.
           DISPLAY 'BUSQCTA-S-SALDO     [' BUSQCTA-S-SALDO ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-BUSQCTA-ERR-ID.
           EXIT.
       
       BUSQCTA-ERR-NUM.    
      *----------------
      * Criterio de búsqueda por número de cuenta erróneo.
       
           INITIALIZE                     AREA-BUSQCTA-ENTRADA.
       
           SET BUSQCTA-CRIT-NUM           TO TRUE.
       
           MOVE 'AAAAAAAAAAAAAAAAAAAA'    TO BUSQCTA-E-NUM.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQCTA-ERR-NUM'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQCTA-E-CRITERIO  [' BUSQCTA-E-CRITERIO ']'.
           DISPLAY 'BUSQCTA-E-NUM       [' BUSQCTA-E-NUM ']'.
       
           CALL "BUSQCTA"                 USING AREA-BUSQCTA.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQCTA-STAT        [' BUSQCTA-STAT ']'.
           DISPLAY 'BUSQCTA-SQLCODE     [' BUSQCTA-SQLCODE ']'.
           DISPLAY 'BUSQCTA-S-ID        [' BUSQCTA-S-ID ']'.
           DISPLAY 'BUSQCTA-S-NUM       [' BUSQCTA-S-NUM ']'.
           DISPLAY 'BUSQCTA-S-SALDO     [' BUSQCTA-S-SALDO ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-BUSQCTA-ERR-NUM.
           EXIT.
       
       BUSQCTA-ID.
      *-----------
      * Cuenta buscada por ID existe.
       
           INITIALIZE                     AREA-BUSQCTA-ENTRADA.
       
           SET BUSQCTA-CRIT-ID            TO TRUE.
       
           MOVE 0000000001                TO BUSQCTA-E-ID.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQCTA-ID'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQCTA-E-CRITERIO  [' BUSQCTA-E-CRITERIO ']'.
           DISPLAY 'BUSQCTA-E-ID        [' BUSQCTA-E-ID ']'.
       
           CALL "BUSQCTA"                 USING AREA-BUSQCTA.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQCTA-STAT        [' BUSQCTA-STAT ']'.
           DISPLAY 'BUSQCTA-SQLCODE     [' BUSQCTA-SQLCODE ']'.
           DISPLAY 'BUSQCTA-S-ID        [' BUSQCTA-S-ID ']'.
           DISPLAY 'BUSQCTA-S-NUM       [' BUSQCTA-S-NUM ']'.
           DISPLAY 'BUSQCTA-S-SALDO     [' BUSQCTA-S-SALDO ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-BUSQCTA-ID.
           EXIT.
       
       BUSQCTA-NUM.
      *------------
      * Cuenta buscada por número de cuenta existe.

           INITIALIZE                     AREA-BUSQCTA-ENTRADA.
       
           SET BUSQCTA-CRIT-NUM           TO TRUE.
       
           MOVE 85478965487541254896      TO BUSQCTA-E-NUM.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQCTA-NUM'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQCTA-E-CRITERIO  [' BUSQCTA-E-CRITERIO ']'.
           DISPLAY 'BUSQCTA-E-NUM       [' BUSQCTA-E-NUM ']'.
       
           CALL "BUSQCTA"                 USING AREA-BUSQCTA.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQCTA-STAT        [' BUSQCTA-STAT ']'.
           DISPLAY 'BUSQCTA-SQLCODE     [' BUSQCTA-SQLCODE ']'.
           DISPLAY 'BUSQCTA-S-ID        [' BUSQCTA-S-ID ']'.
           DISPLAY 'BUSQCTA-S-NUM       [' BUSQCTA-S-NUM ']'.
           DISPLAY 'BUSQCTA-S-SALDO     [' BUSQCTA-S-SALDO ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-BUSQCTA-NUM.
           EXIT.

       BUSQCTA-ID-ASNT.
      *----------------
      * Cuenta buscada por ID NO existe.
       
           INITIALIZE                     AREA-BUSQCTA-ENTRADA.
       
           SET BUSQCTA-CRIT-ID            TO TRUE.
       
           MOVE 9999999991                TO BUSQCTA-E-ID.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQCTA-ID-ASNT'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQCTA-E-CRITERIO  [' BUSQCTA-E-CRITERIO ']'.
           DISPLAY 'BUSQCTA-E-ID        [' BUSQCTA-E-ID ']'.
       
           CALL "BUSQCTA"                 USING AREA-BUSQCTA.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQCTA-STAT        [' BUSQCTA-STAT ']'.
           DISPLAY 'BUSQCTA-SQLCODE     [' BUSQCTA-SQLCODE ']'.
           DISPLAY 'BUSQCTA-S-ID        [' BUSQCTA-S-ID ']'.
           DISPLAY 'BUSQCTA-S-NUM       [' BUSQCTA-S-NUM ']'.
           DISPLAY 'BUSQCTA-S-SALDO     [' BUSQCTA-S-SALDO ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-BUSQCTA-ID-ASNT.
           EXIT.
       
       BUSQCTA-NUM-ASNT.
      *-----------------
      * Cuenta buscada por número de cuenta NO existe.
      
           INITIALIZE                     AREA-BUSQCTA-ENTRADA.
       
           SET BUSQCTA-CRIT-NUM           TO TRUE.
       
           MOVE 99999999999999999999      TO BUSQCTA-E-NUM.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQCTA-NUM-ASNT'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQCTA-E-CRITERIO  [' BUSQCTA-E-CRITERIO ']'.
           DISPLAY 'BUSQCTA-E-NUM       [' BUSQCTA-E-NUM ']'.
       
           CALL "BUSQCTA"                 USING AREA-BUSQCTA.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQCTA-STAT        [' BUSQCTA-STAT ']'.
           DISPLAY 'BUSQCTA-SQLCODE     [' BUSQCTA-SQLCODE ']'.
           DISPLAY 'BUSQCTA-S-ID        [' BUSQCTA-S-ID ']'.
           DISPLAY 'BUSQCTA-S-NUM       [' BUSQCTA-S-NUM ']'.
           DISPLAY 'BUSQCTA-S-SALDO     [' BUSQCTA-S-SALDO ']'.
           DISPLAY '------------------------------------------------'.
       
       FIN-BUSQCTA-NUM-ASNT.
           EXIT.

       PRUEBA-RELACION.
      *----------------
      *    Creación de relación.
           PERFORM INSRLN-ERR-CLI-ID      THRU FIN-INSRLN-ERR-CLI-ID.
           PERFORM INSRLN-ERR-CTA-ID      THRU FIN-INSRLN-ERR-CTA-ID.
           PERFORM INSRLN-ERR-TIPO        THRU FIN-INSRLN-ERR-TIPO.
           PERFORM INS-RLN                THRU FIN-INS-RLN.
       
      *    Búsqueda de relación.
           PERFORM BUSQRLN-ERR-CLI-ID     THRU FIN-BUSQRLN-ERR-CLI-ID.
           PERFORM BUSQRLN-ERR-CTA-ID     THRU FIN-BUSQRLN-ERR-CTA-ID.
           PERFORM BUSQ-RLN               THRU FIN-BUSQ-RLN.
           PERFORM BUSQ-RLN-ASNT          THRU FIN-BUSQ-RLN-ASNT.

       FIN-PRUEBA-RELACION.
           EXIT.
       
       INSRLN-ERR-CLI-ID.
      *------------------
      * Insertar relación con ID de cliente erróneo.

           INITIALIZE                     AREA-INSRLN-ENTRADA.
       
           MOVE 'AAAAAAAAAA'              TO INSRLN-E-CLI-ID.
           MOVE 0000000001                TO INSRLN-E-CTA-ID.
           MOVE 'T'                       TO INSRLN-E-RLN.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INSRLN-ERR-CLI-ID'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSRLN-E-CLI-ID     [' INSRLN-E-CLI-ID ']'.
           DISPLAY 'INSRLN-E-CTA-ID     [' INSRLN-E-CTA-ID ']'.
           DISPLAY 'INSRLN-E-RLN        [' INSRLN-E-RLN ']'.

           CALL "INSRLN"                  USING AREA-INSRLN.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSRLN-STAT         [' INSRLN-STAT ']'.
           DISPLAY 'INSRLN-SQLCODE      [' INSRLN-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INSRLN-ERR-CLI-ID.
           EXIT.
       
       INSRLN-ERR-CTA-ID.
      *------------------
      * Insertar relación con ID de cuenta erróneo.

           INITIALIZE                     AREA-INSRLN-ENTRADA.
       
           MOVE 0000000001                TO INSRLN-E-CLI-ID.
           MOVE 'AAAAAAAAAA'              TO INSRLN-E-CTA-ID.
           MOVE 'T'                       TO INSRLN-E-RLN.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INSRLN-ERR-CTA-ID'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSRLN-E-CLI-ID     [' INSRLN-E-CLI-ID ']'.
           DISPLAY 'INSRLN-E-CTA-ID     [' INSRLN-E-CTA-ID ']'.
           DISPLAY 'INSRLN-E-RLN        [' INSRLN-E-RLN ']'.

           CALL "INSRLN"                  USING AREA-INSRLN.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSRLN-STAT         [' INSRLN-STAT ']'.
           DISPLAY 'INSRLN-SQLCODE      [' INSRLN-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INSRLN-ERR-CTA-ID.
           EXIT.
       
       INSRLN-ERR-TIPO.  
      *----------------
      * Insertar relación con el tipo de la misma erróneo.

           INITIALIZE                     AREA-INSRLN-ENTRADA.
       
           MOVE 0000000001                TO INSRLN-E-CLI-ID.
           MOVE 0000000001                TO INSRLN-E-CTA-ID.
           MOVE 'A'                       TO INSRLN-E-RLN.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INSRLN-ERR-TIPO'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSRLN-E-CLI-ID     [' INSRLN-E-CLI-ID ']'.
           DISPLAY 'INSRLN-E-CTA-ID     [' INSRLN-E-CTA-ID ']'.
           DISPLAY 'INSRLN-E-RLN        [' INSRLN-E-RLN ']'.

           CALL "INSRLN"                  USING AREA-INSRLN.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSRLN-STAT         [' INSRLN-STAT ']'.
           DISPLAY 'INSRLN-SQLCODE      [' INSRLN-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INSRLN-ERR-TIPO.
           EXIT.
       
       INS-RLN.  
      *--------
      * Insertar una relación correcta.

           INITIALIZE                     AREA-INSRLN-ENTRADA.
       
           MOVE 0000000001                TO INSRLN-E-CLI-ID.
           MOVE 0000000001                TO INSRLN-E-CTA-ID.
           MOVE 'T'                       TO INSRLN-E-RLN.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INS-RLN'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSRLN-E-CLI-ID     [' INSRLN-E-CLI-ID ']'.
           DISPLAY 'INSRLN-E-CTA-ID     [' INSRLN-E-CTA-ID ']'.
           DISPLAY 'INSRLN-E-RLN        [' INSRLN-E-RLN ']'.

           CALL "INSRLN"                  USING AREA-INSRLN.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSRLN-STAT         [' INSRLN-STAT ']'.
           DISPLAY 'INSRLN-SQLCODE      [' INSRLN-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INS-RLN.
           EXIT.
       
       BUSQRLN-ERR-CLI-ID.
      *-------------------
      * Buscar una relación con el ID de cliente erróneo.

           INITIALIZE                     AREA-BUSQRLN-ENTRADA.
       
           MOVE 'AAAAAAAAAA'              TO BUSQRLN-E-CLI-ID.
           MOVE 0000000001                TO BUSQRLN-E-CTA-ID.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQRLN-ERR-CLI-ID'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQRLN-E-CLI-ID    [' BUSQRLN-E-CLI-ID ']'.
           DISPLAY 'BUSQRLN-E-CTA-ID    [' BUSQRLN-E-CTA-ID ']'.
       
           CALL "BUSQRLN"                 USING AREA-BUSQRLN.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQRLN-STAT        [' BUSQRLN-STAT ']'.
           DISPLAY 'BUSQRLN-SQLCODE     [' BUSQRLN-SQLCODE ']'.
           DISPLAY 'BUSQRLN-RELACION    [' BUSQRLN-RELACION ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQRLN-ERR-CLI-ID.
           EXIT.
       
       BUSQRLN-ERR-CTA-ID.
      *-------------------
      * Buscar una relación con el ID de cuenta erróneo.

           INITIALIZE                     AREA-BUSQRLN-ENTRADA.
       
           MOVE 0000000001                TO BUSQRLN-E-CLI-ID.
           MOVE 'AAAAAAAAAA'              TO BUSQRLN-E-CTA-ID.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQRLN-ERR-CTA-ID'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQRLN-E-CLI-ID    [' BUSQRLN-E-CLI-ID ']'.
           DISPLAY 'BUSQRLN-E-CTA-ID    [' BUSQRLN-E-CTA-ID ']'.
       
           CALL "BUSQRLN"                 USING AREA-BUSQRLN.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQRLN-STAT        [' BUSQRLN-STAT ']'.
           DISPLAY 'BUSQRLN-SQLCODE     [' BUSQRLN-SQLCODE ']'.
           DISPLAY 'BUSQRLN-RELACION    [' BUSQRLN-RELACION ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQRLN-ERR-CTA-ID.
           EXIT.
       
       BUSQ-RLN.
      *---------
      * Buscar una relación existente.

           INITIALIZE                     AREA-BUSQRLN-ENTRADA.
       
           MOVE 0000000001                TO BUSQRLN-E-CLI-ID.
           MOVE 0000000001                TO BUSQRLN-E-CTA-ID.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQ-RLN'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQRLN-E-CLI-ID    [' BUSQRLN-E-CLI-ID ']'.
           DISPLAY 'BUSQRLN-E-CTA-ID    [' BUSQRLN-E-CTA-ID ']'.
       
           CALL "BUSQRLN"                 USING AREA-BUSQRLN.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQRLN-STAT        [' BUSQRLN-STAT ']'.
           DISPLAY 'BUSQRLN-SQLCODE     [' BUSQRLN-SQLCODE ']'.
           DISPLAY 'BUSQRLN-RELACION    [' BUSQRLN-RELACION ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQ-RLN.
           EXIT.

       BUSQ-RLN-ASNT.
      *--------------
      * Buscar una relación NO existente.
      
           INITIALIZE                     AREA-BUSQRLN-ENTRADA.
       
           MOVE 9999999991                TO BUSQRLN-E-CLI-ID.
           MOVE 9999999991                TO BUSQRLN-E-CTA-ID.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQ-RLN-ASNT'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQRLN-E-CLI-ID    [' BUSQRLN-E-CLI-ID ']'.
           DISPLAY 'BUSQRLN-E-CTA-ID    [' BUSQRLN-E-CTA-ID ']'.
       
           CALL "BUSQRLN"                 USING AREA-BUSQRLN.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQRLN-STAT        [' BUSQRLN-STAT ']'.
           DISPLAY 'BUSQRLN-SQLCODE     [' BUSQRLN-SQLCODE ']'.
           DISPLAY 'BUSQRLN-RELACION    [' BUSQRLN-RELACION ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQ-RLN-ASNT.
           EXIT.
       
       PRUEBA-TARJETA.
      *---------------
      *    Creación de tarjeta.
           PERFORM INSTAR-ERR-ID-CLI      THRU FIN-INSTAR-ERR-ID-CLI.
           PERFORM INSTAR-ERR-ID-CTA      THRU FIN-INSTAR-ERR-ID-CTA.
           PERFORM INSTAR-ERR-NUM         THRU FIN-INSTAR-ERR-NUM.
           PERFORM INSTAR-ERR-CCV         THRU FIN-INSTAR-ERR-CCV.
           PERFORM INSTAR-ERR-FEC         THRU FIN-INSTAR-ERR-FEC.
           PERFORM INS-TAR                THRU FIN-INS-TAR.
       
      *    Búsqueda de tarjeta.
           PERFORM BUSQTAR-ERR-CRIT       THRU FIN-BUSQTAR-ERR-CRIT.
           PERFORM BUSQTAR-ERR-ID         THRU FIN-BUSQTAR-ERR-ID.
           PERFORM BUSQTAR-ERR-NUM        THRU FIN-BUSQTAR-ERR-NUM.
           PERFORM BUSQTAR-ERR-CLI        THRU FIN-BUSQTAR-ERR-CLI.
           PERFORM BUSQTAR-ERR-CTA        THRU FIN-BUSQTAR-ERR-CTA.
           PERFORM BUSQ-TAR-ID            THRU FIN-BUSQ-TAR-ID.
           PERFORM BUSQ-TAR-ID-ASNT       THRU FIN-BUSQ-TAR-ID-ASNT.
           PERFORM BUSQ-TAR-NUM           THRU FIN-BUSQ-TAR-NUM.
           PERFORM BUSQ-TAR-NUM-ASNT      THRU FIN-BUSQ-TAR-NUM-ASNT.
           PERFORM BUSQ-TAR-CC            THRU FIN-BUSQ-TAR-CC.
           PERFORM BUSQ-TAR-CC-ASNT       THRU FIN-BUSQ-TAR-CC-ASNT.
     
       FIN-PRUEBA-TARJETA.
           EXIT.

       INSTAR-ERR-ID-CLI.
      *------------------
      * Insertar una tarjeta con ID de cliente erróneo.

           INITIALIZE                     AREA-INSTAR-ENTRADA.
       
           MOVE 'AAAAAAAAAA'              TO INSTAR-E-ID-CLI.
           MOVE 0000000001                TO INSTAR-E-ID-CTA.
           MOVE 4587965213569874          TO INSTAR-E-NUM.
           MOVE -00052487.45              TO INSTAR-E-CRED.
           MOVE 0623                      TO INSTAR-E-FEC.
           MOVE 123                       TO INSTAR-E-CCV.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INSTAR-ERR-ID-CLI'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSTAR-E-ID-CLI     [' INSTAR-E-ID-CLI ']'.
           DISPLAY 'INSTAR-E-ID-CTA     [' INSTAR-E-ID-CTA ']'.
           DISPLAY 'INSTAR-E-NUM        [' INSTAR-E-NUM ']'.
           DISPLAY 'INSTAR-E-CRED       [' INSTAR-E-CRED ']'.
           DISPLAY 'INSTAR-E-FEC        [' INSTAR-E-FEC ']'.
           DISPLAY 'INSTAR-E-CCV        [' INSTAR-E-CCV ']'.

           CALL "INSTAR"                  USING AREA-INSTAR.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSTAR-STAT         [' INSTAR-S-TAR-ID ']'.
           DISPLAY 'INSTAR-STAT         [' INSTAR-STAT ']'.
           DISPLAY 'INSTAR-S-SQLCODE    [' INSTAR-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INSTAR-ERR-ID-CLI.
           EXIT.

       INSTAR-ERR-ID-CTA.
      *------------------
      * Insertar una tarjeta con ID de cuenta erróneo.

           INITIALIZE                     AREA-INSTAR-ENTRADA.
       
           MOVE 0000000001                TO INSTAR-E-ID-CLI.
           MOVE 'AAAAAAAAAA'              TO INSTAR-E-ID-CTA.
           MOVE 4587965213569874          TO INSTAR-E-NUM.
           MOVE -00052487.45              TO INSTAR-E-CRED.
           MOVE 0623                      TO INSTAR-E-FEC.
           MOVE 123                       TO INSTAR-E-CCV.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INSTAR-ERR-ID-CTA'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSTAR-E-ID-CLI     [' INSTAR-E-ID-CLI ']'.
           DISPLAY 'INSTAR-E-ID-CTA     [' INSTAR-E-ID-CTA ']'.
           DISPLAY 'INSTAR-E-NUM        [' INSTAR-E-NUM ']'.
           DISPLAY 'INSTAR-E-CRED       [' INSTAR-E-CRED ']'.
           DISPLAY 'INSTAR-E-FEC        [' INSTAR-E-FEC ']'.
           DISPLAY 'INSTAR-E-CCV        [' INSTAR-E-CCV ']'.

           CALL "INSTAR"                  USING AREA-INSTAR.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSTAR-STAT         [' INSTAR-S-TAR-ID ']'.
           DISPLAY 'INSTAR-STAT         [' INSTAR-STAT ']'.
           DISPLAY 'INSTAR-S-SQLCODE    [' INSTAR-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INSTAR-ERR-ID-CTA.
           EXIT.
       
       INSTAR-ERR-NUM.
      *---------------
      * Insertar una tarjeta con número erróneo.

           INITIALIZE                     AREA-INSTAR-ENTRADA.
       
           MOVE 0000000001                TO INSTAR-E-ID-CLI.
           MOVE 0000000001                TO INSTAR-E-ID-CTA.
           MOVE 'AAAAAAAAAAAAAAAA'        TO INSTAR-E-NUM.
           MOVE -00052487.45              TO INSTAR-E-CRED.
           MOVE 0623                      TO INSTAR-E-FEC.
           MOVE 123                       TO INSTAR-E-CCV.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INSTAR-ERR-NUM'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSTAR-E-ID-CLI     [' INSTAR-E-ID-CLI ']'.
           DISPLAY 'INSTAR-E-ID-CTA     [' INSTAR-E-ID-CTA ']'.
           DISPLAY 'INSTAR-E-NUM        [' INSTAR-E-NUM ']'.
           DISPLAY 'INSTAR-E-CRED       [' INSTAR-E-CRED ']'.
           DISPLAY 'INSTAR-E-FEC        [' INSTAR-E-FEC ']'.
           DISPLAY 'INSTAR-E-CCV        [' INSTAR-E-CCV ']'.

           CALL "INSTAR"                  USING AREA-INSTAR.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSTAR-STAT         [' INSTAR-S-TAR-ID ']'.
           DISPLAY 'INSTAR-STAT         [' INSTAR-STAT ']'.
           DISPLAY 'INSTAR-S-SQLCODE    [' INSTAR-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INSTAR-ERR-NUM.
           EXIT.

       INSTAR-ERR-CCV.
      *---------------
      * Insertar una tarjeta con CCV erróneo.

           INITIALIZE                     AREA-INSTAR-ENTRADA.
       
           MOVE 0000000001                TO INSTAR-E-ID-CLI.
           MOVE 0000000001                TO INSTAR-E-ID-CTA.
           MOVE 4587965213569874          TO INSTAR-E-NUM.
           MOVE -00052487.45              TO INSTAR-E-CRED.
           MOVE 0623                      TO INSTAR-E-FEC.
           MOVE 'AAA'                     TO INSTAR-E-CCV.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INSTAR-ERR-CCV'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSTAR-E-ID-CLI     [' INSTAR-E-ID-CLI ']'.
           DISPLAY 'INSTAR-E-ID-CTA     [' INSTAR-E-ID-CTA ']'.
           DISPLAY 'INSTAR-E-NUM        [' INSTAR-E-NUM ']'.
           DISPLAY 'INSTAR-E-CRED       [' INSTAR-E-CRED ']'.
           DISPLAY 'INSTAR-E-FEC        [' INSTAR-E-FEC ']'.
           DISPLAY 'INSTAR-E-CCV        [' INSTAR-E-CCV ']'.

           CALL "INSTAR"                  USING AREA-INSTAR.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSTAR-STAT         [' INSTAR-S-TAR-ID ']'.
           DISPLAY 'INSTAR-STAT         [' INSTAR-STAT ']'.
           DISPLAY 'INSTAR-S-SQLCODE    [' INSTAR-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INSTAR-ERR-CCV.
           EXIT.

       INSTAR-ERR-FEC.
      *---------------
      * Insertar una tarjeta con fecha errónea.

           INITIALIZE                     AREA-INSTAR-ENTRADA.
       
           MOVE 0000000001                TO INSTAR-E-ID-CLI.
           MOVE 0000000001                TO INSTAR-E-ID-CTA.
           MOVE 4587965213569874          TO INSTAR-E-NUM.
           MOVE -00052487.45              TO INSTAR-E-CRED.
           MOVE 'AAAA'                    TO INSTAR-E-FEC.
           MOVE 123                       TO INSTAR-E-CCV.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INSTAR-ERR-FEC'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSTAR-E-ID-CLI     [' INSTAR-E-ID-CLI ']'.
           DISPLAY 'INSTAR-E-ID-CTA     [' INSTAR-E-ID-CTA ']'.
           DISPLAY 'INSTAR-E-NUM        [' INSTAR-E-NUM ']'.
           DISPLAY 'INSTAR-E-CRED       [' INSTAR-E-CRED ']'.
           DISPLAY 'INSTAR-E-FEC        [' INSTAR-E-FEC ']'.
           DISPLAY 'INSTAR-E-CCV        [' INSTAR-E-CCV ']'.

           CALL "INSTAR"                  USING AREA-INSTAR.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSTAR-STAT         [' INSTAR-S-TAR-ID ']'.
           DISPLAY 'INSTAR-STAT         [' INSTAR-STAT ']'.
           DISPLAY 'INSTAR-S-SQLCODE    [' INSTAR-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INSTAR-ERR-FEC.
           EXIT.
       
       INS-TAR.
      *--------
      * Insertar una tarjeta correcta.

           INITIALIZE                     AREA-INSTAR-ENTRADA.
       
           MOVE 0000000001                TO INSTAR-E-ID-CLI.
           MOVE 0000000001                TO INSTAR-E-ID-CTA.
           MOVE 4587965213569874          TO INSTAR-E-NUM.
           MOVE -00052487.45              TO INSTAR-E-CRED.
           MOVE 0623                      TO INSTAR-E-FEC.
           MOVE 123                       TO INSTAR-E-CCV.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INS-TAR'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSTAR-E-ID-CLI     [' INSTAR-E-ID-CLI ']'.
           DISPLAY 'INSTAR-E-ID-CTA     [' INSTAR-E-ID-CTA ']'.
           DISPLAY 'INSTAR-E-NUM        [' INSTAR-E-NUM ']'.
           DISPLAY 'INSTAR-E-CRED       [' INSTAR-E-CRED ']'.
           DISPLAY 'INSTAR-E-FEC        [' INSTAR-E-FEC ']'.
           DISPLAY 'INSTAR-E-CCV        [' INSTAR-E-CCV ']'.

           CALL "INSTAR"                  USING AREA-INSTAR.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSTAR-STAT         [' INSTAR-S-TAR-ID ']'.
           DISPLAY 'INSTAR-STAT         [' INSTAR-STAT ']'.
           DISPLAY 'INSTAR-S-SQLCODE    [' INSTAR-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INS-TAR.
           EXIT.
       
       BUSQTAR-ERR-CRIT.    
      *-----------------
      * Buscar tarjeta con criterio erróneo.

           INITIALIZE                     AREA-BUSQTAR-ENTRADA.
       
           MOVE 3                         TO BUSQTAR-E-CRITERIO.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQTAR-ERR-CRIT'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQTAR-E-CRITERIO  [' BUSQTAR-E-CRITERIO ']'.
       
           CALL "BUSQTAR"                 USING AREA-BUSQTAR.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQTAR-STAT        [' BUSQTAR-STAT ']'.
           DISPLAY 'BUSQTAR-SQLCODE     [' BUSQTAR-SQLCODE ']'.
           DISPLAY 'BUSQTAR-S-ID-TAR    [' BUSQTAR-S-ID-TAR ']'.
           DISPLAY 'BUSQTAR-S-ID-CLI    [' BUSQTAR-S-ID-CLI ']'.
           DISPLAY 'BUSQTAR-S-ID-CTA    [' BUSQTAR-S-ID-CTA ']'.
           DISPLAY 'BUSQTAR-S-NUM       [' BUSQTAR-S-NUM ']'.
           DISPLAY 'BUSQTAR-S-CRED      [' BUSQTAR-S-CRED ']'.
           DISPLAY 'BUSQTAR-S-FEC       [' BUSQTAR-S-FEC ']'.
           DISPLAY 'BUSQTAR-S-CCV       [' BUSQTAR-S-CCV ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQTAR-ERR-CRIT.
           EXIT.
       
       BUSQTAR-ERR-ID.     
      *---------------
      * Buscar tarjeta por ID erróneo.

           INITIALIZE                     AREA-BUSQTAR-ENTRADA.
       
           SET BUSQTAR-CRIT-ID            TO TRUE.
       
           MOVE 'AAAAAAAAAA'              TO BUSQTAR-E-ID.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQTAR-ERR-ID'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQTAR-E-ID        [' BUSQTAR-E-ID ']'.
       
           CALL "BUSQTAR"                 USING AREA-BUSQTAR.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQTAR-STAT        [' BUSQTAR-STAT ']'.
           DISPLAY 'BUSQTAR-SQLCODE     [' BUSQTAR-SQLCODE ']'.
           DISPLAY 'BUSQTAR-S-ID-TAR    [' BUSQTAR-S-ID-TAR ']'.
           DISPLAY 'BUSQTAR-S-ID-CLI    [' BUSQTAR-S-ID-CLI ']'.
           DISPLAY 'BUSQTAR-S-ID-CTA    [' BUSQTAR-S-ID-CTA ']'.
           DISPLAY 'BUSQTAR-S-NUM       [' BUSQTAR-S-NUM ']'.
           DISPLAY 'BUSQTAR-S-CRED      [' BUSQTAR-S-CRED ']'.
           DISPLAY 'BUSQTAR-S-FEC       [' BUSQTAR-S-FEC ']'.
           DISPLAY 'BUSQTAR-S-CCV       [' BUSQTAR-S-CCV ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQTAR-ERR-ID.
           EXIT.
       
       BUSQTAR-ERR-NUM.     
      *----------------
      * Buscar tarjeta por número erróneo.
      
           INITIALIZE                     AREA-BUSQTAR-ENTRADA.
       
           SET BUSQTAR-CRIT-NUM           TO TRUE.
       
           MOVE 'AAAAAAAAAAAAAAAA'        TO BUSQTAR-E-NUM.   

           DISPLAY '*** DEBUG ***       PRBBANCO BUSQTAR-ERR-NUM'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQTAR-E-NUM       [' BUSQTAR-E-NUM ']'.
       
           CALL "BUSQTAR"                 USING AREA-BUSQTAR.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQTAR-STAT        [' BUSQTAR-STAT ']'.
           DISPLAY 'BUSQTAR-SQLCODE     [' BUSQTAR-SQLCODE ']'.
           DISPLAY 'BUSQTAR-S-ID-TAR    [' BUSQTAR-S-ID-TAR ']'.
           DISPLAY 'BUSQTAR-S-ID-CLI    [' BUSQTAR-S-ID-CLI ']'.
           DISPLAY 'BUSQTAR-S-ID-CTA    [' BUSQTAR-S-ID-CTA ']'.
           DISPLAY 'BUSQTAR-S-NUM       [' BUSQTAR-S-NUM ']'.
           DISPLAY 'BUSQTAR-S-CRED      [' BUSQTAR-S-CRED ']'.
           DISPLAY 'BUSQTAR-S-FEC       [' BUSQTAR-S-FEC ']'.
           DISPLAY 'BUSQTAR-S-CCV       [' BUSQTAR-S-CCV ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQTAR-ERR-NUM.
           EXIT.

       BUSQTAR-ERR-CLI.     
      *----------------
      * Buscar tarjeta por id de cliente erróneo.
      
           INITIALIZE                     AREA-BUSQTAR-ENTRADA.
       
           SET BUSQTAR-CRIT-CC            TO TRUE.
       
           MOVE 'AAAAAAAAAA'              TO BUSQTAR-E-ID-CLI.
           MOVE 0000000001                TO BUSQTAR-E-ID-CTA.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQTAR-ERR-CLI'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQTAR-E-ID-CLI    [' BUSQTAR-E-ID-CLI ']'.
           DISPLAY 'BUSQTAR-E-ID-CTA    [' BUSQTAR-E-ID-CTA ']'.
       
           CALL "BUSQTAR"                 USING AREA-BUSQTAR.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQTAR-STAT        [' BUSQTAR-STAT ']'.
           DISPLAY 'BUSQTAR-SQLCODE     [' BUSQTAR-SQLCODE ']'.
           DISPLAY 'BUSQTAR-S-ID-TAR    [' BUSQTAR-S-ID-TAR ']'.
           DISPLAY 'BUSQTAR-S-ID-CLI    [' BUSQTAR-S-ID-CLI ']'.
           DISPLAY 'BUSQTAR-S-ID-CTA    [' BUSQTAR-S-ID-CTA ']'.
           DISPLAY 'BUSQTAR-S-NUM       [' BUSQTAR-S-NUM ']'.
           DISPLAY 'BUSQTAR-S-CRED      [' BUSQTAR-S-CRED ']'.
           DISPLAY 'BUSQTAR-S-FEC       [' BUSQTAR-S-FEC ']'.
           DISPLAY 'BUSQTAR-S-CCV       [' BUSQTAR-S-CCV ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQTAR-ERR-CLI.
           EXIT.

       BUSQTAR-ERR-CTA.     
      *----------------
      * Buscar tarjeta por id de cuenta erróneo.
      
           INITIALIZE                     AREA-BUSQTAR-ENTRADA.
       
           SET BUSQTAR-CRIT-CC            TO TRUE.
       
           MOVE 0000000001                TO BUSQTAR-E-ID-CLI.
           MOVE 'AAAAAAAAAA'              TO BUSQTAR-E-ID-CTA.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQTAR-ERR-CTA'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQTAR-E-ID-CLI    [' BUSQTAR-E-ID-CLI ']'.
           DISPLAY 'BUSQTAR-E-ID-CTA    [' BUSQTAR-E-ID-CTA ']'.
       
           CALL "BUSQTAR"                 USING AREA-BUSQTAR.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQTAR-STAT        [' BUSQTAR-STAT ']'.
           DISPLAY 'BUSQTAR-SQLCODE     [' BUSQTAR-SQLCODE ']'.
           DISPLAY 'BUSQTAR-S-ID-TAR    [' BUSQTAR-S-ID-TAR ']'.
           DISPLAY 'BUSQTAR-S-ID-CLI    [' BUSQTAR-S-ID-CLI ']'.
           DISPLAY 'BUSQTAR-S-ID-CTA    [' BUSQTAR-S-ID-CTA ']'.
           DISPLAY 'BUSQTAR-S-NUM       [' BUSQTAR-S-NUM ']'.
           DISPLAY 'BUSQTAR-S-CRED      [' BUSQTAR-S-CRED ']'.
           DISPLAY 'BUSQTAR-S-FEC       [' BUSQTAR-S-FEC ']'.
           DISPLAY 'BUSQTAR-S-CCV       [' BUSQTAR-S-CCV ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQTAR-ERR-CTA.
           EXIT.
       
       BUSQ-TAR-ID.
      *------------
      * Tarjeta buscada por id encontrada.
      
           INITIALIZE                     AREA-BUSQTAR-ENTRADA.
       
           SET BUSQTAR-CRIT-ID            TO TRUE.
         
           MOVE 0000000002                TO BUSQTAR-E-ID.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQ-TAR-ID'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQTAR-E-ID        [' BUSQTAR-E-ID ']'.
       
           CALL "BUSQTAR"                 USING AREA-BUSQTAR.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQTAR-STAT        [' BUSQTAR-STAT ']'.
           DISPLAY 'BUSQTAR-SQLCODE     [' BUSQTAR-SQLCODE ']'.
           DISPLAY 'BUSQTAR-S-ID-TAR    [' BUSQTAR-S-ID-TAR ']'.
           DISPLAY 'BUSQTAR-S-ID-CLI    [' BUSQTAR-S-ID-CLI ']'.
           DISPLAY 'BUSQTAR-S-ID-CTA    [' BUSQTAR-S-ID-CTA ']'.
           DISPLAY 'BUSQTAR-S-NUM       [' BUSQTAR-S-NUM ']'.
           DISPLAY 'BUSQTAR-S-CRED      [' BUSQTAR-S-CRED ']'.
           DISPLAY 'BUSQTAR-S-FEC       [' BUSQTAR-S-FEC ']'.
           DISPLAY 'BUSQTAR-S-CCV       [' BUSQTAR-S-CCV ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQ-TAR-ID.
           EXIT.

       BUSQ-TAR-ID-ASNT.
      *-----------------
      * Tarjeta buscada por id NO encontrada.
      
           INITIALIZE                     AREA-BUSQTAR-ENTRADA.
       
           SET BUSQTAR-CRIT-ID            TO TRUE.
         
           MOVE 9999999991                TO BUSQTAR-E-ID.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQ-TAR-ID-ASNT'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQTAR-E-ID        [' BUSQTAR-E-ID ']'.
       
           CALL "BUSQTAR"                 USING AREA-BUSQTAR.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQTAR-STAT        [' BUSQTAR-STAT ']'.
           DISPLAY 'BUSQTAR-SQLCODE     [' BUSQTAR-SQLCODE ']'.
           DISPLAY 'BUSQTAR-S-ID-TAR    [' BUSQTAR-S-ID-TAR ']'.
           DISPLAY 'BUSQTAR-S-ID-CLI    [' BUSQTAR-S-ID-CLI ']'.
           DISPLAY 'BUSQTAR-S-ID-CTA    [' BUSQTAR-S-ID-CTA ']'.
           DISPLAY 'BUSQTAR-S-NUM       [' BUSQTAR-S-NUM ']'.
           DISPLAY 'BUSQTAR-S-CRED      [' BUSQTAR-S-CRED ']'.
           DISPLAY 'BUSQTAR-S-FEC       [' BUSQTAR-S-FEC ']'.
           DISPLAY 'BUSQTAR-S-CCV       [' BUSQTAR-S-CCV ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQ-TAR-ID-ASNT.
           EXIT.

       BUSQ-TAR-NUM.
      *-------------
      * Tarjeta buscada por número encontrada.
      
           INITIALIZE                     AREA-BUSQTAR-ENTRADA.
       
           SET BUSQTAR-CRIT-NUM           TO TRUE.
         
           MOVE 9999999999999999          TO BUSQTAR-E-NUM.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQ-TAR-NUM'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQTAR-E-NUM       [' BUSQTAR-E-NUM ']'.
       
           CALL "BUSQTAR"                 USING AREA-BUSQTAR.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQTAR-STAT        [' BUSQTAR-STAT ']'.
           DISPLAY 'BUSQTAR-SQLCODE     [' BUSQTAR-SQLCODE ']'.
           DISPLAY 'BUSQTAR-S-ID-TAR    [' BUSQTAR-S-ID-TAR ']'.
           DISPLAY 'BUSQTAR-S-ID-CLI    [' BUSQTAR-S-ID-CLI ']'.
           DISPLAY 'BUSQTAR-S-ID-CTA    [' BUSQTAR-S-ID-CTA ']'.
           DISPLAY 'BUSQTAR-S-NUM       [' BUSQTAR-S-NUM ']'.
           DISPLAY 'BUSQTAR-S-CRED      [' BUSQTAR-S-CRED ']'.
           DISPLAY 'BUSQTAR-S-FEC       [' BUSQTAR-S-FEC ']'.
           DISPLAY 'BUSQTAR-S-CCV       [' BUSQTAR-S-CCV ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQ-TAR-NUM.
           EXIT.
       
       BUSQ-TAR-NUM-ASNT.
      *------------------
      * Tarjeta buscada por número NO encontrada.
      
           INITIALIZE                     AREA-BUSQTAR-ENTRADA.
       
           SET BUSQTAR-CRIT-NUM           TO TRUE.
         
           MOVE 9999999999999999          TO BUSQTAR-E-NUM.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQ-TAR-NUM-ASNT'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQTAR-E-NUM       [' BUSQTAR-E-NUM ']'.
       
           CALL "BUSQTAR"                 USING AREA-BUSQTAR.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQTAR-STAT        [' BUSQTAR-STAT ']'.
           DISPLAY 'BUSQTAR-SQLCODE     [' BUSQTAR-SQLCODE ']'.
           DISPLAY 'BUSQTAR-S-ID-TAR    [' BUSQTAR-S-ID-TAR ']'.
           DISPLAY 'BUSQTAR-S-ID-CLI    [' BUSQTAR-S-ID-CLI ']'.
           DISPLAY 'BUSQTAR-S-ID-CTA    [' BUSQTAR-S-ID-CTA ']'.
           DISPLAY 'BUSQTAR-S-NUM       [' BUSQTAR-S-NUM ']'.
           DISPLAY 'BUSQTAR-S-CRED      [' BUSQTAR-S-CRED ']'.
           DISPLAY 'BUSQTAR-S-FEC       [' BUSQTAR-S-FEC ']'.
           DISPLAY 'BUSQTAR-S-CCV       [' BUSQTAR-S-CCV ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQ-TAR-NUM-ASNT.
           EXIT.

       BUSQ-TAR-CC.
      *------------
      * Tarjeta buscada por combinación id de cliente y 
      * cuenta encontrada.
      
           INITIALIZE                     AREA-BUSQTAR-ENTRADA.
       
           SET BUSQTAR-CRIT-CC            TO TRUE.
         
           MOVE 0000000001                TO BUSQTAR-E-ID-CLI.
           MOVE 0000000001                TO BUSQTAR-E-ID-CTA.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQ-TAR-CC'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQTAR-E-ID-CLI    [' BUSQTAR-E-ID-CLI ']'.
           DISPLAY 'BUSQTAR-E-ID-CTA    [' BUSQTAR-E-ID-CTA ']'.
       
           CALL "BUSQTAR"                 USING AREA-BUSQTAR.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQTAR-STAT        [' BUSQTAR-STAT ']'.
           DISPLAY 'BUSQTAR-SQLCODE     [' BUSQTAR-SQLCODE ']'.
           DISPLAY 'BUSQTAR-S-ID-TAR    [' BUSQTAR-S-ID-TAR ']'.
           DISPLAY 'BUSQTAR-S-ID-CLI    [' BUSQTAR-S-ID-CLI ']'.
           DISPLAY 'BUSQTAR-S-ID-CTA    [' BUSQTAR-S-ID-CTA ']'.
           DISPLAY 'BUSQTAR-S-NUM       [' BUSQTAR-S-NUM ']'.
           DISPLAY 'BUSQTAR-S-CRED      [' BUSQTAR-S-CRED ']'.
           DISPLAY 'BUSQTAR-S-FEC       [' BUSQTAR-S-FEC ']'.
           DISPLAY 'BUSQTAR-S-CCV       [' BUSQTAR-S-CCV ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQ-TAR-CC.
           EXIT.

       BUSQ-TAR-CC-ASNT.
      *-----------------
      * Tarjeta buscada por combinación id de cliente y 
      * cuenta NO encontrada.
      
           INITIALIZE                     AREA-BUSQTAR-ENTRADA.
       
           SET BUSQTAR-CRIT-CC            TO TRUE.
         
           MOVE 9999999991                TO BUSQTAR-E-ID-CLI.
           MOVE 9999999991                TO BUSQTAR-E-ID-CTA.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQ-TAR-CC-ASNT'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQTAR-E-ID-CLI    [' BUSQTAR-E-ID-CLI ']'.
           DISPLAY 'BUSQTAR-E-ID-CTA    [' BUSQTAR-E-ID-CTA ']'.
       
           CALL "BUSQTAR"                 USING AREA-BUSQTAR.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQTAR-STAT        [' BUSQTAR-STAT ']'.
           DISPLAY 'BUSQTAR-SQLCODE     [' BUSQTAR-SQLCODE ']'.
           DISPLAY 'BUSQTAR-S-ID-TAR    [' BUSQTAR-S-ID-TAR ']'.
           DISPLAY 'BUSQTAR-S-ID-CLI    [' BUSQTAR-S-ID-CLI ']'.
           DISPLAY 'BUSQTAR-S-ID-CTA    [' BUSQTAR-S-ID-CTA ']'.
           DISPLAY 'BUSQTAR-S-NUM       [' BUSQTAR-S-NUM ']'.
           DISPLAY 'BUSQTAR-S-CRED      [' BUSQTAR-S-CRED ']'.
           DISPLAY 'BUSQTAR-S-FEC       [' BUSQTAR-S-FEC ']'.
           DISPLAY 'BUSQTAR-S-CCV       [' BUSQTAR-S-CCV ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQ-TAR-CC-ASNT.
           EXIT.

       PRUEBA-MOVIMIENTO.
      *------------------
      *    Creación de movimiento.
           PERFORM INSMOV-ERR-ID          THRU FIN-INSMOV-ERR-ID.
           PERFORM INSMOV-ERR-FEC         THRU FIN-INSMOV-ERR-FEC.
           PERFORM INSMOV-ERR-CPT         THRU FIN-INSMOV-ERR-CPT.
           PERFORM INS-MOV                THRU FIN-INS-MOV.
       
      *    Búsqueda de movimiento.
           PERFORM BUSQMOV-ERR-ID         THRU FIN-BUSQMOV-ERR-ID.
           PERFORM BUSQMOV-ERR-FEC        THRU FIN-BUSQMOV-ERR-FEC.
           PERFORM BUSQ-MOV-ID-FEC        THRU FIN-BUSQ-MOV-ID-FEC.
           PERFORM BUSQ-MOV-ID-FEC-ASNT   THRU FIN-BUSQ-MOV-ID-FEC-ASNT.

       FIN-PRUEBA-MOVIMIENTO.
           EXIT.
           
       INSMOV-ERR-ID.      
      *--------------
      * Insertar movimiento con id erróneo.

           INITIALIZE                     AREA-INSMOV-ENTRADA.
       
           MOVE 'AAAAAAAAAA'              TO INSMOV-E-ID.
           MOVE 20220624050505050505      TO INSMOV-E-FEC.
           MOVE 'MOVIMIENTO1'             TO INSMOV-E-CPT.
           MOVE -00000587.23              TO INSMOV-E-IMPT.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INSMOV-ERR-ID'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSMOV-E-ID         [' INSMOV-E-ID ']'.
           DISPLAY 'INSMOV-E-FEC        [' INSMOV-E-FEC ']'.
           DISPLAY 'INSMOV-E-CPT        [' INSMOV-E-CPT ']'.
           DISPLAY 'INSMOV-E-IMPT       [' INSMOV-E-IMPT ']'.
       
           CALL "INSMOV"                  USING AREA-INSMOV.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSMOV-STAT         [' INSMOV-STAT ']'.
           DISPLAY 'INSMOV-S-SQLCODE    [' INSMOV-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INSMOV-ERR-ID.
           EXIT.
      
       INSMOV-ERR-FEC.      
      *--------------
      * Insertar movimiento con fecha errónea.

           INITIALIZE                     AREA-INSMOV-ENTRADA.
       
           MOVE 0000000001                TO INSMOV-E-ID.
           MOVE 'AAAAAAAAAAAAAAAAAAAA'    TO INSMOV-E-FEC.
           MOVE 'MOVIMIENTO1'             TO INSMOV-E-CPT.
           MOVE -00000587.23              TO INSMOV-E-IMPT.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INSMOV-ERR-FEC'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSMOV-E-ID         [' INSMOV-E-ID ']'.
           DISPLAY 'INSMOV-E-FEC        [' INSMOV-E-FEC ']'.
           DISPLAY 'INSMOV-E-CPT        [' INSMOV-E-CPT ']'.
           DISPLAY 'INSMOV-E-IMPT       [' INSMOV-E-IMPT ']'.
       
           CALL "INSMOV"                  USING AREA-INSMOV.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSMOV-STAT         [' INSMOV-STAT ']'.
           DISPLAY 'INSMOV-S-SQLCODE    [' INSMOV-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INSMOV-ERR-FEC.
           EXIT.

       INSMOV-ERR-CPT.      
      *--------------
      * Insertar movimiento con concepto erróneo.

           INITIALIZE                     AREA-INSMOV-ENTRADA.
       
           MOVE 0000000001                TO INSMOV-E-ID.
           MOVE 20220624050505050505      TO INSMOV-E-FEC.
           MOVE SPACES                    TO INSMOV-E-CPT.
           MOVE -00000587.23              TO INSMOV-E-IMPT.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INSMOV-ERR-CPT'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSMOV-E-ID         [' INSMOV-E-ID ']'.
           DISPLAY 'INSMOV-E-FEC        [' INSMOV-E-FEC ']'.
           DISPLAY 'INSMOV-E-CPT        [' INSMOV-E-CPT ']'.
           DISPLAY 'INSMOV-E-IMPT       [' INSMOV-E-IMPT ']'.
       
           CALL "INSMOV"                  USING AREA-INSMOV.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSMOV-STAT         [' INSMOV-STAT ']'.
           DISPLAY 'INSMOV-S-SQLCODE    [' INSMOV-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INSMOV-ERR-CPT.
           EXIT.

       INS-MOV.      
      *--------
      * Insertar movimiento correcto.

           INITIALIZE                     AREA-INSMOV-ENTRADA.
           
           MOVE 0000000001                TO INSMOV-E-ID.
           MOVE 'MOVIMIENTO1'             TO INSMOV-E-CPT.
           MOVE 20220624050505050505      TO INSMOV-E-FEC.
           MOVE -00000587.23              TO INSMOV-E-IMPT.
       
           DISPLAY '*** DEBUG ***       PRBBANCO INS-MOV'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'INSMOV-E-ID         [' INSMOV-E-ID ']'.
           DISPLAY 'INSMOV-E-FEC        [' INSMOV-E-FEC ']'.
           DISPLAY 'INSMOV-E-CPT        [' INSMOV-E-CPT ']'.
           DISPLAY 'INSMOV-E-IMPT       [' INSMOV-E-IMPT ']'.
       
           CALL "INSMOV"                  USING AREA-INSMOV.
       
           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'INSMOV-STAT         [' INSMOV-STAT ']'.
           DISPLAY 'INSMOV-S-SQLCODE    [' INSMOV-S-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.

       FIN-INS-MOV.
           EXIT.

       BUSQMOV-ERR-ID.     
      *---------------
      * Criterio de búsqueda por id erróneo.

           INITIALIZE                     AREA-BUSQMOV-ENTRADA.
       
           MOVE 'AAAAAAAAAA'              TO BUSQMOV-E-ID.
           MOVE 20220624050505050505      TO BUSQMOV-E-FEC.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQMOV-ERR-ID'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQMOV-E-ID        [' BUSQMOV-E-ID ']'.
           DISPLAY 'BUSQMOV-E-FEC       [' BUSQMOV-E-FEC ']'.

           CALL "BUSQMOV"                 USING AREA-BUSQMOV.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQMOV-STAT        [' BUSQMOV-STAT ']'.
           DISPLAY 'BUSQMOV-SQLCODE     [' BUSQMOV-SQLCODE ']'.
           DISPLAY 'BUSQMOV-S-ID        [' BUSQMOV-S-ID ']'.
           DISPLAY 'BUSQMOV-S-FEC       [' BUSQMOV-S-FEC ']'.
           DISPLAY 'BUSQMOV-S-CPT       [' BUSQMOV-S-CPT ']'.
           DISPLAY 'BUSQMOV-S-IMPT      [' BUSQMOV-S-IMPT ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQMOV-ERR-ID.
           EXIT.
       
       BUSQMOV-ERR-FEC.     
      *----------------
      * Criterio de búsqueda por fecha errónea.

           INITIALIZE                     AREA-BUSQMOV-ENTRADA.
       
           MOVE 0000000001                TO BUSQMOV-E-ID.
           MOVE 'AAAAAAAAAAAAAAAAAAAA'    TO BUSQMOV-E-FEC.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQMOV-ERR-FEC'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQMOV-E-CPT       [' BUSQMOV-E-ID ']'.
           DISPLAY 'BUSQMOV-E-FEC       [' BUSQMOV-E-FEC ']'.

           CALL "BUSQMOV"                 USING AREA-BUSQMOV.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQMOV-STAT        [' BUSQMOV-STAT ']'.
           DISPLAY 'BUSQMOV-SQLCODE     [' BUSQMOV-SQLCODE ']'.
           DISPLAY 'BUSQMOV-S-ID        [' BUSQMOV-S-ID ']'.
           DISPLAY 'BUSQMOV-S-FEC       [' BUSQMOV-S-FEC ']'.
           DISPLAY 'BUSQMOV-S-CPT       [' BUSQMOV-S-CPT ']'.
           DISPLAY 'BUSQMOV-S-IMPT      [' BUSQMOV-S-IMPT ']'.
           DISPLAY '------------------------------------------------'.
           
       FIN-BUSQMOV-ERR-FEC.
           EXIT.
      *
       BUSQ-MOV-ID-FEC. 
      *----------------
      * Búsqueda por id+fecha encontrada.

           INITIALIZE                     AREA-BUSQMOV-ENTRADA.
       
           MOVE 0000000001                TO BUSQMOV-E-ID.
           MOVE 20220624050505050505      TO BUSQMOV-E-FEC.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQ-MOV-ID-FEC'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQMOV-E-ID        [' BUSQMOV-E-ID ']'.
           DISPLAY 'BUSQMOV-E-FEC       [' BUSQMOV-E-FEC ']'.

           CALL "BUSQMOV"                 USING AREA-BUSQMOV.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQMOV-STAT        [' BUSQMOV-STAT ']'.
           DISPLAY 'BUSQMOV-SQLCODE     [' BUSQMOV-SQLCODE ']'.
           DISPLAY 'BUSQMOV-S-ID        [' BUSQMOV-S-ID ']'.
           DISPLAY 'BUSQMOV-S-FEC       [' BUSQMOV-S-FEC ']'.
           DISPLAY 'BUSQMOV-S-CPT       [' BUSQMOV-S-CPT ']'.
           DISPLAY 'BUSQMOV-S-IMPT      [' BUSQMOV-S-IMPT ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQ-MOV-ID-FEC.
           EXIT.

       BUSQ-MOV-ID-FEC-ASNT. 
      *---------------------
      * Búsqueda por id+fecha NO encontrada.

           INITIALIZE                     AREA-BUSQMOV-ENTRADA.
       
           MOVE 9999999991                TO BUSQMOV-E-ID.
           MOVE 20220624050505050506      TO BUSQMOV-E-FEC.
       
           DISPLAY '*** DEBUG ***       PRBBANCO BUSQ-MOV-ID-FEC-ASNT'.
           DISPLAY '*** DEBUG ***       [INICIO]'.
           DISPLAY 'BUSQMOV-E-ID        [' BUSQMOV-E-ID ']'.
           DISPLAY 'BUSQMOV-E-FEC       [' BUSQMOV-E-FEC ']'.

           CALL "BUSQMOV"                 USING AREA-BUSQMOV.

           DISPLAY '*** DEBUG ***       [FIN]'.
           DISPLAY 'BUSQMOV-STAT        [' BUSQMOV-STAT ']'.
           DISPLAY 'BUSQMOV-SQLCODE     [' BUSQMOV-SQLCODE ']'.
           DISPLAY 'BUSQMOV-S-ID        [' BUSQMOV-S-ID ']'.
           DISPLAY 'BUSQMOV-S-FEC       [' BUSQMOV-S-FEC ']'.
           DISPLAY 'BUSQMOV-S-CPT       [' BUSQMOV-S-CPT ']'.
           DISPLAY 'BUSQMOV-S-IMPT      [' BUSQMOV-S-IMPT ']'.
           DISPLAY '------------------------------------------------'.

       FIN-BUSQ-MOV-ID-FEC-ASNT.
           EXIT.