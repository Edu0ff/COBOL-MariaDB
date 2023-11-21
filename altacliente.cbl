       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. ALTACLIENTE.
       AUTHOR. MARÍA EDUARDA ALMEIDA LOIOLA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *------------------------
           EXEC SQL BEGIN DECLARE SECTION    END-EXEC.
           EXEC SQL END   DECLARE SECTION    END-EXEC.

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
          
       01  WK-DATASOURCE                      PIC X(50) VALUE 
                                              'edu/banco@bancoDS'.

       01  WK-NUM-CTA                         PIC 9(20).
       01  WK-NUM-TAR                         PIC 9(16).
       01  WK-ID-DOMICILIO                    PIC 9(10).
       01  WK-ID-CLIENTE                      PIC 9(10).
       01  WK-ID-CUENTA                       PIC 9(10).
       01  WK-ID-TARJETA                      PIC 9(10).
       01  WK-ID-MEDIO                        PIC 9(10).
       01  WK-I                               PIC 9(01).

       LINKAGE SECTION.
      *---------------- 
           COPY 'altacliente.cpy'.

      ******************************************************************
       PROCEDURE DIVISION                     USING AREA-ALTACLIENTE.
      ******************************************************************
           PERFORM INICIALIZAR                THRU FIN-INICIALIZAR.
      * Conecto a BD.
           PERFORM CONECTAR-BD                THRU FIN-CONECTAR-BD.
      * Si el domicilio viene informado lo busco. Si existe guardo su
      * id. Si no existe, lo creo, y guardo el id.
           IF   ALTACLI-DOM-CALLE NOT = ALL SPACES
           THEN PERFORM BUSCAR-DOMICILIO      THRU FIN-BUSCAR-DOMICILIO
                IF   BUSQDOM-STAT-ENC-NO
                THEN PERFORM INSERTAR-DOMICILIO 
                     THRU FIN-INSERTAR-DOMICILIO
                END-IF
           END-IF.
      * Busco el cliente. Si existe, error. Si no existe, lo inserto.
           PERFORM BUSCAR-CLIENTE             THRU BUSCAR-CLIENTE.  

           IF   ALTACLI-STAT-OK
           THEN PERFORM INSERTAR-CLIENTE      THRU FIN-INSERTAR-CLIENTE
           END-IF.
      * Busco mientras hayan cuentas. 
      * Si existe, guardo el id y busco la relación.
      * Si no existe, la inserto.
           PERFORM VARYING WK-I FROM 1 BY 1 
           UNTIL WK-I > ALTACLI-CUE-TAR-CONTADOR
                 MOVE ALTACLI-NUM-CTA(WK-I)  TO WK-NUM-CTA
                 PERFORM BUSCAR-CUENTA       THRU FIN-BUSCAR-CUENTA
                 IF   BUSQCTA-STAT-ENC-NO
                 THEN PERFORM INSERTAR-CUENTA 
                      THRU FIN-INSERTAR-CUENTA
                 END-IF

                 PERFORM BUSCAR-RELACION     THRU FIN-BUSCAR-RELACION
                 IF   BUSQRLN-STAT-ENC-NO
                 THEN PERFORM INSERTAR-RELACION 
                      THRU FIN-INSERTAR-RELACION
                 END-IF

                 IF ALTACLI-NUM-TAR(WK-I) NOT = ALL ZEROES
                 THEN PERFORM BUSCAR-TARJETA      
                      THRU FIN-BUSCAR-TARJETA
                      IF   BUSQTAR-STAT-ENC-NO
                      THEN PERFORM INSERTAR-TARJETA 
                           THRU FIN-INSERTAR-TARJETA
                      ELSE SET ALTACLI-STAT-ERR-TAR-ENC TO TRUE
                           GO                TO FIN-PROGRAMA
                      END-IF
                 END-IF

           END-PERFORM.
           
           PERFORM VARYING WK-I FROM 1 BY 1 
           UNTIL WK-I > ALTACLI-MOV-CONTADOR
                 IF   ALTACLI-MOV-TIPO-MEDIO-C(WK-I)
                 THEN MOVE ALTACLI-MOV-NUM-MEDIO-CTA(WK-I) TO WK-NUM-CTA
                      PERFORM BUSCAR-CUENTA  THRU FIN-BUSCAR-CUENTA
                      IF   BUSQCTA-STAT-ENC-NO
                      THEN SET ALTACLI-STAT-CTA-ENC-NO TO TRUE
                           GO                          TO FIN-PROGRAMA
                      ELSE MOVE WK-ID-CUENTA           TO WK-ID-MEDIO
                      END-IF
                 ELSE MOVE ALTACLI-MOV-NUM-MEDIO-TAR(WK-I) TO WK-NUM-TAR
                      PERFORM BUSCAR-TARJETA THRU FIN-BUSCAR-TARJETA
                      IF   BUSQTAR-STAT-ENC-NO
                      THEN SET ALTACLI-STAT-TAR-ENC-NO TO TRUE
                           GO                          TO FIN-PROGRAMA
                      ELSE MOVE WK-ID-TARJETA          TO WK-ID-MEDIO
                      END-IF
                 END-IF

                 PERFORM BUSCAR-MOVIMIENTO   THRU FIN-BUSCAR-MOVIMIENTO
                 
                 IF   BUSQMOV-STAT-ENC-NO
                 THEN PERFORM INSERTAR-MOVIMIENTO
                      THRU FIN-INSERTAR-MOVIMIENTO
                 END-IF
           
           END-PERFORM.

       FIN-PROGRAMA.
      *-------------
           IF   CONEXMDB-STAT-OK
           THEN IF ALTACLI-STAT-OK
                THEN PERFORM HACER-COMMIT    THRU FIN-HACER-COMMIT
                ELSE PERFORM HACER-ROLLBACK  THRU FIN-HACER-ROLLBACK
                END-IF
                PERFORM DESCONECTAR-BD       THRU FIN-DESCONECTAR-BD
           END-IF.

           EXIT PROGRAM.

       INICIALIZAR.
      *------------
           INITIALIZE                        ALTACLIENTE-SALIDA
                                             WK-NUM-CTA
                                             WK-NUM-TAR
                                             WK-ID-DOMICILIO
                                             WK-ID-CLIENTE
                                             WK-ID-CUENTA
                                             WK-ID-TARJETA
                                             WK-ID-MEDIO
                                             WK-I.

       FIN-INICIALIZAR.
           EXIT.

       CONECTAR-BD.
      *------------
           INITIALIZE                        AREA-CONEXMDB-ENTRADA.

           SET CONEXMDB-E-ACC-ABRIR          TO TRUE.

           MOVE WK-DATASOURCE                TO CONEXMDB-E-DSNAME.

           CALL "CONEXMDB"                   USING AREA-CONEXMDB.

           IF   NOT CONEXMDB-STAT-OK
           THEN SET ALTACLI-STAT-ERR-SQL     TO TRUE
                MOVE CONEXMDB-SQLCODE        TO ALTACLI-SQLCODE
                GO                           TO FIN-PROGRAMA
           END-IF.

       FIN-CONECTAR-BD.
           EXIT.

       DESCONECTAR-BD.
      *---------------
           INITIALIZE                        AREA-CONEXMDB-ENTRADA.

           SET CONEXMDB-E-ACC-CERRAR         TO TRUE.

           CALL "CONEXMDB"                   USING AREA-CONEXMDB.

           IF   NOT CONEXMDB-STAT-OK
           THEN SET ALTACLI-STAT-ERR-SQL     TO TRUE
                MOVE CONEXMDB-SQLCODE        TO ALTACLI-SQLCODE
           END-IF.

       FIN-DESCONECTAR-BD.
           EXIT.

       BUSCAR-DOMICILIO.
      *-----------------
           INITIALIZE                        AREA-BUSQDOM-ENTRADA
                                             WK-ID-DOMICILIO.

           SET BUSQDOM-CRIT-COMBI            TO TRUE.

           MOVE ALTACLI-DOM-NUMERO           TO BUSQDOM-E-NUM.
           MOVE ALTACLI-DOM-CALLE            TO BUSQDOM-E-CALLE.
           MOVE ALTACLI-DOM-CODPOS           TO BUSQDOM-E-COD-POS.
           
           CALL "BUSQDOM"                    USING AREA-BUSQDOM.

           EVALUATE TRUE
           WHEN BUSQDOM-STAT-OK
                MOVE BUSQDOM-S-ID            TO WK-ID-DOMICILIO
           WHEN BUSQDOM-STAT-ENC-NO
                CONTINUE
           WHEN BUSQDOM-STAT-ERR-COD-POS
                SET ALTACLI-STAT-ERR-COD-POS TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN BUSQDOM-STAT-ERR-CALLE
                SET ALTACLI-STAT-ERR-CALLE   TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN OTHER
                SET ALTACLI-STAT-ERR-SQL     TO TRUE
                MOVE BUSQDOM-SQLCODE         TO ALTACLI-SQLCODE
                GO                           TO FIN-PROGRAMA
           END-EVALUATE.

       FIN-BUSCAR-DOMICILIO.
           EXIT.

       INSERTAR-DOMICILIO.
      *-------------------
           INITIALIZE                        AREA-INSDOM-ENTRADA
                                             WK-ID-DOMICILIO.

           MOVE ALTACLI-DOM-CALLE            TO INSDOM-E-CALLE.   
           MOVE ALTACLI-DOM-NUMERO           TO INSDOM-E-NUM.     
           MOVE ALTACLI-DOM-CODPOS           TO INSDOM-E-COD-POS. 
           MOVE ALTACLI-DOM-PROV             TO INSDOM-E-PROV.    
           MOVE ALTACLI-DOM-POBL             TO INSDOM-E-POBL.

           CALL "INSDOM"                     USING AREA-INSDOM.

           EVALUATE TRUE
           WHEN INSDOM-STAT-OK
                MOVE INSDOM-S-DOM-ID         TO WK-ID-DOMICILIO
           WHEN INSDOM-STAT-ERR-CALLE
                SET ALTACLI-STAT-ERR-CALLE   TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN INSDOM-STAT-ERR-COD-POS
                SET ALTACLI-STAT-ERR-COD-POS TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN INSDOM-STAT-ERR-POBL
                SET ALTACLI-STAT-ERR-POBL    TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN INSDOM-STAT-ERR-PROV
                SET ALTACLI-STAT-ERR-PROV    TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN OTHER
                SET ALTACLI-STAT-ERR-SQL     TO TRUE
                MOVE INSDOM-S-SQLCODE        TO ALTACLI-SQLCODE
                GO                           TO FIN-PROGRAMA
           END-EVALUATE.
   
       FIN-INSERTAR-DOMICILIO.
           EXIT.

       BUSCAR-CLIENTE.
      *---------------
           INITIALIZE                        AREA-BUSQCLI-ENTRADA
                                             WK-ID-CLIENTE.

           SET  BUSQCLI-CRIT-NIF             TO TRUE.
           MOVE ALTACLI-CLI-NIF              TO BUSQCLI-E-NIF.

           CALL "BUSQCLI"                    USING AREA-BUSQCLI.

           EVALUATE TRUE
           WHEN BUSQCLI-STAT-OK
                SET ALTACLI-STAT-ERR-CLI-ENC TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN BUSQCLI-STAT-ENC-NO
                CONTINUE
           WHEN BUSQCLI-STAT-ERR-NIF
                SET ALTACLI-STAT-ERR-NIF     TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN OTHER
                SET ALTACLI-STAT-ERR-SQL     TO TRUE
                MOVE BUSQCLI-SQLCODE         TO ALTACLI-SQLCODE
                GO                           TO FIN-PROGRAMA
           END-EVALUATE.

       FIN-BUSCAR-CLIENTE.
           EXIT.

       INSERTAR-CLIENTE.
      *-----------------
           INITIALIZE                        AREA-INSCLI-ENTRADA.

           MOVE WK-ID-DOMICILIO              TO INSCLI-E-ID-DOM
           MOVE ALTACLI-CLI-NIF              TO INSCLI-E-NIF.  
           MOVE ALTACLI-CLI-NOMBRE           TO INSCLI-E-NOM. 
           MOVE ALTACLI-CLI-FEC-NAC          TO INSCLI-E-FEC-NAC.

           CALL "INSCLI"                     USING AREA-INSCLI.

           EVALUATE TRUE
           WHEN INSCLI-STAT-OK
                MOVE INSCLI-S-CLI-ID         TO WK-ID-CLIENTE
           WHEN INSCLI-STAT-ERR-NIF
                SET ALTACLI-STAT-ERR-NIF     TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN INSCLI-STAT-ERR-NOM
                SET ALTACLI-STAT-ERR-NOM     TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN INSCLI-STAT-ERR-FEC-NAC
                SET ALTACLI-STAT-ERR-FEC-NAC TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN OTHER
                SET ALTACLI-STAT-ERR-SQL     TO TRUE
                MOVE INSCLI-S-SQLCODE        TO ALTACLI-SQLCODE
                GO                           TO FIN-PROGRAMA
           END-EVALUATE.
      
       FIN-INSERTAR-CLIENTE.
           EXIT.

       BUSCAR-CUENTA.
      *--------------
           INITIALIZE                        AREA-BUSQCTA-ENTRADA
                                             WK-ID-CUENTA.

           SET BUSQCTA-CRIT-NUM              TO TRUE.

           MOVE WK-NUM-CTA                   TO BUSQCTA-E-NUM.
           
           CALL "BUSQCTA"                    USING AREA-BUSQCTA.

           EVALUATE TRUE
           WHEN BUSQCTA-STAT-OK
                MOVE BUSQCTA-S-ID            TO WK-ID-CUENTA
           WHEN BUSQCTA-STAT-ENC-NO
                CONTINUE
           WHEN BUSQCTA-STAT-ERR-NUM
                SET ALTACLI-STAT-ERR-CTA-NUM TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN OTHER
                SET ALTACLI-STAT-ERR-SQL     TO TRUE
                MOVE BUSQCTA-SQLCODE         TO ALTACLI-SQLCODE
                GO                           TO FIN-PROGRAMA
           END-EVALUATE.

       FIN-BUSCAR-CUENTA.
           EXIT.

       INSERTAR-CUENTA.
      *----------------
           INITIALIZE                        AREA-INSCTA-ENTRADA
                                             WK-ID-CUENTA.
            
           MOVE ALTACLI-NUM-CTA(WK-I)        TO INSCTA-E-NUM.
           MOVE ALTACLI-SALDO-CTA(WK-I)      TO INSCTA-E-SALDO.

           CALL "INSCTA"                     USING AREA-INSCTA.

           EVALUATE TRUE
           WHEN INSCTA-STAT-OK
                MOVE INSCTA-S-CTA-ID         TO WK-ID-CUENTA
           WHEN INSCTA-STAT-ERR-NUM
                SET ALTACLI-STAT-ERR-CTA-NUM TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN OTHER
                SET ALTACLI-STAT-ERR-SQL     TO TRUE
                MOVE INSCTA-S-SQLCODE        TO ALTACLI-SQLCODE
                GO                           TO FIN-PROGRAMA
           END-EVALUATE.

       FIN-INSERTAR-CUENTA.
           EXIT.

       BUSCAR-RELACION.
      *----------------
           INITIALIZE                        AREA-BUSQRLN-ENTRADA.

           MOVE WK-ID-CUENTA                 TO BUSQRLN-E-CTA-ID.
           MOVE WK-ID-CLIENTE                TO BUSQRLN-E-CLI-ID.

           CALL "BUSQRLN"                    USING AREA-BUSQRLN.

           EVALUATE TRUE
           WHEN BUSQRLN-STAT-OK
                SET ALTACLI-STAT-ERR-RLN-ENC TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN BUSQRLN-STAT-ENC-NO
                CONTINUE
           WHEN OTHER
                SET ALTACLI-STAT-ERR-SQL     TO TRUE
                MOVE BUSQRLN-SQLCODE         TO ALTACLI-SQLCODE
                GO                           TO FIN-PROGRAMA
           END-EVALUATE.

       FIN-BUSCAR-RELACION.
           EXIT.

       INSERTAR-RELACION.
      *------------------
           INITIALIZE                        AREA-INSRLN-ENTRADA.

           MOVE WK-ID-CUENTA                 TO INSRLN-E-CTA-ID.
           MOVE WK-ID-CLIENTE                TO INSRLN-E-CLI-ID.
           MOVE ALTACLI-RLN-CTA(WK-I)        TO INSRLN-E-RLN.

           CALL "INSRLN"                     USING AREA-INSRLN.

           EVALUATE TRUE
           WHEN INSRLN-STAT-OK
                CONTINUE
           WHEN INSRLN-STAT-ERR-RLN
                SET ALTACLI-STAT-ERR-RLN-TIP TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN OTHER
                SET ALTACLI-STAT-ERR-SQL     TO TRUE
                MOVE INSRLN-SQLCODE          TO ALTACLI-SQLCODE
                GO                           TO FIN-PROGRAMA
           END-EVALUATE.

       FIN-INSERTAR-RELACION.
           EXIT.

       BUSCAR-TARJETA.
      *---------------
           INITIALIZE                        AREA-BUSQTAR-ENTRADA
                                             WK-ID-TARJETA.

           SET BUSQTAR-CRIT-NUM              TO TRUE. 
           
           MOVE WK-NUM-TAR                   TO BUSQTAR-E-NUM.

           CALL "BUSQTAR"                    USING AREA-BUSQTAR.

           EVALUATE TRUE
           WHEN BUSQTAR-STAT-OK
                MOVE BUSQTAR-S-ID-TAR       TO WK-ID-TARJETA
           WHEN BUSQTAR-STAT-ENC-NO
                CONTINUE
           WHEN BUSQTAR-STAT-ERR-NUM
                SET ALTACLI-STAT-ERR-TAR-NUM TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN OTHER
                SET ALTACLI-STAT-ERR-SQL     TO TRUE
                MOVE BUSQTAR-SQLCODE         TO ALTACLI-SQLCODE
                GO                           TO FIN-PROGRAMA
           END-EVALUATE.

       FIN-BUSCAR-TARJETA.
           EXIT.

       INSERTAR-TARJETA.
      *-----------------
           INITIALIZE                        AREA-INSTAR-ENTRADA.

           MOVE WK-ID-CUENTA                 TO INSTAR-E-ID-CTA.
           MOVE WK-ID-CLIENTE                TO INSTAR-E-ID-CLI.
           MOVE ALTACLI-NUM-TAR(WK-I)        TO INSTAR-E-NUM.
           MOVE ALTACLI-CRE-TAR(WK-I)        TO INSTAR-E-CRED.
           MOVE ALTACLI-FEC-TAR(WK-I)        TO INSTAR-E-FEC.
           MOVE ALTACLI-CCV-TAR(WK-I)        TO INSTAR-E-CCV.

           CALL "INSTAR"                     USING AREA-INSTAR.

           EVALUATE TRUE
           WHEN INSTAR-STAT-OK
                CONTINUE
           WHEN INSTAR-STAT-ERR-NUM
                SET ALTACLI-STAT-ERR-TAR-NUM TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN INSTAR-STAT-ERR-CCV
                SET ALTACLI-STAT-ERR-TAR-CCV TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN INSTAR-STAT-ERR-FEC
                SET ALTACLI-STAT-ERR-TAR-FEC TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN OTHER
                SET ALTACLI-STAT-ERR-SQL     TO TRUE
                MOVE INSTAR-S-SQLCODE        TO ALTACLI-SQLCODE
                GO                           TO FIN-PROGRAMA
           END-EVALUATE.

       FIN-INSERTAR-TARJETA.
           EXIT.

       BUSCAR-MOVIMIENTO.
      *------------------
           INITIALIZE                        AREA-BUSQMOV-ENTRADA.

           MOVE ALTACLI-MOV-FEC(WK-I)        TO BUSQMOV-E-FEC.
           MOVE WK-ID-MEDIO                  TO BUSQMOV-E-ID.

           CALL "BUSQMOV"                    USING AREA-BUSQMOV.

           EVALUATE TRUE
           WHEN BUSQMOV-STAT-OK
                SET ALTACLI-STAT-ERR-MOV-ENC TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN BUSQMOV-STAT-ENC-NO
                CONTINUE
           WHEN BUSQMOV-STAT-ERR-FEC
                SET ALTACLI-STAT-ERR-MOV-FEC TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN OTHER
                SET ALTACLI-STAT-ERR-SQL     TO TRUE
                MOVE BUSQMOV-SQLCODE         TO ALTACLI-SQLCODE
                GO                           TO FIN-PROGRAMA
           END-EVALUATE.

       FIN-BUSCAR-MOVIMIENTO.
           EXIT.

       INSERTAR-MOVIMIENTO.
      *--------------------
           INITIALIZE                       AREA-INSMOV-ENTRADA.

           MOVE WK-ID-MEDIO                 TO INSMOV-E-ID.
           MOVE ALTACLI-MOV-FEC(WK-I)       TO INSMOV-E-FEC.
           MOVE ALTACLI-MOV-CPT(WK-I)       TO INSMOV-E-CPT.
           MOVE ALTACLI-MOV-IMPORTE(WK-I)   TO INSMOV-E-IMPT.

           CALL "INSMOV"                    USING AREA-INSMOV.

           EVALUATE TRUE
           WHEN INSMOV-STAT-OK
                CONTINUE
           WHEN INSMOV-STAT-ERR-CPT
                SET ALTACLI-STAT-ERR-MOV-CPT TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN INSMOV-STAT-ERR-FEC
                SET ALTACLI-STAT-ERR-MOV-FEC TO TRUE
                GO                           TO FIN-PROGRAMA
           WHEN OTHER
                SET ALTACLI-STAT-ERR-SQL     TO TRUE
                MOVE INSMOV-S-SQLCODE        TO ALTACLI-SQLCODE
                GO                           TO FIN-PROGRAMA
           END-EVALUATE.

       FIN-INSERTAR-MOVIMIENTO.
           EXIT.

       HACER-COMMIT.
      *-------------
           EXEC SQL
                commit
           END-EXEC

           IF NOT SQL-SUCCESS
           THEN SET ALTACLI-STAT-ERR-SQL     TO TRUE
                MOVE SQLCODE                 TO ALTACLI-SQLCODE
           END-IF.

       FIN-HACER-COMMIT.
           EXIT.

       HACER-ROLLBACK.
      *---------------
           EXEC SQL
                rollback
           END-EXEC.

           IF NOT SQL-SUCCESS
           THEN SET ALTACLI-STAT-ERR-SQL     TO TRUE
                MOVE SQLCODE                 TO ALTACLI-SQLCODE
           END-IF.

       FIN-HACER-ROLLBACK.
           EXIT. 
