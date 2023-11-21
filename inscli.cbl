       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. INSCLI.
       AUTHOR. MARÍA EDUARDA ALMEIDA LOIOLA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *------------------------
           EXEC SQL BEGIN   DECLARE   SECTION END-EXEC.
           EXEC SQL INCLUDE cliente           END-EXEC.
           EXEC SQL END     DECLARE   SECTION END-EXEC.
           
       LINKAGE SECTION.
      *----------------
           COPY 'inscli.cpy'.
      ******************************************************************
       PROCEDURE DIVISION USING AREA-INSCLI.
      ******************************************************************
           PERFORM INICIALIZAR              THRU FIN-INICIALIZAR.

           EVALUATE TRUE
           WHEN INSCLI-E-NIF = ALL SPACES
                SET INSCLI-STAT-ERR-NIF     TO TRUE

           WHEN INSCLI-E-NOM = ALL SPACES
                SET INSCLI-STAT-ERR-NOM     TO TRUE

           WHEN INSCLI-E-FEC-NAC-X IS NOT NUMERIC OR
                INSCLI-E-FEC-NAC = ALL ZEROES
                SET INSCLI-STAT-ERR-FEC-NAC TO TRUE

           WHEN (INSCLI-E-ID-DOM-X NOT = ALL SPACES) AND
                (INSCLI-E-ID-DOM-X IS NOT NUMERIC)
                SET INSCLI-STAT-ERR-DOM     TO TRUE
           
           WHEN OTHER
                PERFORM GEN-CLI-ID          THRU FIN-GEN-CLI-ID

                IF   INSCLI-STAT-OK
                THEN PERFORM INS-CLI        THRU FIN-INS-CLI
                END-IF
           END-EVALUATE.
       
           EXIT PROGRAM.

       INICIALIZAR.
      *------------
           INITIALIZE                       AREA-INSCLI-SALIDA
                                            REG-CLIENTE
                                            REG-CLIENTE-NULL.
       FIN-INICIALIZAR.
           EXIT.

       GEN-CLI-ID.
      *-----------
           EXEC SQL
                SELECT NEXT VALUE FOR banco.cliente_id_cliente_seq
                INTO :CLIENTE-ID            :CLIENTE-ID-NULL
           END-EXEC.

           MOVE SQLCODE                     TO INSCLI-S-SQLCODE.

           IF   NOT INSCLI-STAT-OK
           THEN SET INSCLI-STAT-ERR-SQL     TO TRUE
                DISPLAY '*** FATAL *** INSCLI GEN-CLI-ID: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-IF.
       FIN-GEN-CLI-ID.
           EXIT.

       INS-CLI.
      *--------
           MOVE INSCLI-E-NIF                TO CLIENTE-NIF.
           MOVE INSCLI-E-NOM                TO CLIENTE-NOM.
           MOVE INSCLI-E-FEC-NAC-X          TO CLIENTE-FEC-NAC.
       
           IF   INSCLI-E-ID-DOM > ZERO
           THEN MOVE INSCLI-E-ID-DOM        TO CLIENTE-ID-DOM
           ELSE MOVE -1                     TO CLIENTE-ID-DOM-NULL
           END-IF.

           EXEC SQL
                INSERT INTO banco.cliente(
                    id_cliente,
                    nif_cliente,
                    nom_cliente,
                    fec_nac_cliente,
                    id_dom
                )
                VALUES(
                    :CLIENTE-ID             :CLIENTE-ID-NULL,
                    :CLIENTE-NIF            :CLIENTE-NIF-NULL,
                    :CLIENTE-NOM            :CLIENTE-NOM-NULL,
                    str_to_date(
                    :CLIENTE-FEC-NAC        :CLIENTE-FEC-NAC-NULL,
                    '%Y%m%d'),
                    :CLIENTE-ID-DOM         :CLIENTE-ID-DOM-NULL
                )
           END-EXEC.

           IF SQL-SUCCESS
           THEN MOVE CLIENTE-ID              TO INSCLI-S-CLI-ID
           ELSE SET  INSCLI-STAT-ERR-SQL     TO TRUE
                MOVE SQLCODE                 TO INSCLI-S-SQLCODE
                DISPLAY '*** FATAL *** INSCLI INS-CLI: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-IF.

       FIN-INS-CLI.
           EXIT.
       