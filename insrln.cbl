       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. INSRLN.
       AUTHOR. MARÍA EDUARDA ALMEIDA LOIOLA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *------------------------
           EXEC SQL BEGIN   DECLARE   SECTION END-EXEC.
           EXEC SQL INCLUDE relacion          END-EXEC.
           EXEC SQL END     DECLARE   SECTION END-EXEC.

       LINKAGE SECTION.
      *----------------
           COPY 'insrln.cpy'.
      ******************************************************************
       PROCEDURE DIVISION USING AREA-INSRLN.
      ******************************************************************
           PERFORM INICIALIZAR              THRU FIN-INICIALIZAR.

           EVALUATE TRUE
           WHEN INSRLN-E-CLI-ID = ZERO
                OR INSRLN-E-CLI-ID IS NOT NUMERIC
                SET INSRLN-STAT-ERR-CLI-ID  TO TRUE

           WHEN INSRLN-E-CTA-ID = ZERO
                OR INSRLN-E-CTA-ID IS NOT NUMERIC
                SET INSRLN-STAT-ERR-CTA-ID  TO TRUE
           
           WHEN NOT (INSRLN-E-TIT OR INSRLN-E-COTIT OR INSRLN-E-AUT)
                SET INSRLN-STAT-ERR-RLN     TO TRUE
           
           WHEN OTHER
                PERFORM INS-RLN             THRU FIN-INS-RLN
           END-EVALUATE.
       
           EXIT PROGRAM.

       INICIALIZAR.
      *------------
           INITIALIZE                       AREA-INSRLN-SALIDA
                                            REG-RELACION
                                            REG-RELACION-NULL.
       FIN-INICIALIZAR.
           EXIT.

       INS-RLN.
      *----------
           MOVE INSRLN-E-CLI-ID             TO RELACION-CLI-ID.
           MOVE INSRLN-E-CTA-ID             TO RELACION-CTA-ID.
           MOVE INSRLN-E-RLN                TO RELACION-RLN.

           EXEC SQL
                INSERT INTO banco.cliente_rln_cuenta(
                    id_cliente,
                    id_medio,
                    tip_rln
                )
                VALUES(
                    :RELACION-CLI-ID        :RELACION-CTA-ID-NULL,
                    :RELACION-CTA-ID        :RELACION-CLI-ID-NULL,
                    :RELACION-RLN           :RELACION-RLN-NULL
                )
           END-EXEC.

           MOVE SQLCODE                     TO INSRLN-SQLCODE.

           IF   NOT SQL-SUCCESS
           THEN SET INSRLN-STAT-ERR-SQL     TO TRUE
                DISPLAY '*** FATAL *** INSRLN INS-RLN: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-IF.

       FIN-INS-RLN.
           EXIT.
       