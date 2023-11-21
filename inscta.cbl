       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. INSCTA.
       AUTHOR. MARÍA EDUARDA ALMEIDA LOIOLA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *------------------------
           EXEC SQL BEGIN   DECLARE   SECTION END-EXEC.
           EXEC SQL INCLUDE cuenta            END-EXEC.
           EXEC SQL END     DECLARE   SECTION END-EXEC.

       LINKAGE SECTION.
      *----------------
           COPY 'inscta.cpy'.
           
      ******************************************************************
       PROCEDURE DIVISION USING AREA-INSCTA.
      ******************************************************************
           PERFORM INICIALIZAR              THRU FIN-INICIALIZAR.

           EVALUATE TRUE
           WHEN INSCTA-E-NUM IS NOT NUMERIC
                OR INSCTA-E-NUM = ZEROES
                SET INSCTA-STAT-ERR-NUM     TO TRUE

           WHEN OTHER
                PERFORM GEN-CTA-ID          THRU FIN-GEN-CTA-ID

                IF   INSCTA-STAT-OK
                THEN PERFORM INS-MEDIO      THRU FIN-INS-MEDIO
                     IF   INSCTA-STAT-OK
                     THEN PERFORM INS-CTA   THRU FIN-INS-CTA
                     END-IF
                END-IF
           END-EVALUATE.
       
           EXIT PROGRAM.

       INICIALIZAR.
      *------------
           INITIALIZE                       AREA-INSCTA-SALIDA
                                            REG-CUENTA
                                            REG-CUENTA-NULL.
       FIN-INICIALIZAR.
           EXIT.

       GEN-CTA-ID.
      *-----------
           EXEC SQL
                SELECT NEXT VALUE FOR banco.medio_id_medio_seq
                INTO :CUENTA-ID             :CUENTA-ID-NULL
           END-EXEC.

           MOVE SQLCODE                     TO INSCTA-S-SQLCODE.

           IF   NOT INSCTA-STAT-OK
           THEN SET INSCTA-STAT-ERR-SQL     TO TRUE
                DISPLAY '*** FATAL *** INSCTA GEN-CTA-ID: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-IF.
       FIN-GEN-CTA-ID.
           EXIT.

       INS-MEDIO.
      *----------
           EXEC SQL
                INSERT INTO banco.medio(
                    id_medio,
                    tip_med                
                )
                VALUES(
                    :CUENTA-ID               :CUENTA-ID-NULL,
                    'C'
                )
           END-EXEC.

           MOVE SQLCODE                      TO INSCTA-S-SQLCODE.

           IF NOT SQL-SUCCESS
           THEN SET INSCTA-STAT-ERR-SQL      TO TRUE
                DISPLAY '*** FATAL *** INSCTA INS-MEDIO: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-IF.
       FIN-INS-MEDIO.
           EXIT.

       INS-CTA.
      *----------
           MOVE INSCTA-E-NUM                 TO CUENTA-NUM.
           MOVE INSCTA-E-SALDO               TO CUENTA-SALDO.

           EXEC SQL
                INSERT INTO banco.cuenta(
                    id_medio,
                    num_cuenta,
                    saldo_cuenta                
                )
                VALUES(
                    :CUENTA-ID               :CUENTA-ID-NULL,
                    :CUENTA-NUM              :CUENTA-NUM-NULL,
                    :CUENTA-SALDO            :CUENTA-SALDO-NULL
                )
           END-EXEC.

           MOVE SQLCODE                      TO INSCTA-S-SQLCODE.

           IF   SQL-SUCCESS
           THEN MOVE CUENTA-ID               TO INSCTA-S-CTA-ID
           ELSE SET INSCTA-STAT-ERR-SQL      TO TRUE
                DISPLAY '*** FATAL *** INSCTA INS-CTA: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-IF.
       FIN-INS-CTA.
           EXIT.

       