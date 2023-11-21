       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. INSTAR.
       AUTHOR. MARÍA EDUARDA ALMEIDA LOIOLA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *------------------------
           EXEC SQL BEGIN   DECLARE   SECTION END-EXEC.
           EXEC SQL INCLUDE tarjeta           END-EXEC.
           EXEC SQL END     DECLARE   SECTION END-EXEC.

       01  WK-FEC-AUX-MES-ANO.
           05  WK-FEC-AUX-MES               PIC 9(02).
           05  WK-FEC-AUX-ANO               PIC 9(02).

       01  WK-FEC-AUX-ANO-MES.
           05  WK-FEC-AUX-AA                PIC 9(02).
           05  WK-FEC-AUX-MM                PIC 9(02).
           05  FILLER                       PIC 9(02) VALUE 01.

       LINKAGE SECTION.
      *----------------
           COPY 'instar.cpy'.
           
      ******************************************************************
       PROCEDURE DIVISION USING AREA-INSTAR.
      ******************************************************************
           PERFORM INICIALIZAR              THRU FIN-INICIALIZAR.

           EVALUATE TRUE
           WHEN INSTAR-E-ID-CLI = ZERO
                OR INSTAR-E-ID-CLI IS NOT NUMERIC
                SET INSTAR-STAT-ERR-CLI-ID  TO TRUE
            
           WHEN INSTAR-E-ID-CTA = ZERO
                OR INSTAR-E-ID-CTA IS NOT NUMERIC
                SET INSTAR-STAT-ERR-CTA-ID  TO TRUE

           WHEN INSTAR-E-NUM IS NOT NUMERIC
                SET INSTAR-STAT-ERR-NUM     TO TRUE

           WHEN INSTAR-E-FEC IS NOT NUMERIC
                SET INSTAR-STAT-ERR-FEC     TO TRUE
            
           WHEN INSTAR-E-CCV IS NOT NUMERIC
                SET INSTAR-STAT-ERR-CCV     TO TRUE

           WHEN OTHER
                PERFORM GEN-ID-TAR          THRU FIN-GEN-ID-TAR

                IF   INSTAR-STAT-OK
                THEN PERFORM INS-MEDIO      THRU FIN-INS-MEDIO
                     IF   INSTAR-STAT-OK
                     THEN PERFORM INS-TAR   THRU FIN-INS-TAR
                     END-IF
                END-IF
           END-EVALUATE.
       
           EXIT PROGRAM.

       INICIALIZAR.
      *------------
           INITIALIZE                       AREA-INSTAR-SALIDA
                                            REG-TARJETA
                                            REG-TARJETA-NULL.
       FIN-INICIALIZAR.
           EXIT.

       GEN-ID-TAR.
      *-----------
           EXEC SQL
                SELECT NEXT VALUE FOR banco.medio_id_medio_seq
                INTO :TARJETA-ID-TAR        :TARJETA-ID-TAR-NULL
           END-EXEC.

           MOVE SQLCODE                     TO INSTAR-S-SQLCODE.

           IF   NOT INSTAR-STAT-OK
           THEN SET INSTAR-STAT-ERR-SQL     TO TRUE
                DISPLAY '*** FATAL *** INSTAR GEN-ID-TAR: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-IF.
       FIN-GEN-ID-TAR.
           EXIT.

       INS-MEDIO.
      *----------
           EXEC SQL
                INSERT INTO banco.medio(
                    id_medio,
                    tip_med                
                )
                VALUES(
                    :TARJETA-ID-TAR          :TARJETA-ID-TAR-NULL,
                    'T'
                )
           END-EXEC.

           MOVE SQLCODE                      TO INSTAR-S-SQLCODE.

           IF   NOT INSTAR-STAT-OK
           THEN SET INSTAR-STAT-ERR-SQL      TO TRUE
                DISPLAY '*** FATAL *** INSTAR INS-MEDIO: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-IF.
       FIN-INS-MEDIO.
           EXIT.

       INS-TAR.
      *--------
           MOVE INSTAR-E-ID-CLI              TO TARJETA-ID-CLI.
           MOVE INSTAR-E-ID-CTA              TO TARJETA-ID-CTA.
           MOVE INSTAR-E-NUM                 TO TARJETA-NUM.
           MOVE INSTAR-E-CRED                TO TARJETA-CRED.
           MOVE INSTAR-E-CCV                 TO TARJETA-CCV.

           MOVE INSTAR-E-FEC                 TO WK-FEC-AUX-MES-ANO.
           MOVE WK-FEC-AUX-MES               TO WK-FEC-AUX-MM.
           MOVE WK-FEC-AUX-ANO               TO WK-FEC-AUX-AA.
           MOVE WK-FEC-AUX-ANO-MES           TO TARJETA-FEC.

           EXEC SQL
                INSERT INTO banco.tarjeta(
                    id_medio,
                    id_cliente,
                    id_medio_cta,
                    num_tarjeta,
                    cred_tarjeta,
                    fec_tarjeta,
                    ccv_tarjeta
                )
                VALUES(
                    :TARJETA-ID-TAR           :TARJETA-ID-TAR-NULL,
                    :TARJETA-ID-CLI           :TARJETA-ID-CLI-NULL,
                    :TARJETA-ID-CTA           :TARJETA-ID-CTA-NULL,
                    :TARJETA-NUM              :TARJETA-NUM-NULL,
                    :TARJETA-CRED             :TARJETA-CRED-NULL,
              str_to_date(:TARJETA-FEC-X :TARJETA-FEC-NULL, '%y%d%m'),
                    :TARJETA-CCV              :TARJETA-CCV-NULL
                )
           END-EXEC.

           MOVE SQLCODE                       TO INSTAR-S-SQLCODE.
           
           IF   SQL-SUCCESS
           THEN MOVE TARJETA-ID-TAR           TO INSTAR-S-TAR-ID
           ELSE SET  INSTAR-STAT-ERR-SQL      TO TRUE
                DISPLAY '*** FATAL *** INSTAR INS-TAR: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-IF.

       FIN-INS-TAR.
           EXIT.

       