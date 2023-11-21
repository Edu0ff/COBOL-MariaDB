       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. INSMOV.
       AUTHOR. MARÍA EDUARDA ALMEIDA LOIOLA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *------------------------
           EXEC SQL BEGIN   DECLARE    SECTION END-EXEC.
           EXEC SQL INCLUDE movimiento         END-EXEC.
           EXEC SQL END     DECLARE    SECTION END-EXEC.

       LINKAGE SECTION.
      *----------------
           COPY 'insmov.cpy'.
      ******************************************************************
       PROCEDURE DIVISION USING AREA-INSMOV.
      ******************************************************************
           PERFORM INICIALIZAR              THRU FIN-INICIALIZAR.

           EVALUATE TRUE
           WHEN INSMOV-E-ID = ZERO
                OR INSMOV-E-ID IS NOT NUMERIC
                SET INSMOV-STAT-ERR-ID      TO TRUE

           WHEN INSMOV-E-FEC = ZERO
                OR INSMOV-E-FEC IS NOT NUMERIC
                SET INSMOV-STAT-ERR-FEC     TO TRUE

           WHEN INSMOV-E-CPT = ALL SPACES
                SET INSMOV-STAT-ERR-CPT     TO TRUE

           WHEN OTHER
                PERFORM INS-MOV             THRU FIN-INS-MOV
           END-EVALUATE.
       
           EXIT PROGRAM.

       INICIALIZAR.
      *------------
           INITIALIZE                       AREA-INSMOV-SALIDA
                                            REG-MOVIMIENTO
                                            REG-MOVIMIENTO-NULL.
       FIN-INICIALIZAR.
           EXIT.

       INS-MOV.
      *--------
           MOVE INSMOV-E-ID                 TO MOVIMIENTO-ID.
           MOVE INSMOV-E-FEC                TO MOVIMIENTO-FEC.
           MOVE INSMOV-E-CPT                TO MOVIMIENTO-CPT.
           MOVE INSMOV-E-IMPT               TO MOVIMIENTO-IMPT.

           EXEC SQL
                INSERT INTO banco.movimiento(
                    id_medio,
                    fec_mov,
                    cnpt_mov,
                    importe_mov
                )
                VALUES(
                    :MOVIMIENTO-ID          :MOVIMIENTO-ID-NULL,
              str_to_date(:MOVIMIENTO-FEC-X :MOVIMIENTO-FEC-NULL, 
                                               '%Y%m%d%H%i%s%f'),
                    :MOVIMIENTO-CPT         :MOVIMIENTO-CPT-NULL,
                    :MOVIMIENTO-IMPT        :MOVIMIENTO-IMPT-NULL
                )
           END-EXEC.

           MOVE SQLCODE                     TO INSMOV-S-SQLCODE

           IF   NOT SQL-SUCCESS
           THEN SET INSMOV-STAT-ERR-SQL     TO TRUE
                DISPLAY '*** FATAL *** INSMOV INS-MOV: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-IF.
       FIN-INS-MOV.
           EXIT.
       