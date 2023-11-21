       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. BUSQMOV.
       AUTHOR. MARÍA EDUARDA ALMEIDA LOIOLA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *------------------------
           EXEC SQL BEGIN DECLARE SECTION  END-EXEC.
           EXEC SQL INCLUDE movimiento     END-EXEC.
           EXEC SQL END DECLARE SECTION    END-EXEC.

       LINKAGE SECTION.
      *---------------- 
           COPY 'busqmov.cpy'.

      ******************************************************************
       PROCEDURE DIVISION USING AREA-BUSQMOV.
      ******************************************************************
           PERFORM INICIALIZAR           THRU FIN-INICIALIZAR.

           PERFORM VALIDAR-ID            THRU FIN-VALIDAR-ID.
           PERFORM VALIDAR-FECHA         THRU FIN-VALIDAR-FECHA.

           IF   BUSQMOV-STAT-OK
           THEN PERFORM BUSCAR-ID-FEC    THRU FIN-BUSCAR-ID-FEC
           END-IF.

           EXIT PROGRAM.

       INICIALIZAR.
      *------------
           INITIALIZE                    AREA-BUSQMOV-SALIDA
                                         REG-MOVIMIENTO
                                         REG-MOVIMIENTO-NULL.
           
       FIN-INICIALIZAR.
           EXIT.

       BUSCAR-ID-FEC.
      *--------------
           MOVE BUSQMOV-E-ID             TO MOVIMIENTO-ID.
           MOVE BUSQMOV-E-FEC            TO MOVIMIENTO-FEC.

           EXEC SQL 
                SELECT
                   id_medio,
                   date_format(fec_mov,'%Y%m%d%H%i%s%f'),
                   cnpt_mov,
                   importe_mov
                INTO 
                   :MOVIMIENTO-ID        :MOVIMIENTO-ID-NULL,
                   :MOVIMIENTO-FEC-X     :MOVIMIENTO-FEC-NULL,
                   :MOVIMIENTO-CPT       :MOVIMIENTO-CPT-NULL,
                   :MOVIMIENTO-IMPT      :MOVIMIENTO-IMPT-NULL
                FROM banco.movimiento
                WHERE id_medio =         :MOVIMIENTO-ID
                AND   fec_mov  =  
                      str_to_date(:MOVIMIENTO-FEC-X,'%Y%m%d%H%i%s%f')
           END-EXEC.

           MOVE SQLCODE                   TO BUSQMOV-SQLCODE.

           EVALUATE TRUE
           WHEN SQL-SUCCESS
                MOVE MOVIMIENTO-ID        TO BUSQMOV-S-ID
                MOVE MOVIMIENTO-FEC-X     TO BUSQMOV-S-FEC
                MOVE MOVIMIENTO-CPT       TO BUSQMOV-S-CPT
                MOVE MOVIMIENTO-IMPT      TO BUSQMOV-S-IMPT

           WHEN SQL-NODATA
                SET BUSQMOV-STAT-ENC-NO   TO TRUE

           WHEN OTHER
                SET BUSQMOV-STAT-ERR-SQL  TO TRUE
                DISPLAY '*** FATAL *** BUSQMOV BUSCAR-ID-FEC: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-EVALUATE.
       FIN-BUSCAR-ID-FEC.
           EXIT.

       VALIDAR-ID.
      *-----------
           IF BUSQMOV-E-ID = ZERO
           THEN SET BUSQMOV-STAT-ERR-ID   TO TRUE
           END-IF.
       FIN-VALIDAR-ID.
           EXIT.

       VALIDAR-FECHA.
      *--------------
           IF BUSQMOV-E-FEC IS NOT NUMERIC
           OR BUSQMOV-E-FEC = ZERO
           THEN SET BUSQMOV-STAT-ERR-FEC  TO TRUE
           END-IF.
       FIN-VALIDAR-FECHA.
           EXIT.