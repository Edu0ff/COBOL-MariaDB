       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. BUSQCTA.
       AUTHOR. MARÍA EDUARDA ALMEIDA LOIOLA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *------------------------
           EXEC SQL BEGIN DECLARE SECTION  END-EXEC.  
           EXEC SQL INCLUDE cuenta         END-EXEC.
           EXEC SQL END DECLARE SECTION    END-EXEC.

       LINKAGE SECTION.
      *---------------- 
           COPY 'busqcta.cpy'.

      ******************************************************************
       PROCEDURE DIVISION USING AREA-BUSQCTA.
      ******************************************************************
           PERFORM INICIALIZAR            THRU FIN-INICIALIZAR.

           EVALUATE TRUE
           WHEN BUSQCTA-CRIT-ID
                PERFORM VALIDAR-ID        THRU FIN-VALIDAR-ID
                IF BUSQCTA-STAT-OK
                THEN 
                   PERFORM BUSCAR-POR-ID  THRU FIN-BUSCAR-POR-ID
                END-IF
           
           WHEN BUSQCTA-CRIT-NUM
                PERFORM VALIDAR-NUM-CTA   THRU FIN-VALIDAR-NUM-CTA
                IF BUSQCTA-STAT-OK
                THEN
                   PERFORM BUSCAR-POR-NUM-CTA  
                   THRU FIN-BUSCAR-POR-NUM-CTA
                END-IF
           
           WHEN OTHER
                SET BUSQCTA-STAT-ERR-CRIT TO TRUE
           END-EVALUATE.

           EXIT PROGRAM.

       INICIALIZAR.
      *------------
           INITIALIZE AREA-BUSQCTA-SALIDA
                      REG-CUENTA
                      REG-CUENTA-NULL.
           
       FIN-INICIALIZAR.
           EXIT.

       BUSCAR-POR-ID.
      *--------------
           MOVE BUSQCTA-E-ID              TO CUENTA-ID.

           EXEC SQL 
             SELECT
                   id_medio,
                   num_cuenta,
                   saldo_cuenta
             INTO 
                   :CUENTA-ID             :CUENTA-ID-NULL,
                   :CUENTA-NUM            :CUENTA-NUM-NULL,
                   :CUENTA-SALDO          :CUENTA-SALDO-NULL
             FROM  banco.cuenta
             WHERE id_medio =             :CUENTA-ID
           END-EXEC.

           MOVE SQLCODE                   TO BUSQCTA-SQLCODE

           EVALUATE TRUE
           WHEN SQL-SUCCESS
                MOVE CUENTA-ID            TO BUSQCTA-S-ID
                MOVE CUENTA-NUM           TO BUSQCTA-S-NUM
                MOVE CUENTA-SALDO         TO BUSQCTA-S-SALDO

           WHEN SQL-NODATA
                SET BUSQCTA-STAT-ENC-NO   TO TRUE

           WHEN OTHER
                SET BUSQCTA-STAT-ERR-SQL  TO TRUE
                DISPLAY '*** FATAL *** BUSQCTA BUSCAR-POR-ID: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-EVALUATE.
       FIN-BUSCAR-POR-ID.
           EXIT.

       BUSCAR-POR-NUM-CTA.
      *-------------------
           MOVE BUSQCTA-E-NUM             TO CUENTA-NUM.

           EXEC SQL
             SELECT
                   id_medio,
                   num_cuenta,
                   saldo_cuenta
             INTO 
                   :CUENTA-ID             :CUENTA-ID-NULL,
                   :CUENTA-NUM            :CUENTA-NUM-NULL,
                   :CUENTA-SALDO          :CUENTA-SALDO-NULL
             FROM  banco.cuenta
             WHERE num_cuenta =           :CUENTA-NUM

           END-EXEC.

           MOVE SQLCODE                   TO BUSQCTA-SQLCODE.

           EVALUATE TRUE
           WHEN SQL-SUCCESS
                MOVE CUENTA-ID            TO BUSQCTA-S-ID
                MOVE CUENTA-NUM           TO BUSQCTA-S-NUM
                MOVE CUENTA-SALDO         TO BUSQCTA-S-SALDO

           WHEN SQL-NODATA
                SET BUSQCTA-STAT-ENC-NO   TO TRUE

           WHEN OTHER
                SET BUSQCTA-STAT-ERR-SQL  TO TRUE
                DISPLAY '*** FATAL *** BUSQCTA BUSCAR-POR-NUM-CTA: ' 
                        'ERROR '
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-EVALUATE.
       FIN-BUSCAR-POR-NUM-CTA.
           EXIT.

       VALIDAR-ID.
      *-----------
           IF   BUSQCTA-E-ID-X IS NOT NUMERIC
           THEN SET BUSQCTA-STAT-ERR-ID   TO TRUE
           END-IF.
       FIN-VALIDAR-ID.
           EXIT.

       VALIDAR-NUM-CTA.
      *----------------
           IF   BUSQCTA-E-NUM-X IS NOT NUMERIC
           THEN SET BUSQCTA-STAT-ERR-NUM  TO TRUE
           END-IF.
       FIN-VALIDAR-NUM-CTA.
           EXIT.
