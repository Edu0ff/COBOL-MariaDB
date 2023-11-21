       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. BUSQTAR.
       AUTHOR. MARÍA EDUARDA ALMEIDA LOIOLA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *------------------------
           EXEC SQL BEGIN DECLARE SECTION  END-EXEC.
           EXEC SQL INCLUDE tarjeta        END-EXEC.
           EXEC SQL END DECLARE SECTION    END-EXEC.

       01  WK-FEC-AUX.
           05  FILLER                      PIC 9(02).
           05  WK-FEC-AUX-MES-ANO          PIC 9(04).

       LINKAGE SECTION.
      *----------------
           COPY 'busqtar.cpy'.

      ******************************************************************
       PROCEDURE DIVISION USING AREA-BUSQTAR.
      ******************************************************************
           PERFORM INICIALIZAR             THRU FIN-INICIALIZAR.

           EVALUATE TRUE
           WHEN BUSQTAR-CRIT-ID
                PERFORM VALIDAR-ID         THRU FIN-VALIDAR-ID
                IF BUSQTAR-STAT-OK
                THEN 
                   PERFORM BUSCAR-POR-ID   THRU FIN-BUSCAR-POR-ID
                END-IF
           
           WHEN BUSQTAR-CRIT-NUM
                PERFORM VALIDAR-NUM-TAR    THRU FIN-VALIDAR-NUM-TAR
                IF BUSQTAR-STAT-OK
                THEN
                   PERFORM BUSCAR-POR-NUM-TAR  
                   THRU FIN-BUSCAR-POR-NUM-TAR
                END-IF

           WHEN BUSQTAR-CRIT-CC
                PERFORM VALIDAR-CLI-CTA    THRU FIN-VALIDAR-CLI-CTA
                IF BUSQTAR-STAT-OK
                THEN
                   PERFORM BUSCAR-POR-CC  
                   THRU FIN-BUSCAR-POR-CC
                END-IF
           
           WHEN OTHER
                SET BUSQTAR-STAT-ERR-CRIT  TO TRUE
           END-EVALUATE.

           EXIT PROGRAM.

       INICIALIZAR.
      *------------
           INITIALIZE AREA-BUSQTAR-SALIDA
                      REG-TARJETA
                      REG-TARJETA-NULL.
           
       FIN-INICIALIZAR.
           EXIT.

       BUSCAR-POR-ID.
      *--------------
           MOVE BUSQTAR-E-ID             TO TARJETA-ID-TAR.

           EXEC SQL 
             SELECT
                   id_medio,
                   id_medio_cta,
                   id_cliente,
                   num_tarjeta,
                   cred_tarjeta,
                   date_format(fec_tarjeta, '%d%m%y'),
                   ccv_tarjeta
             INTO 
                   :TARJETA-ID-TAR       :TARJETA-ID-TAR-NULL,
                   :TARJETA-ID-CTA       :TARJETA-ID-CTA-NULL,
                   :TARJETA-ID-CLI       :TARJETA-ID-CLI-NULL,
                   :TARJETA-NUM          :TARJETA-NUM-NULL,
                   :TARJETA-CRED         :TARJETA-CRED-NULL,
                   :TARJETA-FEC          :TARJETA-FEC-NULL,
                   :TARJETA-CCV          :TARJETA-CCV-NULL
             FROM  banco.tarjeta
             WHERE id_medio =            :TARJETA-ID-TAR
           END-EXEC.

           MOVE SQLCODE                  TO BUSQTAR-SQLCODE.

           EVALUATE TRUE
           WHEN SQL-SUCCESS
                MOVE TARJETA-ID-TAR      TO BUSQTAR-S-ID-TAR
                MOVE TARJETA-ID-CTA      TO BUSQTAR-S-ID-CTA
                MOVE TARJETA-ID-CLI      TO BUSQTAR-S-ID-CLI
                MOVE TARJETA-NUM         TO BUSQTAR-S-NUM
                MOVE TARJETA-CRED        TO BUSQTAR-S-CRED   
                MOVE TARJETA-CCV         TO BUSQTAR-S-CCV

                MOVE TARJETA-FEC         TO WK-FEC-AUX
                MOVE WK-FEC-AUX-MES-ANO  TO BUSQTAR-S-FEC
            
           WHEN SQL-NODATA
                SET BUSQTAR-STAT-ENC-NO  TO TRUE

           WHEN OTHER
                SET BUSQTAR-STAT-ERR-SQL TO TRUE
                DISPLAY '*** FATAL *** BUSQTAR BUSCAR-POR-ID: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-EVALUATE.
       FIN-BUSCAR-POR-ID.
           EXIT.

       BUSCAR-POR-NUM-TAR.
      *-------------------
           MOVE BUSQTAR-E-NUM            TO TARJETA-NUM.

           EXEC SQL 
             SELECT
                   id_medio,
                   id_medio_cta,
                   id_cliente,
                   num_tarjeta,
                   cred_tarjeta,
                   date_format(fec_tarjeta, '%d%m%y'),
                   ccv_tarjeta
             INTO 
                   :TARJETA-ID-TAR       :TARJETA-ID-TAR-NULL,
                   :TARJETA-ID-CTA       :TARJETA-ID-CTA-NULL,
                   :TARJETA-ID-CLI       :TARJETA-ID-CLI-NULL,
                   :TARJETA-NUM          :TARJETA-NUM-NULL,
                   :TARJETA-CRED         :TARJETA-CRED-NULL,
                   :TARJETA-FEC          :TARJETA-FEC-NULL,
                   :TARJETA-CCV          :TARJETA-CCV-NULL
             FROM  banco.tarjeta
             WHERE num_tarjeta =         :TARJETA-NUM
           END-EXEC.

           MOVE SQLCODE                  TO BUSQTAR-SQLCODE.

           EVALUATE TRUE
           WHEN SQL-SUCCESS
                MOVE TARJETA-ID-TAR      TO BUSQTAR-S-ID-TAR
                MOVE TARJETA-ID-CTA      TO BUSQTAR-S-ID-CTA
                MOVE TARJETA-ID-CLI      TO BUSQTAR-S-ID-CLI
                MOVE TARJETA-NUM         TO BUSQTAR-S-NUM
                MOVE TARJETA-CRED        TO BUSQTAR-S-CRED   
                MOVE TARJETA-CCV         TO BUSQTAR-S-CCV

                MOVE TARJETA-FEC         TO WK-FEC-AUX
                MOVE WK-FEC-AUX-MES-ANO  TO BUSQTAR-S-FEC
            
           WHEN SQL-NODATA
                SET BUSQTAR-STAT-ENC-NO  TO TRUE

           WHEN OTHER
                SET BUSQTAR-STAT-ERR-SQL TO TRUE
                DISPLAY '*** FATAL *** BUSQTAR BUSCAR-POR-NUM-TAR: '
                        'ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-EVALUATE.
       FIN-BUSCAR-POR-NUM-TAR.
           EXIT.

       BUSCAR-POR-CC.
      *--------------
           MOVE BUSQTAR-E-ID-CLI         TO TARJETA-ID-CLI.
           MOVE BUSQTAR-E-ID-CTA         TO TARJETA-ID-CTA.

           EXEC SQL 
             SELECT
                   id_medio,
                   id_medio_cta,
                   id_cliente,
                   num_tarjeta,
                   cred_tarjeta,
                   date_format(fec_tarjeta, '%d%m%y'),
                   ccv_tarjeta
             INTO 
                   :TARJETA-ID-TAR       :TARJETA-ID-TAR-NULL,
                   :TARJETA-ID-CTA       :TARJETA-ID-CTA-NULL,
                   :TARJETA-ID-CLI       :TARJETA-ID-CLI-NULL,
                   :TARJETA-NUM          :TARJETA-NUM-NULL,
                   :TARJETA-CRED         :TARJETA-CRED-NULL,
                   :TARJETA-FEC          :TARJETA-FEC-NULL,
                   :TARJETA-CCV          :TARJETA-CCV-NULL
             FROM  banco.tarjeta
             WHERE id_medio_cta =        :TARJETA-ID-CTA
             AND   id_cliente   =        :TARJETA-ID-CLI
           END-EXEC.

           MOVE SQLCODE                  TO BUSQTAR-SQLCODE.

           EVALUATE TRUE
           WHEN SQL-SUCCESS
                MOVE TARJETA-ID-TAR      TO BUSQTAR-S-ID-TAR
                MOVE TARJETA-ID-CTA      TO BUSQTAR-S-ID-CTA
                MOVE TARJETA-ID-CLI      TO BUSQTAR-S-ID-CLI
                MOVE TARJETA-NUM         TO BUSQTAR-S-NUM
                MOVE TARJETA-CRED        TO BUSQTAR-S-CRED   
                MOVE TARJETA-CCV         TO BUSQTAR-S-CCV

                MOVE TARJETA-FEC         TO WK-FEC-AUX
                MOVE WK-FEC-AUX-MES-ANO  TO BUSQTAR-S-FEC
            
           WHEN SQL-NODATA
                SET BUSQTAR-STAT-ENC-NO  TO TRUE

           WHEN OTHER
                SET BUSQTAR-STAT-ERR-SQL TO TRUE
                DISPLAY '*** FATAL *** BUSQTAR BUSCAR-POR-CC: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-EVALUATE.
       FIN-BUSCAR-POR-CC.
           EXIT.

       VALIDAR-ID.
      *-----------
           IF   BUSQTAR-E-ID = ZERO 
                OR BUSQTAR-E-ID IS NOT NUMERIC
           THEN SET BUSQTAR-STAT-ERR-ID  TO TRUE
           END-IF.
       FIN-VALIDAR-ID.
           EXIT.

       VALIDAR-NUM-TAR.
      *----------------
           IF   BUSQTAR-E-NUM IS NOT NUMERIC
           THEN SET BUSQTAR-STAT-ERR-NUM TO TRUE
           END-IF.
       FIN-VALIDAR-NUM-TAR.
           EXIT.

       VALIDAR-CLI-CTA.
      *----------------  
           EVALUATE TRUE
           WHEN BUSQTAR-E-ID-CLI = ZERO
                OR BUSQTAR-E-ID-CLI IS NOT NUMERIC
                SET BUSQTAR-STAT-ERR-CC-CLI TO TRUE

           WHEN BUSQTAR-E-ID-CTA = ZERO
                OR BUSQTAR-E-ID-CTA IS NOT NUMERIC
                SET BUSQTAR-STAT-ERR-CC-CTA TO TRUE

           WHEN OTHER
                CONTINUE
           END-EVALUATE.
       FIN-VALIDAR-CLI-CTA.
           EXIT.
      