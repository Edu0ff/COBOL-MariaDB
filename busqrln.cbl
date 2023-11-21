       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. BUSQRLN.
       AUTHOR. MARÍA EDUARDA ALMEIDA LOIOLA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *------------------------
           EXEC SQL BEGIN DECLARE SECTION  END-EXEC.
           EXEC SQL INCLUDE relacion       END-EXEC.
           EXEC SQL END DECLARE SECTION    END-EXEC.

       LINKAGE SECTION.
      *---------------- 
           COPY 'busqrln.cpy'.

      ******************************************************************
       PROCEDURE DIVISION USING AREA-BUSQRLN.
      ******************************************************************
           PERFORM INICIALIZAR              THRU FIN-INICIALIZAR.

           PERFORM VALIDAR-ID               THRU FIN-VALIDAR-ID.

           IF   BUSQRLN-STAT-OK
           THEN PERFORM BUSCAR-POR-ID       THRU FIN-BUSCAR-POR-ID
           END-IF.

           EXIT PROGRAM.

       INICIALIZAR.
      *------------
           INITIALIZE                       AREA-BUSQRLN-SALIDA
                                            REG-RELACION
                                            REG-RELACION-NULL.
           
       FIN-INICIALIZAR.
           EXIT.

       BUSCAR-POR-ID.
      *--------------
           MOVE BUSQRLN-E-CLI-ID            TO RELACION-CLI-ID.
           MOVE BUSQRLN-E-CTA-ID            TO RELACION-CTA-ID.

           EXEC SQL 
                SELECT
                   id_cliente,
                   id_medio,
                   tip_rln
                INTO 
                   :RELACION-CLI-ID         :RELACION-CLI-ID-NULL,
                   :RELACION-CTA-ID         :RELACION-CTA-ID-NULL,
                   :RELACION-RLN            :RELACION-RLN-NULL
                FROM banco.cliente_rln_cuenta
                WHERE id_cliente =          :RELACION-CLI-ID
                AND   id_medio   =          :RELACION-CTA-ID    
           END-EXEC.

           MOVE SQLCODE                     TO BUSQRLN-SQLCODE.

           EVALUATE TRUE
           WHEN SQL-SUCCESS
                SET BUSQRLN-STAT-OK         TO TRUE
                MOVE RELACION-RLN           TO BUSQRLN-RELACION

           WHEN SQL-NODATA
                SET BUSQRLN-STAT-ENC-NO     TO TRUE

           WHEN OTHER
                SET BUSQRLN-STAT-ERR-SQL    TO TRUE
                DISPLAY '*** FATAL *** BUSQRLN BUSCAR-POR-ID: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-EVALUATE.
       FIN-BUSCAR-POR-ID.
           EXIT.

       VALIDAR-ID.
      *-----------
           EVALUATE TRUE
           WHEN BUSQRLN-E-CLI-ID-X IS NOT NUMERIC
                SET BUSQRLN-STAT-ERR-CLI-ID TO TRUE
           
           WHEN BUSQRLN-E-CTA-ID-X IS NOT NUMERIC
                SET BUSQRLN-STAT-ERR-CTA-ID TO TRUE
           WHEN OTHER
                CONTINUE
           END-EVALUATE.
       FIN-VALIDAR-ID.
           EXIT.
