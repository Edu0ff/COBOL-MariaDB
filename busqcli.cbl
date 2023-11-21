       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. BUSQCLI.
       AUTHOR. MARÍA EDUARDA ALMEIDA LOIOLA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *------------------------
           EXEC SQL BEGIN DECLARE SECTION  END-EXEC.
      * Esto es un include, contiene mis host variables.
      * Incluir el resto de tablas (mis archivos cbl) para facilitar su 
      * reutilización a los programas que quieran acceder a mis tablas 
      * (cliente, domicilio. cuenta...).
           EXEC SQL INCLUDE cliente        END-EXEC.
           EXEC SQL END DECLARE SECTION    END-EXEC.

       LINKAGE SECTION.
      *---------------- 
      * Esto es una COPY. Contiene areas de interfaz de las rutinas.
           COPY 'busqcli.cpy'.

      ******************************************************************
       PROCEDURE DIVISION USING AREA-BUSQCLI.
      ******************************************************************
           PERFORM INICIALIZAR             THRU FIN-INICIALIZAR.

           EVALUATE TRUE
           WHEN BUSQCLI-CRIT-ID
                PERFORM VALIDAR-ID         THRU FIN-VALIDAR-ID
                IF BUSQCLI-STAT-OK
                THEN 
                   PERFORM BUSCAR-POR-ID   THRU FIN-BUSCAR-POR-ID
                END-IF
           
           WHEN BUSQCLI-CRIT-NIF
                PERFORM VALIDAR-NIF        THRU FIN-VALIDAR-NIF
                IF BUSQCLI-STAT-OK
                THEN
                   PERFORM BUSCAR-POR-NIF  THRU FIN-BUSCAR-POR-NIF
                END-IF
           
           WHEN OTHER
                SET BUSQCLI-STAT-ERR-CRIT  TO TRUE
           END-EVALUATE.

           EXIT PROGRAM.

       INICIALIZAR.
      *------------
           INITIALIZE AREA-BUSQCLI-SALIDA
                      REG-CLIENTE
                      REG-CLIENTE-NULL.
           
       FIN-INICIALIZAR.
           EXIT.

       BUSCAR-POR-ID.
      *--------------
           MOVE BUSQCLI-E-ID               TO CLIENTE-ID.

           EXEC SQL 
                SELECT
                   id_cliente,
                   nif_cliente,
                   nom_cliente,
                   cast(date_format(fec_nac_cliente, '%Y%m%d') as int),
                   id_dom
                INTO 
                   :CLIENTE-ID             :CLIENTE-ID-NULL,
                   :CLIENTE-NIF            :CLIENTE-NIF-NULL,
                   :CLIENTE-NOM            :CLIENTE-NOM-NULL,
                   :CLIENTE-FEC-NAC        :CLIENTE-FEC-NAC-NULL,
                   :CLIENTE-ID-DOM         :CLIENTE-ID-DOM-NULL
                FROM banco.cliente
                WHERE id_cliente =         :CLIENTE-ID    
           END-EXEC.

           MOVE SQLCODE                    TO BUSQCLI-SQLCODE.

           EVALUATE TRUE
           WHEN SQL-SUCCESS
                SET BUSQCLI-STAT-OK        TO TRUE
                MOVE CLIENTE-ID            TO BUSQCLI-S-ID
                MOVE CLIENTE-NIF           TO BUSQCLI-S-NIF
                MOVE CLIENTE-FEC-NAC       TO BUSQCLI-S-FEC-NAC
                MOVE CLIENTE-ID-DOM        TO BUSQCLI-S-ID-DOM

           WHEN SQL-NODATA
                SET BUSQCLI-STAT-ENC-NO    TO TRUE

           WHEN OTHER
                SET BUSQCLI-STAT-ERR-SQL   TO TRUE
                DISPLAY '*** FATAL *** BUSQCLI BUSCAR-POR-ID: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-EVALUATE.
       FIN-BUSCAR-POR-ID.
           EXIT.

       BUSCAR-POR-NIF.
      *---------------
           MOVE BUSQCLI-E-NIF              TO CLIENTE-NIF.

           EXEC SQL 
                SELECT
                   id_cliente,
                   nif_cliente,
                   nom_cliente,
                   cast(date_format(fec_nac_cliente, '%Y%m%d') as int),
                   id_dom
                INTO 
                   :CLIENTE-ID      :CLIENTE-ID-NULL,
                   :CLIENTE-NIF     :CLIENTE-NIF-NULL,
                   :CLIENTE-NOM     :CLIENTE-NOM-NULL,
                   :CLIENTE-FEC-NAC :CLIENTE-FEC-NAC-NULL,
                   :CLIENTE-ID-DOM  :CLIENTE-ID-DOM-NULL
                FROM banco.cliente
                WHERE nif_cliente = :CLIENTE-NIF         
           END-EXEC.

           MOVE SQLCODE                    TO BUSQCLI-SQLCODE.

           EVALUATE TRUE
           WHEN SQL-SUCCESS
                SET BUSQCLI-STAT-OK        TO TRUE
                MOVE CLIENTE-ID            TO BUSQCLI-S-ID
                MOVE CLIENTE-NIF           TO BUSQCLI-S-NIF
                MOVE CLIENTE-FEC-NAC       TO BUSQCLI-S-FEC-NAC
                MOVE CLIENTE-ID-DOM        TO BUSQCLI-S-ID-DOM

           WHEN SQL-NODATA
                SET BUSQCLI-STAT-ENC-NO    TO TRUE

           WHEN OTHER
                SET BUSQCLI-STAT-ERR-SQL   TO TRUE
                DISPLAY '*** FATAL *** BUSQCLI BUSCAR-POR-NIF: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-EVALUATE.
       FIN-BUSCAR-POR-NIF.
           EXIT.

       VALIDAR-ID.
      *-----------
           IF   BUSQCLI-E-ID-X IS NOT NUMERIC
           THEN SET BUSQCLI-STAT-ERR-ID    TO TRUE
           END-IF.
       FIN-VALIDAR-ID.
           EXIT.

       VALIDAR-NIF.
      *------------
           IF   BUSQCLI-E-NIF = ALL SPACES
           THEN SET BUSQCLI-STAT-ERR-NIF   TO TRUE
           END-IF.
       FIN-VALIDAR-NIF.
           EXIT.
