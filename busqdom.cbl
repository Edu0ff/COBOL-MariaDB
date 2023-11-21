       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. BUSQDOM.
       AUTHOR. MARÍA EDUARDA ALMEIDA LOIOLA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *------------------------
           EXEC SQL BEGIN DECLARE SECTION  END-EXEC.   
           EXEC SQL INCLUDE domicilio      END-EXEC.
           EXEC SQL END DECLARE SECTION    END-EXEC.
       
       01  CTE-NULL                        PIC X(01) VALUE '@'.
       
       LINKAGE SECTION.
      *---------------- 
           COPY 'busqdom.cpy'.

      ******************************************************************
       PROCEDURE DIVISION USING AREA-BUSQDOM.
      ******************************************************************
           PERFORM INICIALIZAR               THRU FIN-INICIALIZAR.

           EVALUATE TRUE
           WHEN BUSQDOM-CRIT-ID
                PERFORM VALIDAR-ID           THRU FIN-VALIDAR-ID
                IF BUSQDOM-STAT-OK
                THEN 
                   PERFORM BUSCAR-POR-ID     THRU FIN-BUSCAR-POR-ID
                END-IF
           
           WHEN BUSQDOM-CRIT-COMBI
                PERFORM VALIDAR-COMBI        THRU FIN-VALIDAR-COMBI
                IF BUSQDOM-STAT-OK
                THEN
                   PERFORM BUSCAR-POR-COMBI  THRU FIN-BUSCAR-POR-COMBI
                END-IF
           
           WHEN OTHER
                SET BUSQDOM-STAT-ERR-CRIT    TO TRUE
           END-EVALUATE.

           EXIT PROGRAM.

       INICIALIZAR.
      *------------
           INITIALIZE                        AREA-BUSQDOM-SALIDA
                                             REG-DOMICILIO
                                             REG-DOMICILIO-NULL.
           
       FIN-INICIALIZAR.
           EXIT.

       BUSCAR-POR-ID.
      *--------------
           MOVE BUSQDOM-E-ID                 TO DOMICILIO-ID.

           EXEC SQL 
                SELECT
                   id_dom,
                   calle_dom,
                   num_dom,
                   lpad(cod_post_dom,5,'0'),
                   prov_dom,
                   pobl_dom
                INTO 
                   :DOMICILIO-ID             :DOMICILIO-ID-NULL,
                   :DOMICILIO-CALLE          :DOMICILIO-CALLE-NULL,
                   :DOMICILIO-NUM            :DOMICILIO-NUM-NULL,
                   :DOMICILIO-COD-POS        :DOMICILIO-COD-POS-NULL,
                   :DOMICILIO-PROV           :DOMICILIO-PROV-NULL,
                   :DOMICILIO-POBL           :DOMICILIO-POBL-NULL
                FROM banco.domicilio
                WHERE id_dom = :DOMICILIO-ID    
           END-EXEC.

           MOVE SQLCODE                      TO BUSQDOM-SQLCODE.

           EVALUATE TRUE
           WHEN SQL-SUCCESS
                MOVE DOMICILIO-ID            TO BUSQDOM-S-ID
                MOVE DOMICILIO-CALLE         TO BUSQDOM-S-CALLE
                MOVE DOMICILIO-NUM           TO BUSQDOM-S-NUM
                MOVE DOMICILIO-PROV          TO BUSQDOM-S-PROV
                MOVE DOMICILIO-POBL          TO BUSQDOM-S-POBL
                MOVE DOMICILIO-COD-POS       TO BUSQDOM-S-COD-POS

           WHEN SQL-NODATA
                SET BUSQDOM-STAT-ENC-NO      TO TRUE

           WHEN OTHER
                SET BUSQDOM-STAT-ERR-SQL     TO TRUE
                DISPLAY '*** FATAL *** BUSQDOM BUSCAR-POR-ID: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-EVALUATE.
       FIN-BUSCAR-POR-ID.
           EXIT.

       BUSCAR-POR-COMBI.
      *-----------------
           MOVE BUSQDOM-E-CALLE              TO DOMICILIO-CALLE.

      *    No existe el concepto de nulidad en COBOL. Como el número es
      *    opcional, debemos utilizar una cadena de texto que represente
      *    el nulo, y asignar el valor de búsqueda o nulo a la HOST
      *    variable de filtro dependiendo de si me viene o no informado. 
           IF   BUSQDOM-E-NUM = ALL SPACES
           THEN MOVE CTE-NULL                TO DOMICILIO-NUM
           ELSE MOVE BUSQDOM-E-NUM           TO DOMICILIO-NUM 
           END-IF.
           
           MOVE BUSQDOM-E-COD-POS            TO DOMICILIO-COD-POS.

           EXEC SQL 
                SELECT
                   id_dom,
                   calle_dom,
                   num_dom,
                   lpad(cod_post_dom,5,'0'),
                   prov_dom,
                   pobl_dom
                INTO 
                   :DOMICILIO-ID             :DOMICILIO-ID-NULL,
                   :DOMICILIO-CALLE          :DOMICILIO-CALLE-NULL,
                   :DOMICILIO-NUM            :DOMICILIO-NUM-NULL,
                   :DOMICILIO-COD-POS        :DOMICILIO-COD-POS-NULL,
                   :DOMICILIO-PROV           :DOMICILIO-PROV-NULL,
                   :DOMICILIO-POBL           :DOMICILIO-POBL-NULL
                FROM banco.domicilio
                WHERE calle_dom           =  :DOMICILIO-CALLE 
                AND   IFNULL(num_dom,'@') =  :DOMICILIO-NUM
                AND   cod_post_dom        =  :DOMICILIO-COD-POS
           END-EXEC.

           MOVE SQLCODE                      TO BUSQDOM-SQLCODE.

           EVALUATE TRUE
           WHEN SQL-SUCCESS
                MOVE DOMICILIO-ID            TO BUSQDOM-S-ID
                MOVE DOMICILIO-CALLE         TO BUSQDOM-S-CALLE
                MOVE DOMICILIO-NUM           TO BUSQDOM-S-NUM
                MOVE DOMICILIO-PROV          TO BUSQDOM-S-PROV
                MOVE DOMICILIO-POBL          TO BUSQDOM-S-POBL
                MOVE DOMICILIO-COD-POS       TO BUSQDOM-S-COD-POS

           WHEN SQL-NODATA
                SET BUSQDOM-STAT-ENC-NO      TO TRUE

           WHEN OTHER
                SET BUSQDOM-STAT-ERR-SQL     TO TRUE
                DISPLAY '*** FATAL *** BUSQDOM BUSCAR-POR-COMBI: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-EVALUATE.
       FIN-BUSCAR-POR-COMBI.
           EXIT.

       VALIDAR-ID.
      *-----------
           IF   BUSQDOM-E-ID-X IS NOT NUMERIC
           THEN SET BUSQDOM-STAT-ERR-ID      TO TRUE
           END-IF.
       FIN-VALIDAR-ID.
           EXIT.

       VALIDAR-COMBI.
      *--------------
           EVALUATE TRUE
           WHEN BUSQDOM-E-CALLE = ALL SPACES
                SET BUSQDOM-STAT-ERR-CALLE   TO TRUE

           WHEN BUSQDOM-E-COD-POS-X IS NOT NUMERIC
                OR BUSQDOM-E-COD-POS = ZEROES
                SET BUSQDOM-STAT-ERR-COD-POS TO TRUE

           WHEN OTHER
                CONTINUE
           END-EVALUATE.
       FIN-VALIDAR-COMBI.
           EXIT.