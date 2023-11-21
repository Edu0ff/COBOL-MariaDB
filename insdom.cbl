       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. INSDOM.
       AUTHOR. MARÍA EDUARDA ALMEIDA LOIOLA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *------------------------
           EXEC SQL BEGIN   DECLARE   SECTION END-EXEC.
           EXEC SQL INCLUDE domicilio         END-EXEC.
           EXEC SQL END     DECLARE   SECTION END-EXEC.

       01  CTE-NULL                         PIC S9(01) VALUE -1.

       LINKAGE SECTION.
      *----------------
           COPY 'insdom.cpy'.
      ******************************************************************
       PROCEDURE DIVISION USING AREA-INSDOM.
      ******************************************************************
           PERFORM INICIALIZAR              THRU FIN-INICIALIZAR.

           EVALUATE TRUE
           WHEN INSDOM-E-CALLE = ALL SPACES
                SET INSDOM-STAT-ERR-CALLE   TO TRUE

           WHEN INSDOM-E-COD-POS-X IS NOT NUMERIC
                SET INSDOM-STAT-ERR-COD-POS TO TRUE

           WHEN INSDOM-E-PROV = ALL SPACES
                SET INSDOM-STAT-ERR-PROV    TO TRUE
               
           WHEN INSDOM-E-POBL = ALL SPACES
                SET INSDOM-STAT-ERR-POBL    TO TRUE
           
           WHEN OTHER
                PERFORM GEN-DOM-ID          THRU FIN-GEN-DOM-ID

                IF   INSDOM-STAT-OK
                THEN PERFORM INS-DOM        THRU FIN-INS-DOM
                END-IF
           END-EVALUATE.
       
           EXIT PROGRAM.

       INICIALIZAR.
      *------------
           INITIALIZE                       AREA-INSDOM-SALIDA
                                            REG-DOMICILIO
                                            REG-DOMICILIO-NULL.
       FIN-INICIALIZAR.
           EXIT.

       GEN-DOM-ID.
      *-----------
           EXEC SQL
                SELECT NEXT VALUE FOR banco.domicilio_id_dom_seq
                INTO :DOMICILIO-ID     :DOMICILIO-ID-NULL
           END-EXEC.

           MOVE SQLCODE                       TO INSDOM-S-SQLCODE.

           IF   NOT SQL-SUCCESS
           THEN SET INSDOM-STAT-ERR-SQL       TO TRUE
                DISPLAY '*** FATAL *** INSDOM GEN-ID-DOM: ERROR'
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-IF.
       FIN-GEN-DOM-ID.
           EXIT.

       INS-DOM.
      *--------
           MOVE INSDOM-E-CALLE                TO DOMICILIO-CALLE.
           
           IF   INSDOM-E-NUM NOT = ALL SPACES
           THEN MOVE INSDOM-E-NUM             TO DOMICILIO-NUM
           ELSE MOVE CTE-NULL                 TO DOMICILIO-NUM-NULL
           END-IF.

           MOVE INSDOM-E-COD-POS              TO DOMICILIO-COD-POS.
           MOVE INSDOM-E-POBL                 TO DOMICILIO-POBL.
           MOVE INSDOM-E-PROV                 TO DOMICILIO-PROV.

           EXEC SQL
                INSERT INTO banco.domicilio(
                    id_dom,
                    calle_dom,
                    num_dom,
                    cod_post_dom,
                    prov_dom,
                    pobl_dom
                )
                VALUES(
                    :DOMICILIO-ID             :DOMICILIO-ID-NULL,
                    :DOMICILIO-CALLE          :DOMICILIO-CALLE-NULL,
                    :DOMICILIO-NUM            :DOMICILIO-NUM-NULL,
                    :DOMICILIO-COD-POS        :DOMICILIO-COD-POS-NULL,
                    :DOMICILIO-PROV           :DOMICILIO-PROV-NULL,
                    :DOMICILIO-POBL           :DOMICILIO-POBL-NULL
                )
           END-EXEC.
            
           MOVE SQLCODE                       TO INSDOM-S-SQLCODE.
           
           IF SQL-SUCCESS
           THEN MOVE DOMICILIO-ID             TO INSDOM-S-DOM-ID
           ELSE SET INSDOM-STAT-ERR-SQL       TO TRUE
                DISPLAY '*** FATAL *** INSDOM INS-DOM: ERROR '
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-IF.

       FIN-INS-DOM.
           EXIT.
       