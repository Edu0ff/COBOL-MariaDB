       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. PRBALTACLIENTE.
       AUTHOR. MARÍA EDUARDA ALMEIDA LOIOLA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *------------------------
           COPY 'altacliente.cpy'.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
           
           PERFORM PRUEBA-1                THRU FIN-PRUEBA-1.
           STOP RUN.

       PRUEBA-1.
      *---------
           INITIALIZE                      ALTACLIENTE-ENTRADA.
           
           MOVE 'JUAN PEPE'                TO ALTACLI-CLI-NOMBRE.        
           MOVE 20000523                   TO ALTACLI-CLI-FEC-NAC.
           MOVE '856215489D'               TO ALTACLI-CLI-NIF.

           MOVE 'CALLE DE LA PLAZA'        TO ALTACLI-DOM-CALLE.             
           MOVE 965                        TO ALTACLI-DOM-NUMERO.                
           MOVE 78951                      TO ALTACLI-DOM-CODPOS.             
           MOVE 'BARCELONA'                TO ALTACLI-DOM-PROV.               
           MOVE 'MIPOBLACIÓN'              TO ALTACLI-DOM-POBL.

           MOVE 1                          TO ALTACLI-CUE-TAR-CONTADOR.
           MOVE 78965412369852145874       TO ALTACLI-NUM-CTA(1).
           MOVE +00007854.66               TO ALTACLI-SALDO-CTA(1).
           MOVE 'T'                        TO ALTACLI-RLN-CTA(1).  
           MOVE 7896325648125645           TO ALTACLI-NUM-TAR(1).
           MOVE -00008952.74               TO ALTACLI-CRE-TAR(1).
           MOVE 0225                       TO ALTACLI-FEC-TAR(1).
           MOVE 965                        TO ALTACLI-CCV-TAR(1).

           MOVE 1                       TO ALTACLI-MOV-CONTADOR.
           MOVE 'MERCADONA'             TO ALTACLI-MOV-CPT(1).
           MOVE -00000851.51            TO ALTACLI-MOV-IMPORTE(1).
           MOVE ZEROES                  TO ALTACLI-MOV-NUM-MEDIO-CTA(1).
           MOVE 7896325648125645        TO ALTACLI-MOV-NUM-MEDIO-TAR(1).
           MOVE 'T'                     TO ALTACLI-MOV-TIPO-MEDIO(1).
           MOVE 20230815050505050502    TO ALTACLI-MOV-FEC(1).

           DISPLAY '*** DEBUG ***         PRBALTACLIENTE'.
           DISPLAY '*** DEBUG ***         [INICIO]'.
           DISPLAY 'ALTACLI-CLI-NOMBRE    [' ALTACLI-CLI-NOMBRE ']'.
           DISPLAY 'ALTACLI-CLI-FEC-NAC   [' ALTACLI-CLI-FEC-NAC ']'.
           DISPLAY 'ALTACLI-CLI-NIF       [' ALTACLI-CLI-NIF ']'.
           DISPLAY '---------'.
           DISPLAY 'ALTACLI-DOM-CALLE     [' ALTACLI-DOM-CALLE ']'.
           DISPLAY 'ALTACLI-DOM-NUMERO    [' ALTACLI-DOM-NUMERO ']'.
           DISPLAY 'ALTACLI-DOM-CODPOS    [' ALTACLI-DOM-CODPOS ']'.
           DISPLAY 'ALTACLI-DOM-PROV      [' ALTACLI-DOM-PROV ']'.
           DISPLAY 'ALTACLI-DOM-POBL      [' ALTACLI-DOM-POBL ']'.
           DISPLAY '---------'.
           DISPLAY 'ALTACLI-CUE-TAR-CONTADOR'  
           '[' ALTACLI-CUE-TAR-CONTADOR ']'.
           DISPLAY 'ALTACLI-NUM-CTA       [' ALTACLI-NUM-CTA(1) ']'.
           DISPLAY 'ALTACLI-SALDO-CTA     [' ALTACLI-SALDO-CTA(1) ']'.
           DISPLAY 'ALTACLI-RLN-CTA       [' ALTACLI-RLN-CTA(1) ']'.
           DISPLAY 'ALTACLI-NUM-TAR       [' ALTACLI-NUM-TAR(1) ']'.
           DISPLAY 'ALTACLI-CRE-TAR       [' ALTACLI-CRE-TAR(1) ']'.
           DISPLAY 'ALTACLI-FEC-TAR       [' ALTACLI-FEC-TAR(1) ']'.
           DISPLAY 'ALTACLI-CCV-TAR       [' ALTACLI-CCV-TAR(1) ']'.
           DISPLAY '---------'.
           DISPLAY 'ALTACLI-MOV-CONTADOR  [' ALTACLI-MOV-CONTADOR ']'.
           DISPLAY 'ALTACLI-MOV-CPT       [' ALTACLI-MOV-CPT(1) ']'.
           DISPLAY 'ALTACLI-MOV-IMPORTE   [' ALTACLI-MOV-IMPORTE(1) ']'.
           DISPLAY 'ALTACLI-MOV-NUM-MEDIO-CTA' 
           '[' ALTACLI-MOV-NUM-MEDIO-CTA(1) ']'.
           DISPLAY 'ALTACLI-MOV-NUM-MEDIO-TAR' 
           '[' ALTACLI-MOV-NUM-MEDIO-TAR(1) ']'.
           DISPLAY 'ALTACLI-MOV-TIPO-MEDIO'    
           '[' ALTACLI-MOV-TIPO-MEDIO(1) ']'.
           DISPLAY 'ALTACLI-MOV-FEC       [' ALTACLI-MOV-FEC(1) ']'.

           CALL "ALTACLIENTE"            USING AREA-ALTACLIENTE.
       
           DISPLAY '*** DEBUG ***        [FIN]'.
           DISPLAY 'ALTACLI-STAT         [' ALTACLI-STAT ']'.
           DISPLAY 'ALTACLI-SQLCODE      [' ALTACLI-SQLCODE ']'.
           DISPLAY '------------------------------------------------'.

       FIN-PRUEBA-1.
           EXIT.

