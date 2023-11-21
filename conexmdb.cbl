       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. CONEXMDB.
       AUTHOR. MARÍA EDUARDA ALMEIDA LOIOLA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *------------------------
           EXEC SQL BEGIN DECLARE SECTION  END-EXEC.
       01  HV-DATASOURCE                   PIC X(50).
           EXEC SQL END DECLARE SECTION    END-EXEC.
       
       LINKAGE SECTION.
      *---------------- 
           COPY 'conexmdb.cpy'.

      ******************************************************************
       PROCEDURE DIVISION USING AREA-CONEXMDB.
      ******************************************************************
       
           PERFORM INICIALIZAR                 THRU FIN-INICIALIZAR.
           PERFORM VALIDAR-ACCION              THRU FIN-VALIDAR-ACCION.
           
           IF CONEXMDB-STAT-OK
           THEN IF CONEXMDB-E-ACC-ABRIR
                THEN PERFORM VALIDAR-DSNAME       
                     THRU FIN-VALIDAR-DSNAME
                     
                     IF CONEXMDB-STAT-OK
                     THEN PERFORM CONECTAR     THRU FIN-CONECTAR
                     ELSE DISPLAY 'DATA SOURCE INCORRECTO O NO INFORMAD'
                                  'O.'
                     END-IF
                ELSE PERFORM DESCONECTAR       THRU FIN-DESCONECTAR
                END-IF
           ELSE DISPLAY 'ACCION ERRONEA O NO INFORMADA.'
           END-IF.

           EXIT PROGRAM.
       
       INICIALIZAR.
      *------------
           INITIALIZE AREA-CONEXMDB-SALIDA.
       FIN-INICIALIZAR.
           EXIT.

       VALIDAR-ACCION.
      *---------------
           IF NOT (CONEXMDB-E-ACC-ABRIR OR CONEXMDB-E-ACC-CERRAR)
           THEN SET CONEXMDB-STAT-ERR-ACC    TO TRUE
           END-IF.
       FIN-VALIDAR-ACCION.
           EXIT.

       VALIDAR-DSNAME.
      *---------------
           IF CONEXMDB-E-DSNAME = ALL SPACES
           THEN SET CONEXMDB-STAT-ERR-DSNAME TO TRUE
           END-IF.
       FIN-VALIDAR-DSNAME.
           EXIT.

       CONECTAR.
      *---------
           MOVE CONEXMDB-E-DSNAME            TO HV-DATASOURCE.
      
           EXEC SQL
               CONNECT TO :HV-DATASOURCE
           END-EXEC.
           
           IF NOT SQL-SUCCESS
           THEN MOVE SQLCODE                 TO CONEXMDB-SQLCODE
                SET CONEXMDB-STAT-ERR-SQL    TO TRUE
                DISPLAY 'ERROR DE CONEXION A BASE DE DATOS ' 
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-IF.

       FIN-CONECTAR.
           EXIT.

       DESCONECTAR.
      *------------
           EXEC SQL
               DISCONNECT
           END-EXEC.
           
           IF NOT SQL-SUCCESS
           THEN MOVE SQLCODE                 TO CONEXMDB-SQLCODE
                SET CONEXMDB-STAT-ERR-SQL    TO TRUE
                DISPLAY 'ERROR DE DESCONEXION DE BASE DE DATOS ' 
                DISPLAY 'SQLCODE  [' SQLCODE ']'
                DISPLAY 'SQLERRML [' SQLERRML ']'
                DISPLAY 'SQLERRMC [' SQLERRMC ']'
           END-IF.

       FIN-DESCONECTAR.
           EXIT.