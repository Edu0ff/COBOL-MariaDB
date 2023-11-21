       01  AREA-INSDOM.
           05  AREA-INSDOM-ENTRADA.
               10  INSDOM-E-CALLE                PIC X(35).
               10  INSDOM-E-NUM                  PIC X(03).
               10  INSDOM-E-COD-POS              PIC 9(05).
               10  INSDOM-E-COD-POS-X            REDEFINES 
                   INSDOM-E-COD-POS              PIC X(05).
               10  INSDOM-E-PROV                 PIC X(16).
               10  INSDOM-E-POBL                 PIC X(16).

           05  AREA-INSDOM-SALIDA.
               10  INSDOM-S-DOMICILIO.
                   15  INSDOM-S-DOM-ID           PIC 9(10).
               10  INSDOM-S-SQLCODE              PIC S9(09) COMP-5.
               10  INSDOM-STAT                   PIC S9(01) VALUE 0.
                   88  INSDOM-STAT-OK            VALUE 0.
                   88  INSDOM-STAT-ERR-CALLE     VALUE -1.
                   88  INSDOM-STAT-ERR-COD-POS   VALUE -2.
                   88  INSDOM-STAT-ERR-PROV      VALUE -3.
                   88  INSDOM-STAT-ERR-POBL      VALUE -4.
                   88  INSDOM-STAT-ERR-SQL       VALUE -5.
