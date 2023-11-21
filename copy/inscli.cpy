       01  AREA-INSCLI.
           05  AREA-INSCLI-ENTRADA.
               10  INSCLI-E-NIF                   PIC X(10).
               10  INSCLI-E-NOM                   PIC X(57).
               10  INSCLI-E-FEC-NAC               PIC 9(08).
               10  INSCLI-E-FEC-NAC-X             REDEFINES 
                   INSCLI-E-FEC-NAC               PIC X(08).
               10  INSCLI-E-ID-DOM                PIC 9(10).
               10  INSCLI-E-ID-DOM-X              REDEFINES 
                   INSCLI-E-ID-DOM                PIC X(10).
                   
           05  AREA-INSCLI-SALIDA.
               10  INSCLI-S-CLIENTE.
                   15  INSCLI-S-CLI-ID            PIC 9(10).
               10  INSCLI-S-SQLCODE               PIC S9(09) COMP-5.
               10  INSCLI-STAT                    PIC S9(01) VALUE 0.
                   88  INSCLI-STAT-OK             VALUE 0.
                   88  INSCLI-STAT-ERR-NIF        VALUE 1.
                   88  INSCLI-STAT-ERR-NOM        VALUE -1.
                   88  INSCLI-STAT-ERR-FEC-NAC    VALUE -2.
                   88  INSCLI-STAT-ERR-DOM        VALUE -3.
                   88  INSCLI-STAT-ERR-SQL        VALUE -4.
