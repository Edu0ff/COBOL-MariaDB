       01  AREA-BUSQDOM.
           05  AREA-BUSQDOM-ENTRADA.
               10  BUSQDOM-E-ID                  PIC 9(10).
               10  BUSQDOM-E-ID-X                REDEFINES BUSQDOM-E-ID
                                                 PIC X(10).
               10  BUSQDOM-E-CALLE               PIC X(35).
               10  BUSQDOM-E-NUM                 PIC X(03).
               10  BUSQDOM-E-COD-POS             PIC 9(05).
               10  BUSQDOM-E-COD-POS-X           REDEFINES 
                   BUSQDOM-E-COD-POS             PIC X(05).
               10  BUSQDOM-E-CRITERIO            PIC 9(01) VALUE 0.
                   88  BUSQDOM-CRIT-ID           VALUE 0.
                   88  BUSQDOM-CRIT-COMBI        VALUE 1.
                   
           05  AREA-BUSQDOM-SALIDA.
               10  BUSQDOM-DOMICILIO.
                   15  BUSQDOM-S-ID              PIC 9(10).
                   15  BUSQDOM-S-CALLE           PIC X(45).
                   15  BUSQDOM-S-NUM             PIC X(03).
                   15  BUSQDOM-S-POBL            PIC X(16).
                   15  BUSQDOM-S-PROV            PIC X(16).
                   15  BUSQDOM-S-COD-POS         PIC 9(05).
               10  BUSQDOM-SQLCODE               PIC S9(09) COMP-5.
               10  BUSQDOM-STAT                  PIC S9(01) VALUE 0.
                   88  BUSQDOM-STAT-OK           VALUE 0.
                   88  BUSQDOM-STAT-ENC-NO       VALUE 1.
                   88  BUSQDOM-STAT-ERR-CRIT     VALUE -1.
                   88  BUSQDOM-STAT-ERR-ID       VALUE -2.
                   88  BUSQDOM-STAT-ERR-COD-POS  VALUE -3.
                   88  BUSQDOM-STAT-ERR-CALLE    VALUE -4.
                   88  BUSQDOM-STAT-ERR-SQL      VALUE -5.
                   