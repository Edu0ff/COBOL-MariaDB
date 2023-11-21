       01  AREA-BUSQMOV.
           05  AREA-BUSQMOV-ENTRADA.
               10  BUSQMOV-E-ID                PIC 9(10).
               10  BUSQMOV-E-FEC               PIC 9(20).
               
           05  AREA-BUSQMOV-SALIDA.
               10  BUSQMOV-MOVIMIENTO.
                   15  BUSQMOV-S-ID            PIC 9(10).
                   15  BUSQMOV-S-FEC           PIC X(20).
                   15  BUSQMOV-S-CPT           PIC X(49).
                   15  BUSQMOV-S-IMPT          PIC S9(08)V99 
                                               LEADING SEPARATE.
               10  BUSQMOV-SQLCODE             PIC S9(09) COMP-5.
               10  BUSQMOV-STAT                PIC S9(01) VALUE 0.
                   88  BUSQMOV-STAT-OK         VALUE 0.
                   88  BUSQMOV-STAT-ENC-NO     VALUE 1.
                   88  BUSQMOV-STAT-ERR-ID     VALUE -1.
                   88  BUSQMOV-STAT-ERR-FEC    VALUE -2.
                   88  BUSQMOV-STAT-ERR-SQL    VALUE -3.
