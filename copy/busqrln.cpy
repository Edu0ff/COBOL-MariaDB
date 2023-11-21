       01  AREA-BUSQRLN.
           05  AREA-BUSQRLN-ENTRADA.
               10  BUSQRLN-E-CLI-ID               PIC 9(10).
               10  BUSQRLN-E-CLI-ID-X             REDEFINES 
                   BUSQRLN-E-CLI-ID               PIC X(10).
               10  BUSQRLN-E-CTA-ID               PIC 9(10).
               10  BUSQRLN-E-CTA-ID-X             REDEFINES 
                   BUSQRLN-E-CTA-ID               PIC X(10).

           05  AREA-BUSQRLN-SALIDA.
               10  BUSQRLN-RELACION               PIC X(05).
               10  BUSQRLN-SQLCODE                PIC S9(09) COMP-5.
               10  BUSQRLN-STAT                   PIC S9(01) VALUE 0.
                   88  BUSQRLN-STAT-OK            VALUE 0.
                   88  BUSQRLN-STAT-ENC-NO        VALUE 1.
                   88  BUSQRLN-STAT-ERR-CTA-ID    VALUE -1.
                   88  BUSQRLN-STAT-ERR-CLI-ID    VALUE -2.
                   88  BUSQRLN-STAT-ERR-SQL       VALUE -3.
