       01  AREA-BUSQTAR.
           05  AREA-BUSQTAR-ENTRADA.
               10  BUSQTAR-E-ID                  PIC 9(10).
               10  BUSQTAR-E-NUM                 PIC X(16).
               10  BUSQTAR-E-ID-CLI              PIC 9(10).
               10  BUSQTAR-E-ID-CTA              PIC 9(10).
               10  BUSQTAR-E-CRITERIO            PIC 9(01) VALUE 0.
                   88  BUSQTAR-CRIT-ID           VALUE 0. 
                   88  BUSQTAR-CRIT-NUM          VALUE 1.
                   88  BUSQTAR-CRIT-CC           VALUE 2.
                   
           05  AREA-BUSQTAR-SALIDA.
               10  BUSQTAR-TARJETA.
                   15  BUSQTAR-S-ID-TAR          PIC 9(10).
                   15  BUSQTAR-S-ID-CLI          PIC 9(10).
                   15  BUSQTAR-S-ID-CTA          PIC 9(10).
                   15  BUSQTAR-S-NUM             PIC X(16).
                   15  BUSQTAR-S-CRED            PIC S9(08)V99 
                                                 LEADING SEPARATE.
                   15  BUSQTAR-S-FEC             PIC 9(04).
                   15  BUSQTAR-S-CCV             PIC X(03).
               10  BUSQTAR-SQLCODE               PIC S9(09) COMP-5.
               10  BUSQTAR-STAT                  PIC S9(01) VALUE 0.
                   88  BUSQTAR-STAT-OK           VALUE 0.
                   88  BUSQTAR-STAT-ENC-NO       VALUE 1.
                   88  BUSQTAR-STAT-ERR-CRIT     VALUE -1.
                   88  BUSQTAR-STAT-ERR-ID       VALUE -2.
                   88  BUSQTAR-STAT-ERR-NUM      VALUE -3.
                   88  BUSQTAR-STAT-ERR-CC-CLI   VALUE -4.
                   88  BUSQTAR-STAT-ERR-CC-CTA   VALUE -5.
                   88  BUSQTAR-STAT-ERR-SQL      VALUE -6.
