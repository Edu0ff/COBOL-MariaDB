       01  AREA-INSTAR.
           05  AREA-INSTAR-ENTRADA.
               10  INSTAR-E-ID-CLI               PIC 9(10).
               10  INSTAR-E-ID-CTA               PIC 9(10).
               10  INSTAR-E-NUM                  PIC X(16).
               10  INSTAR-E-CRED                 PIC S9(08)V99.
               10  INSTAR-E-FEC                  PIC 9(04).
               10  INSTAR-E-CCV                  PIC X(03).

           05  AREA-INSTAR-SALIDA.
               10  INSTAR-S-TARJETA.
                   15  INSTAR-S-TAR-ID           PIC 9(10).
               10  INSTAR-S-SQLCODE              PIC S9(09) COMP-5.
               10  INSTAR-STAT                   PIC S9(01) VALUE 0.
                   88  INSTAR-STAT-OK            VALUE 0.
                   88  INSTAR-STAT-ERR-CLI-ID    VALUE -1.
                   88  INSTAR-STAT-ERR-CTA-ID    VALUE -2.
                   88  INSTAR-STAT-ERR-NUM       VALUE -3.
                   88  INSTAR-STAT-ERR-CCV       VALUE -4.
                   88  INSTAR-STAT-ERR-FEC       VALUE -5.
                   88  INSTAR-STAT-ERR-SQL       VALUE -6.
