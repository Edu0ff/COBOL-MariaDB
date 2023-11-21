       01  AREA-CONEXMDB.
           05  AREA-CONEXMDB-ENTRADA.
               10  CONEXMDB-E-ACC                  PIC 9(01) VALUE 0.
                   88  CONEXMDB-E-ACC-ABRIR        VALUE 0.
                   88  CONEXMDB-E-ACC-CERRAR       VALUE 1.
               10  CONEXMDB-E-DSNAME               PIC X(50).
               
           05  AREA-CONEXMDB-SALIDA.
               10  CONEXMDB-SQLCODE                PIC S9(09) COMP-5.
               10  CONEXMDB-STAT                   PIC S9(01) VALUE 0.
                   88  CONEXMDB-STAT-OK            VALUE 0.
                   88  CONEXMDB-STAT-ERR-SQL       VALUE -1.
                   88  CONEXMDB-STAT-ERR-ACC       VALUE -2.
                   88  CONEXMDB-STAT-ERR-DSNAME    VALUE -3.
