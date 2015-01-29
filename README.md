DEPRECATED
===============


Anorm, SQL data access *from* Play Scala
==========================================

Anorm uses plain SQL to make your database request and provides several API to parse and transform the resulting dataset. Anorm is Not a Object Relational Mapper


How to use it
------------------

Anorm standalone uses sbt 0.10

```bash
#in working copy
sbt test publish-local
```

Then add this to your dependencies ```"anorm" %% "anorm" % "0.1" ```

Basic usage
------------- 
```scala
// Inject connection

import play.db.Config._
setConnection((Unit) => {... /* Function returning java.sql.Connection */ })


// Use
import play.db.anorm._

//... as anorm in Play
```

