import org.scalatest._
import java.sql._

class SmokeTest extends FunSuite {
 
  test("smokeTest") {
    Class.forName("org.hsqldb.jdbc.JDBCDriver" );
    val c = DriverManager.getConnection("jdbc:hsqldb:mem:mymemdb", "SA", "");
    
   
    
    def using[Closeable <: {def close(): Unit}, B](closeable: Closeable)(getB: Closeable => B): B =
       try {
         getB(closeable)
       } finally {
         closeable.close()
       }
    
    
    using(c.createStatement) {
      st => { 
        val querys = Seq(""" 
        CREATE TABLE customer
        (FirstName varchar(255),
        LastName varchar(255))
        """,""" insert into customer(FirstName, LastName) VALUES('John','Doe') """)
        
        querys map {s  =>  st.execute(s)}
       }
    }
    
    
    //Configure ANORM
    import play.db.Config._
    setConnection(Unit => {c})
    
    //USE ANORM
    import play.db.anorm._ 
    
    val query = SQL("Select * from customer")
    assert(query.execute() === true)
    
    val customers = query().map(row => 
        row[String]("FirstName") -> row[String]("LastName")
    ).toList
    
    assert(customers.head === ("John","Doe"))
    
    c.close
    println("so far, it doesn't smoke ...")
  } 
}