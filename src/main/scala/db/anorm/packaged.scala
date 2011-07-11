
 
 
package play.db
 
package anorm {
 
case class WithDefaults(defaultConvention: PartialFunction[AnalyserInfo,String] = asIs) {
 
    import java.lang.reflect.Method
 
    def getParametersNames(m:Method):Seq[String] = {
            import scala.collection.JavaConversions._
            play.classloading.enhancers.LocalvariablesNamesEnhancer.lookupParameterNames(m)
    } 
 
 
 
 
    abstract class MagicParser2[ A1, A2,R]( 
        tableDescription:Option[Description2],
        conventions: PartialFunction[AnalyserInfo,String] = defaultConvention)
        (implicit c1:ColumnTo[A1], c2:ColumnTo[A2], r:Manifest[R]) extends MParser2[ A1, A2, R] {
        
            lazy val p1 = c1
            lazy val p2 = c2
 
            lazy val typeName = r.erasure.getSimpleName
            lazy val containerName = tableDescription.map(_.table).orElse(conventions.lift(TableC(typeName))).getOrElse(typeName)
 
            def thisClass:Class[_] = implicitly[Manifest[this.type]].erasure
 
 
            lazy val columnNames = tableDescription.flatMap(_.columns).getOrElse {
                thisClass.getDeclaredMethods()
                         .filter(_.getName()=="apply")
                         .find(_.getParameterTypes().length == 2)
                         .map(getParametersNames)
                         .map( _.map(c =>  conventions(ColumnC(containerName,c)) ))
                         .collect{case Seq( a1, a2) => ( a1, a2)}
                         .get
 
           }
    }
 
    abstract class Magic2[ A1, A2, R](
        tableDescription:Option[Description2],
        conventions: PartialFunction[AnalyserInfo,String] = defaultConvention)
       (implicit ptt1:(ColumnTo[A1],ToStatement[A1]), ptt2:(ColumnTo[A2],ToStatement[A2]), r:Manifest[R]) extends MagicParser2[ A1, A2, R](tableDescription = tableDescription, conventions = conventions)(
ptt1._1, ptt2._1,r) with  M2[ A1, A2, R]{
 
        lazy val pt1 = ptt1 
        lazy val pt2 = ptt2 
}
 
    case class TheMagicParser2[ A1, A2, R](
        cons: Function2[ A1, A2, R],
        tableDescription:Option[Description2],
        conventions: PartialFunction[AnalyserInfo,String] = defaultConvention)
       (implicit c1:ColumnTo[A1], c2:ColumnTo[A2], r:Manifest[R]) extends MagicParser2[ A1, A2, R](tableDescription = tableDescription, conventions = conventions){
           override def thisClass = cons.getClass
           def apply(a1:A1, a2:A2):R = cons( a1, a2)
 
}
    case class TheMagic2[ A1, A2, R](
        companion: Companion2[ A1, A2, R],
        tableDescription:Option[Description2],
        conventions: PartialFunction[AnalyserInfo,String] = defaultConvention)
       (implicit ptt1:(ColumnTo[A1],ToStatement[A1]), ptt2:(ColumnTo[A2],ToStatement[A2]), r:Manifest[R]) extends Magic2[ A1, A2, R](
                     tableDescription = tableDescription,
                     conventions = conventions)( ptt1, ptt2,r){
           override def thisClass = companion.getClass
           def apply(a1:A1, a2:A2):R = companion( a1, a2)
           def unapply(r:R):Option[( A1, A2)] = companion.unapply(r)
    }        
 
    case class Description2(table:String,columns: Option[(String, String)]=None)
 
    trait Companion2[ A1, A2, R]{
         def apply(a1:A1, a2:A2):R
         def unapply(r:R):Option[( A1, A2)]
    }
 
 
 
    abstract class MagicParser3[ A1, A2, A3,R]( 
        tableDescription:Option[Description3],
        conventions: PartialFunction[AnalyserInfo,String] = defaultConvention)
        (implicit c1:ColumnTo[A1], c2:ColumnTo[A2], c3:ColumnTo[A3], r:Manifest[R]) extends MParser3[ A1, A2, A3, R] {
        
            lazy val p1 = c1
            lazy val p2 = c2
            lazy val p3 = c3
 
            lazy val typeName = r.erasure.getSimpleName
            lazy val containerName = tableDescription.map(_.table).orElse(conventions.lift(TableC(typeName))).getOrElse(typeName)
 
            def thisClass:Class[_] = implicitly[Manifest[this.type]].erasure
 
 
            lazy val columnNames = tableDescription.flatMap(_.columns).getOrElse {
                thisClass.getDeclaredMethods()
                         .filter(_.getName()=="apply")
                         .find(_.getParameterTypes().length == 3)
                         .map(getParametersNames)
                         .map( _.map(c =>  conventions(ColumnC(containerName,c)) ))
                         .collect{case Seq( a1, a2, a3) => ( a1, a2, a3)}
                         .get
 
           }
    }
 
    abstract class Magic3[ A1, A2, A3, R](
        tableDescription:Option[Description3],
        conventions: PartialFunction[AnalyserInfo,String] = defaultConvention)
       (implicit ptt1:(ColumnTo[A1],ToStatement[A1]), ptt2:(ColumnTo[A2],ToStatement[A2]), ptt3:(ColumnTo[A3],ToStatement[A3]), r:Manifest[R]) extends MagicParser3[ A1, A2, A3, R](tableDescription = tableDescription, conventions = conventions)(
ptt1._1, ptt2._1, ptt3._1,r) with  M3[ A1, A2, A3, R]{
 
        lazy val pt1 = ptt1 
        lazy val pt2 = ptt2 
        lazy val pt3 = ptt3 
}
 
    case class TheMagicParser3[ A1, A2, A3, R](
        cons: Function3[ A1, A2, A3, R],
        tableDescription:Option[Description3],
        conventions: PartialFunction[AnalyserInfo,String] = defaultConvention)
       (implicit c1:ColumnTo[A1], c2:ColumnTo[A2], c3:ColumnTo[A3], r:Manifest[R]) extends MagicParser3[ A1, A2, A3, R](tableDescription = tableDescription, conventions = conventions){
           override def thisClass = cons.getClass
           def apply(a1:A1, a2:A2, a3:A3):R = cons( a1, a2, a3)
 
}
    case class TheMagic3[ A1, A2, A3, R](
        companion: Companion3[ A1, A2, A3, R],
        tableDescription:Option[Description3],
        conventions: PartialFunction[AnalyserInfo,String] = defaultConvention)
       (implicit ptt1:(ColumnTo[A1],ToStatement[A1]), ptt2:(ColumnTo[A2],ToStatement[A2]), ptt3:(ColumnTo[A3],ToStatement[A3]), r:Manifest[R]) extends Magic3[ A1, A2, A3, R](
                     tableDescription = tableDescription,
                     conventions = conventions)( ptt1, ptt2, ptt3,r){
           override def thisClass = companion.getClass
           def apply(a1:A1, a2:A2, a3:A3):R = companion( a1, a2, a3)
           def unapply(r:R):Option[( A1, A2, A3)] = companion.unapply(r)
    }        
 
    case class Description3(table:String,columns: Option[(String, String, String)]=None)
 
    trait Companion3[ A1, A2, A3, R]{
         def apply(a1:A1, a2:A2, a3:A3):R
         def unapply(r:R):Option[( A1, A2, A3)]
    }
 
 
 
    abstract class MagicParser4[ A1, A2, A3, A4,R]( 
        tableDescription:Option[Description4],
        conventions: PartialFunction[AnalyserInfo,String] = defaultConvention)
        (implicit c1:ColumnTo[A1], c2:ColumnTo[A2], c3:ColumnTo[A3], c4:ColumnTo[A4], r:Manifest[R]) extends MParser4[ A1, A2, A3, A4, R] {
        
            lazy val p1 = c1
            lazy val p2 = c2
            lazy val p3 = c3
            lazy val p4 = c4
 
            lazy val typeName = r.erasure.getSimpleName
            lazy val containerName = tableDescription.map(_.table).orElse(conventions.lift(TableC(typeName))).getOrElse(typeName)
 
            def thisClass:Class[_] = implicitly[Manifest[this.type]].erasure
 
 
            lazy val columnNames = tableDescription.flatMap(_.columns).getOrElse {
                thisClass.getDeclaredMethods()
                         .filter(_.getName()=="apply")
                         .find(_.getParameterTypes().length == 4)
                         .map(getParametersNames)
                         .map( _.map(c =>  conventions(ColumnC(containerName,c)) ))
                         .collect{case Seq( a1, a2, a3, a4) => ( a1, a2, a3, a4)}
                         .get
 
           }
    }
 
    abstract class Magic4[ A1, A2, A3, A4, R](
        tableDescription:Option[Description4],
        conventions: PartialFunction[AnalyserInfo,String] = defaultConvention)
       (implicit ptt1:(ColumnTo[A1],ToStatement[A1]), ptt2:(ColumnTo[A2],ToStatement[A2]), ptt3:(ColumnTo[A3],ToStatement[A3]), ptt4:(ColumnTo[A4],ToStatement[A4]), r:Manifest[R]) extends MagicParser4[ A1, A2, A3, A4, R](tableDescription = tableDescription, conventions = conventions)(
ptt1._1, ptt2._1, ptt3._1, ptt4._1,r) with  M4[ A1, A2, A3, A4, R]{
 
        lazy val pt1 = ptt1 
        lazy val pt2 = ptt2 
        lazy val pt3 = ptt3 
        lazy val pt4 = ptt4 
}
 
    case class TheMagicParser4[ A1, A2, A3, A4, R](
        cons: Function4[ A1, A2, A3, A4, R],
        tableDescription:Option[Description4],
        conventions: PartialFunction[AnalyserInfo,String] = defaultConvention)
       (implicit c1:ColumnTo[A1], c2:ColumnTo[A2], c3:ColumnTo[A3], c4:ColumnTo[A4], r:Manifest[R]) extends MagicParser4[ A1, A2, A3, A4, R](tableDescription = tableDescription, conventions = conventions){
           override def thisClass = cons.getClass
           def apply(a1:A1, a2:A2, a3:A3, a4:A4):R = cons( a1, a2, a3, a4)
 
}
    case class TheMagic4[ A1, A2, A3, A4, R](
        companion: Companion4[ A1, A2, A3, A4, R],
        tableDescription:Option[Description4],
        conventions: PartialFunction[AnalyserInfo,String] = defaultConvention)
       (implicit ptt1:(ColumnTo[A1],ToStatement[A1]), ptt2:(ColumnTo[A2],ToStatement[A2]), ptt3:(ColumnTo[A3],ToStatement[A3]), ptt4:(ColumnTo[A4],ToStatement[A4]), r:Manifest[R]) extends Magic4[ A1, A2, A3, A4, R](
                     tableDescription = tableDescription,
                     conventions = conventions)( ptt1, ptt2, ptt3, ptt4,r){
           override def thisClass = companion.getClass
           def apply(a1:A1, a2:A2, a3:A3, a4:A4):R = companion( a1, a2, a3, a4)
           def unapply(r:R):Option[( A1, A2, A3, A4)] = companion.unapply(r)
    }        
 
    case class Description4(table:String,columns: Option[(String, String, String, String)]=None)
 
    trait Companion4[ A1, A2, A3, A4, R]{
         def apply(a1:A1, a2:A2, a3:A3, a4:A4):R
         def unapply(r:R):Option[( A1, A2, A3, A4)]
    }
 
 
 
    abstract class MagicParser5[ A1, A2, A3, A4, A5,R]( 
        tableDescription:Option[Description5],
        conventions: PartialFunction[AnalyserInfo,String] = defaultConvention)
        (implicit c1:ColumnTo[A1], c2:ColumnTo[A2], c3:ColumnTo[A3], c4:ColumnTo[A4], c5:ColumnTo[A5], r:Manifest[R]) extends MParser5[ A1, A2, A3, A4, A5, R] {
        
            lazy val p1 = c1
            lazy val p2 = c2
            lazy val p3 = c3
            lazy val p4 = c4
            lazy val p5 = c5
 
            lazy val typeName = r.erasure.getSimpleName
            lazy val containerName = tableDescription.map(_.table).orElse(conventions.lift(TableC(typeName))).getOrElse(typeName)
 
            def thisClass:Class[_] = implicitly[Manifest[this.type]].erasure
 
 
            lazy val columnNames = tableDescription.flatMap(_.columns).getOrElse {
                thisClass.getDeclaredMethods()
                         .filter(_.getName()=="apply")
                         .find(_.getParameterTypes().length == 5)
                         .map(getParametersNames)
                         .map( _.map(c =>  conventions(ColumnC(containerName,c)) ))
                         .collect{case Seq( a1, a2, a3, a4, a5) => ( a1, a2, a3, a4, a5)}
                         .get
 
           }
    }
 
    abstract class Magic5[ A1, A2, A3, A4, A5, R](
        tableDescription:Option[Description5],
        conventions: PartialFunction[AnalyserInfo,String] = defaultConvention)
       (implicit ptt1:(ColumnTo[A1],ToStatement[A1]), ptt2:(ColumnTo[A2],ToStatement[A2]), ptt3:(ColumnTo[A3],ToStatement[A3]), ptt4:(ColumnTo[A4],ToStatement[A4]), ptt5:(ColumnTo[A5],ToStatement[A5]), r:Manifest[R]) extends MagicParser5[ A1, A2, A3, A4, A5, R](tableDescription = tableDescription, conventions = conventions)(
ptt1._1, ptt2._1, ptt3._1, ptt4._1, ptt5._1,r) with  M5[ A1, A2, A3, A4, A5, R]{
 
        lazy val pt1 = ptt1 
        lazy val pt2 = ptt2 
        lazy val pt3 = ptt3 
        lazy val pt4 = ptt4 
        lazy val pt5 = ptt5 
}
 
    case class TheMagicParser5[ A1, A2, A3, A4, A5, R](
        cons: Function5[ A1, A2, A3, A4, A5, R],
        tableDescription:Option[Description5],
        conventions: PartialFunction[AnalyserInfo,String] = defaultConvention)
       (implicit c1:ColumnTo[A1], c2:ColumnTo[A2], c3:ColumnTo[A3], c4:ColumnTo[A4], c5:ColumnTo[A5], r:Manifest[R]) extends MagicParser5[ A1, A2, A3, A4, A5, R](tableDescription = tableDescription, conventions = conventions){
           override def thisClass = cons.getClass
           def apply(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5):R = cons( a1, a2, a3, a4, a5)
 
}
    case class TheMagic5[ A1, A2, A3, A4, A5, R](
        companion: Companion5[ A1, A2, A3, A4, A5, R],
        tableDescription:Option[Description5],
        conventions: PartialFunction[AnalyserInfo,String] = defaultConvention)
       (implicit ptt1:(ColumnTo[A1],ToStatement[A1]), ptt2:(ColumnTo[A2],ToStatement[A2]), ptt3:(ColumnTo[A3],ToStatement[A3]), ptt4:(ColumnTo[A4],ToStatement[A4]), ptt5:(ColumnTo[A5],ToStatement[A5]), r:Manifest[R]) extends Magic5[ A1, A2, A3, A4, A5, R](
                     tableDescription = tableDescription,
                     conventions = conventions)( ptt1, ptt2, ptt3, ptt4, ptt5,r){
           override def thisClass = companion.getClass
           def apply(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5):R = companion( a1, a2, a3, a4, a5)
           def unapply(r:R):Option[( A1, A2, A3, A4, A5)] = companion.unapply(r)
    }        
 
    case class Description5(table:String,columns: Option[(String, String, String, String, String)]=None)
 
    trait Companion5[ A1, A2, A3, A4, A5, R]{
         def apply(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5):R
         def unapply(r:R):Option[( A1, A2, A3, A4, A5)]
    }
 
 
}
}
