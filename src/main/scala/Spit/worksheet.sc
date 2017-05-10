val t1 = (1 , 'a')
val t2 = (2 , 'b')
val t3 = (3 , 'c')
val t4 = (4 , 'd')

val lst: List[(Int, Char)] = List(t1, t2, t3, t4)

lst.indexWhere(x => x._1 ==3)
