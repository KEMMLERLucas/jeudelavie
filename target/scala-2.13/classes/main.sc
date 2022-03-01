type Grille = List[(Int,Int)]
val g=List((-1,1), (0,1), (1,2), (2,0), (2,1))
val l = List(" XX",
    " X",
    "XXX")
def adjqlis(l:List[(Int,Int)], i:(Int,Int)):List[(Int,Int)] = l match {
    case Nil => List(i) //ou i::Nil
    case t::q => t::(adjqlis(q, i))
}
def stringToListe(ligne: String, acc:Int,indice: Int,grille: List[(Int,Int)]):List[(Int,Int)]=grille match{
    case Nil=>grille
    case _=>
        if(ligne.charAt(acc)==" "){
            print("Non")
            stringToListe(ligne,acc+1,indice,grille)
        } else {
            print("Oui")
            adjqlis(grille,(indice,acc))
            stringToListe(ligne,acc+1,indice,grille)
        }
}
def chaineToGrille2(liste: List[String],acc:Int,grille: Grille):Grille= liste match {
    case Nil => grille
    case t::_ => stringToListe(t,0,acc+1,grille)
}
def chaineToGrille(liste: List[String]): Grille={
    chaineToGrille2(liste,0,List())
}
val l = List(" XX","  X","XXX")

chaineToGrille(l)