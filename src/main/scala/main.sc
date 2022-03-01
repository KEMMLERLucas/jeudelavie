import scala.::

type Grille = List[(Int,Int)]
val g=List((-1,1), (0,1), (1,2), (2,0), (2,1))
val l = List(" XX",
    "  X",
    "XXX")
//Fonctionne
def adjqlis(l:List[(Int,Int)], i:(Int,Int)):List[(Int,Int)] = l match {
    case Nil => List(i) //ou i::Nil
    case t::q => t::(adjqlis(q, i))
}
//Ne fonctionne pas
def stringToListe(ligne: String, acc:Int,indice: Int,grille: List[(Int,Int)]):List[(Int,Int)] ={
    if(acc>ligne.length){
        return grille
    }
    if(ligne.charAt(acc)==" "){
        stringToListe(ligne,acc+1,indice,grille)
    } else {
        adjqlis(grille,(indice,acc))
        stringToListe(ligne,acc+1,indice,grille)
    }
}
//On manque des appels récursifs
def chaineToGrille2(liste: List[String],acc:Int,grille: Grille):Grille= liste match {
    case Nil => grille
    case t::_ => {
        print("La grille :"+ grille +"\n");
        //print((stringToListe(t,0,acc+1,grille)))
        stringToListe(t,0,acc+1,grille)
    }
}
def chaineToGrille(liste: List[String]): Grille={
    chaineToGrille2(liste,0,List())
}
val l = List(" XX","  X","XXX")
//val g=
//chaineToGrille(l)
//adjqlis(g,(-1,1))


/*def chainesToGrille3(l:List[String]): Grille={
    def aux(posX:Int,posY:Int,grille: Grille):Grille={
        //print("posx: " +posX + " posy: "+posY);
        val chaineActu=l(posX);
        print("\n Chaine actuelle :" +chaineActu +"\n");
        //val charActuelle=chaineActu(posY);
        //print("\n Valeur actuelle:"+charActuelle+"\n");
        if(l.length<=posX+1){
            print("Le tableau est finis\n")
            grille
        }else{
            if(chaineActu.charAt(posY).compare('X')==0){
                print("\n On rentre ici, on a un X! on vérifie la taille du tableau\n")

                if(chaineActu.length<=posY+1) {
                    print("AJout x mais on change de ligne");
                    adjqlis(grille,(posX,posY))
                    aux(posX+1,0,grille)
                }else{
                    print("On ajoute le x, mais on reste dans la même ligne \n")
                    aux(posX, posY, grille)
                }

            }else{
                print("\n On rentre ici, on a rien du tout, on vérifie la taille du tableau\n")
                if(chaineActu.length<=posY+1) {
                    print("On change de ligne\n");
                    aux(posX+1,0,grille)
                }else{
                    print("On reste sur la même ligne")
                    aux(posX, posY+1, grille)
                }
            }
        }

    }
    aux(0,0,Nil);
}*/


//chainesToGrille3(l)

def chainesToGrille3(l:List[String]): Grille={
    def aux(posX:Int,posY:Int): Grille={
        if(l.length<posX+1){
            //Lors de la prochaine itération, on va sortir du tableau
            Nil//On retourne la grille, normalement remplie
        }else{//On est dans le tableau
            val chaineActuelle=l(posX);
            val res=(posX,posY);
            if(chaineActuelle.charAt(posY).compare('X')==0){//Lors que l'ont prend l'élément de la chaine actuelle et que l'on regarde sa valeur à la pos X, c'est égal à 0 donc c'est vrai
                //On doit vérifier que, lors de la prochaine itération, posY ne dépasse pas le tableau
                if(posY+1>=chaineActuelle.length){
                    //Lors de la prochaine itération, on sera en dehors de la chaîne. On l'ajoute tout de même à la grille

                    //On appelle aux sur la prochaine colone
                    res::aux(posX+1,0);
                }else{//Lors de la prochaine itération, c'est toujours bon.
                    //Lors de la prochaine itération, on ne sera pas en dehors de la chaîne.

                    res::aux(posX,posY+1)
                }
            }else{//Ici, on a pas de X mais bien un 0
                //On doit vérifier que, lors de la prochaine itération, posY ne dépasse pas le tableau
                if(posY+1>=chaineActuelle.length){
                    //Lors de la prochaine itération, on sera en dehors de la chaîne.
                    //On appelle aux sur la prochaine colone
                    res::aux(posX+1,0);
                }else{//Lors de la prochaine itération, c'est toujours bon.
                    //Lors de la prochaine itération, on ne sera pas en dehors de la chaîne.
                    res::aux(posX,posY+1)
                }
            }
        }
    }
    aux(0,0)
}

chainesToGrille3(l)

assert(chainesToGrille3(l) == List((0,0), (0,1), (0,2), (1,0), (1,1), (1,2), (2,0), (2,1), (2,2)))