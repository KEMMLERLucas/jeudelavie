type Grille = List[(Int,Int)]
val g=List((-1,1), (0,1), (1,2), (2,0), (2,1))
val l = List(" XX",
    "  X",
    "XXX")
val l = List(" XX","  X","XXX")
def chainesToGrille(l:List[String]): Grille={
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
//chainesToGrille(l)
//assert(chainesToGrille(l) == List((0,0), (0,1), (0,2), (1,0), (1,1), (1,2), (2,0), (2,1), (2,2)))
println(5);





//1.2
//Récupérer le min et le max x et y
/*def maxX(g:Grille): Int{

}*/
//Faire le parcours et regarder si les deux