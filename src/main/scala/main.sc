type Grille = List[(Int,Int)]
val test2=List((-1,1), (0,1), (1,2), (2,0), (2,1))
val t=List((0,1),(0,2),(1,2),(2,0),(2,1),(2,2))
val l = List(" XX",
    "  X",
    "XXX")

def chainesToGrille(l:List[String]): Grille={
    def aux(posX:Int,posY:Int): Grille={
        if(posX >= l.length){
            //Lors de la prochaine itération, on va sortir du tableau
            Nil//On retourne la grille, normalement remplie
        }else{//On est dans le tableau
            val chaineActuelle=l(posX)
            val res=(posX,posY)
            if(chaineActuelle(posY)=='X'){//Lors que l'ont prend l'élément de la chaine actuelle et que l'on regarde sa valeur à la pos X, c'est égal à 0 donc c'est vrai
                //On doit vérifier que, lors de la prochaine itération, posY ne dépasse pas le tableau
                if(posY+1>=chaineActuelle.length){
                    //Lors de la prochaine itération, on sera en dehors de la chaîne. On l'ajoute tout de même à la grille

                    //On appelle aux sur la prochaine colone
                    res::aux(posX+1,0)
                }else{//Lors de la prochaine itération, c'est toujours bon.
                    //Lors de la prochaine itération, on ne sera pas en dehors de la chaîne.

                    res::aux(posX,posY+1)
                }
            }else{//Ici, on a pas de X mais bien un 0
                //On doit vérifier que, lors de la prochaine itération, posY ne dépasse pas le tableau
                if(posY+1>=chaineActuelle.length){
                    //Lors de la prochaine itération, on sera en dehors de la chaîne.
                    //On appelle aux sur la prochaine colone
                    aux(posX+1,0)
                }else{//Lors de la prochaine itération, c'est toujours bon.
                    //Lors de la prochaine itération, on ne sera pas en dehors de la chaîne.
                    aux(posX,posY+1)
                }
            }
        }
    }
    aux(0,0)
}
chainesToGrille(l)
assert(chainesToGrille(l) == t)

//1.2
//Récupérer le min et le max x et y

def maxX(g:Grille):Int={
    def aux(g:Grille,max:Int):Int =g match {
        case Nil => max
        case t::q =>
            val (x,y) = t
            if(x>max){
                aux(q,x)
            } else {
                aux(q,max)
            }
    }
    val (a,b)=g.head
    aux(g.tail,a)
}

def maxY(g:Grille):Int={
    def aux(g:Grille,max:Int):Int =g match {
        case Nil => max
        case t::q =>
            val (x,y) = t
            if(y>max){
                aux(q,y)
            } else {
                aux(q,max)
            }
    }
    val (a,b)=g.head
    aux(g.tail,b)
}

def minX(g:Grille):Int={
    def aux(g:Grille,min:Int):Int =g match {
        case Nil=>min
        case t::q=>
            val(x,y)=t
            if(x<min){
                aux(q,x)
            } else {
                aux(q, min)
            }
    }
    val (a,b)=g.head
    aux(g.tail,a)
}

def minY(g:Grille):Int={
    def aux(g:Grille,min:Int):Int =g match {
        case Nil=>min
        case t::q=>
            val(x,y)=t
            if(y<min){
                aux(q,y)
            } else {
                aux(q, min)
            }
    }
    val (a,b)=g.head
    aux(g.tail,b)
}
val(c,d)=t.head
println("C: "+ c+ "D:"+d)
//Test des méthodes min et max
maxX(t)
maxY(t)//Boucle peut-être ?

minX(t)//Boucle

minY(t)
//Faire le parcours et regarder si les valeurs sont présentes dans la grille.
println("Grille transformee: ")
print(chainesToGrille(l))
def estDedans(g:Grille,v:(Int,Int)): Boolean = g match {
    case Nil => false;
    case t::q =>
        if(t==v){
            true
        } else {
            estDedans(q,v)
        }
}

def afficherGrille(grille: Grille):Unit={
    val MaxXColonne : Int = maxX(grille)
    val MaxYLigne = maxY(grille)
    val MinXColonne = minX(grille)
    val MinYLigne = minY(grille)
   //println("\nMinXColonne: " + MinXColonne + "\nMinYLigne : " +MinYLigne+"\nMaxXColonne"+MaxXColonne + "\nMaxYLigne"+MaxYLigne )
    def aux(colonne:Int, ligne:Int):Unit={
        if(ligne>MaxYLigne){
        }else{
            if(colonne>MaxXColonne){
                print("\n")
                aux(MinXColonne,ligne+1)
            }else{
                //println("\n Ligne: "+ligne + " Colonne: "+colonne)
                if(estDedans(grille,(ligne,colonne))){
                    print("X")
                    aux(colonne+1,ligne)
                }else{
                    print("_")
                    aux(colonne+1,ligne)
                }
            }
        }
    }
    aux(MinXColonne,MinYLigne)
}
afficherGrille(t)

//question3
def voisine8(x:Int,y:Int):List[(Int, Int)]={
    List((x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1))
}
//question 4


def combientSontDedans(g:Grille,test:Grille, acc:Int): Int = test match{
    case Nil => acc
    case t::q =>
        if(estDedans(g,t)){
            combientSontDedans(g,q,acc+1)
        } else {
            combientSontDedans(g,q,acc)
        }
}
def survivante(grille:Grille): Grille = {
    def aux1(grilleRes: Grille, acc: Grille): Grille = grilleRes match {
        case Nil => acc
        case t::q =>
            val (x,y)=t
            if(aux2(voisine8(x, y)) == 2 || aux2(voisine8(x, y))==3 ) aux1(q, acc.concat(t::Nil))
            else aux1(q,acc)

    }

    def aux2(l:List[(Int, Int)]): Int = {
        l.intersect(grille).length
    }

    aux1(grille, List[(Int, Int)]())
}


def candidate(grille: Grille): Grille = {
    def aux1(grille: Grille, acc: Grille): Grille = grille match {
        case Nil => acc
        case t::q =>
            val (x,y)=t
            if(aux2(voisine8(x, y)) == 2 || aux2(voisine8(x, y)) == 3 )aux1(q, acc)
            else aux1(q, acc.concat(t::Nil))
    }

    def aux2(l:List[(Int, Int)]): Int = {
        l.intersect(grille).length
    }

    aux1(grille, List[(Int, Int)]())
}


def naissances(grille: Grille): Grille ={
    def aux1(grilleRes: Grille, acc: Grille): Grille = grilleRes match {
        case Nil => acc
        case t::q =>
            if(aux2(candidate(grille)) == 3) aux1(q, acc.concat(t::Nil))
            else aux1(q, acc)

    }

    def aux2(l:List[(Int, Int)]): Int = {
        l.intersect(grille).length
    }

    aux1(grille, List[(Int, Int)]())
}


//grille initialise affiche en fonction d'un nb d'étapes n et
// affiche n itérations de la simulation
def jeuDeLaVie(dep: Grille, n: Int): Unit = {
    println("Etape suivante : ")
    afficherGrille(dep)
    if (n > 0) {
        jeuDeLaVie((survivante(dep) ++ naissances(dep)), n - 1)
    }
}

jeuDeLaVie(t,4)

def voisine4(l:Int, c:Int):List[(Int,Int)] = {
    List((l, c-1),(l-1, c),(l, c+1),(l+1, c))
}

def naisJdlv(nbVoisines:Int):Boolean={
    nbVoisines == 3
}
def survisJdlv(nbVoisines:Int): Boolean = {
    nbVoisines == 3 || nbVoisines == 2
}
def naisFredkin(nbVoisines:Int):Boolean={
    nbVoisines == 3 || nbVoisines == 1
}
def survisFredkin(nbVoisines:Int) : Boolean = {
    nbVoisines == 2 || nbVoisines == 4
}


def survivanteG(grille: Grille, voisines:(Int,Int)=>List[(Int,Int)], regles:Int=>Boolean): Grille = {
    def aux1(grilleRes: Grille, acc: Grille): Grille = grilleRes match {
        case Nil => acc
        case t::q =>{
            val (x,y)=t
            if (regles(aux2(voisines(x, y))))aux1(q, acc.concat(t::Nil))
            else aux1(q, acc)
        }

    }

    def aux2(l:List[(Int, Int)]): Int = {
        l.intersect(grille).length
    }

    aux1(grille, List[(Int, Int)]())
}



def candidateG(grille:Grille, voisines:(Int,Int)=>List[(Int,Int)], regles:Int=>Boolean): Grille = {
    def aux1(grilleRes: Grille, acc: Grille): Grille = grille match {
        case t::q =>{
            val (x,y)=t
            if (regles(aux2(voisines(x,y)))) aux1(q, acc)
            else aux1(q, acc:::t::Nil)
        }
        case Nil => acc
    }

    def aux2(l:List[(Int, Int)]): Int = {
        l.intersect(grille).length
    }

    aux1(grille, List[(Int, Int)]())
}


def naissancesG(grille: Grille, voisines:(Int,Int)=>List[(Int,Int)], reglejdlv:Int=>Boolean, regleNait:Int=>Boolean): Grille ={
    @scala.annotation.tailrec
    def aux1(grilleRes: Grille, acc: Grille): Grille = grilleRes match {
        case Nil => acc
        case t::q=>{
            if(regleNait(aux2(candidateG(grille, voisines, reglejdlv))))aux1(q, acc.concat(t::Nil))
            else aux1(q, acc)
        }
    }

    def aux2(l:List[(Int, Int)]): Int = {
        l.intersect(grille).length
    }

    aux1(grille, List[(Int, Int)]())
}