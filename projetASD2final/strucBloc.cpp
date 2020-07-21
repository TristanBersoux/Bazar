#include "strucBloc.hpp"


//constructeur d'une structure de blocs vide
strucBloc::strucBloc(){}

//destructeur
strucBloc::~strucBloc(){}


//affiche la valeur associée à la variable c. Renvoie un message d'erreur si non trouvée
void strucBloc::afficher(string c){
	bloc monBloc;
	auto iterator = blockchain.begin();
	while( (iterator != blockchain.end()) && !(monBloc.est_dans(c))){
		monBloc = *iterator;
		advance(iterator,1);
	}
	if(monBloc.est_dans(c)){
		cout << "| " << c << " = " << monBloc.lecture(c)<< endl;
	}else{cout <<"| error: " << c << " not found!" <<endl <<endl;}
}

//ajoute un nouveau bloc dans blockchain (au début)
void strucBloc::ajouterBloc(){
	bloc nouveau;
	blockchain.push_front(nouveau);
}


//retire le bloc en début de blockchain
void strucBloc::retirerBloc(){
	blockchain.pop_front();
}


//appelle la fonction "declarer" du dernier bloc ajouté
int strucBloc::declarer(string c, int v){
	int erreur;
	erreur = blockchain.front().declarer(c,v);
	return erreur;
}

//incrémente la valeur associée à la variable c
//retourne 0 si l'incrémentation s'est bien passée, 1 sinon
int strucBloc::incrementer(string c){
	bloc monBloc;
	int erreur = 0;
	auto iterator = blockchain.begin();
	while( (iterator != blockchain.end()) && !(monBloc.est_dans(c))){
		monBloc = *iterator;
		advance(iterator,1);
	}
	if(monBloc.est_dans(c)){
		monBloc.modifier(c,monBloc.lecture(c)+1);
		blockchain.emplace(iterator,monBloc);
		advance(iterator,-2);
		blockchain.erase(iterator);
	
	}else{erreur=1;}
	return erreur;
}

//appele la fonction "listeVariables" du dernier bloc ajouté
void strucBloc::listeVariables(){
	blockchain.front().listeVariables();
}
	
