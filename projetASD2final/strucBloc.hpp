#include <iostream>
#include <list>
#include <iterator>
#include "bloc.hpp"

using namespace std;


class strucBloc{
public:
	//constructeur d'une structure de blocs vide
	strucBloc();

	//destructeur
	~strucBloc();

	//affiche la valeur associée à la variable c. Affiche un message d'erreur si non trouvée
	void afficher(string c);


	//ajoute un nouveau bloc dans blockchain (au début)
	void ajouterBloc();

	//retire le bloc en début de blockchain
	void retirerBloc();

	//appelle la fonction "declarer" du dernier bloc ajouté
	//retourne 0 si la déclaration s'est bien passée, 1 sinon
	int declarer(string c, int v);
	
	//incrémente la valeur associée à la variable c
	//retourne 0 si l'incrémentation s'est bien passée, 1 sinon
	int incrementer(string c);

	//appelle la fonction "listeVariables" du dernier bloc ajouté
	void listeVariables();





private:

	list<bloc> blockchain;

};
