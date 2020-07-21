#include "bloc.hpp"

//constructeur d'un bloc vide
bloc::bloc(){}
//destructeur
bloc::~bloc(){}
		
//ajoute la variable nommée c, ayant pour valeur V
//retourne 0 si la déclaration s'est bien passée, 1 sinon
int bloc::declarer(string c, int v){
	int error = 0; //0 pour succès
	if(bloc::est_dans(c)){
		error = 1;
	}else{
		table.insert(t_entree(c,v));
		file.push(c);
	}
	return error;
}

//renvoie vraie si la variable c est dans le bloc
bool bloc::est_dans(string c){
	return !(table.end() == table.find(c));
}

//renvoie la valeur associée à la variable c
//Pré-condition : c est présente dans le bloc
int bloc::lecture(string c){
	return (table.find(c)->second);
}

//modifie en v la valeur de la variable c
//Pré-condition : c est présente dans le bloc
void bloc::modifier(string c, int v){
	table.find(c)->second = v;
}

//liste les variables du bloc par ordre chronologique d'ajout
void bloc::listeVariables(){
	string variable;
	std::cout << "Variables du bloc : ";
	while (!(file.empty())){
		variable = file.front();
		std::cout << variable << "=" << table.find(variable)->second << ";";
		file.pop();
	}
	std::cout << std::endl;
}
