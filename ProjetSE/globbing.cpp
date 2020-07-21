#include "globbing.hpp"
#include <unistd.h>

using namespace std;

void strcpy_list(list<string> liste, char** resultat){
    
    int cpt = 0;
    char temp[50][40]; //50 mots de 40 lettres
    
    for(string courant: liste){
        strcpy(temp[cpt], courant.c_str());
        resultat[cpt] = temp[cpt];
        cpt++;
    }

}

int globber(command_t commande, char** pret_pour_execution){
    
    glob_t globbuf;
    char* temp = new char[50];
    bool premier = true;
    
    for(string courant: commande){
        strcpy(temp, courant.c_str());
        if(premier){
            glob(temp, GLOB_NOCHECK, NULL, &globbuf);
            premier = false;
            cout << globbuf.gl_pathc << endl;
        }else{
            glob(temp, GLOB_NOCHECK | GLOB_APPEND, NULL, &globbuf);
            cout << globbuf.gl_pathc << endl;
        }
    }
    for(int i = 0; i < globbuf.gl_pathc ; i++){
        pret_pour_execution[i] = globbuf.gl_pathv[i];
    } 
    return globbuf.gl_pathc;
}

void executer(command_t commande){

    char** resultat = new char*[50];
    globber(commande, resultat);
    execvp(resultat[0], resultat);


}
