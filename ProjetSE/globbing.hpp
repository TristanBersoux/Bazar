#include <glob.h>
#include <string>
#include <list>
#include <cstring>
#include <iostream>
#include "parsing.hpp"

//void strcpy_list(std::list<std::string> liste, char** resultat);

int globber(command_t commande, char** pret_pour_execution);

void executer(command_t commande);
