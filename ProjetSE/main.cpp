#include "globbing.hpp"
#include "parsing.hpp"
#include <iostream>
#include "parsing.hpp"
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdio.h>
#include <vector>

using namespace std;

int main(){
    
    string line;
    getline(cin, line);
    vector<compound_command_t> commands;
    parse_line(line, commands);
    
    for(auto compound_command : commands){
        vector<pid_t> processus;	
        for(auto simple_command : compound_command){
            fork();
            if (pid==0){
                executer(simple_command);
            }else{
                processus.push_back(pid);
            }    
        }
        for (auto pid : processus){
	 		wait(0);
	 	}	
    }
    
    /*int nb = globber(l_test, test);
    
    for(int i = 0; i < nb; i++){
        cout << test[i] << endl;
    }*/
    
    return EXIT_SUCCESS;

}
