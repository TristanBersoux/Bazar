import numpy as np

#On applique juste les formules du cours
def LU_Doolittle(mat,n): 
  
    L = [[0 for x in range(n)] for y in range(n)] 
    U = [[0 for x in range(n)] for y in range(n)]

    #Construire les matrices de cette manière cause des arrondis différents de 0.
    #L = np.empty(n*n, dtype=float)
    #U = np.empty(n*n, dtype=float)
    #L = L.reshape(n,n)
    #U = U.reshape(n,n)

    for i in range(n): 
  
        #Construction de U

        for k in range(i,n):  
            #Somme des Lij*Ujk
            somme = 0 
            for j in range(i): 
                somme = somme + (L[i][j] * U[j][k]) 
  
            #Calcul de Uik
            U[i][k] = mat[i][k] - somme 
  
        #Construction de L
        
        for k in range(i,n): 
            if (i == k): #Si on est sur la diagonale de L
                L[i][i] = 1 
            else: 
  
                #Somme des Lkj*Uji
                somme = 0 
                for j in range(i): 
                    somme = somme + (L[k][j]*U[j][i]) 
  
                #Calcul de Lki
                L[k][i] = (mat[k][i] - somme)/U[i][i]
  
    print("On a L")
    for i in range(n):
        print(L[i])
    print("Et U")
    for i in range(n):
        print(U[i])
  
#test
mat = [[5, 2, 2], 
       [1, 6, 2], 
       [2, 4, 3]] 
  
LU_Doolittle(mat, 3) 