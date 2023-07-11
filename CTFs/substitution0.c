#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main (){

    char a_lower[] = "abcdefghijklmnopqrstuvwxyz";
    char a_upper[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    char b_lower[] = "vouhmjlteszcdkwixnqyfapgbr";
    char b_upper[] = "VOUHMJLTESZCDKWIXNQYFAPGBR";

    char frase[] = "Tmnmfiwk Cmlnvkh vnwqm, peyt v lnvam vkh qyvymcb ven, vkh onwflty dm ytm ommycm jnwd v lcvqq uvqm ek pteut ey pvq mkucwqmh. Ey pvq v omvfyejfc quvnvovmfq, vkh, vy ytvy yedm, fkzkwpk yw kvyfnvceqyq—wj uwfnqm v lnmvy inerm ek v quemkyejeu iweky wj aemp. Ytmnm pmnm ypw nwfkh ocvuz qiwyq kmvn wkm mgynmdeyb wj ytm ovuz, vkh v cwkl wkm kmvn ytm wytmn. Ytm quvcmq pmnm mgummheklcb tvnh vkh lcwqqb, peyt vcc ytm viimvnvkum wj ofnkeqtmh lwch. Ytm pmelty wj ytm ekqmuy pvq amnb nmdvnzvocm, vkh, yvzekl vcc yteklq ekyw uwkqehmnvyewk, E uwfch tvnhcb ocvdm Sfieymn jwn teq wiekewk nmqimuyekl ey. Ytm jcvl eq: ieuwUYJ{5FO5717F710K_3A0CF710K_357OJ9JJ}"; 

    for(int i = 0; i < strlen(frase); i++) {

        int j;
        if(frase[i] >= 65 && frase[i] <= 90){
        
            for(j = 0; frase[i] != b_upper[j]; j++);
            frase[i] = a_upper[j];

        }
        if(frase[i] >= 97 && frase[i] <= 122){
        
            for(j = 0; frase[i] != b_lower[j]; j++);
            frase[i] = a_lower[j];

        }
    }
    printf("%s\n", frase);

    return 0;
}