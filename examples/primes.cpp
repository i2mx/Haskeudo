#include <iostream>
#include <string>
#include <array>

signed main() {
    // finding all primes numbers up to 500
    std::array<bool,501> composite;
    for (int i = 1; i <= 500; i++) {
        composite[i] = false;
    }
    for (int i = 2; i <= 500; i++) {
        if ((!composite[i])) {
            std::cout << i << std::endl;
            for (int j = (i * i); j <= 500; j+= i) {
                composite[j] = true;
            }
        }
    }
    return 0; 
}
