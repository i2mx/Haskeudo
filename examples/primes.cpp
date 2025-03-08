#include <iostream>
#include <string>
#include <array>

signed main() {
    // finding all primes numbers up to 1000
    std::array<bool,1001> composite;
    for (int i = 1; i <= 1000; i++) {
        composite[i] = false;
    }
    for (int i = 2; i <= 1000; i++) {
        if ((!composite[i])) {
            std::cout << i << std::endl;
            for (int j = (i * i); j <= 1000; j+= i) {
                composite[j] = true;
            }
        }
    }
    return 0; 
}
