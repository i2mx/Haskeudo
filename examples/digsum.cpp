#include <iostream>
#include <string>
#include <array>

signed main() {
    int x;
    std::cin >> x;
    int digsum;
    digsum = 0;
    while ((x > 0)) {
        digsum = (digsum + (x % 10));
        x = (x / 10);
    }
    for (int i = 1; i <= 1; i++) {
        std::cout << "sum = " << digsum << std::endl;
    }
    return 0; 
}
