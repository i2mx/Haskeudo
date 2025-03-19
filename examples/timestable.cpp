#include <iostream>
#include <string>
#include <array>

signed main() {
    int n;
    std::cin >> n;
    for (int i = 1; i <= n; i++) {
        std::cout << n << " * " << i << " = " << (n * i) << std::endl;
    }
    return 0; 
}
