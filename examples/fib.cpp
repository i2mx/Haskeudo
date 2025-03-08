#include <iostream>
#include <string>
#include <array>

signed main() {
    int n;
    std::cin >> n;
    for (int i = 0; i <= n; i++) {
        // calculates the i-th fibonacci number
        int f0;
        int f1;
        int f2;
        f0 = 0;
        f1 = 1;
        for (int j = 1; j <= i; j++) {
            f2 = f0;
            f0 = f1;
            f1 = (f0 + f2);
        }
        std::cout << f0 << std::endl;
    }
    return 0; 
}
