#include <iostream>
#include <string>
#include <array>

int square(int x) {
    return (x * x);
}
signed main() {
    std::cout << (square(3) + square(4)) << std::endl;
    return 0;
}
