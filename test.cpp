#include <iostream>
#include <string>
#include <array>

signed main() {
    std::array<int,11> arr;
    for (int i = 1; i <= 10; i++) {
        int x;
        std::cin >> arr[i];
        std::cout << arr[i] << (arr[i] * arr[i]) << std::endl;
    }
    return 0; 
}
