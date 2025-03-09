#include <iostream>
#include <string>
#include <array>

signed main() {
    std::string Password;
    std::cout << "Enter password: " << std::endl;
    std::cin >> Password;
    if ((Password == "admin")) {
        std::cout << "Access granted" << std::endl;
    }
    else {
        std::cout << "Access denied" << std::endl;
    }
    return 0;
}
