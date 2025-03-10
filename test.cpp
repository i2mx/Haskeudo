#include <iostream>
#include <string>
#include <array>

int s() {
    return 1;
}
int a(int x) {
    return x;
}
void greet(std::string name) {
    std::cout << "Hello " << name << std::endl;
}
signed main() {
    std::cout << s() << std::endl;
    (void) greet("World");
    return 0; 
}
