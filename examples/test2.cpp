#include <random> 
#include <string> 
#include <iostream> 
#include <random> 
#include <sstream> 
#include <cctype> 
#include <array> 

int INT(const float& num1) { 
    return (int)num1; 
} 

float RANDOM() { 
    return std::rand(); 
} 

// STRING FUNCTIONS 

std::string MID(const std::string& ThisString, int x, int y) { 
    return ThisString.substr(x-1, y); 
} 

int LENGTH(const std::string& ThisString) { 
    return ThisString.length(); 
} 

std::string SUBSTRING(const std::string& ThisString, int start, int end) { 
    return ThisString.substr(start-1, end-start+1); 
} 

std::string LEFT(const std::string& ThisString, int x) { 
    return ThisString.substr(0, x); 
} 

std::string RIGHT(const std::string& ThisString, int x) { 
    return ThisString.substr(ThisString.length() - x, x); 
} 

char LCASE(char ThisChar) { 
    return std::tolower(ThisChar); 
} 

char UCASE(char ThisChar) { 
    return std::toupper(ThisChar); 
} 

std::string TO_UPPER(const std::string& ThisString) { 
    std::string result = ThisString; 
    for (char& c : result) { 
        c = std::toupper(c); 
    } 
    return result; 
} 

std::string TO_LOWER(const std::string& ThisString) { 
    std::string result = ThisString; 
    for (char& c : result) { 
        c = std::tolower(c); 
    } 
    return result; 
} 

std::string NUM_TO_STRING(double x) { 
    std::ostringstream oss; 
    oss << x; 
    return oss.str(); 
} 

double STRING_TO_NUM(const std::string& x) { 
    return std::stod(x); 
} 

int ASC(char ThisChar) { 
    return static_cast<int>(ThisChar); 
} 

char CHR(int x) { 
    return static_cast<char>(x); 
}

float co(std::string x) {
    return (1.0 + (4.5 * 13));
}

signed main() {
    std::cout << co("hello world") << std::endl;
    return 0; 
}
