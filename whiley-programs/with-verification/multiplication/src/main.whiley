
import std::io

public export method main():
    io::println("Multiplication")
    int x = 10
    int y = 3
    int f = mul(x, y)
    io::print("mul(")
    io::print(x)
    io::print(", ")
    io::print(y)
    io::print(") = ")
    io::println(f)

function mul(int a, int b) -> (int result)
requires a != 0
requires b != 0
ensures result >= a || -result >= a
ensures result >= b || -result >= b:
    return a * b
