
import std::io

public export method main():
    io::println("Boolean-Expressions Test")
    bool a = true
    bool b = false
    bool c = null

    io::print("a = ")
    io::println(a)
    io::print("b = ")
    io::println(b)
    io::print("c = ")
    io::println(c)

    bool x = a && b
    bool y = a || b
    bool z = (a && b) || (b && a)

    io::println()
    io::println("x = a && b")
    io::println("y = a || b")
    io::println("z = (a && b) || (b && a)")

    io::print("x = ")
    io::println(x)
    io::print("y = ")
    io::println(y)
    io::print("z = ")
    io::println(z)

    x = a && c
    io::println()
    io::println("x = a && c")
    io::print("x = ")
    io::println(x)
