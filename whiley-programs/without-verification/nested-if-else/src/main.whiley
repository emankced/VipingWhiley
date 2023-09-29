
import std::io

public export method main():
    io::println("Nested If-Else Test")
    int x = 42
    int y = 1337
    int z = 9000
    if (x + y) + z == x + (y + z):
        if (x + y) + z == x + (y + z):
            io::println("yay")
    else:
        io::println(":D")
        if (x + y) * z == x + (y * z):
            io::println("Oi")
            if true && z == 500:
                io::println("Nope")
            else:
                io::println("Else")
        else if (x + y) * z > x + (y * z):
            io::println("?")
