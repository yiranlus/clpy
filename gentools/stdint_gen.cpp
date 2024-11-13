#include <vector>
#include <algorithm>
#include <iostream>
using namespace std;

vector<int> used_size;

void outdef(ostream &out, const char *type, const int &size) {
    static bool Py_ssize_t_genned = false;
    static bool ptr_t_genned = false;
    if (find(used_size.begin(), used_size.end(), size) != used_size.end())
        return;
    out << "typedef signed "   << type << " int"  << size*8 << "_t;" << endl;
    out << "typedef unsigned " << type << " uint" << size*8 << "_t;" << endl;
    if (!ptr_t_genned && sizeof(int*) == size) {
        out << "typedef signed "   << type << " intptr_t;" << endl;
        out << "typedef unsigned " << type << " uintptr_t;" << endl;
    }
    if (!Py_ssize_t_genned && sizeof(size_t) == size) {
        out << "typedef unsigned "   << type << " size_t;" << endl;
        out << "typedef signed "   << type << " Py_ssize_t;" << endl;
        Py_ssize_t_genned = true;
    }
    used_size.push_back(size);
}

#define OUTDEF(type) outdef(cout, #type, sizeof(type))

int main() {
    OUTDEF(char);
    OUTDEF(short);
    OUTDEF(int);
    OUTDEF(long);
    OUTDEF(long long);
    return 0;
}
