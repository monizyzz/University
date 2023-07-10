int while_loop(int x, int y, int n)
{
    do {
        x += n;
        y *= n;
        n--;
    } while (n > 0);
    return (x+y);
}

int main () {
    int x = 4, y = 2, n = 3, r;
    r = while_loop(x, y, n);
    return r;
}