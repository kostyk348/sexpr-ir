/* Сгенерировано из IR: три функции VibeDarling, скомпилированные нативно.
   Нужно только для замера альтернативы: сколько стоит цикл
   «написать C → скомпилировать → слинковать → запустить тест». */

#include <stdio.h>

static long long objc_constant_array_object_at_index(long long index, long long count)
{
    if (index >= count) {
        return -1;
    }
    return index;
}

static long long objc_bytecount_thousand(long long style)
{
    if (style == 0 || style == 1) {
        return 1024;
    }
    return 1000;
}

static long long objc_bytecount_fraction_digits(long long unit_idx, long long adaptive)
{
    if (!(adaptive == 0)) {
        if (unit_idx <= 1) {
            return 0;
        }
        if (unit_idx == 2) {
            return 1;
        }
        return 2;
    }
    return 2;
}

static int failures;

static void check(const char *name, long long got, long long want)
{
    if (got != want) {
        failures++;
        printf("  %s: got %lld, want %lld  FAIL\n", name, got, want);
    } else {
        printf("  %s: %lld  ok\n", name, got);
    }
}

int main(void)
{
    check("index 0/3", objc_constant_array_object_at_index(0, 3), 0);
    check("index 2/3", objc_constant_array_object_at_index(2, 3), 2);
    check("index 3/3", objc_constant_array_object_at_index(3, 3), -1);
    check("index 4/3", objc_constant_array_object_at_index(4, 3), -1);
    check("index 0/0", objc_constant_array_object_at_index(0, 0), -1);
    check("style 0", objc_bytecount_thousand(0), 1024);
    check("style 1", objc_bytecount_thousand(1), 1024);
    check("style 2", objc_bytecount_thousand(2), 1000);
    check("style 3", objc_bytecount_thousand(3), 1000);
    check("unit 0 adaptive", objc_bytecount_fraction_digits(0, 1), 0);
    check("unit 1 adaptive", objc_bytecount_fraction_digits(1, 1), 0);
    check("unit 2 adaptive", objc_bytecount_fraction_digits(2, 1), 1);
    check("unit 3 adaptive", objc_bytecount_fraction_digits(3, 1), 2);
    check("unit 0 fixed", objc_bytecount_fraction_digits(0, 0), 2);
    check("unit 7 fixed", objc_bytecount_fraction_digits(7, 0), 2);

    printf("итог: %s\n", failures ? "есть провалы" : "все случаи прошли");
    return failures != 0;
}
