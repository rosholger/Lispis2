#include <cxxtest/TestSuite.h>
#include <vm.h>

class Tests : public CxxTest::TestSuite {
public:
    void testIntern() {
        {
            VM vm;
            Symbol a1 = intern(&vm, "define");
            Symbol b1 = intern(&vm, "lambda");
            Symbol c1 = intern(&vm, "foo");
            Symbol d1 = intern(&vm, "bar");
            Symbol e1 = intern(&vm, "baz");
            Symbol f1 = intern(&vm, "asda");
            Symbol g1 = intern(&vm, "AAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
            Symbol h1 = intern(&vm, "oloads");
            Symbol i1 = intern(&vm, "fallingo");
            Symbol j1 = intern(&vm, "poaisd");
            Symbol k1 = intern(&vm, "kjgasd");
            Symbol l1 = intern(&vm, "fadadada");
            Symbol m1 = intern(&vm, "foobar");
            Symbol n1 = intern(&vm, "fahjs");
            Symbol o1 = intern(&vm, "jaah");
            
            Symbol a2 = intern(&vm, "define");
            Symbol b2 = intern(&vm, "lambda");
            Symbol c2 = intern(&vm, "foo");
            Symbol d2 = intern(&vm, "bar");
            Symbol e2 = intern(&vm, "baz");
            Symbol f2 = intern(&vm, "asda");
            Symbol g2 = intern(&vm, "AAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
            Symbol h2 = intern(&vm, "oloads");
            Symbol i2 = intern(&vm, "fallingo");
            Symbol j2 = intern(&vm, "poaisd");
            Symbol k2 = intern(&vm, "kjgasd");
            Symbol l2 = intern(&vm, "fadadada");
            Symbol m2 = intern(&vm, "foobar");
            Symbol n2 = intern(&vm, "fahjs");
            Symbol o2 = intern(&vm, "jaah");
            ETS_ASSERT_EQUALS(a1.id, a2.id);
            ETS_ASSERT_EQUALS(b1.id, b2.id);
            ETS_ASSERT_EQUALS(c1.id, c2.id);
            ETS_ASSERT_EQUALS(d1.id, d2.id);
            ETS_ASSERT_EQUALS(e1.id, e2.id);
            ETS_ASSERT_EQUALS(f1.id, f2.id);
            ETS_ASSERT_EQUALS(g1.id, g2.id);
            ETS_ASSERT_EQUALS(h1.id, h2.id);
            ETS_ASSERT_EQUALS(i1.id, i2.id);
            ETS_ASSERT_EQUALS(j1.id, j2.id);
            ETS_ASSERT_EQUALS(k1.id, k2.id);
            ETS_ASSERT_EQUALS(l1.id, l2.id);
            ETS_ASSERT_EQUALS(m1.id, m2.id);
            ETS_ASSERT_EQUALS(n1.id, n2.id);
            ETS_ASSERT_EQUALS(o1.id, o2.id);
            ETS_ASSERT_DIFFERS(a1.id, b2.id);
            ETS_ASSERT_DIFFERS(b1.id, c2.id);
            ETS_ASSERT_DIFFERS(c1.id, d2.id);
            ETS_ASSERT_DIFFERS(d1.id, e2.id);
            ETS_ASSERT_DIFFERS(e1.id, f2.id);
            ETS_ASSERT_DIFFERS(f1.id, g2.id);
            ETS_ASSERT_DIFFERS(g1.id, h2.id);
            ETS_ASSERT_DIFFERS(h1.id, i2.id);
            ETS_ASSERT_DIFFERS(i1.id, j2.id);
            ETS_ASSERT_DIFFERS(j1.id, k2.id);
            ETS_ASSERT_DIFFERS(k1.id, l2.id);
            ETS_ASSERT_DIFFERS(l1.id, m2.id);
            ETS_ASSERT_DIFFERS(m1.id, n2.id);
            ETS_ASSERT_DIFFERS(n1.id, o2.id);
            ETS_ASSERT_DIFFERS(o1.id, a2.id);
            freeVM(&vm);
        }
    }

    void test2Intern() {
        {
            VM vm;
            Symbol a1 = intern(&vm, "and");
            Symbol o1 = intern(&vm, "or");
            Symbol lt1 = intern(&vm, "<");
            Symbol d1 = intern(&vm, "define");
            Symbol tf21 = intern(&vm, "testFunc2");
            Symbol l1 = intern(&vm, "lambda");
            Symbol b1 = intern(&vm, "b");
            Symbol tf1 = intern(&vm, "testFunc");
            Symbol tf22 = intern(&vm, "testFunc2");
            Value v1 = {V_STRING};
            v1.str = tf21.str;
            Value v2 = {V_STRING};
            v2.str = tf22.str;
            ETS_ASSERT_EQUALS(hashValue(v1), hashValue(v2));
            ETS_ASSERT_EQUALS(tf21.id, tf22.id);
            freeVM(&vm);
        }
    }

    void testBoolean() {
        VM vm;
        doString(&vm, "true", 0, false);
        bool ret = popBoolean(&vm);
        ETS_ASSERT(ret);
        doString(&vm, "false", 0, false);
        ret = popBoolean(&vm);
        ETS_ASSERT(!ret);
    }

#define EPSILON 0.0000000000000001
#define COMP_DOUB(d, v) (((d) - v) < EPSILON && ((d) - v) > -EPSILON)

    void testDouble() {
        VM vm;
        doString(&vm, "0", 0, false);
        double ret = popDouble(&vm);
        ETS_ASSERT(COMP_DOUB(ret, 0));
        doString(&vm, "1.0", 0, false);
        ret = popDouble(&vm);
        ETS_ASSERT(COMP_DOUB(ret, 1.0));
        doString(&vm, "2000.0", 0, false);
        ret = popDouble(&vm);
        ETS_ASSERT(COMP_DOUB(ret, 2000.0));
        doString(&vm, "-2000.0", 0, false);
        ret = popDouble(&vm);
        ETS_ASSERT(COMP_DOUB(ret, -2000.0));
        doString(&vm, "-.0", 0, false);
        ret = popDouble(&vm);
        ETS_ASSERT(COMP_DOUB(ret, 0));
        doString(&vm, "-0.5", 0, false);
        ret = popDouble(&vm);
        ETS_ASSERT(COMP_DOUB(ret, -0.5));
    }

    void testQuote() {
        VM vm;
        {
            doString(&vm, "(quote a)", 0, false);
            Symbol ret = popSymbol(&vm);
            ETS_ASSERT_EQUALS(ret, intern(&vm, "a"));
        }
        {
            doString(&vm, "(quote true)", 0, false);
            bool ret = popBoolean(&vm);
            ETS_ASSERT(ret);
        }
        {
            doString(&vm, "(quote 0.5)", 0, false);
            double ret = popDouble(&vm);
            ETS_ASSERT(COMP_DOUB(ret, 0.5));
        }

        // Turn on when we can quote lists
        if (false) {
            doString(&vm, "(quote (a b))", 0, false);
            dup(&vm); // '(a b) '(a b)
            car(&vm); // '(a b) a
            Symbol a = popSymbol(&vm); // '(a b)
            ETS_ASSERT_EQUALS(a, intern(&vm, "a"));
            cdr(&vm); // '(b)
            dup(&vm); // '(b) '(b)
            car(&vm); // '(b) b
            Symbol b = popSymbol(&vm); // '(b)
            ETS_ASSERT_EQUALS(b, intern(&vm, "b"));
            cdr(&vm); // '()
            ETS_ASSERT(isEmptyList(&vm));
        }
    }
};