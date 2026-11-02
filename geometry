#include <iostream>
#include <cmath>
#include <vector>
#include <iomanip>
#include <algorithm>
#include <map>
#include <bitset>
#include <string>
#include <numeric>
#include <set>
#include <random>
#include <complex>
#include <ctime>
#include <stack>
#include <type_traits>

using namespace std;
using ll = long long;
using dbl = long double;
dbl eps = 1e-9;
dbl pi = acosl(-1);

//структура для точек и векторов
template <typename T>
struct pt {
    T x, y;

    pt() : x(0), y(0) {}

    template <typename T1>
    pt (const T1& x, const T1& y) : x(x), y(y) {}

    //вектор перпендикулярный данному
    pt perp() const {
        return pt{-y, x};
    }

    //длина вектора
    dbl lth() const {
        return hypot(x, y);
    }
};

//функция-фабрика для точек и векторов
//через координаты
template <typename T1, typename T2>
auto make_pt(const T1& x, const T2& y) {
    using tar_t = common_type_t<T1, T2>;
    pt<tar_t> t(x, y);
    return t;
}

//через точку начала и конца вектора
template <typename T1, typename T2>
auto make_pt(const pt<T1>& beg, const pt<T2>& end) {
    using tar_t = common_type_t<T1, T2>;
    pt<tar_t> t(end.x-beg.x, end.y-beg.y);
    return t;
}

//нахождение полярного угла точки
template <typename T>
dbl ang(const pt<T>& t) {
    dbl res = atan2(t.y, t.x);
    if (res < 0) {
        return res+2*pi;
    }
    return res;
}

//угол между двумя векторами
template <typename T1, typename T2>
dbl ang(pt<T1> a, pt<T2> b) {
    dbl angl = abs(ang(a)-ang(b));
    if (angl > pi) {
        angl = 2*pi-angl;
    }
    return angl;
}

//операции для векторов
//сложение
template <typename T1, typename T2>
auto operator+(const pt<T1>& t1, const pt<T2>& t2) {
    return make_pt((t1.x+t2.x), (t1.y+t2.y));
}

//вычитание
template <typename T1, typename T2>
auto operator-(const pt<T1>& t1, const pt<T2>& t2) {
    return make_pt((t1.x-t2.x), (t1.y-t2.y));
}

//умножение на константу
template <typename T1, typename T2>
auto operator*(const pt<T1>& t1, const T2& c) {
    return make_pt((t1.x*c), (t1.y*c));
}

template <typename T1, typename T2>
auto operator*(const T2& c, const pt<T1>& t1) {
    return make_pt((c*t1.x), (c*t1.y));
}

//деление на константу
template <typename T1, typename T2>
auto operator/(const pt<T1>& t1, const T2& c) {
    return make_pt((static_cast<dbl>(1)*t1.x/c), (static_cast<dbl>(1)*t1.y/c));
}

//скалярное произведение
template <typename T1, typename T2>
auto operator*(const pt<T1>& t1, const pt<T2>& t2) {
    return t1.x*t2.x+t1.y*t2.y;
}

//псевдоскалярное произведение
template <typename T1, typename T2>
auto operator&(const pt<T1>& t1, const pt<T2>& t2) {
    return t1.x*t2.y-t1.y*t2.x;
}

//равенство (работает и для векторов и для точек)
template <typename T1, typename T2>
bool operator==(const pt<T1>& t1, const pt<T2>& t2) {
    return ((abs(t1.x - t2.x) <= eps) && (abs(t1.y - t2.y) <= eps));
}

//неравенство (работает и для векторов и для точек)
template <typename T1, typename T2>
bool operator!=(const pt<T1>& t1, const pt<T2>& t2) {
    return !(t1 == t2);
}

//псевдоскалярное произведение меньше 0
template <typename T1, typename T2, typename T3>
bool cw(const pt<T1>& t1, const pt<T2>& t2, const pt<T3>& t3) {
    if ((make_pt(t1, t2)&make_pt(t1, t3)) < eps) {
        return true;
    }
    return false;
}

//псевдоскалярное произведение больше 0
template <typename T1, typename T2, typename T3>
bool ccw(const pt<T1>& t1, const pt<T2>& t2, const pt<T3>& t3) {
    if ((make_pt(t1, t2)&make_pt(t1, t3)) > -eps) {
        return true;
    }
    return false;
}

//площадь треугольника
template <typename T1, typename T2, typename T3>
auto s(const pt<T1>& t1, const pt<T2>& t2, const pt<T3>& t3) {
    return make_pt(t1, t2)&make_pt(t1, t3);
}

//класс для прямых
template <typename T = dbl>
struct line {
    T a, b, c;
    
    template <typename T1, typename T2>
    line (const T1& p1, const T2& p2) {
        a = p2.y-p1.y;
        b = p1.x-p2.x;
        c = p1.y*p2.x-p2.y*p1.x;
    }

    line() : a(0), b(0), c(0) {}

    line(const T& a, const T& b, const T& c) : a(a), b(b), c(c) {}

    //прямая перпендекулярная данной
    line perp() const{
        return line{-b, a, 0};
    }

    //прямая перпендикулярная данной и проходящая через заданную точку
    template <typename pt_t>
    auto perp(const pt_t& t) const{
        T perp_c = static_cast<T>((b*t.x)-(a*t.y));
        return line{-b, a, perp_c};
    }

    //проверка наличия точки на прямой
    template<typename pt_t>
    bool on(const pt_t& t) const {
        if (abs(static_cast<dbl>(a)*t.x+static_cast<dbl>(b)*t.y+c) > eps) return false;
        return true;
    }
};

//функции-фабрики для прямых
//через две точки
template <typename T1, typename T2>
auto make_line(const pt<T1>& t1, const pt<T2>& t2) {
    using tar_t = common_type_t<T1, T2>;
    line <tar_t> l(t1, t2);
    return l;
}

//через коэфициенты
template <typename T>
auto make_line(const T& a, const T& b, const T& c) {
    line <T> l(a, b, c);
    return l;
}

//операции с прямыми
//параллельность
template <typename T1, typename T2>
bool operator|| (const line<T1>& l1, const line<T2>& l2) {
    return abs((static_cast<dbl>(1)*l1.a*l2.b)-(static_cast<dbl>(1)*l1.b*l2.a)) < eps;
}

//равенство
template<typename T1, typename T2>
bool operator== (const line<T1>& l1, const line<T2>& l2) {
    if (!(l1||l2)) return false;
    return ((abs(l1.a*l2.c-l2.a*l1.c) < eps)&&(abs(l1.b*l2.c-l2.b*l1.c) < eps));
}

//точка пересечения двух прямых
template <typename T1, typename T2>
pt<dbl> operator* (const line<T1>& l1, const line<T2>& l2) {
    if (l1||l2) return {NAN, NAN};
    pt<dbl> res((static_cast<dbl>(1)*l2.c*l1.b-static_cast<dbl>(1)*l1.c*l2.b)/(static_cast<dbl>(1)*l1.a*l2.b-static_cast<dbl>(1)*l2.a*l1.b), (static_cast<dbl>(1)*l2.c*l1.a-static_cast<dbl>(1)*l1.c*l2.a)/(static_cast<dbl>(1)*l1.b*l2.a-static_cast<dbl>(1)*l2.b*l1.a));
    return res;
}

//структура для отрезков
template <typename T = dbl>
struct seg {
    pt<T> beg, end;

    template <typename T1, typename T2>
    seg(const T1& t1, const T2& t2) : beg(static_cast<T> (t1.x), static_cast<T> (t1.y)), end(static_cast<T> (t2.x), static_cast<T> (t2.y)) {}

    //проверка наличия точки на отрезке
    template<typename pt_t>
    bool on(const pt_t& t) const {
        auto l = make_line(beg, end);
        if (abs(static_cast<dbl>(l.a)*t.x+static_cast<dbl>(l.b)*t.y+l.c) > eps) return false;
        return (t.x >= min(beg.x, end.x)-eps && t.x <= max(beg.x, end.x)+eps && t.y >= min(beg.y, end.y)-eps && t.y <= max(beg.y, end.y)+eps);
    }
};

//функция-фабрика для отрезков
template <typename T1, typename T2>
auto make_seg(const pt<T1>& t1, const pt<T2>& t2) {
    using tar_t = common_type_t<T1, T2>;
    seg <tar_t> seg(t1, t2);
    return seg;
}

//операция приведения отрезка к прямой
template <typename T>
auto to_l(const seg<T>& t) {
    auto l = make_line(t.beg, t.end);
    return l;
}

//структура для лучей
template <typename T = dbl>
struct half_l {
    pt <T> beg;
    pt <T> dir;

    template <typename T1, typename T2>
    half_l (const pt<T1>& t1, const pt<T2>& t2) : beg(static_cast<T> (t1.x), static_cast<T> (t1.y)), dir(static_cast<T> (t2.x-t1.x), static_cast <T> (t2.y-t1.y)) {}

    //проверка наличия точки на луче
    template <typename pt_t>
    bool on(const pt_t& t) const {
        pt<T> end{beg.x+dir.x, beg.y+dir.y};
        auto l = make_line(beg, end);
        if (!l.on(t)) return false;
        auto dir1 = make_pt(t.x-beg.x, t.y-beg.y);
        if (dir1.x*dir.x+dir.y*dir1.y >= -eps) return true;
        return false;
    }
};

//функция-фабрика для лучей
template <typename T1, typename T2>
auto make_half_l(const pt<T1>& t1, const pt<T2>& t2) {
    using tar_t = common_type_t<T1, T2>;
    half_l <tar_t> half_l(t1, t2);
    return half_l;
}

//операция приведения луча к прямой
template <typename T>
auto to_l(const half_l<T>& t) {
    auto t1 = make_pt(static_cast<T> (t.beg.x+t.dir.x), static_cast<T> (t.beg.y+t.dir.y));
    auto l = make_line(t.beg, t1);
    return l;
}

//расстояния между примитивами
//точки
template <typename T1, typename T2>
dbl dist(const pt<T1>& t1, const pt<T2>& t2) {
    return hypot((t1.x-t2.x), (t1.y-t2.y));
}

template <typename T1, typename T2>
dbl dist(const pt<T1>& t, const line<T2>& l) {
    auto l1 = l.perp(t);
    auto t1 = l*l1;
    return dist(t, t1);
}

template <typename T1, typename T2>
dbl dist(const pt<T1>& t, const seg<T2>& pl) {
    auto l = make_line(pl.beg, pl.end);
    auto l1 = l.perp(t);
    auto t1 = l*l1;
    if (pl.on(t1)) return dist(t, t1);
    return min(dist(t, pl.beg), dist(t, pl.end));
}

template <typename T1, typename T2>
dbl dist(const pt<T1>& t, const half_l<T2>& hl) {
    auto l = to_l(hl);
    auto t1 = l*l.perp(t);
    if (hl.on(t1)) return dist(t, t1);
    return dist(t, hl.beg);
}

//отрезки

template <typename T1, typename T2>
dbl dist(const seg<T1>& pl, const pt<T2>& t) {
    return dist(t, pl);
}

template <typename T1, typename T2>
dbl dist(const seg<T1>& pl1, const seg<T2>& pl2) {
    auto l1 = to_l(pl1), l2 = to_l(pl2);
    if (!(l1 || l2)) {
        auto t = l1*l2;
        if (pl1.on(t) && pl2.on(t)) {
            return 0;
        }
    }
    return min(min(dist(pl1.beg, pl2), dist(pl1.end, pl2)), min(dist(pl2.beg, pl1), dist(pl2.end, pl1)));
}

template <typename T1, typename T2>
dbl dist(const seg<T1>& pl, const half_l<T2>& hl) {
    auto l1 = to_l(pl), l2 = to_l(hl);
    if (!(l1 || l2)) {
        auto t = l1*l2;
        if (pl.on(t) && hl.on(t)) {
            return 0;
        }
    }
    return min(min(dist(pl.beg, hl), dist(pl.end, hl)), dist(hl.beg, pl));
}

template <typename T1, typename T2>
dbl dist(const seg<T1>& pl, const line<T2>& l) {
    auto l1 = to_l(pl);
    if (!(l1 || l)) {
        auto t = l*l1;
        if (l.on(t) && pl.on(t)) {
            return 0;
        }
    }
    return min(dist(pl.beg, l), dist(pl.end, l));
}

//лучи

template <typename T1, typename T2>
dbl dist(const half_l<T1>& hl, const pt<T2>& t) {
    return dist(t, hl);
}

template <typename T1, typename T2>
dbl dist(const half_l<T1>& hl, const seg<T2>& pl) {
    return dist(pl, hl);
}

template <typename T1, typename T2>
dbl dist(const half_l<T1>& hl1, const half_l<T2>& hl2) {
    auto l1 = to_l(hl1), l2 = to_l(hl2);
    if (!(l1 || l2)) {
        auto t = l1*l2;
        if (hl1.on(t) && hl2.on(t)) {
            return 0;
        }
    }
    return min(dist(hl1.beg, hl2), dist(hl2.beg, hl1));
}

template <typename T1, typename T2>
dbl dist(const half_l<T1>& hl, const line<T2>& l) {
    auto l1 = to_l(hl);
    if (!(l1 || l)) {
        auto t = l*l1;
        if (l.on(t) && hl.on(t)) {
            return 0;
        }
    }
    return dist(hl.beg, l);
}

//прямые

template <typename T1, typename T2>
dbl dist(const line<T1>& l, const pt<T2>& t) {
    return dist(t, l);
}

template <typename T1, typename T2>
dbl dist(const line<T1>& l, const seg<T2>& pl) {
    return dist(pl, l);
}

template <typename T1, typename T2>
dbl dist(const line<T1>& l, const half_l<T2>& hl) {
    return dist(hl, l);
}

template <typename T1, typename T2>
dbl dist(const line<T1>& l1, const line<T2>& l2) {
    if (!(l1 || l2)) return 0;
    if (l1 == l2) return 0;
    //расстояние между параллельными прямыми
    dbl k = 1;
    if (abs(static_cast<dbl>(1)*l2.a) > eps) {
        k = static_cast<dbl>(1)*(l1.a)/l2.a;
    } else {
        k = static_cast<dbl>(1)*(l1.b)/l2.b;
    }
    dbl c2 = l2.c*k;
    dbl num = abs(static_cast<dbl>(1)*l1.c-c2);
    dbl den = hypot(l1.a, l1.b);
    return den < eps ? 0 : num/den;
}

//структура для окружностей
template <typename T>
struct circle {
    pt <T> c;
    T r;

    circle() : c(make_pt(static_cast<T>(0), static_cast<T>(0))), r(0) {}
    
    template <typename T1, typename T2>
    circle(const pt<T1>& t, const T2& r) : c(static_cast<T>(t.x), static_cast<T>(t.y)), r(r) {}

    //проверка наличия точки в окружности
    template <typename T1>
    bool in(const pt<T1>& t) const {
        return dist(t, c) <= r+eps;
    }

    //проверка наличия точки на окружности
    template <typename T1>
    bool on(const pt<T1>& t) const {
        return ((dist(t, c) >= r-eps) && (dist(t, c) <= r+eps));
    }
};

//функция-фабрика для окружности
template <typename T1, typename T2, typename T3>
auto make_circle(const T1& cx, const T2& cy, const T3& r) {
    using tar_t = common_type_t<T1, T2, T3>;
    auto t = make_pt(cx, cy);
    circle<tar_t> c1(t, r);
    return c1;
}

//функция-фабрика для окружности
template <typename T1, typename T2>
auto make_circle(const pt<T1>& t, const T2& r) {
    using tar_t = common_type_t<T1, T2>;
    circle<tar_t> c1(t, r);
    return c1;
}

//пересечение окружности и прямой
template <typename T1, typename T2>
vector <pt<dbl>> operator*(const circle<T1>& c, const line<T2>& l) {
    dbl lth = dist(c.c, l);
    if (lth > c.r-eps && lth < c.r+eps) {
        auto l1 = l.perp(c.c);
        return {l1*l};
    } else if (lth > c.r-eps) {
        return {};
    } else {
        auto l1 = l.perp(c.c);
        auto t = l*l1;
        auto t1 = make_pt(t, c.c);
        pt<dbl> r;
        if (abs(t1.lth()) < eps) {
            r = make_pt(static_cast<dbl>(-l.b), static_cast<dbl>(l.a));
        } else {
            r = t1.perp();
        }
        dbl lth1 = sqrtl(static_cast<dbl>(1)*c.r*c.r-static_cast<dbl>(1)*t1.lth()*t1.lth());
        r = r*lth1/r.lth();
        return{t+r, t-r};
    }
}

template <typename T1, typename T2>
vector <pt<dbl>> operator*(const line<T2>& l, const circle<T1>& c) {
    return c*l;
}

//пересечение отрезка и прямой
template <typename T1, typename T2>
vector <pt<dbl>> operator*(const circle<T1>& c, const seg<T2>& pl) {
    vector <pt<dbl>> ans1 = to_l(pl)*c;
    vector <pt<dbl>> ans2;
    for (auto a : ans1) {
        if (pl.on(a)) {
            ans2.push_back(a);
        }
    }
    return ans2;
}

template <typename T1, typename T2>
vector <pt<dbl>> operator*(const seg<T2>& pl, const circle<T1>& c) {
    return c*pl;
}

//пересечение двух окружностей
template <typename T1, typename T2>
vector <pt<dbl>> operator*(const circle<T1>& c1, const circle<T2>& c2) {
    if (c1.c == c2.c) {
        if (abs(static_cast<dbl>(1)*c1.r-static_cast<dbl>(1)*c2.r) <= eps) {
            return {{NAN, NAN}, {NAN, NAN}, {NAN, NAN}};
        }
        return {};
    }
    auto l = make_line(2*(static_cast<dbl>(1)*c2.c.x-static_cast<dbl>(1)*c1.c.x), 2*(static_cast<dbl>(1)*c2.c.y-static_cast<dbl>(1)*c1.c.y), static_cast<dbl>(1)*c1.c.y*static_cast<dbl>(1)*c1.c.y + static_cast<dbl>(1)*c1.c.x*static_cast<dbl>(1)*c1.c.x - static_cast<dbl>(1)*c2.c.y*static_cast<dbl>(1)*c2.c.y - static_cast<dbl>(1)*c2.c.x*static_cast<dbl>(1)*c2.c.x + static_cast<dbl>(1)*c2.r*static_cast<dbl>(1)*c2.r - static_cast<dbl>(1)*c1.r*static_cast<dbl>(1)*c1.r);
    return l*c1;
}

//касательная к окружности через точку
template <typename T1, typename T2>
vector <line <dbl>> tan_l(circle <T1>& c, pt <T2>& t) {
    if (c.on(t)) {
        auto l = make_line(c.c, t);
        auto l1 = l.perp(t);
        return {make_line(static_cast<dbl>(l1.a), static_cast<dbl>(l1.b), static_cast<dbl>(l1.c))};
    } else if (c.in(t)) {
        return {};
    } else {
        auto t1 = make_pt((static_cast<dbl>(1)*(c.c.x+t.x))/2, (static_cast<dbl>(1)*(c.c.y+t.y))/2);
        auto c1 = make_circle(t1.x, t1.y, dist(t1, t));
        vector <pt<dbl>> res = c1*c;
        return {make_line(res[0], t), make_line(res[1], t)};
    }
}

//длина сегмента окружности по точкам его конца на окружности(меньшего из двух)
template <typename T1, typename T2, typename T3>
dbl lth(const circle<T1>& c, const pt<T2>& t1, const pt<T3>& t2) {
    pt r1 = make_pt(c.c, t1), r2 = make_pt(c.c, t2);
    dbl ang = acosl(static_cast<dbl>(1)*r1*r2/(r1.lth()*r2.lth()));
    return c.r*ang;
}

//построение окружности по 3 точкам
template <typename T1, typename T2, typename T3>
auto circ_circle(const pt<T1>& t1, const pt<T2>& t2, const pt<T3>& t3) {
    pt<dbl> p1 = make_pt((dbl)t1.x, (dbl)t1.y);
    pt<dbl> p2 = make_pt((dbl)t2.x, (dbl)t2.y);
    pt<dbl> p3 = make_pt((dbl)t3.x, (dbl)t3.y);
    auto l = make_line(p1, p2);
    if (l.on(p3)) return make_circle(static_cast<dbl>(0), static_cast<dbl>(0), static_cast<dbl>(0));
    auto l1 = make_line(p1, p2).perp(make_pt((p1.x+p2.x)/2, (p1.y+p2.y)/2));
    auto l2 = make_line(p1, p3).perp(make_pt((p1.x+p3.x)/2, (p1.y+p3.y)/2));
    return make_circle(l1*l2, dist(l1*l2, p1));
}

//проверка равенства 2 окружностей
template <typename T1, typename T2>
bool operator==(const circle<T1>& c1, const circle<T2>& c2) {
    return (c1.c == c2.c) && (abs(c1.r-c2.r) <= eps);
}

//проверка неавенства 2 окружностей
template <typename T1, typename T2>
bool operator!=(const circle<T1>& c1, const circle<T2>& c2) {
    return !(c1 == c2);
}

template<typename T>
bool comp(pt<T> a, pt<T> b) {
    return a.x == b.x ? a.y < b.y : a.x < b.x;
}

template <typename T>
struct point_set {
    vector <pt<T>> pt_set;
    bool sort_it_coord = true;
    bool sort_it_ang = true;

    //дефолтный компоратор
    point_set() {}

    //добавление элемента
    //можно только того же типа T
    void push_back(const pt<T>& t) {
        if (!pt_set.empty()) {
            if (pt_set.back().x > t.x || (pt_set.back().x == t.x && pt_set.back().y > t.y)) sort_it_coord = false;
            dbl ang_t = ang(t);
            dbl ang_lst = ang(pt_set.back());
            if (ang_t < ang_lst) sort_it_ang = false;
        }
        pt_set.push_back(t);
    }

    //создание по вектору точек
    template<typename T1>
    point_set(const vector<pt<T1>>& mas) {
        for (auto t : mas) {
            push_back(pt<T>(static_cast<T>(t.x), static_cast<T>(t.y)));
        }
    }

    //размер
    int size() const {
        return pt_set.size();
    }

    //проверка на пустоту
    bool empty() const {
        return pt_set.size() == 0;
    }

    //разворот вектора точек
    void reverse1() {
        reverse(pt_set.begin(), pt_set.end());
        sort_it_coord = false;
        sort_it_ang = false;
    }

    //обращение и изменение конретного элемента
    pt<T>& operator[](int ind) {
        return pt_set[ind];
    }

    const pt<T>& operator[](int ind) const{
        return pt_set[ind];
    }

    //удаление элемента с конца
    void pop_back() {
        if (!pt_set.empty()) pt_set.pop_back();
    }

    //конечный элемент
    pt<T> back() const{
        if (!pt_set.empty()) return pt_set.back();
        return {NAN, NAN};
    }

    //сортировка по координате
    void sort_coord() {
        if (sort_it_coord) return;
        sort(pt_set.begin(), pt_set.end(), comp<T>);
        sort_it_coord = true;
        sort_it_ang = false;
    }

    //сортировка по полярному углу
    void sort_ang() {
        if (sort_it_ang) return;
        vector <pair <dbl, int>> pt_set1;
        for (int i = 0; i < (int) pt_set.size(); ++i) {
            pt_set1.push_back({ang(pt_set[i]), i});
        }
        sort(pt_set1.begin(), pt_set1.end());
        vector<pt<T>> ans;
        for (int i = 0; i < (int) pt_set1.size(); ++i) {
            ans.push_back(pt_set[pt_set1[i].second]);
        }
        pt_set = ans;
        sort_it_ang = true;
        sort_it_coord = false;
    }

    //сортировка по полярному углу относительно точки
    template<typename pt_t>
    void sort_ang(const pt<pt_t>& t) {
        vector <pair <dbl, int>> pt_set1;
        for (int i = 0; i < (int) pt_set.size(); ++i) {   
            pt_set1.push_back({ang(pt_set[i]-t), i});
        }
        sort(pt_set1.begin(), pt_set1.end());
        vector<pt<T>> ans;
        for (int i = 0; i < (int) pt_set1.size(); ++i) {
            ans.push_back(pt_set[pt_set1[i].second]);
        }
        pt_set = ans;
        sort_it_coord = false;
        sort_it_ang = false;
    }

    //сортировка по произвольному компоратору
    template<typename Comp>
    void sort_comp(const Comp& comp) {
        sort(pt_set.begin(), pt_set.end(), comp);
        sort_it_ang = false;
        sort_it_coord = false;
    }

    
    //построение вехней огибающей
    point_set<T> build_up_line() {
        sort_coord();
        point_set<T> up;
        up.push_back(pt_set[0]);
        for (int i = 1; i < (int) pt_set.size(); ++i) {
            while (up.size() > 1 && ccw(up[up.size()-2], up[up.size()-1], pt_set[i])) up.pop_back();
            up.push_back(pt_set[i]);
        }
        return up;
    }

    //построение нижней огибающей
    point_set<T> build_down_line() {
        sort_coord();
        point_set<T> down;
        down.push_back(pt_set[0]);
        for (int i = 1; i < (int) pt_set.size(); ++i) {
            while (down.size() > 1 && cw(down[down.size()-2], down[down.size()-1], pt_set[i])) down.pop_back();
            down.push_back(pt_set[i]);
        }
        return down;
    }

    //построение выпуклой оболочки алгоритмом Эндрю
    point_set<T> build_ch_andrew() {
        auto up = build_up_line();
        auto down = build_down_line();
        auto ch = up;
        down.reverse1();
        for (int i = 1; i < down.size()-1; ++i) {
            ch.push_back(down[i]);
        }
        ch.reverse1();
        return ch;
    }

    //проверка наличия точки в треугольнике
    //не для треугольника вернёт результат по первым трём точкам
    //если меньше 3 точек вернёт false
    template<typename T1>
    bool in(const pt<T1>& t) const{
        if (size() < 3) return false;
        T1 ss = abs(s(pt_set[0], pt_set[1], t))+abs(s(pt_set[0], pt_set[2], t))+abs(s(pt_set[1], pt_set[2], t));
        return abs(ss-abs(s(pt_set[0], pt_set[1], pt_set[2]))) <= eps;
    }

    //проверка наличия точки на треугольнике
    //не для треугольника вернёт результат по первым трём точкам
    //если меньше 3 точек вернёт false
    template<typename T1>
    bool on(const pt<T1>& t) const{
        if (size() < 3) return false;
        return (make_seg(pt_set[0], pt_set[1]).on(t) || make_seg(pt_set[0], pt_set[2]).on(t) || make_seg(pt_set[1], pt_set[2]).on(t));
    }
};

//функция фабрика для множества из точек
template<typename T>
auto make_point_set(const vector<pt<T>>& mas) {
    point_set<T> p_set(mas);
    return p_set;
}

//проверка наличия точки внутри фигуры(точки против часовой)
template<typename T1, typename T2>
bool operator>(const point_set<T1>& pt_set, const pt<T2>& t) {
    if (!ccw(pt_set[0], pt_set[1], t)) return false;
    if (!cw(pt_set[0], pt_set.back(), t)) return false;
    int l = 1, r = pt_set.size()-1;
    while (r-l > 1) {
        int m = (l+r)/2;
        if (ccw(pt_set[0], pt_set[m], t)) {
            l = m;
        } else {
            r = m;
        }
    }
    vector <pt<T1>> mas1 = {pt_set[0], pt_set[l], pt_set[l+1]};
    auto pt_set1 = make_point_set(mas1);
    return pt_set1.in(t);
}

//возвращает треугольник, в который попадает точка при триангуляции
//если никуда не попадает, то возвращает (-1, -1, -1)
template<typename T1, typename T2>
vector <int> triangle_in(const point_set<T1>& pt_set, const pt<T2>& t) {
    if (!ccw(pt_set[0], pt_set[1], t)) return {-1, -1, -1};
    if (!cw(pt_set[0], pt_set.back(), t)) return {-1, -1, -1};
    int l = 1, r = pt_set.size()-1;
    while (r-l > 1) {
        int m = (l+r)/2;
        if (ccw(pt_set[0], pt_set[m], t)) {
            l = m;
        } else {
            r = m;
        }
    }
    vector <pt<T1>> mas1 = {pt_set[0], pt_set[l], pt_set[l+1]};
    auto pt_set1 = make_point_set(mas1);
    if (pt_set1.in(t)) {
        return {0, l, l+1};
    } else {
        return {-1, -1, -1};
    }
}

int main() {}
