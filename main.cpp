#include <iostream>
#include <fstream>
#include <cstdio>
#include <cstring>
#include <cmath>
#include <algorithm>
#include <map>
#define clr(x) memset(x, 0, sizeof(x))
using namespace std;

const int maxn = 1005;
const double pi = acos(-1);
ofstream os;

struct Node {
	int id, realid;
	double x, y;
	Node() { id = 0; realid = 0; x = 0; y = 0; };
};

bool cmp(Node n1, Node n2) {
	return n1.id < n2.id;
}

int eps(double x) {
	if(fabs(x) < 1e-4) return 0;
	if(x < 0) return -1;
	return 1;
}

bool cmp2(Node p1,Node p2)
{
    if(eps(p1.x-p2.x) != 0)
		return eps(p1.x - p2.x) < 0;
    return eps(p1.y - p2.y) < 0;
}

struct Edge {
	int to, next;
};

class Loop {
public:

	int n;
	Node init[maxn], point[maxn];
	map<int, int> id_map;
	map<string, int>ismap;
	Edge e[maxn * maxn];
	int mat[maxn][maxn];
	int e_tot;
	int head[maxn];
	int id_cnt;

	void Init(int n) {
		this -> n = n;
		id_map.clear();
		ismap.clear();
		id_cnt = 1;
		clr(head);
		clr(mat);
		e_tot = 1;
	}

	double cross(Node p0, Node p1, Node p2) {
		return (p1.x - p0.x)*(p2.y - p0.y) - (p2.x - p0.x)*(p1.y - p0.y);
	}

	//判断Q点是否在线段p1,p2上
	bool check_online(Node p1, Node p2, Node Q) {
		if(eps(cross(p1, Q, p2)) == 0 &&
		   eps(Q.x - min(p1.x, p2.x)) >= 0 && eps(Q.x - max(p1.x, p2.x)) <= 0 &&
		   eps(Q.y - min(p1.y, p2.y)) >= 0 && eps(Q.y - max(p1.y, p2.y)) <= 0 )
			return true;
		return false;
	}

	//判断线段 a, b  线段 c,d  是否相交
	bool check_two_line(Node a,Node b, Node c, Node d) {
		double h, i, j, k;
		h = cross(a, b, c);
		i = cross(a, b, d);
		j = cross(c, d, a);
		k = cross(c, d, b);
		return eps(h * i) <= 0 && eps(j * k) <= 0;
	}


    //判断其他点是否在环内部
	bool checkcha(Node a[], int cnt) {

		bool vis[maxn] = { 0 };
		for(int i = 0; i < cnt; i++) {
			vis[a[i].id] = 1;
		}
        Node P, Q, p1, p2;
		for(int i = 1; i <= n; i++) {
			if(vis[init[i].id]) continue;
			Q = init[i];
			P.x = 1001; P.y = Q.y;
			int sum = 0;
			for(int j = 0; j < cnt; j++) {
				p1 = a[j], p2 = a[(j + 1) % cnt];
				if(check_online(p1, p2, Q)) {
					return false;
				}
				if(eps(p1.y - p2.y) == 0) continue;
				if(eps(Q.y - min(p1.y, p2.y)) > 0 && eps(Q.y - max(p1.y, p2.y)) <= 0) {
					if(check_two_line(Q, P, p1, p2))
						sum++;
				}
			}
			if(sum % 2 == 1) {
				return false;
			}
        }
		return true;
    }

    //判断Q是否在a[]内部
	bool dian(Node a[], int cnt, Node Q) {
        Node P, p1, p2;
        P.x = 1001, P.y = Q.y;
		int sum = 0;
		for(int j = 0; j < cnt; j++) {
			p1 = point[j], p2 = point[(j + 1) % cnt];
			if(check_online(p1, p2, Q)) {
				return true;
			}
			if(p1.y == p2.y) continue;
			if(Q.y > min(p1.y, p2.y) && Q.y <= max(p1.y, p2.y)) {
				if(check_two_line(Q, P, p1, p2))
					sum++;
			}
		}
		if(sum % 2 == 1) return true;
		return false;
    }

    //判断a[]这个环内部是否有还有点
	bool checkcha2(Node a[], int cnt) {
		for(int i = 0; i < cnt; i++) {
			for(int j = i + 1; j < cnt; j++) {
				if(i == 0) {
					if(j >= i + 2 && j <= cnt -2) {
						if(mat[a[i].id][a[j].id]) {
							Node n0;
							n0.x = (a[i].x + a[j].x) / 2;
							n0.y = (a[i].y + a[j].y) / 2;
							if(dian(a, cnt, n0)) {
								return false;
							}
						}
					}
				} else {
					if(j >= i + 2 && j <= cnt - 1) {
						if(mat[a[i].id][a[j].id]) {
							Node n0; n0.x = (a[i].x + a[j].x) / 2; n0.y = (a[i].y + a[j].y) / 2;
							if(dian(a, cnt, n0)) {
								return false;
							}
						}
					}
				}
			}
		}
		return true;
	}

	//新得到的环是否出现以前出现过
	bool check_map(Node a[], int cnt) {
		if(cnt <= 2) return false;
		Node b[maxn];
		for(int i = 0; i < cnt; i++) {
			b[i] = a[i];
		}
		sort(b, b + cnt, cmp);
		string s;
		for(int i = 0; i < cnt; i++) {
			char str[10];
			sprintf(str, "%d", b[i].id);
			s += str;
		}
		s += '\0';
		if(ismap[s]) {
			return false;
		} else {
			return true;
		}
	}

	void add_map(Node a[], int cnt) {
		Node b[maxn];
		for(int i = 0; i < cnt; i++) {
			b[i] = a[i];
		}
		sort(b, b + cnt, cmp);
		string s;
		for(int i = 0; i < cnt; i++) {
			char str[10];
			sprintf(str, "%d", b[i].id);
			s += str;
		}
		s += '\0';
		ismap[s] = true;
	}

	double C(Node p0, Node p1, Node p2) {
		double x1 = p1.x - p0.x; double y1 = p1.y - p0.y;
		double x2 = p2.x - p0.x; double y2 = p2.y - p0.y;
		double a = sqrt( x1*x1 + y1*y1 );
		double b = sqrt( x2*x2 + y2*y2 );
		double ab = x1*x2 + y1*y2;
		double caob = ab / ( a * b );
		double jiaodu = acos(caob);
		return jiaodu / pi * 180 + 1e-6;
	}

	void Add(int u, int v) {
		e[e_tot].to = v;
		e[e_tot].next = head[u];
		head[u] = e_tot++;
	}

	void solve() {
		for(int i = 1; i <= n; i++) {
			for(int j = 1; j <= n; j++) {
				if(i == j || !mat[i][j])  continue;  //mat为邻接矩阵
				bool vis[maxn] = { 0 };
				int cnt = 0;
				vis[i] = vis[j] = 1;
				point[cnt++] = init[i];
				point[cnt++] = init[j];
				int num = n;
				while(num --) {
					double ang1 = 181;
					int id_num1 = n + 1;
					int now = point[cnt - 1].id;
				//	printf("now = %d  point[cnt - 1].id = %d   point[0].id = %d\n", now, point[cnt - 1].id ,point[0].id);
					for(int k = head[now]; k; k = e[k].next) {
						int ne = e[k].to;
						if(vis[ne] && ne != point[0].id) {
                            continue;
						}
						double a1 = cross(point[cnt - 1], point[cnt - 2], init[ne]);

						if(eps(a1) > 0) {
							double a2 = C(point[cnt - 1], point[cnt - 2], init[ne]);
							if(ang1 > a2) {
								ang1 = a2;
								id_num1 = ne;
							}
						}
					}
					if(id_num1 <= n) {
						point[cnt++] = init[id_num1];
						vis[id_num1] = 1;
					} else {
						double ang2 = -1;
						int id_num2 = n + 1;
						for(int k = head[now]; k; k = e[k].next) {
							int ne = e[k].to;
							if(vis[ne] && ne != point[0].id) continue;
							double a1 = cross(point[cnt - 1], point[cnt - 2], init[ne]);
							if(eps(a1) < 0) {
								double a2 = C(point[cnt - 1], point[cnt - 2], init[ne]);
								if(ang2 < a2) {
									ang2 = a2;
									id_num2 = ne;
								}
							}
						}
						if(id_num2 <= n) {
							point[cnt++] = init[id_num2];
							vis[id_num2] = 1;
						} else {
							double ang2 = -1;
							int id_num2 = n + 1;
							for(int k = head[now]; k; k = e[k].next) {
								int ne = e[k].to;
								if(vis[ne] && ne != point[0].id) continue;
								double a1 = cross(point[cnt - 1], point[cnt - 2], init[ne]);
								if(eps(a1) == 0) {
									id_num2 = ne;
								}
							}
							if(id_num2 <= n) {
								point[cnt++] = init[id_num2];
								vis[id_num2] = 1;
							}
						}
					}
					if(cnt >= 2 && (point[cnt - 1].id == point[0].id)) {
					    if(check_map(point, cnt - 1) && checkcha(point, cnt - 1) && checkcha2(point, cnt - 1)) {
                            for(int i = 0; i < cnt - 2; i++) {
                                printf("%d ", point[i].realid);
                                os << point[i].realid << " ";
                            }
                            cout << point[cnt - 2].realid;
                            os << point[cnt - 2].realid;
                            os << endl;
                            cout << endl;
                            add_map(point, cnt - 1);
                        }
						break;
					}
				}
			}
		}
	}

	void Input() {
		for(int i = 1; i <= n; i++) {
			scanf("%d %lf %lf",&init[i].realid, &init[i].x, &init[i].y);
			if(!id_map[init[i].realid]) id_map[init[i].realid] = id_cnt++;
			init[i].id = id_map[init[i].realid];
		}
		for(int i = 1; i <= n; i++) {
			int u, v;
			scanf("%d",&u);
			u = id_map[u];
			for(int j = 1; j <= 4; j++) {
				scanf("%d",&v);
				if(v == 0) continue;
				v = id_map[v];
				mat[u][v] = mat[v][u] = 1;
				Add(u, v);
	//			Add(v, u);
			}
		}
	}
};

Loop l;

int main() {
	int n;
	freopen("node1.txt", "r", stdin);
	os.open("result.txt");
	while(EOF != scanf("%d",&n) ) {
        l.Init(n);
		l.Input();
		l.solve();
	}
	os.close();
	return 0;
}
