// WaterBucket.cpp : Defines the entry point for the console application.
//

#include <set>
#include <vector>
#include <iostream>
#include <limits>
using namespace std;
struct state{ int x, y, z; };
bool operator<(const state& a, const state& b)
{
	return  (a.x < b.x) ||
		((a.x == b.x) &&
		((a.y < b.y) ||
		(a.y == b.y && a.z < b.z)));
};
bool operator==(const state& a, const state& b)
{
	return a.x == b.x && a.y == b.y && a.z == b.z;
}
typedef int state::*B;
struct pourAction { B source, destination; };
const B X = &state::x;
const B Y = &state::y;
const B Z = &state::z;
const vector<pourAction> allowed{ { X, Y }, { X, Z }, { Y, X }, { Y, Z }, { Z, X }, { Z, Y } };

const state start{ 12, 0, 0 };
const state goal{ 6, 6, 0 };
const state capacity{ 12, 8, 5 };
int min(int x, int y) { return x < y ? x : y; }
state DoPour(pourAction const& p, state const& cur)
{
	if (cur.*(p.source) == 0 || cur.*(p.destination) == capacity.*(p.destination))
		return cur;
	else
	{
		state next(cur);
		int k = min(cur.*(p.source), capacity.*(p.destination) - cur.*(p.destination));
		next.*(p.source) -= k;
		next.*(p.destination) += k;
		return next;
	}
}
set<state> seen;

bool Solve(const state& current)
{
	for (auto &p : allowed)
	{
		if (current == goal)
		{
			return true;
		}
		else
		{
			state next = DoPour(p, current);
			if (seen.find(next) != seen.end()) 
				continue;
			else
			{
				seen.insert(next);
				if (Solve(next))
				{
					cout << "state " << current.x << ", " << current.y << ", " << current.z << endl;
					return true;
				}
			}

		}
	}
	return false;
}

int main(int argc, char* argv[])
{

	if (Solve(start))
		cout << "Done" << endl;
	else
		cout << "No Solution" << endl;
	return 0;
}

