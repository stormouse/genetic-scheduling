#include <vector>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <map>
#include <random>
#include <exception>
#include <ctime>
using namespace std;

const int NO_CLASS = -1;

struct Course {
	int id;				// for reverse locating
	int time_per_week;
	vector<int> teachers;
};

struct Room {
	int id;
	int capacity;
};

struct Dept {
	int id;
	int n_students;
	vector<Course*> courses;
};

struct Klass {
	int time;
	int teacher;
	Dept *dept;
	Course *course;
	Room *room;
	bool cause_conflict;
};

typedef vector<Klass> Schedule;
typedef vector<Schedule> Population;


// global parameters

int n_classes_day = 6;
int n_days_week = 7;
int n_classes_week = n_days_week * n_classes_day; // = n_classes_day * n_days_week
int n_classes_in_schedule; // best_sched.size()

map<int, Klass> classes;
map<int, Dept> depts;
map<int, Course> courses;
map<int, Room> rooms;


// functions

std::default_random_engine generator;
std::uniform_real_distribution<double> distribution(0.0, 1.0);


int randint(int min, int max) { return (int)floor(distribution(generator) * (max - min)) + min; }
double randreal(double min, double max) { return distribution(generator) * (max - min) + min; }
//double dice() { double d = randreal(0.0, 1.0); if (d >= 1.0) cout << "dice error: " << d << endl; return d; }
double dice() { return 1.0 * rand() / RAND_MAX; }

void schedule_copy(Schedule& src, Schedule& dst) {
	if (src.size() != dst.size()) dst.resize(src.size());
	copy(src.begin(), src.end(), dst.begin());
}

void population_copy(Population& src, Population& dst) {
	if (dst.size() != src.size()) dst.resize(src.size());
	int n = src.size();
	for (int i = 0; i < n; i++)
		schedule_copy(src[i], dst[i]);
}

int32_t hash_schedule(const Schedule& s) {
	int32_t hash = 7;
	for (auto k : s) {
		hash *= (1 + k.course->id + k.dept->id + k.teacher + k.time);
		hash = hash % 127;
	}
	return hash;
}

int tc, rc, dc;
int btc, brc, bdc;

double fitness(Schedule& sched) {
	double conflict_weight = 100.0;
	double glitches_weight = 1.0;
	int n_conflicts = 0;
	int n_glitches = 0;

	int teacher_conflict = 0, room_conflict = 0, dept_conflict = 0;
	for (auto it = sched.begin(); it != sched.end(); it++) 
		it->cause_conflict = false;

	for (auto it = sched.begin(); it != sched.end(); it++) {
		
		// insufficient room capacity
		if (it->room->capacity < it->dept->n_students) n_conflicts++;

		for (auto jt = it+1; jt != sched.end(); jt++) {

			// spacial conflict
			if (it->time == jt->time && it->room == jt->room) {
				it->cause_conflict = true;
				n_conflicts++; room_conflict++;
			}
			// teacher dismemberment conflict
			if (it->time == jt->time && it->teacher == jt->teacher) {
				it->cause_conflict = true;
				n_conflicts++; teacher_conflict++;
			}
			// dept dismemberment conflict
			if (it->time == jt->time && it->dept == jt->dept) {
				it->cause_conflict = true;
				n_conflicts++; dept_conflict++;
			}
			
			// same dept, same course but different teacher
			if (it->dept == jt->dept && it->course == jt->course && it->teacher != jt->teacher) {
				it->cause_conflict = true;
				n_glitches++;
			}
			// same dept, same courses too close to each other (in time manner)
			if (it->dept == jt->dept && it->course == jt->course && abs(it->time - jt->time) < n_classes_day / 2) {
				it->cause_conflict = true;
				n_glitches++;
			}

		}
	}
	tc = teacher_conflict; rc = room_conflict; dc = dept_conflict;
	return -(conflict_weight * n_conflicts + glitches_weight * n_glitches);
}


Schedule gen_schedule() {
	Schedule schedule;
	for (auto dpair : depts) {
		Dept d = dpair.second;
		for (Course* c : d.courses) {
			for (int _ = 0; _ < c->time_per_week; _++) {
				Klass k;
				k.course = c;
				k.dept = &depts[d.id];// &d
				k.room = &rooms[randint(0, rooms.size())];
				k.teacher = c->teachers[randint(0, c->teachers.size())];
				k.time = randint(0, n_classes_week);
				schedule.push_back(k);
			}
		}
	}

	return schedule;
}


// genetic parameters

double			best_fitness;
Schedule		best_sched;
Population		pop;
vector<double>	pop_fitness;
Population		next_pop;

int pop_size = 20;
int select_n = 4;
double mutate_rate = 1e-4;
double mutate_rate_conflict = 0.98;	// if a lot of blank cell, increase mutate_rate_conflict; else increase mutate_rate
double crossover_rate = 0.5;
double mutate_decay = 1;

// annealing parameters
double init_temperature = 0.08;
double current_temperature = init_temperature;

double temperature() {
	current_temperature -= 1e-3;
	return current_temperature;
}

void initialize() {
	pop.clear();
	pop_fitness.resize(pop_size);
	
	int best_i = 0;
	for (int i = 0; i < pop_size; i++) {
		Schedule s = gen_schedule();
		pop.push_back(s);
		
		pop_fitness[i] = fitness(s);
		if (pop_fitness[i] > pop_fitness[best_i]) {
			best_i = i;
		}
	}

	best_fitness = pop_fitness[best_i];
	n_classes_in_schedule = pop[best_i].size();
	best_sched.clear();
	best_sched.resize(n_classes_in_schedule);
	schedule_copy(pop[best_i], best_sched);

	fitness(best_sched);
	btc = tc; brc = rc; bdc = dc;
}

vector<Schedule*> select() {
	// zip schedule and fitness
	struct zip { Schedule* schedule; double fitness; };
	auto cmp = [](const zip& a, const zip& b) { return a.fitness > b.fitness; };
	
	vector<zip> zips;
	for (int i = 0; i < pop_size; i++) {
		zip z = { &pop[i], pop_fitness[i] };
		zips.push_back(z);
	}

	// sort individuals by fitness
	sort(zips.begin(), zips.end(), cmp);

	vector<Schedule*> winners;
	int sa = 0;
	if (zips[0].fitness > best_fitness) {
		schedule_copy(*(zips[0].schedule), best_sched);
		best_fitness = zips[0].fitness;
		fitness(best_sched);
		btc = tc; brc = rc; bdc = dc;
	}
	// else sa = dice() < temperature() ? 1 : 0;	// SA: discard the best individual with probability P
	for (int i = 0; i < select_n; i++)
		winners.push_back(zips[i + sa].schedule);
	
	return winners;
}


Schedule crossover(const Schedule& s1, const Schedule& s2) {
	Schedule s; s.resize(n_classes_in_schedule);
	for (int i = 0; i < n_classes_in_schedule; i++)
		s[i] = randreal(0.0, 1.0) < 0.5 ? s1[i] : s2[i];
	return s;
}


void mutate(Schedule& s) {
	for (int i = 0; i < n_classes_in_schedule; i++) {
		double rate = s[i].cause_conflict ? mutate_rate_conflict : mutate_rate;
		if (dice() < rate) s[i].room = &rooms[randint(0, rooms.size())];
		if (dice() < rate) s[i].teacher = s[i].course->teachers[randint(0, s[i].course->teachers.size())];
		if (dice() < rate) s[i].time = randint(0, n_classes_week);
	}
}


void evolve() {

	double last_best = best_fitness;

	// mutate some of the individuals
	next_pop.clear();
	next_pop.resize(select_n);

	// select best n schedules(individuals) and add to new generation
	auto candidates = select();
	for (int i = 0; i < select_n; i++) {
		next_pop[i].resize(n_classes_in_schedule);
		Schedule *t = candidates[i];
		schedule_copy(*candidates[i], next_pop[i]);
	}


	// crossover (pop_size - n) times to generate next population with size = pop_size
	for (int i = select_n; i < pop_size; i++) {
		if (dice() < crossover_rate) {
			int a = randint(0, select_n); int b = randint(a + 1, a + select_n - 1) % select_n;
			next_pop.push_back(crossover(*candidates[a], *candidates[b]));
		}
		else {
			Schedule tmp(n_classes_in_schedule);
			schedule_copy(next_pop[0], tmp);
			next_pop.push_back(tmp);
		}
	}

	// mutate
	for_each(next_pop.begin()+select_n, next_pop.end(), mutate);

	// DEBUG PRINT
	/*cout << "[DBG]next_pop: "; int tct = 0;
	for (auto s : next_pop) { tct++; cout << hash_schedule(s); if (tct < pop_size) cout << "-"; }
	cout << endl;*/

	// pop = next_pop
	population_copy(next_pop, pop);

	// new round
	for (int i = 0; i < pop_size; i++) 
		pop_fitness[i] = fitness(pop[i]);

	// mutate rate decrease
	// if(mutate_rate > 1e-3)
	mutate_rate *= mutate_decay;
}


void run(int n_iterations, bool show_progress=false) {
	//for (int t = 0; t < n_iterations; t++) {
	//	evolve();
	//	if (show_progress) {
	//		printf_s("Iteration %d: best fitness %.3lf; Conflicts: TC %d, RC %d, DC %d\n", t + 1, best_fitness, btc, brc, bdc);
	//	}
	//}
	double threshold = -1.0;
	int t = 0;
	if (threshold < -1e-8)
	{
		while (best_fitness < threshold)
		{
			evolve();
			if (show_progress && ((t+1)%1==0)) {
				printf_s("Iteration %d: best fitness %.3lf; Conflicts: TC %d, RC %d, DC %d\n", t + 1, best_fitness, btc, brc, bdc);
			}
			t++;
		}
	}

}

struct KlassInstance {
	int dept;
	int teacher;
	int course;
	int room;
};

typedef vector< vector < KlassInstance > > Timetable;


Timetable fill_table(Schedule& schedule) {
	Timetable table_for_depts;
	int n_dept = depts.size();
	
	table_for_depts.resize(n_dept);

	for (int d = 0; d < n_dept; d++)
	{
		table_for_depts[d].resize(n_classes_week);
		for (int t = 0; t < n_classes_week; t++)
			table_for_depts[d][t] = { d, NO_CLASS, NO_CLASS, NO_CLASS };
	}

	for (Klass k : schedule) {
		int tow = k.time;
		int dept = k.dept->id;
		int teacher = k.teacher;
		int course = k.course->id;
		int room = k.room->id;
		table_for_depts[dept][tow] = { dept, teacher, course, room };
	}

	return table_for_depts;
}


void print_table(Timetable& table) {
	int n = depts.size();
	int w = n_days_week, h = n_classes_day;
	for (int i = 0; i < n; i++) {
		cout << "\n\n---------- Class: " << i + 1 << "-----------\n" << endl;
		cout << "\tMon\tTue\tWed\tThu\tFri\tSat\tSun" << endl;
		for (int y = 0; y < h; y++) {
			cout << y + 1;
			for (int x = 0; x < w; x++) {
				auto inst = table[i][x * h + y];
				if (inst.course == NO_CLASS) cout << "\t" << "_";
				else cout << "\t" << inst.course;
			}
			cout << endl;
		}
	}
}


void read_input() {
	
	rooms.clear(); courses.clear(); depts.clear();

	ifstream *in;

	// room information
	in = new ifstream("classroom_info", ios::in);
	int room_id, capa;
	while (in->peek() != EOF) {
		*in >> room_id >> capa;
		rooms[room_id] = { room_id, capa };
	}
	in->close(); delete in;
	
	// course information
	in = new ifstream("course_info", ios::in);
	int course_id, teacher_id, time_per_week;
	while (in->peek() != EOF) {
		*in >> course_id >> teacher_id >> time_per_week;
		if (courses.find(course_id) == courses.end()) {
			courses[course_id].id = course_id;
			courses[course_id].time_per_week = time_per_week;
			courses[course_id].teachers.clear();
		}
		courses[course_id].teachers.push_back(teacher_id);
	}
	in->close(); delete in;


	// department information
	in = new ifstream("dept_info", ios::in);
	int dept_id, n_students; //, course_id
	while (in->peek() != EOF) {
		*in >> dept_id >> n_students >> course_id;
		if (depts.find(dept_id) == depts.end()) { 
			depts[dept_id].id = dept_id;
			depts[dept_id].n_students = n_students;
			depts[dept_id].courses.clear();
		}
		depts[dept_id].courses.push_back(&courses[course_id]);
	}
	in->close(); delete in;

	cout << "Check data >" << endl;
	cout << "# departments: " << depts.size() << endl;
	cout << "# courses: " << courses.size() << endl;
	cout << "# rooms: " << rooms.size() << endl;
}

void run_scheduling() {
	time_t start = time(NULL);
	run(50, true);
	cout << "timeit: " << time(NULL) - start << " seconds"<<endl;
	Timetable table = fill_table(best_sched);
	print_table(table);
}

Schedule load_schedule(const string& filename) {
	Schedule schedule;
	map<int, map<int, int> > dept_course_count;
	dept_course_count.clear();

	for (auto dpair : depts) {
		Dept d = dpair.second;
		if (dept_course_count.find(d.id) == dept_course_count.end())
			dept_course_count[d.id] = map<int, int>();
		for (Course *c : d.courses) {
			dept_course_count[d.id][c->id] = c->time_per_week;
		}
	}

	fstream in(filename, ios::in);

	int line_count = 0;

	int dept_id, time, course_id, teacher_id, room_id;
	while (in.peek()!=EOF) {
		in >> dept_id >> time >> course_id >> teacher_id >> room_id;
		Klass k;
		k.course = &courses[course_id];
		k.dept = &depts[dept_id];
		k.room = &rooms[room_id];
		k.teacher = teacher_id;
		k.time = time;
		schedule.push_back(k);

		dept_course_count[dept_id][course_id] --;
		if (dept_course_count[dept_id][course_id] < 0)
			cout << "ERROR ON DEPT " << dept_id << " COURSE " << course_id << endl;
		line_count++;
	}

	cout << line_count << "/";
	

	for (auto dpair : depts) {
		Dept d = dpair.second;
		for (Course* c : d.courses) {
			for (int _ = 0; _ < dept_course_count[d.id][c->id]; _++) {
				Klass k;
				k.course = c;
				k.dept = &depts[d.id];// &d
				k.room = &rooms[randint(0, rooms.size())];
				k.teacher = c->teachers[randint(0, c->teachers.size())];
				k.time = randint(0, n_classes_week);
				schedule.push_back(k);
				line_count++;
			}
		}
	}

	cout << line_count << " class units are filled in." << endl;

	return schedule;
}

void load_and_initialize(const string& filename) {
	pop.clear();
	pop_fitness.resize(pop_size);

	int best_i = 0;
	for (int i = 0; i < pop_size; i++) {
		Schedule s;
		if (i == 0) s = load_schedule(filename);
		else s = gen_schedule();
		pop.push_back(s);

		pop_fitness[i] = fitness(s);
		if (pop_fitness[i] > pop_fitness[best_i]) {
			best_i = i;
		}
	}

	best_fitness = pop_fitness[best_i];
	n_classes_in_schedule = pop[best_i].size();
	best_sched.clear();
	best_sched.resize(n_classes_in_schedule);
	schedule_copy(pop[best_i], best_sched);

	fitness(best_sched);
	btc = tc; brc = rc; bdc = dc;
}

//int main(int argc, char** argv) {
int main() {
	generator.seed(6);
	read_input();
	//initialize();
	load_and_initialize("init.txt");
	run_scheduling();
	system("pause");
	return 0;
}

