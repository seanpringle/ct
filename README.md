# Ct

Ct is an experimental language that:

* Transpiles to C11
* Is C-like and allows C code blocks inline
* Uses [structural typing](https://en.wikipedia.org/wiki/Structural_type_system) and interfaces
* Has three-tier memory management: stack, [region](https://en.wikipedia.org/wiki/Region-based_memory_management) and heap
* Has namespaces; safe(r) strings; bounds checking; no pointer arithmetic; no semicolons; threads

Ct is influenced by Go and [Cyclone](https://en.wikipedia.org/wiki/Cyclone_%28programming_language%29). Ct is nothing like C++ except that language also happened to start life as a transpiler.

# Example

```C
struct vec2 {
	double x
	double y
}

struct vec3 {
	double x
	double y
	double z
}

void add(vec2 v, double x, y) {
	v.x = v.x + x
	v.y = v.y + y
}

void dostuff() {
	vec3 v3 = { x = 1, y = 2, z = 3 }

	// works because vec3 is implicitly also a vec2
	add(v3, 1, 1)
}
```

# Under the hood

A Ct struct definition generates two C struct types:

* A concrete type, which is a C struct with the same fields
* An interface type, which is a C struct with field pointers

Consider the `vec2` above:

```C
struct vec2 {
	double x
	double y
}
```

`vec2` transpiles to:

```C
struct vec2_concrete {
	double x;
	double y;
};

typedef struct {
	double *x;
	double *y;
} vec2;
```

`add` transpiles to:

```C
void add(vec2 v, double x, double y) {
	*v.x = *v.x + x;
	*v.y = *v.y + y;
}
```

The call to `add()` that passes a `vec3` transpiles to:

```C
add((vec2){ &v3.x, &v3.y }, 1, 1);
```

Any other concrete type that contains `x` and `y` fields with type `double` can also be passed to `add()`.

That's about it. In Ct "interfaces" are really just syntactic sugar for passing around sets of pointers.

# Embedding (composition)

Embedding a struct within a struct works like Go, and is handy to ensure interfaces stay in sync. `vec3` could have been defined like:

```C
struct vec3 {
	embed vec2
	double z
}
```

`embed` causes the `vec2` fields to be duplicated in `vec3`. Go would say the fields are *promoted*. Worth noting that C11, and for years GCC with `-fms-extensions`, can do similar things with anonymous and/or embedded structs. Ct does it at the transpile stage in order to gather enough information to generate interface types.

# Performance

Interfaces are passed by value on the stack and incur a pointer's worth of overhead for each field. Consider it incentive to keep interfaces small and focused. It's fine to create a bunch of small structs to be lean (sub)interfaces to a larger struct.

Interfaces imply a layer of indirection accessing structs, but no more than passing a any C pointer around. Probably a net loss though when a C program might just pass around the smaller concrete structs by value instead.

There's no run-time like in Go. Ct interfaces are compile-time constructs which the C compiler is free to optimize away.

# Memory management

Three tiers:

* stack
* [region](https://en.wikipedia.org/wiki/Region-based_memory_management)
* heap

Regions are something like a simple form of manual garbage collection. Variables allocated in a `region` are considered temporary and persist until the whole region is released. There's no need to manually `free` stuff.

```C
vec2 dostuff() {

	// local variable on the stack
	vec2 alpha = { x = 1, y = 2 }

	// temporary variable in the current region
	temp vec2 beta = { x = 3, y = 4 }

	// heap allocation
	heap vec2 gamma = { x = 5, y = 6 }

	free(vec2)

	return beta
}
```

`beta` persists in a temporary scratch space after `dostuff` returns. Somewhere further up the call stack would be a `region` block that determines the life time of temporary allocations:

```C
void callsStuff() {

	...

	region {
		vec2 beta = doStuff()
		moreStuff(beta)
		yetMoreStuff(beta)
	}

	// beta has been freed

	...
}
```

Region blocks:

* can be nested
* are per-thread
* guarantee allocation alignment

A default region is created when a thread starts, and freed when it stops.

If a program has a main loop, that's probably a good spot for a catch-all region.

# Inline C code

Double-braced blocks are passed to the C compiler verbatim:

```C
void callSomeRawC(vec2 v2) {{
	fprintf(stderr, "%f,%f\n", *v2.x, *v2.y);
	void *footgun = malloc(...);
	...
}}
```

# What's the point?

There has to be a point?
