package day3

import (
	"bufio"
	"os"
	"testing"
)

func TestPathToWire(t *testing.T) {
	input := "R1,U1,L1,D1"
	expect := wire{
		start: point{0, 0},
		end:   point{0, 0},
		path: wirePath{
			point{0, 0}: struct{}{},
			point{1, 0}: struct{}{},
			point{1, 1}: struct{}{},
			point{0, 1}: struct{}{},
		},
	}

	result := pathToWire(input)
	if result.start != expect.start || result.end != expect.end {
		t.Errorf("Fail, expect %v, got %v", expect, result)
	}

	if len(result.path) != len(expect.path) {
		t.Errorf("Fail, expect %v, got %v", expect, result)
	}
	for k, v := range result.path {
		if v != expect.path[k] {
			t.Errorf("Fail, expect %v, got %v", expect, result)
		}
	}
}

func TestCrossing(t *testing.T) {
	w1 := pathToWire("R1,U1,L1,D1")
	w2 := pathToWire("D1,R2,U2,L2")

	expect := map[point]struct{}{
		point{0, 1}: struct{}{},
		point{1, 1}: struct{}{},
	}

	result := crossing(w1, w2)
	if len(result) != len(expect) {
		t.Errorf("Fail, expect %v, got %v", expect, result)
	}
	for _, v := range result {
		if _, ok := expect[v]; !ok {
			t.Errorf("Fail, expect %v, got %v", expect, result)
		}
	}
}

func TestManhattanDistance(t *testing.T) {
	input := []struct {
		paths    []string
		distance uint
	}{
		{[]string{"R8,U5,L5,D3", "U7,R6,D4,L4"}, 6},
		{[]string{"R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"}, 159},
		{[]string{"R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"}, 135},
	}

	for _, in := range input {
		result := manhattanDistance(in.paths[0], in.paths[1])
		if in.distance != result {
			t.Errorf("Fail, expect %v, got %v", in.distance, result)
		}
	}
}

func TestDay3(t *testing.T) {
	file, err := os.Open("input")
	if err != nil {
		t.Fatalf("Fail %v", err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var paths []string
	for scanner.Scan() {
		paths = append(paths, scanner.Text())
	}
	if err = scanner.Err(); err != nil {
		t.Fatalf("Fail %v", err)
	}

	result := manhattanDistance(paths[0], paths[1])
	// Prints only when the run with -v flag
	t.Logf("day3, part 1 answer is %v", result)
}
